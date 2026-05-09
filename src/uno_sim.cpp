// uno_sim.cpp
// High-performance Uno simulation in C++. Mirrors the rules in R/05-game.R
// but works on int-encoded cards for speed.
//
// Encoding:
//   colour: 0=red, 1=yellow, 2=green, 3=blue, 4=black (wild)
//   type:   0=number, 1=skip, 2=reverse, 3=draw_two, 4=wild, 5=wild_draw_four
//   value:  0-9 for numbers, -1 otherwise
// A card is a struct { int colour; int value; int type; }.
//
// Strategy codes: 0=random, 1=aggressive, 2=colour_matcher.

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <random>
#include <string>

using namespace Rcpp;

struct UCard {
  int colour;  // 0..4
  int value;   // -1 or 0..9
  int type;    // 0..5
};

static std::vector<UCard> build_deck() {
  std::vector<UCard> d;
  d.reserve(108);
  for (int c = 0; c < 4; ++c) {
    d.push_back({c, 0, 0});                 // one zero
    for (int v = 1; v <= 9; ++v) {           // two of each 1-9
      d.push_back({c, v, 0});
      d.push_back({c, v, 0});
    }
    for (int t : {1, 2, 3}) {                // two of each action
      d.push_back({c, -1, t});
      d.push_back({c, -1, t});
    }
  }
  for (int i = 0; i < 4; ++i) {
    d.push_back({4, -1, 4});                 // wild
    d.push_back({4, -1, 5});                 // wild +4
  }
  return d;
}

static bool is_wild_t(int t) { return t == 4 || t == 5; }

static bool matches(const UCard& c, const UCard& top, int active_colour) {
  if (is_wild_t(c.type)) return true;
  if (c.colour == active_colour) return true;
  if (c.type == 0 && top.type == 0 && c.value == top.value) return true;
  if (c.type != 0 && c.type == top.type) return true;
  return false;
}

static bool has_colour(const std::vector<UCard>& hand, int colour) {
  for (const auto& c : hand)
    if (c.colour == colour && !is_wild_t(c.type)) return true;
  return false;
}

static int pick_dominant_colour(const std::vector<UCard>& hand,
                                std::mt19937& rng) {
  int counts[4] = {0, 0, 0, 0};
  for (const auto& c : hand) {
    if (c.colour < 4) counts[c.colour]++;
  }
  int best = -1, best_n = -1;
  for (int i = 0; i < 4; ++i) {
    if (counts[i] > best_n) { best_n = counts[i]; best = i; }
  }
  if (best_n <= 0) {
    std::uniform_int_distribution<int> u(0, 3);
    return u(rng);
  }
  return best;
}

// Returns the index in `legal` to play.
static int pick_card(int strategy,
                     const std::vector<UCard>& hand,
                     const std::vector<int>& legal,
                     int active_colour,
                     std::mt19937& rng) {
  if (strategy == 0) {                       // random
    std::uniform_int_distribution<int> u(0, (int)legal.size() - 1);
    return legal[u(rng)];
  }
  if (strategy == 1) {                       // aggressive: prefer action cards
    int best = legal[0];
    int best_pri = -1;
    for (int idx : legal) {
      int t = hand[idx].type;
      int pri =
        (t == 5) ? 6 :
        (t == 3) ? 5 :
        (t == 1) ? 4 :
        (t == 2) ? 3 :
        (t == 4) ? 2 : 1;
      if (pri > best_pri) { best_pri = pri; best = idx; }
    }
    return best;
  }
  if (strategy == 2) {                       // colour_matcher
    for (int idx : legal) {
      if (hand[idx].colour == active_colour && !is_wild_t(hand[idx].type))
        return idx;
    }
    std::uniform_int_distribution<int> u(0, (int)legal.size() - 1);
    return legal[u(rng)];
  }
  return legal[0];
}

static void shuffle_vec(std::vector<UCard>& v, std::mt19937& rng) {
  std::shuffle(v.begin(), v.end(), rng);
}

// Reset wild cards to colour 4 when they go back into the draw pile.
static void normalise_wilds(std::vector<UCard>& v) {
  for (auto& c : v) {
    if (is_wild_t(c.type)) c.colour = 4;
  }
}

//' Simulate Uno games (Rcpp).
//'
//' @param n number of games
//' @param strategies integer vector, one per player (0=random, 1=aggressive,
//'   2=colour_matcher)
//' @param hand_size starting hand size
//' @param stacking allow +2/+4 stacking on like-typed cards
//' @param seed RNG seed
//' @return a DataFrame with columns game, winner_idx, winner_strategy,
//'   n_turns, cards_left
//' @export
// [[Rcpp::export]]
DataFrame cpp_simulate_games(int n,
                             IntegerVector strategies,
                             int hand_size = 7,
                             bool stacking = false,
                             int seed = 1) {
  int n_players = strategies.size();
  if (n_players < 2 || n_players > 4)
    stop("strategies must have length 2..4");
  if (hand_size < 1 || hand_size > 20)
    stop("hand_size out of range");

  std::mt19937 rng((unsigned)seed);

  IntegerVector out_game(n);
  IntegerVector out_winner_idx(n);
  IntegerVector out_winner_strategy(n);
  IntegerVector out_n_turns(n);
  IntegerVector out_cards_left(n);

  for (int g = 0; g < n; ++g) {
    std::vector<UCard> deck = build_deck();
    shuffle_vec(deck, rng);

    // Deal
    std::vector<std::vector<UCard>> hands(n_players);
    for (int p = 0; p < n_players; ++p) {
      hands[p].reserve(hand_size + 8);
      for (int k = 0; k < hand_size; ++k) {
        hands[p].push_back(deck.back());
        deck.pop_back();
      }
    }

    // Flip first card; re-flip wild +4
    UCard top;
    while (true) {
      top = deck.back();
      deck.pop_back();
      if (top.type != 5) break;
      // reinsert at random
      std::uniform_int_distribution<int> u(0, (int)deck.size());
      deck.insert(deck.begin() + u(rng), top);
    }
    std::vector<UCard> discard;
    discard.push_back(top);

    int active_colour;
    if (is_wild_t(top.type)) {
      std::uniform_int_distribution<int> u(0, 3);
      active_colour = u(rng);
    } else {
      active_colour = top.colour;
    }

    int current = 0;
    int direction = 1;
    int pending_draw = 0;

    // Initial top effect (skip / reverse / draw two)
    auto draw_one = [&](std::vector<UCard>& target) {
      if (deck.empty()) {
        if (discard.size() <= 1) return;  // pathological; just no-op
        UCard keep = discard.back();
        discard.pop_back();
        deck = std::move(discard);
        normalise_wilds(deck);
        shuffle_vec(deck, rng);
        discard.clear();
        discard.push_back(keep);
      }
      target.push_back(deck.back());
      deck.pop_back();
    };

    auto advance = [&](int steps) {
      int idx = current;
      for (int i = 0; i < steps; ++i) {
        idx = ((idx + direction) % n_players + n_players) % n_players;
      }
      current = idx;
    };

    if (top.type == 1) advance(1);
    else if (top.type == 2) {
      direction = -direction;
      current = (direction == -1) ? n_players - 1 : 0;
    } else if (top.type == 3) {
      for (int k = 0; k < 2; ++k) draw_one(hands[current]);
      advance(1);
    }

    int winner = -1;
    int turns = 0;
    int max_turns = 5000;

    while (winner < 0 && turns < max_turns) {
      ++turns;
      int strat = strategies[current];

      // Handle pending draw stack
      if (pending_draw > 0) {
        const UCard& top_now = discard.back();
        std::vector<int> stackable;
        for (size_t i = 0; i < hands[current].size(); ++i) {
          if (top_now.type == 3 && hands[current][i].type == 3)
            stackable.push_back((int)i);
          if (top_now.type == 5 && hands[current][i].type == 5)
            stackable.push_back((int)i);
        }
        if (stacking && !stackable.empty()) {
          int idx = pick_card(strat, hands[current], stackable, active_colour, rng);
          UCard played = hands[current][idx];
          hands[current].erase(hands[current].begin() + idx);
          discard.push_back(played);
          if (is_wild_t(played.type)) {
            active_colour = pick_dominant_colour(hands[current], rng);
          } else {
            active_colour = played.colour;
          }
          if (played.type == 3) pending_draw += 2;
          else if (played.type == 5) pending_draw += 4;
          if (hands[current].empty()) { winner = current; break; }
          advance(1);
          continue;
        } else {
          for (int k = 0; k < pending_draw; ++k) draw_one(hands[current]);
          pending_draw = 0;
          advance(1);
          continue;
        }
      }

      // Normal turn: find legal cards
      const UCard& top_now = discard.back();
      std::vector<int> legal;
      legal.reserve(hands[current].size());
      for (size_t i = 0; i < hands[current].size(); ++i) {
        if (matches(hands[current][i], top_now, active_colour)) {
          if (hands[current][i].type == 5) {
            // W4 only legal if no card of active colour
            if (!has_colour(hands[current], active_colour)) {
              legal.push_back((int)i);
            }
          } else {
            legal.push_back((int)i);
          }
        }
      }

      if (!legal.empty()) {
        int idx = pick_card(strat, hands[current], legal, active_colour, rng);
        UCard played = hands[current][idx];
        hands[current].erase(hands[current].begin() + idx);
        discard.push_back(played);

        if (is_wild_t(played.type)) {
          active_colour = pick_dominant_colour(hands[current], rng);
        } else {
          active_colour = played.colour;
        }

        if (hands[current].empty()) { winner = current; break; }

        // Apply effect
        switch (played.type) {
          case 0: advance(1); break;
          case 1: advance(2); break;
          case 2:
            direction = -direction;
            if (n_players == 2) advance(2); else advance(1);
            break;
          case 3:
            if (stacking) {
              pending_draw += 2;
              advance(1);
            } else {
              advance(1);
              for (int k = 0; k < 2; ++k) draw_one(hands[current]);
              advance(1);
            }
            break;
          case 4: advance(1); break;
          case 5:
            if (stacking) {
              pending_draw += 4;
              advance(1);
            } else {
              advance(1);
              for (int k = 0; k < 4; ++k) draw_one(hands[current]);
              advance(1);
            }
            break;
        }
      } else {
        // Draw one
        size_t before = hands[current].size();
        draw_one(hands[current]);
        if (hands[current].size() > before) {
          UCard d = hands[current].back();
          bool playable = matches(d, top_now, active_colour);
          if (d.type == 5 && has_colour(hands[current], active_colour)) {
            playable = false;
          }
          if (playable) {
            hands[current].pop_back();
            discard.push_back(d);
            if (is_wild_t(d.type)) {
              active_colour = pick_dominant_colour(hands[current], rng);
            } else {
              active_colour = d.colour;
            }
            if (hands[current].empty()) { winner = current; break; }
            switch (d.type) {
              case 0: advance(1); break;
              case 1: advance(2); break;
              case 2:
                direction = -direction;
                if (n_players == 2) advance(2); else advance(1);
                break;
              case 3:
                if (stacking) { pending_draw += 2; advance(1); }
                else { advance(1); for (int k = 0; k < 2; ++k) draw_one(hands[current]); advance(1); }
                break;
              case 4: advance(1); break;
              case 5:
                if (stacking) { pending_draw += 4; advance(1); }
                else { advance(1); for (int k = 0; k < 4; ++k) draw_one(hands[current]); advance(1); }
                break;
            }
          } else {
            advance(1);
          }
        } else {
          advance(1);
        }
      }
    }

    int total_left = 0;
    for (const auto& h : hands) total_left += (int)h.size();

    out_game[g]              = g + 1;
    out_winner_idx[g]        = (winner < 0) ? NA_INTEGER : winner + 1;
    out_winner_strategy[g]   = (winner < 0) ? NA_INTEGER : strategies[winner];
    out_n_turns[g]           = turns;
    out_cards_left[g]        = total_left;
  }

  return DataFrame::create(
    _["game"]            = out_game,
    _["winner_idx"]      = out_winner_idx,
    _["winner_strategy"] = out_winner_strategy,
    _["n_turns"]         = out_n_turns,
    _["cards_left"]      = out_cards_left,
    _["stringsAsFactors"] = false
  );
}
