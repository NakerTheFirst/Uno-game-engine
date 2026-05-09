# inst/app/app.R
# Uno Shiny Dashboard — three tabs:
#   1. Play   : interactive game against bots
#   2. Simulate: Rcpp-powered strategy comparison
#   3. Leaderboard: persistent session leaderboard

library(shiny)
library(R6)
library(methods)
library(ggplot2)
library(DT)
library(dplyr)
library(tibble)
library(purrr)

# ── Load package source (works both from devtools and installed) ─────────────
if (requireNamespace("uno", quietly = TRUE)) {
  # Installed package – classes are available automatically
  library(uno)
} else {
  # Development / source mode – load R files directly
  pkg_root <- tryCatch(
    rprojroot::find_package_root_file(path = dirname(dirname(getwd()))),
    error = function(e) NULL
  )
  r_root <- if (!is.null(pkg_root)) file.path(pkg_root, "R") else
    file.path(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "R")

  `%||%` <- function(a, b) if (is.null(a)) b else a
  for (f in c("01-card.R","02-deck.R","03-player.R","04-game-config.R",
              "05-game.R","06-results.R","07-simulate-r.R","08-simulate-cpp.R")) {
    p <- file.path(r_root, f)
    if (file.exists(p)) source(p)
  }
}

# ── Colour helpers ───────────────────────────────────────────────────────────
COLOUR_CSS <- c(
  red    = "#E53935",
  yellow = "#FDD835",
  green  = "#43A047",
  blue   = "#1E88E5",
  black  = "#212121"
)
COLOUR_FG <- c(red="white", yellow="#212121", green="white", blue="white", black="white")

card_ui <- function(card, idx = NULL, clickable = FALSE) {
  bg  <- COLOUR_CSS[card$colour]
  fg  <- COLOUR_FG[card$colour]
  lbl <- card$label()
  icon_map <- c(
    skip     = "⊘",
    reverse  = "↺",
    draw_two = "+2",
    wild     = "🌈",
    wild_draw_four = "+4"
  )
  icon <- if (card$type == "number") as.character(card$value) else icon_map[card$type]

  btn_id <- if (!is.null(idx)) paste0("play_", idx) else NULL

  tags$div(
    class = if (clickable) "uno-card clickable-card" else "uno-card",
    style = sprintf(
      "background:%s;color:%s;border:3px solid %s;",
      bg, fg,
      if (clickable) "gold" else "rgba(255,255,255,0.4)"
    ),
    `data-idx` = idx,
    if (clickable && !is.null(btn_id))
      actionButton(btn_id, label = NULL, class = "card-btn-overlay")
    else NULL,
    tags$div(class = "card-icon", icon),
    tags$div(class = "card-label", lbl)
  )
}

card_back_ui <- function() {
  tags$div(
    class = "uno-card card-back",
    tags$div(class = "card-icon", "🂠"),
    tags$div(class = "card-label", "UNO")
  )
}

# ── Strategy choice labels ───────────────────────────────────────────────────
STRATEGIES <- c("Random" = "random",
                "Aggressive" = "aggressive",
                "Colour Matcher" = "colour_matcher")

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #1a472a; color: #f0f0f0; font-family: 'Segoe UI', sans-serif; }
      .uno-card {
        display: inline-flex; flex-direction: column; align-items: center;
        justify-content: center; width: 70px; height: 105px;
        border-radius: 10px; margin: 4px; font-weight: bold;
        position: relative; user-select: none; transition: transform .15s;
        box-shadow: 2px 4px 8px rgba(0,0,0,0.5);
        cursor: default;
      }
      .clickable-card { cursor: pointer; }
      .clickable-card:hover { transform: translateY(-8px) scale(1.05); }
      .card-icon { font-size: 1.6rem; line-height:1; }
      .card-label { font-size: 0.58rem; margin-top:4px; text-align:center; }
      .card-back { background: #b71c1c; border: 3px dashed white; }
      .card-btn-overlay {
        position:absolute; top:0; left:0; width:100%; height:100%;
        opacity:0; cursor:pointer; border:none; background:transparent;
      }
      .section-title { color:#ffd700; font-weight:700; margin:12px 0 6px; font-size:1.1rem; }
      .discard-area { display:flex; align-items:center; gap:16px; margin:8px 0; }
      .colour-pill {
        display:inline-block; padding:4px 14px; border-radius:20px;
        font-weight:bold; font-size:0.85rem;
      }
      .hand-area { display:flex; flex-wrap:wrap; gap:4px; margin:8px 0; }
      .game-log {
        background:rgba(0,0,0,0.35); border-radius:8px; padding:10px;
        font-size:0.78rem; max-height:180px; overflow-y:auto;
        font-family:monospace; color:#c8e6c9;
      }
      .panel-card {
        background:rgba(0,0,0,0.3); border-radius:12px; padding:16px; margin-bottom:16px;
      }
      .nav-tabs .nav-link { color:#ffd700 !important; }
      .nav-tabs .nav-link.active { background:#2e7d32 !important; color:#fff !important; }
      .btn-uno { background:#e53935; color:white; font-weight:bold; border:none;
                 border-radius:8px; padding:8px 18px; margin:4px; }
      .btn-uno:hover { background:#c62828; color:white; }
      .btn-draw { background:#1565c0; color:white; font-weight:bold; border:none;
                  border-radius:8px; padding:8px 18px; margin:4px; }
      .btn-draw:hover { background:#0d47a1; color:white; }
      .status-bar {
        background:rgba(255,255,255,0.1); border-radius:8px; padding:8px 14px;
        font-size:0.9rem; margin-bottom:10px;
      }
      h4 { color:#ffd700; }
      .shiny-plot-output { background:transparent !important; }
      select, .form-control { background:#1b5e20 !important; color:#f0f0f0 !important;
                              border-color:#388e3c !important; }
    "))
  ),

  titlePanel(
    tags$span("🃏 UNO Simulator", style="color:#ffd700; font-size:2rem; font-weight:900;")
  ),

  tabsetPanel(
    id = "main_tabs",

    # ────────────────────────── PLAY TAB ──────────────────────────
    tabPanel("🎮 Play",
      fluidRow(
        column(3,
          div(class="panel-card",
            h4("Game Setup"),
            numericInput("n_bots", "Number of bots (1-3):", 1, min=1, max=3),
            lapply(1:3, function(i) {
              conditionalPanel(
                condition = sprintf("input.n_bots >= %d", i),
                selectInput(sprintf("bot%d_strat", i),
                            sprintf("Bot %d strategy:", i),
                            choices = STRATEGIES, selected = "random")
              )
            }),
            checkboxInput("play_stacking", "Allow +2/+4 stacking", FALSE),
            actionButton("start_game", "Start New Game", class="btn-uno",
                         style="width:100%;margin-top:8px;"),
            hr(style="border-color:#388e3c"),
            div(class="status-bar", uiOutput("turn_status"))
          )
        ),
        column(9,
          div(class="panel-card",
            div(class="section-title", "Discard Pile & Active Colour"),
            div(class="discard-area",
              uiOutput("discard_ui"),
              uiOutput("colour_indicator")
            ),
            div(class="section-title", "Bot hands"),
            uiOutput("bot_hands_ui"),
            div(class="section-title", "Your hand — click a card to play it"),
            div(class="hand-area", uiOutput("player_hand_ui")),
            div(style="margin-top:8px;",
              actionButton("draw_action", "Draw Card", class="btn-draw"),
              actionButton("pass_action", "Pass Turn",  class="btn-draw",
                           style="background:#455a64;"),
              uiOutput("wild_colour_ui")
            ),
            div(class="section-title", "Game Log"),
            div(class="game-log", uiOutput("game_log_ui")),
            uiOutput("game_over_ui")
          )
        )
      )
    ),

    # ────────────────────────── SIMULATE TAB ──────────────────────
    tabPanel("📊 Simulate",
      fluidRow(
        column(3,
          div(class="panel-card",
            h4("Simulation Settings"),
            numericInput("sim_n_players", "Players:", 2, min=2, max=4),
            lapply(1:4, function(i) {
              conditionalPanel(
                condition = sprintf("input.sim_n_players >= %d", i),
                selectInput(sprintf("sim_strat%d", i),
                            sprintf("Player %d:", i),
                            choices = STRATEGIES)
              )
            }),
            numericInput("sim_n_games", "Games to simulate:", 1000, min=10, max=50000, step=100),
            checkboxInput("sim_stacking", "Allow stacking", FALSE),
            numericInput("sim_seed", "Seed:", 42, min=1),
            actionButton("run_sim", "Run Simulation ▶", class="btn-uno",
                         style="width:100%;margin-top:8px;"),
            hr(style="border-color:#388e3c"),
            h4("Strategy Comparison"),
            numericInput("study_n", "Games per combo:", 500, min=50, max=10000, step=50),
            actionButton("run_study", "Compare All Strategies", class="btn-uno",
                         style="width:100%;")
          )
        ),
        column(9,
          div(class="panel-card",
            uiOutput("sim_status"),
            fluidRow(
              column(6, plotOutput("win_rate_plot", height="300px")),
              column(6, plotOutput("turns_dist_plot", height="300px"))
            ),
            hr(style="border-color:#388e3c"),
            h4("Raw results (first 200 rows)"),
            DTOutput("sim_table")
          ),
          div(class="panel-card",
            h4("Strategy Comparison Study"),
            uiOutput("study_status"),
            plotOutput("study_plot", height="320px")
          )
        )
      )
    ),

    # ────────────────────────── LEADERBOARD TAB ───────────────────
    tabPanel("🏆 Leaderboard",
      fluidRow(
        column(4,
          div(class="panel-card",
            h4("Session Leaderboard"),
            p("Completed games from the Play tab are recorded here automatically.",
              style="font-size:0.85rem; color:#a5d6a7;"),
            actionButton("clear_lb", "Reset Leaderboard", class="btn-uno",
                         style="background:#6a1a1a;width:100%;"),
            hr(style="border-color:#388e3c"),
            uiOutput("lb_summary_ui")
          )
        ),
        column(8,
          div(class="panel-card",
            h4("Player Rankings"),
            DTOutput("lb_table"),
            hr(style="border-color:#388e3c"),
            h4("Win Rate by Strategy"),
            plotOutput("lb_plot", height="280px")
          )
        )
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Reactive state ──
  rv <- reactiveValues(
    game          = NULL,
    leaderboard   = new_leaderboard(),
    drawn_card    = NULL,   # card drawn this turn (may be played immediately)
    waiting_wild  = FALSE,  # TRUE when we need user to pick a wild colour
    pending_idx   = NULL,   # card index waiting for wild colour
    sim_results   = NULL,
    study_results = NULL,
    game_started  = FALSE
  )

  # ── Start a new game ────────────────────────────────────────────
  observeEvent(input$start_game, {
    n_bots <- input$n_bots
    bot_strats <- vapply(seq_len(n_bots),
                         function(i) input[[sprintf("bot%d_strat", i)]],
                         character(1))
    strategies <- c("human", bot_strats)
    n_players  <- length(strategies)

    cfg <- tryCatch(
      methods::new("GameConfig",
        n_players  = as.integer(n_players),
        hand_size  = 7L,
        stacking   = input$play_stacking,
        force_play = TRUE,
        seed       = NA_integer_
      ),
      error = function(e) {
        showNotification(paste("Config error:", e$message), type="error")
        NULL
      }
    )
    if (is.null(cfg)) return()

    specs <- lapply(seq_along(strategies), function(i) {
      nm <- if (i == 1) "You" else sprintf("Bot%d[%s]", i - 1, strategies[i])
      list(name = nm, strategy = strategies[i])
    })

    rv$game         <- Game$new(cfg, specs)
    rv$drawn_card   <- NULL
    rv$waiting_wild <- FALSE
    rv$pending_idx  <- NULL
    rv$game_started <- TRUE

    # Let bots play if they go first (turn order)
    private_auto_bots()
  })

  private_auto_bots <- function() {
    g <- rv$game
    if (is.null(g) || g$is_over()) return()
    while (!g$is_over() && g$current_player()$strategy != "human") {
      g$auto_turn()
    }
    rv$game <- g
  }

  # ── Play a card from hand ────────────────────────────────────────
  observe({
    lapply(1:20, function(i) {
      btn_id <- paste0("play_", i)
      observeEvent(input[[btn_id]], {
        g <- rv$game
        if (is.null(g) || g$is_over()) return()
        p <- g$current_player()
        if (p$strategy != "human") return()
        hand <- p$hand()
        if (i > length(hand)) return()
        card <- hand[[i]]
        if (card$is_wild()) {
          rv$pending_idx  <- i
          rv$waiting_wild <- TRUE
        } else {
          tryCatch({
            g$play_card(i)
            rv$drawn_card   <- NULL
            rv$waiting_wild <- FALSE
            rv$game <- g
            private_auto_bots()
            if (g$is_over()) record_result(g)
          }, error = function(e) {
            showNotification(paste("Illegal play:", e$message), type="warning")
          })
        }
      }, ignoreInit = TRUE)
    })
  })

  # ── Wild colour chosen ──────────────────────────────────────────
  observeEvent(input$choose_wild_colour, {
    g   <- rv$game
    wc  <- input$wild_colour_pick
    idx <- rv$pending_idx
    if (is.null(g) || is.null(idx) || !rv$waiting_wild) return()
    tryCatch({
      g$play_card(idx, wild_colour = wc)
      rv$waiting_wild <- FALSE
      rv$pending_idx  <- NULL
      rv$drawn_card   <- NULL
      rv$game <- g
      private_auto_bots()
      if (g$is_over()) record_result(g)
    }, error = function(e) {
      showNotification(paste("Wild error:", e$message), type="error")
    })
  })

  # ── Draw a card ─────────────────────────────────────────────────
  observeEvent(input$draw_action, {
    g <- rv$game
    if (is.null(g) || g$is_over()) return()
    if (g$current_player()$strategy != "human") return()
    tryCatch({
      drawn <- g$draw_card()
      rv$drawn_card <- drawn
      rv$game <- g
      # Check if drawn card is immediately playable
      p   <- g$current_player()
      top <- g$top_card()
      if (drawn$matches(top, g$active_colour) &&
          (drawn$type != "wild_draw_four" || !p$has_colour(g$active_colour))) {
        showNotification(
          "Card drawn is playable! Click it in your hand, or click Pass.",
          type = "message", duration = 4
        )
      }
    }, error = function(e) {
      showNotification(paste("Draw error:", e$message), type="warning")
    })
  })

  # ── Pass turn ───────────────────────────────────────────────────
  observeEvent(input$pass_action, {
    g <- rv$game
    if (is.null(g) || g$is_over()) return()
    if (g$current_player()$strategy != "human") return()
    g$pass_turn()
    rv$drawn_card <- NULL
    rv$game <- g
    private_auto_bots()
  })

  record_result <- function(g) {
    tryCatch({
      res <- g$result()
      rv$leaderboard <- add_game_result(rv$leaderboard, res)
    }, error = function(e) NULL)
  }

  # ── Clear leaderboard ───────────────────────────────────────────
  observeEvent(input$clear_lb, {
    rv$leaderboard <- new_leaderboard()
  })

  # ── UI outputs: Play tab ────────────────────────────────────────
  output$turn_status <- renderUI({
    g <- rv$game
    if (is.null(g)) return(tags$em("Start a game to begin!", style="color:#a5d6a7;"))
    if (g$is_over()) {
      res <- g$result()
      return(tags$strong(sprintf("🎉 %s wins! (%d turns)", res$winner, res$n_turns),
                         style="color:#ffd700;"))
    }
    p <- g$current_player()
    tags$div(
      tags$strong(sprintf("Turn: %s", p$name), style="color:#80cbc4;"),
      tags$br(),
      tags$span(sprintf("Active colour: %s  |  Deck: %d cards",
                        toupper(g$active_colour), g$deck$size()),
                style="font-size:0.8rem;")
    )
  })

  output$discard_ui <- renderUI({
    g <- rv$game
    if (is.null(g)) return(card_back_ui())
    card_ui(g$top_card())
  })

  output$colour_indicator <- renderUI({
    g <- rv$game
    if (is.null(g)) return(NULL)
    col <- g$active_colour
    tags$div(
      class = "colour-pill",
      style = sprintf("background:%s;color:%s;", COLOUR_CSS[col], COLOUR_FG[col]),
      toupper(col)
    )
  })

  output$bot_hands_ui <- renderUI({
    g <- rv$game
    if (is.null(g)) return(NULL)
    players <- g$players
    lapply(seq_along(players)[-1], function(i) {
      p <- players[[i]]
      tags$div(
        tags$span(sprintf("%s (%d cards):", p$name, p$hand_size()),
                  style="color:#a5d6a7; font-size:0.85rem;"),
        div(class="hand-area",
          lapply(seq_len(p$hand_size()), function(j) card_back_ui())
        )
      )
    })
  })

  output$player_hand_ui <- renderUI({
    g <- rv$game
    if (is.null(g) || g$is_over()) return(NULL)
    human <- g$players[[1]]
    top   <- g$top_card()
    hand  <- human$hand()
    if (length(hand) == 0) return(tags$em("No cards!"))

    legal_idx <- human$legal_indices(top, g$active_colour)
    # Exclude W4 if player has active colour
    legal_idx <- legal_idx[vapply(legal_idx, function(i) {
      c <- hand[[i]]
      if (c$type != "wild_draw_four") return(TRUE)
      !human$has_colour(g$active_colour)
    }, logical(1))]

    lapply(seq_along(hand), function(i) {
      clickable <- (i %in% legal_idx) &&
                   (g$current_player()$strategy == "human") &&
                   !g$is_over()
      card_ui(hand[[i]], idx = i, clickable = clickable)
    })
  })

  output$wild_colour_ui <- renderUI({
    if (!rv$waiting_wild) return(NULL)
    div(
      style="display:inline-flex;align-items:center;gap:8px;margin:6px 0;",
      tags$strong("Choose colour:", style="color:#ffd700;"),
      selectInput("wild_colour_pick", NULL,
                  choices = c("Red"="red","Yellow"="yellow",
                              "Green"="green","Blue"="blue"),
                  width = "120px"),
      actionButton("choose_wild_colour", "Confirm", class="btn-uno")
    )
  })

  output$game_log_ui <- renderUI({
    g <- rv$game
    if (is.null(g)) return(NULL)
    entries <- rev(g$log)
    tags$ul(style="margin:0;padding-left:16px;",
      lapply(entries[1:min(30, length(entries))], function(e) tags$li(e))
    )
  })

  output$game_over_ui <- renderUI({
    g <- rv$game
    if (is.null(g) || !g$is_over()) return(NULL)
    res <- g$result()
    div(style="text-align:center;margin-top:12px;",
      tags$h3(sprintf("🎉 %s wins in %d turns! 🎉", res$winner, res$n_turns),
              style="color:#ffd700;"),
      actionButton("start_game", "Play Again", class="btn-uno")
    )
  })

  # ── Simulation tab ──────────────────────────────────────────────
  observeEvent(input$run_sim, {
    n_p <- input$sim_n_players
    strats <- vapply(seq_len(n_p),
                     function(i) input[[sprintf("sim_strat%d", i)]],
                     character(1))

    withProgress(message = "Running simulation…", value = 0.5, {
      res <- tryCatch(
        simulate_games(
          n         = as.integer(input$sim_n_games),
          strategies = strats,
          stacking  = input$sim_stacking,
          seed      = as.integer(input$sim_seed)
        ),
        error = function(e) {
          showNotification(paste("Simulation error:", e$message), type="error")
          NULL
        }
      )
    })
    rv$sim_results <- res
  })

  output$sim_status <- renderUI({
    res <- rv$sim_results
    if (is.null(res)) return(tags$em("Configure and run a simulation above.",
                                     style="color:#a5d6a7;"))
    n_games <- nrow(res)
    wt <- table(res$winner_strategy)
    tags$div(
      tags$strong(sprintf("Completed %d games.", n_games)),
      tags$span(
        paste(
          sapply(names(wt), function(s) sprintf("%s: %d wins (%.1f%%)",
                                                 s, wt[[s]], 100*wt[[s]]/n_games)),
          collapse = "  |  "),
        style="font-size:0.85rem; color:#a5d6a7; margin-left:8px;"
      )
    )
  })

  output$win_rate_plot <- renderPlot({
    res <- rv$sim_results
    if (is.null(res)) return(NULL)
    wt <- as.data.frame(table(strategy = res$winner_strategy))
    wt$pct <- 100 * wt$Freq / sum(wt$Freq)
    ggplot(wt, aes(x = strategy, y = pct, fill = strategy)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.3,
                colour = "white", fontface = "bold") +
      scale_fill_manual(values = c(random="#1E88E5", aggressive="#E53935",
                                   colour_matcher="#43A047")) +
      labs(title = "Win Rate by Strategy", x = NULL, y = "Win %") +
      theme_minimal(base_size = 13) +
      theme(plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            text = element_text(colour = "white"),
            axis.text = element_text(colour = "#a5d6a7"),
            panel.grid.major = element_line(colour = "rgba(255,255,255,0.1)"),
            panel.grid.minor = element_blank())
  }, bg = "transparent")

  output$turns_dist_plot <- renderPlot({
    res <- rv$sim_results
    if (is.null(res)) return(NULL)
    ggplot(res, aes(x = n_turns, fill = winner_strategy)) +
      geom_histogram(bins = 40, colour = "black", alpha = 0.85) +
      scale_fill_manual(values = c(random="#1E88E5", aggressive="#E53935",
                                   colour_matcher="#43A047"),
                        name = "Winner") +
      labs(title = "Game Length Distribution", x = "Turns", y = "Count") +
      theme_minimal(base_size = 13) +
      theme(plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            text = element_text(colour = "white"),
            axis.text = element_text(colour = "#a5d6a7"),
            panel.grid.major = element_line(colour = "rgba(255,255,255,0.1)"),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill = "transparent"))
  }, bg = "transparent")

  output$sim_table <- renderDT({
    res <- rv$sim_results
    if (is.null(res)) return(NULL)
    datatable(head(res, 200),
              options = list(pageLength = 10, scrollX = TRUE,
                             dom = "tp", initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().node()).css('color','#e0e0e0');",
                               "}")),
              rownames = FALSE,
              class = "cell-border compact")
  })

  # ── Strategy comparison study ────────────────────────────────────
  observeEvent(input$run_study, {
    grid <- list(
      c("random","random"),
      c("aggressive","random"),
      c("colour_matcher","random"),
      c("aggressive","colour_matcher"),
      c("random","aggressive","colour_matcher"),
      c("random","random","aggressive","colour_matcher")
    )
    withProgress(message = "Comparing strategies…", value = 0.5, {
      res <- tryCatch(
        run_simulation_study(grid,
                             n_per_combo = as.integer(input$study_n),
                             seed = 1L),
        error = function(e) {
          showNotification(paste("Study error:", e$message), type="error")
          NULL
        }
      )
    })
    rv$study_results <- res
  })

  output$study_status <- renderUI({
    if (is.null(rv$study_results))
      return(tags$em("Click 'Compare All Strategies' to run a multi-combo study.",
                     style="color:#a5d6a7;"))
    NULL
  })

  output$study_plot <- renderPlot({
    res <- rv$study_results
    if (is.null(res)) return(NULL)

    long <- tidyr::pivot_longer(res,
      cols = starts_with("win_rate_"),
      names_to  = "strategy",
      names_prefix = "win_rate_",
      values_to = "win_rate"
    )
    long$strategy <- factor(long$strategy,
                            levels = c("random","aggressive","colour_matcher"))
    long$combo_label <- sprintf("[%s]", long$strategies)

    ggplot(long, aes(x = combo_label, y = win_rate, fill = strategy)) +
      geom_col(position = "dodge", width = 0.7) +
      scale_fill_manual(values = c(random="#1E88E5", aggressive="#E53935",
                                   colour_matcher="#43A047"),
                        name = "Strategy") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = "Win Rate per Strategy across Combinations",
           x = "Strategy Combo", y = "Win Rate") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, colour = "#a5d6a7"),
            plot.background  = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            text = element_text(colour = "white"),
            axis.text = element_text(colour = "#a5d6a7"),
            panel.grid.major = element_line(colour = "rgba(255,255,255,0.1)"),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill = "transparent"))
  }, bg = "transparent")

  # ── Leaderboard tab ─────────────────────────────────────────────
  output$lb_summary_ui <- renderUI({
    lb <- rv$leaderboard
    n  <- length(lb$games)
    if (n == 0) return(tags$em("No games recorded yet.", style="color:#a5d6a7;"))
    tags$div(
      tags$strong(sprintf("Total games: %d", n)),
      tags$br(),
      tags$span(sprintf("Session started: %s",
                        format(lb$started, "%H:%M:%S")),
                style="font-size:0.8rem;color:#a5d6a7;")
    )
  })

  output$lb_table <- renderDT({
    lb <- rv$leaderboard
    s  <- summary(lb)
    if (nrow(s) == 0) return(datatable(tibble(Message="No games yet")))
    datatable(s,
              options = list(pageLength = 10, dom="tp"),
              rownames = FALSE,
              colnames = c("Player","Games","Wins","Win Rate"),
              class = "cell-border compact") |>
      formatPercentage("win_rate", digits = 1)
  })

  output$lb_plot <- renderPlot({
    lb <- rv$leaderboard
    s  <- summary(lb)
    if (nrow(s) == 0) return(NULL)
    ggplot(s, aes(x = reorder(player, win_rate), y = win_rate, fill = player)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = sprintf("%.0f%%", 100 * win_rate)),
                hjust = -0.1, colour = "white", fontface = "bold") +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.15)) +
      labs(title = "Win Rate by Player", x = NULL, y = "Win Rate") +
      theme_minimal(base_size = 13) +
      theme(plot.background  = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            text = element_text(colour = "white"),
            axis.text = element_text(colour = "#a5d6a7"),
            panel.grid.major = element_line(colour = "rgba(255,255,255,0.1)"),
            panel.grid.minor = element_blank())
  }, bg = "transparent")
}

shinyApp(ui = ui, server = server)
