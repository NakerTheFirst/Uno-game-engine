# tests/testthat/setup.R
# Loads the package source so tests run without devtools::load_all()
# being called manually. When running via devtools::test() this is
# handled automatically; when running interactively, source this first.

library(R6)
library(methods)

# Null-coalescing operator used in game.R
`%||%` <- function(a, b) if (is.null(a)) b else a

# Source all R files in dependency order
r_files <- c(
  "01-card.R", "02-deck.R", "03-player.R",
  "04-game-config.R", "05-game.R",
  "06-results.R", "07-simulate-r.R"
)
pkg_root <- tryCatch(
  rprojroot::find_package_root_file(),
  error = function(e) NULL
)
if (!is.null(pkg_root)) {
  for (f in r_files) {
    src <- file.path(pkg_root, "R", f)
    if (file.exists(src)) source(src)
  }
}
