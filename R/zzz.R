.onAttach <- function(...) {
  assign(".pkgenv", new.env(parent = emptyenv()), envir = .GlobalEnv)
  assign("latex_plot_counter", 1L, envir = .pkgenv)
}
