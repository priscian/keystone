.onAttach <- function(...)
{
  ## Create globally accessible storage environment that minimally pollutes .GlobalEnv
  assign(".pkgenv", new.env(parent = emptyenv()), envir = .GlobalEnv)
  assign("latex_plot_counter", 1L, envir = .pkgenv)

  ## Prepare parallel processing; set 'options(keystone_parallel = TRUE)' before loading package
  #if ("doFuture" %in% rownames(installed.packages())) {
  if (!is.null(getOption("keystone_parallel")) && getOption("keystone_parallel")) {
    doFuture::registerDoFuture()
    future::plan(future::multisession,
      workers = ifelse(future::availableCores() > 1L, future::availableCores() - 1L, 1L))
  }
}


.onDetach <- function(...)
{
  ## Explicitly close multisession workers
  if (!is.null(getOption("keystone_parallel")) && getOption("keystone_parallel")) {
    # future::plan(strategy = NULL) # ? "default"?
    future::plan(future::sequential)
  }
}
