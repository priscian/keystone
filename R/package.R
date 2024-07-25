#' @export
reload_all <- function(
  package_name,
  ...,
  export_all = FALSE,
  redocument = FALSE,
  option_name = "reload_all_package_dirs"
)
{
  currentwd <- getwd()
  switchArgs <- list(
    EXPR = package_name,
    "." # Default option.
  )
  switchArgs <- utils::modifyList(switchArgs, getOption(option_name), keep.null = TRUE)
  packagewd <- do.call(switch, switchArgs)

  devtools::load_all(packagewd, export_all = export_all, ...)
  if (redocument)
    devtools::document(packagewd)

  return (nop())
}

## usage:
# reload_all("climeseries", redocument = TRUE)
# reload_all("keystone", redocument = TRUE)


## Adapted from package "mclust"
#' @export
package_params <- function(..., `__NAMESPACE__`, `__VARNAME__`)
{
  current <- get(`__VARNAME__`, envir = asNamespace(`__NAMESPACE__`))

  if (nargs() == 0)
    return (current)

  args <- list(...)
  if (length(args) == 1 && is.null(names(args))) {
    arg <- args[[1]]
    switch(mode(arg),
      list = args <- arg,
      # character = return (.package_var[[arg]]),
      character =
        return (substitute((`__VAR__`[[arg]]), list(`__VAR__` = as.name(`__VARNAME__`))) %>%
          eval()),
      stop("Invalid argument: ", dQuote(arg))
    )
  }

  if (length(args) == 0)
    return(current)

  argsNames <- names(args)

  if (is.null(argsNames))
    stop("Options must be given by name")

  changed <- current[argsNames]
  current[argsNames] <- args
  assign(`__VARNAME__`, current, envir = asNamespace(`__NAMESPACE__`))

  invisible(current)
}
