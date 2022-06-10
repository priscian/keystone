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
