#' @export
set_browse_once <- function()
{
  browseOnce <- function() {
    old <- getOption("error")
    function() {
      options(error = old)
      browser()
    }
  }
  #environment(browseOnce) = .GlobalEnv

  options(error = browseOnce())
}

#' @export
sbo <- set_browse_once

## usage:
# sbo()
# f <- function() stop("!")
# f() # Will invoke browser.
# f() # Won't invoke browser; need to reset with 'sbo()' again.


#' @export
trace_package <- function(package_name, untrace = FALSE)
{
  packageName <- deparse(substitute(package_name))

  internalFunNames <- as.vector(utils::lsf.str("package:" %_% packageName, all.names = TRUE))
  internalFuns <- paste0(packageName, ":::", internalFunNames)

  where <- eval(parse(text = internalFuns[1]))
  if (!untrace)
    r <- trace(internalFunNames, where = where)
  else
    r <- untrace(internalFunNames, where = where)

  invisible(r)
}


#' @export
untrace_package <- function(...)
{
  trace_package(..., untrace = TRUE)
}


#' @export
stop_no_error = function(message = "Not an error.", ...)
{
  stop(message, ...)
}
