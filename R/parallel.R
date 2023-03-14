## A "future.apply"-version of the 'sapply()' function.
#' @export
psapply <- function(
  X,
  FUN,
  ...,
  simplify = TRUE,
  USE.NAMES = TRUE,
  future.seed = TRUE,
  future.globals.maxSize = Inf, # E.g. 850 * 1024^2 = 891289600 MB to set upper limit
  .parallel = TRUE # Use 'options(keystone_parallel = FALSE)' to turn off universally
)
{
  if ((!is.null(getOption("keystone_parallel")) && !getOption("keystone_parallel")) || !.parallel)
    return (sapply(X = X, FUN = FUN, ..., USE.NAMES = USE.NAMES, simplify = simplify))

  if (!is.null(future.globals.maxSize)) {
    op <- options("future.globals.maxSize")
    options(future.globals.maxSize = future.globals.maxSize)
  }

  FUN <- match.fun(FUN)
  answer <- future.apply::future_lapply(X = X, FUN = FUN, future.seed = future.seed, ...)

  if (!is.null(future.globals.maxSize)) {
    options(op)
  }

  if (USE.NAMES && is.character(X) && is.null(names(answer)))
    names(answer) <- X
  if (!isFALSE(simplify) && length(answer))
    simplify2array(answer, higher = (simplify == "array"))
  else answer
}


## A "future.apply"-version of the 'plyr::l_ply()' function (simplified parameters).
#' @export
pl_ply <- function (
  .data,
  .fun,
  ...,
  future.seed = TRUE,
  future.globals.maxSize = Inf, # E.g. 850 * 1024^2 = 891289600 MB to set upper limit
  .parallel = TRUE # Use 'options(keystone_parallel = FALSE)' to turn off universally
)
{
  if ((!is.null(getOption("keystone_parallel")) && !getOption("keystone_parallel")) || !.parallel)
    return (plyr::l_ply(.data = .data, .fun = .fun, ..., .parallel = FALSE))

  if (!is.null(future.globals.maxSize)) {
    op <- options("future.globals.maxSize")
    options(future.globals.maxSize = future.globals.maxSize)
  }

  .fun <- match.fun(.fun)
  answer <- future.apply::future_lapply(X = .data, FUN = .fun, future.seed = future.seed, ...)

  if (!is.null(future.globals.maxSize)) {
    options(op)
  }

  return (invisible(NULL))
}
