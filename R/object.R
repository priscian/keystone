## Compare internal representations of R objects:
## http://stackoverflow.com/questions/9278715/value-reference-equality-for-same-named-function-in-package-namespace-environmen
#' @export
are_same <- function(x, y)
{
  f <- function(x) utils::capture.output(.Internal(inspect(x)))
  all(f(x) == f(y))
}


#' @export
is_invalid <- function(x, ...)
{
  if (missing(x) || is.null(x) || length(x) == 0L)
    return (TRUE)

  if (is.list(x))
    return (all(sapply(x, is_invalid)))
  else if (is.vector(x))
    return (all(is.na(x)))
  else
    return (FALSE)
}


# Add class(es) to an object:
#' @export
add_class <- function(o, classes, after = 0)
{
  if (missing(o))
    stop("Object is missing.")

  currentClasses <- oldClass(o)

  if (missing(classes))
    return (currentClasses)

  if (is.character(after)) {
    w <- after[1] == currentClasses
    if (!is_invalid(w))
      after <- w
  }

  return (structure(o, class = append(currentClasses, classes, after)))
}
