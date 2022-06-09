#' @export
`%nin%` <- function(x, table) {
  match(x, table, nomatch = 0) == 0
}


## Do '%in%' match but return NA's for missing values of 'x'.
#' @export
`%eq%` <- function (x, table)
{
  out <- x %in% table
  is.na(out) <- is.na(x)

  return (out)
}

## usage:
# c(NA, "frog", NA, "toad", "cat", "apple") %in% c("frog", "fish", NA, "buffalo")
# c(NA, "frog", NA, "toad", "cat", "apple") %eq% c("frog", "fish", NA, "buffalo")
# match(x = c(NA, "frog", NA, "toad", "cat", "apple"), table = c("frog", "fish", NA, "buffalo"), nomatch = NA_integer_)
# output:
#   [1]  TRUE  TRUE  TRUE FALSE FALSE FALSE
#   [1]    NA  TRUE    NA FALSE FALSE FALSE
#   [1]  3  1  3 NA NA NA


#' @export
`%neq%` <- function(x, ...)
{
  return (!`%eq%`(x, ...))
}


## These fuzzy equalities are necessary or useful sometimes.
#' @export
all_equal <- function(...) Vectorize(all.equal, "target", SIMPLIFY = FALSE)(...)


#' @export
is_equal <- function(..., simplify = TRUE) sapply(all_equal(...), function(x) is.logical(x) && x, simplify = simplify)
