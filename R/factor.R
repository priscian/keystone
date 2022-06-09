## Factorize continuous variable by quantile or other cutpoints.
#' @export
cut2q <- function(v, cuts, trim = FALSE, fmt = "%1.1f", dash = "--", ...)
{
  att <- attributes(v); att <- att[names(att) %nin% c("levels")] # Save object attributes.
  att$class <- c(att$class, "factor") # Note that 'c(NULL, "factor")' is the same as 'c("factor")'.

  if (missing(cuts)) {
    cuts <- stats::quantile(v, na.rm = TRUE)
    trim <- TRUE
  }
  if (trim)
    cuts <- head(tail(cuts, -1L), -1L)

  x <- sprintf(fmt, cuts)
  ranges <- character()
  for (i in seq_along(x)) {
    if (i == 1L) ranges <- c(ranges, "< " %_% x[i])
    if (i == length(x)) ranges <- c(ranges, ">= " %_% x[i])
    else ranges <- c(ranges, x[i] %_% dash %_% x[i + 1L])
  }

  f <- Hmisc::cut2(v, cuts, ...)
  levels(f) <- ranges

  attributes(f) <- utils::modifyList(attributes(f), att, keep.null = TRUE) # Restore object attributes.

  return (f)
}
