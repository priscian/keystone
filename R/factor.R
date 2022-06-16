#' @export
unfactor  <- function(x, ...)
  UseMethod("unfactor")


#' @export
unfactor.default <- function(x, ...)
{
  return (x)
}


#' @export
unfactor.factor <- function(x, ...)
{
  return (levels(x)[x])
}


#' @export
unfactor.data.frame <- function(x, columns = NULL, ...)
{
  isPointer <- FALSE
  if (inherits(x, "pointer")) isPointer <- TRUE

  if (!isPointer) {
    if (is.null(columns))
      columns <- seq(ncol(x))

    for (i in columns) {
      if (inherits(x[, i], "factor"))
        x[, i] <- unfactor(x[, i], ...)
    }

    return (x)
  }
  else {
    if (is.null(columns))
      columns <- seq(ncol(..(x)))

    for (i in columns) {
      if (inherits(..(x)[, i], "factor"))
        ..(x)[, i] <- unfactor(..(x)[, i], ...)
    }

    return (nop())
  }
}


#' @export
unfactor.pointer <- function(x, ...)
{
  if (inherits(..(x), "data.frame"))
    unfactor.data.frame(x, ...)
  else if (inherits(..(x), "factor"))
    return (unfactor.factor(..(x), ...))
  else
    stop("Pointer does not reference a relevant object type.")
}


#' @export
drop_levels <- function(x, reorder = FALSE, ...)
  UseMethod("drop_levels")


#' @export
drop_levels.factor <- function(x, reorder = FALSE, ...)
{
  x <- x[, drop = TRUE]
  if (reorder) x <- stats::reorder(x, ...)

  return (x)
}


#' @export
drop_levels.data.frame <- function(x, reorder = FALSE, ...)
{
  isPointer <- FALSE
  if (inherits(x, "pointer")) isPointer <- TRUE

  if (!isPointer) {
    for (i in seq(ncol(x))) {
      if (inherits(x[, i], "factor"))
        x[, i] <- drop_levels(x[, i], reorder)
    }

    return (x)
  }
  else {
    for (i in seq(ncol(..(x)))) {
      if (inherits(..(x)[, i], "factor"))
        ..(x)[, i] <- drop_levels(..(x)[, i], reorder)
    }
  }
}


#' @export
drop_levels.pointer <- function(x, reorder = FALSE, ...)
{
  if (inherits(..(x), "data.frame"))
    drop_levels.data.frame(x, reorder, ...)
  else if (inherits(..(x), "factor"))
    return (drop_levels.factor(..(x), reorder, ...))
  else
    stop("Pointer does not reference a relevant object type.")
}

## usage:
# x <- data.frame(a = 1:5, b = letters[1:5], c = rep(3.14, 5), d = as.factor(c(1,2,1,3,4)))
# Hmisc::units(x$d) <- "mg^2"
# Hmisc::label(x$d) <- "duh"
# x1 <- x[1:4, ]
# x1$d
# x2 <- drop_levels(x1)
# x2$d


# Wrapper for 'addNA()' function that keeps factor attributes.
#' @export
add_na <- function(x, ...)
{
  x <- as.factor(x)

  classes <- class(x)
  att <- attributes(x); att <- att[names(att) %nin% c("levels")] # Save object attributes.

  x <- addNA(x, ...)

  attributes(x) <- utils::modifyList(attributes(x), att, keep.null = TRUE) # Restore object attributes.
  class(x) <- unique(classes, c(class(x)))

  return (x)
}


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
