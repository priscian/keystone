# http://stackoverflow.com/questions/16118050/how-to-check-if-a-vector-contains-n-consecutive-numbers
#' @export
seqle <- function(x, incr = 1)
{
  if (!is.numeric(x)) x <- as.numeric(x)
  n <- length(x)
  y <- x[-1L] != x[-n] + incr
  #y <- abs(x[-1L] - x[-n] - incr) > .Machine$double.eps ^ 0.5 # Possible enhancement for numerics. See Web link above.
  i <- c(which(y | is.na(y)), n)

  list(lengths = diff(c(0L, i)), values = x[head(c(0L, i) +1L, -1L)])
}


## Find leading and trailing NAs in a vector; returns 'FALSE' for leading/trailing NAs, 'TRUE' for NA-enwrapped values.
#' @export
na_unwrap <- function(x, ...)
  UseMethod("na_unwrap")


#' @export
na_unwrap.matrix <- function(x, ...)
{
  apply(apply(x, 2, na_unwrap.default, ...), 1, any)
}


#' @export
na_unwrap.data.frame <- function(x, ...)
{
  na_unwrap.matrix(x, ...)
}


#' @export
na_unwrap.default <- function(x, type=c("both", "head", "tail", "none"), ...)
{
  type <- match.arg(type)

  nai <- stats:::na.omit.default(x) # Changed 14 Jan. 2017 to work with "ts" objects.
  #s <- rle(attr(nai, "na.action")) # See external function definition.
  s <- seqle(attr(nai, "na.action")) # See external function definition.

  leadi <- head(s$values, 1L)
  leadr <- NULL
  if (!is.na(leadi)) {
    if (leadi == 1L)
      leadr <- leadi:(leadi + head(s$lengths, 1L) - 1L)
  }

  traili <- tail(s$values, 1L)
  trailr <- NULL
  if (!is.na(traili)) {
    if (traili + tail(s$lengths, 1L) - 1L == length(x))
      trailr <- traili:(length(x))
  }

  r <- rep(TRUE, length(x))

  switch(type,
    both = r[c(leadr, trailr)] <- FALSE,
    head = r[c(leadr)] <- FALSE,
    tail = r[c(trailr)] <- FALSE
  )

  return (r)
}

## usage:
# na_unwrap(inst$Keeling)
# na_unwrap(inst$GISTEMP[inst$year %in% 1900:2000]) # No leading/trailing NAs.


#' @export
shift <- function(x, ...)
  UseMethod("shift")


#' @export
shift.default <- function (x, i = 1L, roll = TRUE, na_rm = FALSE)
{
  if (i == 0L) return (x)

  naRm <- function(x, na_rm)
  {
    if (!na_rm) return (x)

    x[setdiff(seq_along(x), attr(stats::na.omit(x), "na.action"))]
  }

  n  <- length(x)
  if (n == 0L) return (x)

  j <- i %% n

  if (roll && j == 0L) return (naRm(x, na_rm))

  if (!roll && j == 0L) {
    x[seq_along(x)] <- NA

    return (naRm(x, na_rm))
  }

  if (!roll && i > n) {
    rv <- x
    rv[seq_along(rv)] <- NaN
  }
  else {
    shifted <- 1L:(n - j)
    if (i > 0L)
      shifted <- (n - j + 1L):n

    if (!roll) x[shifted] <- NA
    if (na_rm) x[shifted] <- NaN

    rv <- x[c((n - j + 1L):n, shifted)]
    if (i > 0L)
      rv <- x[c(shifted, 1L:(n - j))]
  }

  if (na_rm)
    rv <- rv[!is.nan(rv)]

  return (rv)
}

## usage:
# shift(1:10)
# shift(1:10, roll = FALSE)
# shift(1:10, -1)
# shift(1:10, -1, roll = FALSE)
# shift(1:10, 5)
# shift(1:10, 5, roll = FALSE)
# shift(1:10, -5)
# shift(1:10, -5, roll = FALSE)
# shift(1:10, 5, roll = FALSE, na_rm = TRUE)
# shift(1:10, -5, roll = FALSE, na_rm = TRUE)


#' @export
shift.data.frame <- function(x, i, ...)
{
  if (!is.list(i)) {
    i <- as.list(rep(i, length.out = length(x)))
    names(i) <- names(x)
  }

  for(j in names(i))
    x[[j]] <- shift.default(x[[j]], i[[j]], ...)

  x
}


#' @export
chunk <- function(x, size, ...)
  UseMethod("chunk")


#'@export
chunk.default <- function(x, size, ...)
{
  split(x, as.numeric(gl(length(x), size, length(x))))
}


#'@export
chunk.data.frame <- function(x, size, by_column = FALSE, ...)
{
  if (by_column) {
    s <- chunk.default(seq(NCOL(x)), size, ...)

    sapply(s, function(y) x[, y, drop = FALSE], simplify = FALSE)
  } else {
    s <- chunk.default(seq(NROW(x)), size, ...)

    sapply(s, function(y) x[y, , drop = FALSE], simplify = FALSE)
  }
}


#'@export
chunk.matrix <- function(x, size, ...)
{
  chunk.data.frame(x, size, ...)
}


## Which values of 'v' are closest to the given values of 'x'?
#' @export

nearest <- function(v, x, value = FALSE)
{
  d <- data.table::data.table(v, value = v)
  data.table::setattr(d, "sorted", "v")
  data.table::setkey(d, v) # Sort the data

  ## Binary search
  ## N.B. Can't really get at 'J()' without making this package depend on "data.table" --
  ## V. https://stackoverflow.com/questions/22001945/how-is-j-function-implemented-in-data-table
  m <- d[J(x), roll = "nearest"]$value

  l <- which(v == m)

  if (value)
    v[l]
  else
    l
}

#' @export
nearest_below <- function(v, x, value = FALSE) { l <- which(v == max(v[(v < x)])); if (value) v[l] else l }

#' @export
nearest_above <- function(v, x, value = FALSE) { l <- which(v == min(v[(v > x)])); if (value) v[l] else l }


## http://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position
#' @export
split_at <- function(x, pos, split_after = FALSE, ...)
  UseMethod("split_at")


#' @export
split_at.data.frame <- function(x, pos, split_after = FALSE, simplify = FALSE, ...)
{
  sapply(split_at.default(seq(nrow(x)), pos = pos, split_after = split_after, ...),
    function(a)
    {
      x[a, ]
    }, simplify = simplify)
}


#' @export
split_at.default <- function(x, pos, split_after = FALSE, ...)
{
  if (is.logical(pos)) {
    if (length(pos) != length(x)) {
      warning("'pos' is not the same length as 'x', so it's being trimmed or repeated to match.")
      pos <- rep(pos, length.out = length(x))
    }
    pos <- which(pos)
  }

  unname(split(x, cumsum(seq_along(x) %in% (pos + as.integer(split_after)))))
}


## Return all combinations of successive vectors in their given order.
#' @export
combine_groups <- function(x, combine_fun = base::paste, ...)
{
  comb_factory <- function(...)
  {
    ## Make sure that '...' comes from 'comb_factory()' by NOT giving it as a formal argument of the following function:
    function(x, y) { as.vector(t(outer(x, y, FUN = combine_fun, ...))) }
  }
  comb <- comb_factory(...)

  Reduce(comb, x)
}

## usage:
# colParts <- list(c("Global", "NH", "SH", "Tropics", "NH Extratropic", "SH Extratropic",
#   "NH Polar", "SH Polar"), c("", " Land", " Ocean"))
# combine_groups(colParts, sep = "")


## Select every nth element of a vector starting at element 'offset'.
#' @export
every_nth_element <- function(x, interval, offset = 0)
{
  x[mod(seq_along(x), interval) == mod(offset, interval)]
}


## Create a named list from object symbols
#' @export
named_list <- function(
  ..., # object symbols, separated by commas
  ENVIR # if not missing, environment from which to take all objects en masse as a list; concat'd w/ '...' objects
)
{
  edots <- get_dots(..., evaluate = TRUE)

  r <- structure(edots$evaluated, .Names = sapply(edots$arguments, as.character))
  blankIndex <- trimws(edots$dots_names) == ''
  names(r)[!blankIndex] <- edots$dots_names[!blankIndex]

  if (!missing(ENVIR)) {
    r <- c(r, as.list(ENVIR))
  }

  r
}

## usage:
# fish <- "frog"; named_list(10, raw, notfish = fish, pi = 3.14, TRUE, ENVIR = list2env(named_list(333, beast = 666)))
