## Code "borrowed" from 'MESS::auc()' (https://cran.r-project.org/web/packages/MESS/).
#' @export
integratex <- function(
  x, y,
  from = min(x), to = max(x),
  type = c("spline", "linear"),
  absolutearea = FALSE,
  integrate... = list(),
  ...
)
{
  type <- match.arg(type)
  if (length(x) != length(y))
    stop("x and y must have the same length")
  if (length(unique(x)) < 2L)
    return (NA)
  if (type == "linear") {
    if (absolutearea)
      y <- y - min(y)
    values <- stats::approx(x, y, xout=sort(unique(c(from, to, x[x > from & x < to]))), ...)
    res <- 0.5 * sum(diff(values$x) * (values$y[-1] + values$y[-length(values$y)]))
    if (absolutearea)
      res <- res - min(y) * (max(x) - min(x))
  }
  else {
    if (absolutearea)
      myfunction <- function(x)
      {
        abs(stats::splinefun(x, y, method = "natural"))
      }
    else myfunction <- stats::splinefun(x, y, method = "natural")

    integrateArgs <- list(
      f = myfunction,
      lower = from,
      upper = to
    )
    integrateArgs <- utils::modifyList(integrateArgs, integrate..., keep.null = TRUE)
    res <- do.call(stats::integrate, integrateArgs)
    #res <- stats::integrate(myfunction, lower = from, upper = to)$value
  }

  res
}


## Use convolution filter to calculate n-month moving average.
#' @export
moving_average <- function(x, n, sides = 1L, ...) # 'n' is the window size.
{
  if (is.null(n)) return (x)
  r <- stats::filter(x, rep(1/n, n), sides = sides, ...)
  colnames(r) <- colnames(x)

  return (r)
}

#' @export
MA <- moving_average


#' @export
interpNA <- function(
  x,
  method = c("linear", "before", "after", "none"),
  unwrap = TRUE,
  skip_all_is_na = TRUE,
  ...
)
{
  if (!inherits(x, "matrix") && !inherits(x, "timeSeries"))
    x <- as(x, "matrix")

  if (method[1] == "none")
    return (x)

  fun <- stats::approx
  if (method[1] %nin% c("linear", "before", "after", "none")) { # '?stats::spline' for available "method"s.
    ## The following code removes any unmatched arguments from a call to 'FUN()';
    ## e.g. 'stats::spline()' doesn't have a formal argument 'f', which is nonetheless passed in below.
    fun <- function(...)
    {
      FUN <- stats::spline; d <- get_dots(...)
      a <- d$arguments[trimws(names(d$arguments)) %in% c("", formalArgs(FUN))]

      do.call(FUN, a, quote = FALSE, envir = parent.frame())
    }
  } #else unwrap = FALSE

  interpVectorNA <- function(x, method, f, ...)
  {
    n <- length(x)
    idx <- (1:n)[!is.na(x)]
    y <- fun(x = idx, y = x[idx], xout = 1:n, method = method, f = f)$y

    ## If spline interpolation, allow terminal NAs to be interpolated.
    if (!unwrap) return (y)

    ## If any leading/trailing NAs remain, interpolate them from the first/last value.
    y[!na_unwrap(y, "head")] <- y[head(which(!is.na(y)), 1)]
    y[!na_unwrap(y, "tail")] <- y[tail(which(!is.na(y)), 1)]

    r <- x
    r[na_unwrap(x, ...)] <- y[na_unwrap(x, ...)]

    r
  }

  method <- method[1]
  f <- 0
  if (method == "before") {
    method <- "constant"
    f <- 0
  }
  if (method == "after") {
    method <- "constant"
    f <- 1
  }
  for (i in 1:ncol(x)) {
    if (skip_all_is_na) {
      if (all(is.na(x[, i])))
        next
    }
    x[, i] <- interpVectorNA(x[, i], method, f, ...)
  }

  x
}


## 'cumsum()' with 'na.rm = TRUE' equivalent.
#' @export
cum_sum <- function(x, ...) `[<-`(x, !is.na(x), value = cumsum(na.omit(x), ...))


## 'diff()' with 'na.rm = TRUE' equivalent.
#' @export
diff_ <- function(x, ...)
{
  ## Interpolate missings by last observation carried forward (LOCF)
  x %>% data.table::nafill("locf") %>%
    ## Interpolate leading missings by next observation carried backward (NOCB)
    data.table::nafill("nocb") %>%
    ## Calculate diff on interpolated data set
    diff(...)
}


## V. https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in/25555105#25555105
#' @export
geometric_mean <- function(x, na.rm = TRUE, zero.propagate = FALSE)
{
  if (any(x < 0, na.rm = TRUE)) {
    return (NaN)
  }
  if (zero.propagate) {
    if(any(x == 0, na.rm = TRUE)) {
      return (0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
  }
}


## Returns all permutations of vector x as an array; addresses the lack in base R.
#' @export
permute <-
  function(x) { P <- NULL; for(i in seq_along(x)) P <- rbind(P, cbind(x[i], permute(x[-i]))); P }
