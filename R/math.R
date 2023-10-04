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


## Returns position, signal height, & approximate full width at half maximum (FWHM, ~ 2.36σ for Gaussian) peak height.
## Adapted from 'IDPmisc::peaks()' to add full width at one-tenth maximum (FWTM, ~ 4.29σ for Gaussian) peak height.
#' @export
find_peaks <- function(
  x, y = NULL,
  minPH, # Minimum height of peak to be reported
  minPW, # Minimum width of peak at half maximum to be reported
  thr, # Threshold below which the signal should not be processed
  ## 'stepF' indirectly defines the accuracy of the selection criteria 'minPH' & 'minPW', & of the value of the
  ##   calculated width: the smaller, the more accurate, & the slower the function. 'stepF' must be < 0.5.
  stepF = 0.49,
  height_divisor = 2, # finds full peak width at 1/height_divisor maximum; i.e. height_divisor = 2 is FWHM
  sort_on_x = TRUE,
  truncate_overlaps = FALSE
)
{
  ok <- function (x)
  {
    if (is.logical(x)) {
      x[is.na(x)] <- FALSE

      return(x)
    }
    else stop("'x' must be logical!\n")
  }


  get_xy <- function (x, y = NULL, unidim_allowed = TRUE, sort_on_x = TRUE)
  {
    if (missing(x))
      xarg <- "x"
    else xarg <- deparse(substitute(x))
    if (missing(y))
      yarg <- "y"
    else yarg <- deparse(substitute(y))
    if (is.matrix(x) | is.data.frame(x)) {
      if (ncol(x) > 1) {
        if (is.null(y))
          y <- x[, 2]
        else stop("'", xarg, "' must have only 1 column when y is supplied separately\n")
      }
      x <- x[, 1]
    }
    else if (is.list(x)) {
      if (length(x) > 1) {
        y <- x[[2]]
        x <- x[[1]]
        if (length(y) != length(x))
          stop("First and second element of the list must have identical length!\n")
      }
      else x <- x[[1]]
    }
    if (is.null(y)) {
      if (!unidim_allowed)
        stop("'", yarg, "' is not defined!\n")
      y <- x
      x <- 1:length(x)
    }
    else {
      y <- unlist(y)
      if (length(y) != length(x))
        stop("Vector '", yarg, "' and 'x' must have identical lengths!\n")
    }

    r <- dataframe(x = x, y = y)
    if (sort_on_x)
      r %<>% dplyr::arrange(x)

    return (r)
  }


  xy <- get_xy(x, y, sort_on_x = sort_on_x)
  xx <- xy$x
  yy <- xy$y
  min_yy <- min(yy, na.rm = TRUE)
  max_yy <- max(yy, na.rm = TRUE)
  yy <- c(min_yy, yy)
  xx <- c(xx[1], xx)
  if (missing(thr))
    thr <- min(yy, na.rm = TRUE)
  if (missing(minPH))
    minPH <- diff(range(yy), na.rm = TRUE) / 10
  if (missing(minPW))
    minPW <- 0
  if (stepF >= 0.5)
    stop("'stepF' must be smaller than 0.5")
  peak_x <- peak_y <- peak_w <- peak_w_min <- peak_w_max <- NULL
  lev <- thr - stepF * minPH
  len_yy <- length(yy)
  repeat {
    ## Starting at the minimum threshold 'thr', step up though the y levels
    lev <- lev + stepF * minPH#; cat(sprintf("'lev' = %s", lev), fill = TRUE)
    if (lev >= max_yy)
      break
    hi <- ok(yy > lev) # Which y values are higher than the current level 'lev'?
    ## The 'start' & 'end' indices mark between them continuous stretches of y values > current level
    start <- which(diff(c(FALSE, hi)) > 0)
    end <- which(diff(c(hi, FALSE)) < 0)
    len <- length(start) # Both 'start' & 'end' will always have the same length
    if (len == 0)
      next
    ## Step through & evaluate all continuous stretches of y values > current level
    for (ii in 1:len) {
      ## 'x', 'y', & 'i' characterize the current stretch over-threshold y values:
      x <- xx[start[ii]:end[ii]]
      y <- yy[start[ii]:end[ii]]
      i <- which.max(y)
      if (is.element(x[i], peak_x)) # 'if (x[i] %in% peak_x) ...' i.e. have we already found this peak?
        next
      miny <- min(yy[max(1, start[ii] - 1):min(end[ii] + 1, len_yy)], na.rm = TRUE) # Keep indices within vector bounds
      PH <- y[i] - miny # Full height of peak under evaluation
      ## Now get subset of this stretch's y values > half the peak's height as a 0/1 string (for RegExp processing):
      flit <- paste(as.numeric(y > (miny + PH/height_divisor)), collapse = "")
      PW_match <- gregexpr("1+", flit)[[1]] # Find all continuous runs of "1"
      wyi <- which(y[i] == y)[1]
      ## Find which continuous run of "1" contains the peak, & calculate FW[height_divisor⁻¹th]M
      PW <- mapply(structure(PW_match, .Names = PW_match), attr(PW_match,"match.length"),
        FUN = function(a, b) { wyi %between% c(a, a + b) }, SIMPLIFY = TRUE) %>% which %>%
        {
          if (is_invalid(.))
            structure(0, peak_w_min = NA_real_, peak_w_max = NA_real_)
          else {
            x_min <- x[PW_match[.]]
            x_max <- x[PW_match[.] + attr(PW_match,"match.length")[.]]
            structure(abs(x_max - x_min), peak_w_min = x_min, peak_w_max = x_max)
          }
        }
      if (!is.na(PW) && PH >= minPH && PW >= minPW) {
        peak_x <- c(peak_x, x[i])
        peak_y <- c(peak_y, y[i])
        peak_w <- c(peak_w, PW %>% as.vector)
        peak_w_min <- c(peak_w_min, attr(PW, "peak_w_min"))
        peak_w_max <- c(peak_w_max, attr(PW, "peak_w_max"))
        #browser()
      }
    }
  }

  res <- dataframe(x = peak_x, y = peak_y, w = peak_w, w_min = peak_w_min, w_max = peak_w_max) %>%
    dplyr::arrange(x)

  if (truncate_overlaps) {
    if (NROW(res) > 1) {
      plyr::l_ply(seq(NROW(res) - 1) + 1,
        function(a) { if (res[a - 1, ]$w_max > res[a, ]$w_min) { res[a - 1, ]$w_max <<- res[a, ]$w_min } })
    }
  }

  return (res)
}
