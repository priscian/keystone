#' @export
make_current_timestamp <- function(fmt = "%Y-%m-%d", use_seconds = FALSE, seconds_sep = "+")
{
  sysTime <- Sys.time()
  timestamp <- format(sysTime, fmt)
  if (use_seconds)
    timestamp <-
      paste(timestamp,
        sprintf("%05d", lubridate::period_to_seconds(lubridate::hms(format(Sys.time(), "%H:%M:%S")))), sep = seconds_sep)

  return (timestamp)
}


## For non-decreasing dates, possibly with NAs, get 'diff()' whose sum equals last(x) - first(x).
#' @export
diffs <- function(x, to_na = NULL, ...)
{
  r <- diff(zoo::na.locf(x, na.rm = FALSE), ...)
  if (!is.null(to_na))
    is.na(r) <- r %in% to_na

  r
}

## usage:
# x <- structure(c(NA, 16456, 16473, NA, NA, 16517, 16531, 16535, 16540, 16546, 16559, 16573, 16587, 16598, 16615, 16629, 16643, 16657, 16671, 16716, 16729, 16743, NA, 16772, 16783, 16805, 16820, 16834), class = "Date")
# diffs(x)
