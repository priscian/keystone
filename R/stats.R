## Coefficient of variation
#' @export
cv <- function(x, na.rm = FALSE, ...) stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)


## Robust coefficient of variation (rCV)
## "The RCV is equal to 0.75 multiplied by the interquartile range divided by the median."
#' @export
rcv <- function(x, na.rm = FALSE, IQR... = list(), ...)
{
  IQRArgs <- list(
    x = x,
    na.rm = na.rm,
    type = 7
  )
  IQRArgs <- utils::modifyList(IQRArgs, IQR..., keep.null = TRUE)

  0.75 * (do.call(stats::IQR, IQRArgs) / stats::median(x, na.rm = na.rm))
}


## BD Biosciences robust standard deviation (BD-rSD)
#' @export
bd_rsd <- function(x, na.rm = FALSE, ...) stats::median(abs(x - stats::median(x, na.rm = na.rm)), na.rm = na.rm) * 1.4826


## BD Biosciences robust coefficient of variation (BD-rCV)
#' @export
bd_rcv <- function(x, na.rm = FALSE, ...) bd_rsd(x, na.rm = na.rm) / stats::median(x, na.rm = na.rm)
