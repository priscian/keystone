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


## Adapted from 'fANCOVA::loess.as()'; automatically choose 'span' parameter for 'stats::loess()'
optimize_span <- function(
  model,
  criterion = c("aicc", "gcv"),
  span_range = c(0.05, 0.95),
  seed = 666
)
{
  as.crit <- function(x)
  {
    span <- x$pars$span
    traceL <- x$trace.hat
    sigma2 <- sum(x$residuals^2)/(x$n - 1)
    aicc <- log(sigma2) + 1 + 2 * (2 * (traceL + 1))/(x$n - traceL - 2)
    gcv <- x$n * sigma2/(x$n - traceL)^2
    result <- list(span = span, aicc = aicc, gcv = gcv)

    return(result)
  }

  criterion <- match.arg(criterion)
  fn <- function(span)
  {
    mod <- update(model, span = span)

    as.crit(mod)[[criterion]]
  }

  if (!is.null(seed))
    set.seed(seed)
  result <- stats::optimize(fn, span_range)

  #return (list(span = result$minimum, criterion = result$objective))
  return (result$minimum)
}


## Drop-in replacement for 'stats::loess()' that chooses optimal span/bandwidth parameter when 'span = NULL';
##   also produces a quick-view viz for 'plot = TRUE'.
#' @export
LOESS <- function(
  formula,
  data,
  span = formals(stats::loess)$span,
  plot = FALSE,
  optimize_span... = list(),
  ...
)
{
  ## See equivalent code in 'stats::loess()' to create data frame from 'formula':
  # mf <- match.call(expand.dots = FALSE)
  # mf$span <- mf$plot <- mf$optimize_span... <- mf$... <- NULL
  # mf[[1L]] <- quote(stats::model.frame)
  # mf <- eval(mf, parent.frame())

  opt.span <- optimize_span %>% `environment<-`(environment()) # Otherwise 'stats::optimize()' fails
  form <- formula
  # form <- formula %>% `environment<-`(environment()) # Allows use of e.g. 'stats::model.frame()'

  if (missing(data))
    data <- NULL

  if (is.null(span)) {
    optimize_spanArgs <- list(
      model = stats::loess(formula = form, data = data, ...)
    )
    optimize_spanArgs <- utils::modifyList(optimize_spanArgs, optimize_span..., keep.null = TRUE)

    span <- do.call(opt.span, optimize_spanArgs)
  }

  ## 'mf' replaces calculations above:
  if (plot)
    mf <- stats::loess(formula = form, data = data, span = span, method = "model.frame", ...)
  mod <- stats::loess(formula = form, data = data, span = span, ...)

  if (plot) { ## Adapted from 'fANCOVA::loess.as()'
    x <- mod$x
    modPlot <- update(mod, control = loess.control(surface = "direct"))

    if (NCOL(x) == 1) {
      m <- 100
      xNew <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = m)
      formVars <- all.vars(form)
      fitNew <- stats::predict(modPlot, dataframe(xNew) %>% `names<-`(tail(formVars, -1)))

      plot(form, mf, col = "grey", xlab = formVars[2], ylab = formVars[1], ...)
      lines(xNew, fitNew, lwd = 1.5, ...)
      mtext(sprintf("span: %1.2f", span), side = 3)
    } else {
      m <- 50
      x1 <- seq(min(x[, 1]), max(x[, 1]), len = m)
      x2 <- seq(min(x[, 2]), max(x[, 2]), len = m)
      formVars <- all.vars(form)
      xNew <- expand.grid(x1 = x1, x2 = x2) %>%
        `names<-`(tail(formVars, -1)) %>% `length<-`(2)
      fitNew <- matrix(stats::predict(modPlot, xNew), m, m)

      graphics::persp(x1, x2, fitNew, theta = 40, phi = 30, ticktype = "detailed",
        xlab = formVars[2], ylab = formVars[3], zlab = formVars[1], col = "lightblue", expand = 0.6)
      mtext(sprintf("span: %1.2f", span), side = 3)
    }
  }

  return (mod)
}

## usage:
# (cars.lo <- LOESS(dist ~ speed, cars, span = NULL, plot = TRUE))
# n2 <- 100
# x21 <- runif(n2, min = 0, max = 3)
# x22 <- runif(n2, min = 0, max = 3)
# sd2 <- 0.25
# e2 <- rnorm(n2, sd = sd2)
# y2 <- sin(2 * x21) + sin(2 * x22) + 1 + e2
# # (y2.fit <- fANCOVA::loess.as(cbind(x21, x22), y2, plot = TRUE))
# dat <- cbind(x21, x22, y2) %>% as.data.frame
# (y2.fit <- LOESS(y2 ~ x21 + x22, dat, span = NULL, plot = TRUE))
