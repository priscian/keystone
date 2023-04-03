#' @export
only_selected_series <- function(x, series, common_columns = NULL, sort = FALSE, range = NULL, x_var = NULL, ...)
{
  if (missing(series))
    series <- NULL

  colNames <- c(intersect(colnames(x), c(common_columns, series)))
  if (!sort)
    colNames <- char_sort(colNames, series)
  r <- x[, colNames, ...]

  if (!is.null(range)) {
    xVar <- x_var
    r <- r[r[[xVar]] >= ifelse(is.na(range[1]), min(r[[xVar]], na.rm = TRUE),
      range[1]) & r[[xVar]] <= ifelse(is.na(range[2]), max(r[[xVar]]), range[2]), ]
  }

  r
}

#' @export
oss <- only_selected_series


#' @export
view_only_selected_series <- function(..., fun = View)
{
  fun(only_selected_series(...))
}

#' @export
vss <- view_only_selected_series


#' @export
plot_series <- function(
  x, # This should be a data frame.
  series,
  x_var,
  start = NULL, end = NULL,
  ma = NULL, ma_sides = 1L,
  interpolate = FALSE,
  plot_type = c("single", "multiple"),
  type = "l",
  col = NULL, col_fun = colorspace::rainbow_hcl, col_fun... = list(l = 65), alpha = 0.9, lwd = 2,
  main = "", xlab = "", ylab = "",
  unit = NULL,
  dev.new... = list(), bg = scales::alpha("gray", 0.1),
  add = FALSE,
  xaxs = "r", yaxs = "r", cex.xaxis = 1.0, cex.yaxis = 1.0,
  xaxisTicks... = list(), yaxisTicks... = list(), # Arguments to grDevices::axisTicks()
  grid... = list(),
  conf_int = FALSE, conf_int_suffix = "_uncertainty", ci_alpha = 0.3, polygon... = list(),
  trend = FALSE, trend_lwd = lwd, trend_legend_inset = c(0.2, 0.2), trend... = list(), extra_trends = list(),
  loess = FALSE, loess... = list(), loess_series = NULL, lines.loess... = list(),
  segmented = FALSE, segmented... = list(),
  plot.segmented... = list(),
  mark_segments = c("none", "lines", "points"), vline... = list(), points.segmented... = list(),
  legend_fun = graphics::legend, legend... = list(),
  getMarginWidth... = list(), # To use, at least choose where the legend should go, e.g. 'side = 1'.
  get_margin_width_expr = expression({ legend...$x <- get_cardinal_point("ne") }),
  start_callback = NULL, end_callback = NULL,
  sign = FALSE,
  sign_callback =
    rlang::expr(text(graphics::par("usr")[2], graphics::par("usr")[3], labels = "@priscian", adj = c(1.0, -0.5))),
  save_png = FALSE, save_png_dir = ".", save_png_filename = "image.png",
  png... = list(),
  ...
)
{
  plot_type <- match.arg(plot_type)
  mark_segments <- match.arg(mark_segments)

  ## This is to avoid an roxygen error described here: https://github.com/klutometis/roxygen/issues/592
  if (is.null(unit))
    unit <- "\u00b0C"

  ci_series <- NULL
  if (conf_int)
    ci_series <- grep(conf_int_suffix %_% "$", colnames(x), perl = TRUE, value = TRUE)

  y <- zoo::zoo(x[, c(x_var, series, ci_series)], order.by = x[[x_var]])
  y <- subset(y, na_unwrap(as.matrix(y[, c(series, ci_series)]))) # Remove trailing NAs.

  w <- y
  if (is.null(ma)) local({
    interpCols <- NULL
    if (is.logical(interpolate)) {
      if (all(interpolate))
        ## Ensure that 'w' keeps its 'zoo' attributes:
        w <<- w %@>% interpNA("linear", unwrap = TRUE)
      else if (any(interpolate)) {
        interpCols <- series[interpolate]
      }
    }
    else {
      interpCols <- interpolate
    }

    if (!is.null(interpCols)) {
      for (i in interpCols)
        ## Ensure that 'w' keeps its 'zoo' attributes:
        w[, i] <<- drop(w[, i] %@>% interpNA("linear", unwrap = TRUE))
    }
  })

  ## Create moving-average variables if requested.
  maText <- ""
  if (!is.null(ma)) {
    #w[, c(series, ci_series)] <- MA(w[, c(series, ci_series)], ma, sides = ma_sides); w <- zoo::zoo(w, order.by = zoo::index(y))
    ## Ensure that 'w' keeps its 'zoo' attributes:
    w[, c(series, ci_series)] <- w[, c(series, ci_series)] %@>% MA(ma, sides = ma_sides)
    maText <- "(" %_% ma %_% "-month moving average)"
  }

  y <- stats::window(y, start = start, end = end, extend = TRUE) # N.B. This can't be 'extend()'ed.
  w <- stats::window(w, start = start, end = end, extend = TRUE)
  #wz <- zoo::as.zoo(w) # Unnecessary, but for legacy's sake.
  wz <- w

  ## Series colors.
  if (is.null(col)) {
    col <- seq_along(series)
    col_funArgs <- list(
      n = length(col)
    )
    col_funArgs <- utils::modifyList(col_funArgs, col_fun..., keep.null = TRUE)
    col <- suppressWarnings(do.call(col_fun, col_funArgs))
    ## Some other possible function calls:
    #col <- grDevices::rainbow(length(col))
    #col <- grDevices::terrain.colors(length(col))
    #col <- grDevices::topo.colors(length(col))
    #col <- suppressWarnings(RColorBrewer::brewer.pal(length(col), "Spectral")) # Or "Paired".
    #col <- colorRamps::matlab.like2(length(col)) # From package "colorRamps".
  }
  col <- rep(col, length.out = length(series))
  col <- scales::alpha(col, alpha)
  names(col) <- series

  ## Arguments for saving the plot.
  if (save_png) {
    pngArgs <- list(
      filename = paste(save_png_dir, save_png_filename, sep = "/"),
      width = 12.5,
      height = 7.3,
      units = "in",
      res = 600
    )
    pngArgs <- utils::modifyList(pngArgs, png..., keep.null = TRUE)
    do.call(grDevices::png, pngArgs)
  }

  ## Arguments for plotting.
  if (dev.cur() == 1L) { # If a graphics device is active, plot there instead of opening a new device.
    dev.newArgs <- list(
      width = 12.5, height = 7.3 # New default device of 1200 × 700 px at 96 DPI
      # width = 7.3, height = 7.3 # New default device of 700 × 700 px at 96 DPI
    )
    dev.newArgs <- utils::modifyList(dev.newArgs, dev.new..., keep.null = TRUE)
    do.call(dev.new, dev.newArgs)
  }

  ## Find CIs now so that plot region can expand automatically to accomodate them.
  if (conf_int) {
    confintNames <- intersect(series %_% conf_int_suffix, colnames(w))
    if (length(confintNames) != 0L) {
      seriesNames <- stringr::str_match(confintNames, "^(.*?)" %_% conf_int_suffix %_% "$")[, 2L]
      polygon_args <- sapply(seq_along(confintNames),
        function(i)
        {
          value <- w[, seriesNames[i]]
          ci <- w[, confintNames[i]]
          upper <- value + ci/2; lower <- value - ci/2
          ciCol <- scales::alpha(col[seriesNames[i]], ci_alpha)
          cidf <- dataframe(x = y[, x_var], lower = lower, upper = upper) %>% tidyr::drop_na()

          polygonArgs <- list(
            x = c(cidf$x, rev(cidf$x)),
            y = c(cidf$upper, rev(cidf$lower)),
            col = ciCol,
            border = NA
          )
          polygonArgs <- utils::modifyList(polygonArgs, polygon..., keep.null = TRUE)

          polygonArgs
        }, simplify = FALSE)

      names(polygon_args) <- confintNames
    }
  }

  ## If there are CIs & 'ylim' is NULL, make extra vertical room for the CIs.
  dots <- get_dots(..., evaluate = TRUE)
  y_lim <- dots$evaluated$ylim

  if (is.null(y_lim) && exists("polygon_args")) {
    range_y_ci <- sapply(polygon_args,
      function(i) { r <- range(i$y, na.rm = TRUE); names(r) <- c("lo", "hi"); r },
      simplify = FALSE)
    range_y_series <- sapply(series,
      function(i) { r <- range(w[, i, drop = FALSE], na.rm = TRUE); names(r) <- c("lo", "hi"); r },
      simplify = FALSE)

    range_y <- c(range_y_ci, range_y_series); elmNames <- names(range_y)
    y_lims <- Reduce(rbind, range_y); rownames(y_lims) <- elmNames

    y_lim <- c(min(y_lims, na.rm = TRUE), max(y_lims, na.rm = TRUE))
  }

  plotArgs1 <- list(
    x = w[, series],
    plot.type = plot_type,
    type = "n",
    xaxs = xaxs, yaxs = yaxs,
    xaxt = "n", yaxt = "n",
    bty = "n",
    xlab = xlab, ylab = ylab, main = main
  )
  plotArgs1 <- utils::modifyList(plotArgs1, dots$evaluated, keep.null = TRUE)
  plotArgs1$ylim <- y_lim

  if (!add) {
    op <- par(mar = c(5, 5, 4, 2) + 0.1) # Add some extra space for exponents &c. on the left margin.

    do.call("plot", plotArgs1) # I.e. 'plot.zoo()'

    if (!is_invalid(getMarginWidth...)) {
      legendArgs = list(xpd = TRUE)
      legend... <- utils::modifyList(legend..., legendArgs, keep.null = TRUE)

      legendText <- series %_% ifelse(loess, " (+ LOESS)", "")
      if (!is.null(legend...$legend)) legendText <- legend...$legend

      getMarginWidthArgs <- list(
        labels = legendText,
        is.legend = TRUE
      )
      getMarginWidthArgs <- utils::modifyList(getMarginWidthArgs, getMarginWidth..., keep.null = TRUE)
      newMarginInfo <- do.call(plotrix::getMarginWidth, getMarginWidthArgs)

      par(mar = c(par("mar")[1:3], newMarginInfo$newmar))
      expr <- get_margin_width_expr
      if (!is.null(expr)) {
        if (is.function(expr)) {
          expr()
        } else if (rlang::is_expression(expr)) {
          rlang::eval_tidy(expr)
        } else if (is.expression(expr)) {
          eval(expr)
        }
      }
    }
  }
  if (maText != "") graphics::mtext(maText, 3L)

  ## N.B. I need more control over the grid here:
  if (!add) {
    nx <- ny <- NULL
    if (!is_invalid(xaxisTicks...)) nx <- NA
    if (!is_invalid(yaxisTicks...)) ny <- NA

    #graphics::grid(nx = NULL, ny = NULL, col = "gray", lty = "solid", lwd = par("lwd"))
    gridArgs <- list(
      nx = nx, ny = ny,
      col = "gray",
      lty = "solid",
      lwd = par("lwd")
    )
    gridArgs <- utils::modifyList(gridArgs, grid..., keep.null = TRUE)

    do.call(graphics::grid, gridArgs)

    # grDevices::axisTicks(usr = par("usr")[1:2], log = FALSE, nint = 5) # E.g.
    if (!is_invalid(xaxisTicks...)) {
      xaxisTicksArgs <- list(
        usr = par("usr")[1:2],
        log = FALSE
      )
      xaxisTicksArgs <- utils::modifyList(xaxisTicksArgs, xaxisTicks..., keep.null = TRUE)
      forceAllTicks <- xaxisTicksArgs$force; xaxisTicksArgs$force <- NULL

      xaxisTicks <- do.call(grDevices::axisTicks, xaxisTicksArgs)

      if (!is.null(forceAllTicks) && is.logical(forceAllTicks) && forceAllTicks)
        mapply(graphics::axis, side = 1, at = xaxisTicks, labels = xaxisTicks, tick = FALSE, cex.axis = cex.xaxis)
      else
        graphics::axis(side = 1, at = xaxisTicks, labels = xaxisTicks, tick = FALSE, cex.axis = cex.xaxis)

      abline(v = xaxisTicks, col = gridArgs$col, lty = gridArgs$lty, lwd = gridArgs$lwd)
    } else {
      ## V. https://stackoverflow.com/questions/22470114/removing-top-and-right-borders-from-boxplot-frame-in-r/62185904#62185904
      #graphics::box(bty = "l") # L-shaped box
      graphics::axis(1, labels = TRUE, lwd = 0, lwd.ticks = 0, cex.axis = cex.xaxis) # Draw x-axis
    }

    if (!is_invalid(yaxisTicks...)) {
      yaxisTicksArgs <- list(
        usr = par("usr")[3:4],
        log = FALSE
      )
      yaxisTicksArgs <- utils::modifyList(yaxisTicksArgs, yaxisTicks..., keep.null = TRUE)
      forceAllTicks <- yaxisTicksArgs$force; yaxisTicksArgs$force <- NULL

      yaxisTicks <- do.call(grDevices::axisTicks, yaxisTicksArgs)

      if (!is.null(forceAllTicks) && is.logical(forceAllTicks) && forceAllTicks)
        mapply(graphics::axis, side = 2, at = yaxisTicks, labels = yaxisTicks, tick = FALSE, cex.axis = cex.yaxis)
      else
        graphics::axis(side = 2, at = yaxisTicks, labels = yaxisTicks, tick = FALSE, cex.axis = cex.yaxis)

      abline(h = yaxisTicks, col = gridArgs$col, lty = gridArgs$lty, lwd = gridArgs$lwd)
    } else {
      graphics::axis(2, labels = TRUE, lwd = 0, lwd.ticks = 0, cex.axis = cex.yaxis) # Draw y-axis
    }
  }

  if (!is.null(bg)) {
    graphics::rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], border = NA, col = bg)
  }

  ## Evaluate expression after creating graphics device but before plotting series.
  if (!is.null(start_callback))
    plinth::poly_eval(start_callback)

  ## Plot confidence bands for series that have them.
  if (conf_int) {
    if (exists("polygon_args"))
      plyr::l_ply(polygon_args, function(i) do.call(what = graphics::polygon, args = i))
  }

  par(new = TRUE)
  if (add) {
    plotArgs2 <- list(
      x = wz[, plotSeries[i]],
      type = type,
      col = col[i], lwd = lwd,
      xaxs = xaxs, yaxs = yaxs,
      xaxt = "s", yaxt = "s",
      #bty = "n",
      xlab = "", ylab = ""
    )
    plotArgs2 <- utils::modifyList(plotArgs2, dots$evaluated, keep.null = TRUE)
    plotArgs2$ylim <- y_lim

    plotSeries <- series
    for (i in seq_along(plotSeries))
      do.call("lines", plotArgs2) # I.e. 'lines.zoo()'
  }
  else {
    plotArgs2 <- list(
      x = wz[, series],
      screens = 1L, plot.type = plot_type,
      type = type,
      col = col, lwd = lwd,
      xaxs = xaxs, yaxs = yaxs,
      xaxt = "n", yaxt = "n",
      bty = "n",
      xlab = "", ylab = ""
    )
    plotArgs2 <- utils::modifyList(plotArgs2, dots$evaluated, keep.null = TRUE)
    plotArgs2$ylim <- y_lim

    do.call("plot", plotArgs2) # I.e. 'plot.zoo()'
  }

  legendArgs <- list(
    x = "topleft",
    legend = series %_% ifelse(loess, " (+ LOESS)", ""),
    col = col,
    lwd = lwd,
    bty = "n",
    cex = 0.8
  )
  legendArgs <- utils::modifyList(legendArgs, legend..., keep.null = TRUE)
  do.call(legend_fun, legendArgs)

  ## LOESS smooth.
  if (loess) local({
    loessSeries <- series
    if (!is_invalid(loess_series))
      loessSeries <- loess_series
    for (s in loessSeries) {
      # loessArgs = list(
      #   formula = eval(substitute(s ~ x, list(s = as.name(s), x = as.name(x_var)))),
      #   data = y[, c(x_var, series)],
      #   span = 0.2
      # )
      xl <- as.vector(y[, x_var])
      yl <- as.vector(y[, s])
      loessArgs = list(
        formula = yl ~ xl,
        span = 0.2
      )
      loessArgs <- utils::modifyList(loessArgs, loess..., keep.null = TRUE)

      l <- do.call(stats::loess, loessArgs)

      lines.loessArgs <- list(
        x = drop(l$x),
        y = l$fit,
        lwd = 2
      )
      lines.loessArgs <- modifyList(lines.loessArgs, lines.loess..., keep.null = TRUE)
      if (is_invalid(lines.loessArgs$col))
        lines.loessArgs$col = col[s]

      do.call(graphics::lines, lines.loessArgs)
    }
  })

  r <- list()

  ## Linear trends.
  if (trend) local({
    trendArgs <- list(
      data = as.data.frame(y[, c(x_var, series)]),
      range = range(as.data.frame(w)[, x_var], na.rm = TRUE),
      lwd = trend_lwd,
      legend_inset = trend_legend_inset,
      trend_multiplier = 0,
      rate_expression = sprintf("expression(Delta ~ \"= %%+1.2f %s/%s\")", unit, x_var),
      keep_default_trends = TRUE,
      sort_by_name = FALSE
    )
    if (!is_invalid(trend...$keep_default_trends))
      trendArgs$keep_default_trends <- trend...$keep_default_trends
    if (trendArgs$keep_default_trends) {
      if (is_invalid(trendArgs$m))
        trendArgs$m <- list()
      length(trendArgs$m) <- length(series); names(trendArgs$m) <- series
    }
    trendArgs <- utils::modifyList(trendArgs, trend..., keep.null = TRUE)
    if (!is_invalid(extra_trends))
      trendArgs$m <- append(trendArgs$m, extra_trends)

    for (i in seq_along(trendArgs$m)) {
      if (is_invalid(trendArgs$m[[i]]$range))
        trendArgs$m[[i]]$range <- trendArgs$range
      if (is_invalid(trendArgs$m[[i]]$col))
        trendArgs$m[[i]]$col <- col[names(trendArgs$m)[i]]
      if (is_invalid(trendArgs$m[[i]]$lwd))
        trendArgs$m[[i]]$lwd <- trendArgs$lwd

      trendArgs$m[[i]]$sdata <- trendArgs$data %>%
        dplyr::select(c(intersect(common_columns, colnames(trendArgs$data)), names(trendArgs$m)[i])) %>%
        dplyr::filter(!!as.name(x_var) >= trendArgs$m[[i]]$range[1] & !!as.name(x_var) <= trendArgs$m[[i]]$range[2])
      trendArgs$m[[i]]$lm <-
        lm(eval(substitute(b ~ x,
          list(b = as.name(names(trendArgs$m)[i]), x = as.name(x_var)))), data = trendArgs$m[[i]]$sdata, x = TRUE)
      trendArgs$m[[i]]$change <- coef(trendArgs$m[[i]]$lm)[2] * diff(range(trendArgs$m[[i]]$lm$model[, 2]))
      trendArgs$m[[i]]$rate <- coef(trendArgs$m[[i]]$lm)[2] * trendArgs$trend_multiplier
      # trendArgs$m[[i]]$rateText <-
      #   eval(substitute(expression(paste(Delta, " = ", r, phantom(l), unit, denom_text, sep = "")),
      #     list(r = sprintf(trendArgs$m[[i]]$rate, fmt = trendArgs$fmt), unit = trendArgs$unit,
      #       denom_text = trendArgs$denom_text)))
      trendArgs$m[[i]]$rateText <- eval_js(sprintf(trendArgs$rate_expression, trendArgs$m[[i]]$rate))
    }
    if (trendArgs$sort_by_name)
      trendArgs$m <- trendArgs$m[sort(names(trendArgs$m))]

    legendText <- NULL
    for (i in seq_along(trendArgs$m)) {
      ## Set clipping region for 'abline()'.
      xRange <- range(trendArgs$m[[i]]$sdata[!is.na(trendArgs$m[[i]]$sdata[, names(trendArgs$m)[i]]), x_var], na.rm = TRUE)
      yRange <- range(trendArgs$m[[i]]$sdata[, names(trendArgs$m)[i]], na.rm = TRUE)

      usr <- par("usr")
      graphics::clip(xRange[1], xRange[2], yRange[1], yRange[2])

      graphics::abline(trendArgs$m[[i]]$lm, col = trendArgs$m[[i]]$col, lwd = trendArgs$m[[i]]$lwd)

      ## Reset clipping to plot region.
      do.call(graphics::clip, as.list(usr))

      legendText <- c(legendText, trendArgs$m[[i]]$rateText)
    }

    if (!is.null(trendArgs$legend_inset))
      legend("bottomright", inset = trendArgs$legend_inset, legend = legendText, col = sapply(trendArgs$m, function(a) a$col),
        lwd = sapply(trendArgs$m, function(a) a$lwd), bty = "n", cex = 0.8)

    r$trend <<- trendArgs$m
  })

  ## Segmented linear regression.
  if (segmented) local({
    segmentedArgs <- list(
      x = y, # Was 'x = x' (06 Dec 2022)
      series = series,
      start = start,
      end = end,
      x_var = x_var
    )
    if (!is_invalid(segmented...$x) && is.list(segmented...$x)) { # V. "?utils::modifyList" for why this is necessary.
      segmentedArgs$x <- segmented...$x
      segmented...$x <- NULL
    }
    segmentedArgs <- utils::modifyList(segmentedArgs, segmented..., keep.null = TRUE)
    sm <- do.call("fit_segmented_model", segmentedArgs)

    for (i in names(sm$piecewise)) {
      ## Set clipping region for 'plot.segmented()' and 'abline()'.
      xRange <- range(wz[!is.na(wz[, i]), x_var], na.rm = TRUE)
      yRange <- range(wz[, i], na.rm = TRUE)
      usr <- graphics::par("usr")
      graphics::clip(xRange[1], xRange[2], yRange[1], yRange[2])

      x <- sm$piecewise[[i]]$sm

      plot.segmentedArgs <- list(
        x = x,
        add = TRUE,
        rug = FALSE,
        #lty = "longdash",
        lwd = 2,
        pch = NA_integer_, # Not necessary
        col = col[i]
      )
      plot.segmentedArgs <- utils::modifyList(plot.segmentedArgs, plot.segmented..., keep.null = TRUE)

      if (!is.null(x) && inherits(x, "segmented")) {
        dev_null <- do.call("plot", plot.segmentedArgs)

        if (mark_segments != "none") {
          ## Reset clipping to whole plot region.
          do.call(graphics::clip, as.list(usr))

          if (mark_segments == "lines") {
            vlineArgs <- list(
              mark_x = sprintf(sm$piecewise[[i]]$sm$psi[, 2], fmt = "%1.1f")
            )
            vlineArgs <- utils::modifyList(vlineArgs, vline..., keep.null = TRUE)
            do.call("vline", vlineArgs)
          } else if (mark_segments == "points") {
            points.segmentedArgs <- list(
              x = sm$piecewise[[i]]$sm,
              col = plot.segmentedArgs$col,
              pch = 4 # Like '×'
            )
            points.segmentedArgs <- utils::modifyList(points.segmentedArgs, points.segmented..., keep.null = TRUE)
            do.call(graphics::points, points.segmentedArgs)
          }

          ## Turn clipping back on for any further plotting.
          graphics::clip(xRange[1], xRange[2], yRange[1], yRange[2])
        }
      } else {
        abline(sm$piecewise[[i]]$lm, col = plot.segmentedArgs$col, lwd = plot.segmentedArgs$lwd)
      }

      ## Reset clipping to plot region.
      do.call(graphics::clip, as.list(usr))
    }

    r$segmented <<- sm
  })

  if (!is.null(end_callback))
    plinth::poly_eval(end_callback)

  if (sign)
    eval(sign_callback)

  if (save_png)
    dev.off()

  if (!add)
    par(op)

  if (length(r) > 0L)
    return (invisible(r))

  return (nop())
}


## Style suggested by https://tamino.wordpress.com/2014/12/04/a-pause-or-not-a-pause-that-is-the-question/
#' @export
plot_sequential_trend <- function(
  x, # A data frame
  series,
  x_var,
  start = NULL, end = NULL,
  trend_multiplier = 1,
  main = NULL, xlab = NULL, ylab = NULL,
  use_polygon = FALSE,
  mark_xs = NULL,
  baseline = FALSE,
  plot... = list(),
  ci... = list(),
  vline... = list(),
  m_text = NULL,
  ...
)
{
  d <- x
  if (!is.null(start))
    d <- x[x[[x_var]] >= start, ]

  series <- series[1L]

  means <- unclass(by(d[[series]], d[[x_var]], mean, na.rm = TRUE))
  xNames <- names(means)
  means <- as.vector(means); names(means) <- xNames
  means <- means[na_unwrap(means)]
  rates <- rep(NA, length(means))
  rates <- data.frame(rate = rates, lwr = rates, upr = rates)
  rownames(rates) <- names(means)
  xs <- as.numeric(rownames(rates))

  local({
    for (i in seq_along(means)) {
      y <- means[i:length(means)]
      x <- as.numeric(names(y))

      m <- stats::lm(y ~ x)
      r <- stats::coef(m)[2L] * trend_multiplier
      ci <- suppressWarnings(t(stats::confint(m, "x"))) * trend_multiplier
      rates[i, ] <<- c(r, ci)
    }
  })

  if (is.null(start)) start <- head(xs, 1L)
  if (is.null(end)) end <- tail(xs, 1L)

  attr(rates, "series") <- series
  attr(rates, "trend_start") <- start
  attr(rates, "trend_end") <- tail(xs, 1L)

  ccRates <- rates[xs <= end & stats::complete.cases(rates), ]
  ccXs <- as.numeric(rownames(ccRates))[xs <= end & stats::complete.cases(rates)]

  plotArgs <- list(
    x = ccXs,
    y = ccRates[, "rate"],
    type = "o",
    pch = 16,
    #ylim = c(-1.0, 1.0),
    ylim = c(min(ccRates[, "lwr"]), max(ccRates[, "upr"])),
    lwd = 2,
    col = "black",
    panel.first = quote(abline(h = 0.0, col = "darkgray", lty = "dashed")),
    xlab = ifelse(!is_invalid(xlab), xlab, "Start value of trend"),
    ylab = ifelse(!is_invalid(ylab), ylab, "Trend"),
    main = ifelse(!is_invalid(main), main, "Linear Trend + 95% CIs")
  )
  plotArgs <- utils::modifyList(plotArgs, plot..., keep.null = TRUE)

  if (is.language(plotArgs$main))
    plotArgs$main <- eval(plotArgs$main)

  if (is.null(m_text))
    m_text <- quote("Trend from start value to " %_% tail(names(means), 1))

  if (dev.cur() == 1) # If a graphics device is active, plot there instead of opening a new device.
    dev.new(width = 12.5, height = 7.3) # New default device of 1200 × 700 px at 96 DPI.
  do.call(plot, plotArgs)
  mtext(eval(m_text))

  if (use_polygon) {
    ciArgs <- list(
      x = c(ccXs, rev(ccXs)),
      y = c(ccRates[, "lwr"], rev(ccRates[, "upr"])),
      col = scales::alpha("gray", 0.6),
      border = NA # Border color; NULL means use par("fg"), NA omits borders.
    )
    ciArgs <- modifyList(ciArgs, ci..., keep.null = TRUE)

    do.call(graphics::polygon, ciArgs, keep.null = TRUE)
  }
  else { # Use error bars to show confidence intervals.
    ciArgs <- list(
      x0 = ccXs,
      y0 = ccRates[, "lwr"],
      x1 = ccXs,
      y1 = ccRates[, "upr"],
      length = 0.03,
      angle = 90,
      code = 3
    )
    ciArgs <- utils::modifyList(ciArgs, ci..., keep.null = TRUE)

    do.call(graphics::arrows, ciArgs)
  }

  if (!is.null(mark_xs)) {
    vlineArgs <- list(
      mark_x = mark_xs,
      abline... = list(abline... = list(col = scales::alpha("red", 0.4)))
    )
    vlineArgs <- utils::modifyList(vlineArgs, vline..., keep.null = TRUE)

    do.call(vline, vlineArgs)
  }

  return (rates)
}


#' @export
create_smooth_variables <- function(
  x,
  series = NULL,
  x_var,
  unwrap = TRUE,
  force_names = TRUE,
  pad_by = NULL,
  seq... = list(),
  interpNA... = list(),
  loess_suffix = " (LOESS fit)",
  loess... = list(),
  interpolated_suffix = " (interpolated)",
  keep_interpolated = FALSE,
  deriv = NULL, # 0, 1, or 2; or some combo of them
  frfast... = list(),
  deriv_suffix_template = " (derivative %d)",
  lower_ci_suffix = "_lower", upper_ci_suffix = "_upper",
  interpolated_derivative = FALSE,
  plot_series_friendly = TRUE,
  verbose = FALSE,
  ...
)
{
  if (is.null(series))
    series <- colnames(x)[colnames(x) != x_var]

  d <- tibble::as_tibble(x)
  if (unwrap)
    d <- subset(d, na_unwrap(d[, series]))

  seqArgs <- list(
    from = min(d[[x_var]], na.rm = TRUE),
    to = max(d[[x_var]], na.rm = TRUE),
    by = pad_by
  )
  seqArgs <- utils::modifyList(seqArgs, seq..., keep.null = TRUE)

  if (!is.null(pad_by)) {
    attrs <- attributes(d)

    seq_vals <- do.call(seq, seqArgs)
    temp <- sort(unique(c(d[[x_var]], seq_vals)))
    tbl <- dataframe(temp); names(tbl) <- x_var
    d <- dplyr::right_join(d, tbl, by = x_var)

    ## dplyr joins remove attributes, so:
    missingAttrs <- dplyr::setdiff(names(attrs), names(attributes(d)))
    plyr::l_ply(missingAttrs,
      function(i)
      {
        attr(d, i) <<- attrs[[i]]
      })
  }

  interpNAArgs <- list(
    method = "fmm"
  )

  if (is.numeric(series) && force_names)
    series <- colnames(d)[series]

  d_interp <- sapply(series,
    function(a)
    {
      interpNAArgs$x <- d[, a]
      interpNAArgs <- utils::modifyList(interpNAArgs, interpNA..., keep.null = TRUE)

      drop(do.call(interpNA, interpNAArgs))
    }, simplify = FALSE) %>% `names<-`(names(.) %_% interpolated_suffix) %>% dataframe()
  d <- dplyr::bind_cols(d, d_interp)
  rm(d_interp)

  d_loess <- sapply(series,
    function(a)
    {
      loessArgs = list(
        formula = eval(substitute(s ~ x_var, list(s = as.name(a %_% interpolated_suffix), x_var = as.name(x_var)))),
        data = d,
        span = 0.2
      )
      loessArgs <- utils::modifyList(loessArgs, loess..., keep.null = TRUE)

      l <- do.call(stats::loess, loessArgs)
      stats::predict(l, newdata = d[[x_var]]) # Use 'newdata' here to include NAs.
    }, simplify = FALSE) %>% `names<-`(names(.) %_% loess_suffix) %>% dataframe()
  d <- dplyr::bind_cols(d, d_loess)
  rm(d_loess)

  #attr_frfast <- sapply(series,
  attr_frfast <- psapply(series,
    function(a)
    {
      if (is.null(deriv))
        return (NULL)

      if (verbose) cat("  Processing column \"" %_% a %_% "\".... ")

      if (interpolated_derivative)
        gam_data <- dataframe(d[, x_var, drop = FALSE], y = d[[a %_% interpolated_suffix]]) %>%
          tidyr::drop_na() # Complete cases only
      else
        gam_data <- dataframe(x[, x_var, drop = FALSE], y = x[[a]]) %>%
          tidyr::drop_na() # Complete cases only

      temp <- if (!is.null(pad_by)) seq_vals else x[[x_var]]
      kbin <- sum(temp >= min(gam_data[[x_var]], na.rm = TRUE) & temp <= max(gam_data[[x_var]], na.rm = TRUE))

      frfastArgs <- list(
        formula = eval(substitute(y ~ s(x_var, k = knots),
          list(x_var = as.name(x_var), knots = ifelse(NROW(gam_data) < 10, NROW(gam_data), 10)))),
        data = gam_data,
        #h0 = -1,
        p = 3,
        kbin = kbin,
        nboot = 100,
        smooth = "splines",
        seed = 666,
        cluster = FALSE,
        ncores = NULL
      )
      frfastArgs <- utils::modifyList(frfastArgs, frfast..., keep.null = TRUE)

      if (frfastArgs$smooth != "splines")
        frfastArgs$formula <- eval(substitute(y ~ x_var, list(x_var = as.name(x_var))))

      deriv <- sort(deriv)

      z <- do.call(npregfast::frfast, frfastArgs)
      zz <- dataframe(z$x, z$p[, 1, 1], z$pl[, deriv + 1, 1], z$p[, deriv + 1, 1], z$pu[, deriv + 1, 1])
      colnames(zz) <- c(x_var, a,
        sapply(c(lower_ci_suffix, "", upper_ci_suffix),
          function(b) paste0(a %_% sapply(deriv, sprintf, fmt = deriv_suffix_template), b),
          simplify = FALSE)) %>% unlist

      zzz <- zz
      if (plot_series_friendly) {
        ## Make a data set compatible with 'plinth::plot_series()'.
        dv <- sapply(deriv, sprintf, fmt = deriv_suffix_template)
        ll <- lower_ci_suffix
        ul <- upper_ci_suffix

        reCis <- "(" %_% paste(Hmisc::escapeRegex(trimws(c(dv %_% ll, dv %_% ul))), collapse = "|") %_% ")"
        ciCols <- grep(reCis, colnames(zz), value = TRUE)
        ciVars <- sapply(c(ll, ul), function(b) paste0(a %_% dv, b), simplify = FALSE)
        ciPairs <- chunk(combine_groups(list(a %_% dv, c(ll, ul)), sep = ""), 2)

        uncVars <- a %_% dv %_% "_uncertainty"
        unc <- dplyr::bind_cols(mapply(uncVars, ciPairs,
          FUN = function(i, j)
          {
            zz %>% dplyr::select(j) %>% dplyr::mutate(!! i := .[[j[2]]] - .[[j[1]]])
          }, SIMPLIFY = FALSE))
        zzz <- dplyr::bind_cols(list(dplyr::select(zz, c(x_var, a, a %_% dv)), unc))
      }

      if (verbose) { cat("Done.", fill = TRUE); flush.console() }

      zzz
    }, simplify = FALSE) %>% purrr::compact()
    attr(d, "frfast") <- attr_frfast
    rm(attr_frfast)

  if (!keep_interpolated) {
    for (i in series)
      d[[i %_% interpolated_suffix]] <- NULL
  }

  plyr::l_ply(names(attr(d, "frfast")),
    function(a)
    {
      attr(d, "frfast")[[a]] <<- dplyr::arrange(attr(d, "frfast")[[a]], !! rlang::sym(c(x_var)))
    })

  d
}
