## Fit segmented linear models to selected data.
#' @export
fit_segmented_model <- function(x,
  series,
  start = NULL, end = NULL,
  yearly = FALSE, x_var = NULL,
  breakpoints... = list(),
  segmented... = list(), seg.control... = list(seed = 100),
  ...
)
{
  r <- list(data = x, series = series)
  r$range <- list(start = start, end = end)

  g <- r$data
  xVar <- x_var

  r$piecewise <- list()
  for (i in r$series) {
    r$piecewise[[i]] <- list()

    h <- oss(g, i, common_columns = xVar)[na_unwrap(g[[i]]), , drop = FALSE]
    h <- h[h[[xVar]] >= ifelse(!is.null(start), start, -Inf) & h[[xVar]] <= ifelse(!is.null(end), end, Inf), ]

    breakpointsArgs <- list(
      formula = eval(substitute(Y ~ X, list(X = as.name(xVar), Y = as.name(i)))),
      data = h,
      breaks = NULL
    )
    breakpointsArgs <- utils::modifyList(breakpointsArgs, breakpoints..., keep.null = TRUE)
    r$piecewise[[i]]$bp <- do.call(strucchange::breakpoints, breakpointsArgs)

    #r$piecewise[[i]]$breaks <- r$piecewise[[i]]$bp$X[, xVar][r$piecewise[[i]]$bp$breakpoint]
    r$piecewise[[i]]$breaks <-
      r$piecewise[[i]]$bp$X[, which(colnames(r$piecewise[[i]]$bp$X) %in%
        c(xVar, backtick(xVar)))][r$piecewise[[i]]$bp$breakpoint]

    seg.controlArgs <- list(
      #stop.if.error = TRUE,
      fix.npsi = TRUE,
      K = length(r$piecewise[[i]]$breaks),
      n.boot = 250,
      random = FALSE,
      h = 0.3
    )
    seg.controlArgs <- utils::modifyList(seg.controlArgs, seg.control..., keep.null = TRUE)
    segControl <- do.call(segmented::seg.control, seg.controlArgs)

    ## Set up new variable names to avoid potential problems in 'segmented::segmented()':
    hh <- h %>% dplyr::rename(x := !!xVar, y := !!i)
    r$piecewise[[i]]$lm <- lm(y ~ x, data = hh, x = TRUE, y = TRUE)

    segmentedArgs <- list(
      obj = r$piecewise[[i]]$lm,
      #seg.Z = substitute(~ X, list(X = as.name(xVar))),
      seg.Z = ~ x,
      psi = r$piecewise[[i]]$breaks,
      control = segControl
    )
    segmentedArgs <- utils::modifyList(segmentedArgs, segmented..., keep.null = TRUE)

    run_segmented <- function()
    {
      mf <- model.frame(r$piecewise[[i]]$lm)

      while (TRUE) {
        withRestarts({
          sm <- do.call(segmented::segmented, segmentedArgs)
          break
        },
          restart = function() {
            ## Which breakpoint is closest to the start or end of the time series?
            if (length(segmentedArgs$psi) > 1L) {
              segmentedArgs$psi <<- segmentedArgs$psi[-which.min(pmin(segmentedArgs$psi, NROW(mf) - segmentedArgs$psi + 1))]
            }
          })
      }

      sm
    }

    tryCatch({
      withCallingHandlers({
          sm <- run_segmented()
        },
          error = function(e) {
            message("Error: ", e$message); flush.console()
            if (any(grepl("one coef is NA: breakpoint(s) at the boundary", e$message, fixed = TRUE)))
            #if (any(grepl("number of items to replace is not a multiple of replacement length", e$message, fixed = TRUE)) && length(segmentedArgs$psi) > 1L)
              invokeRestart("restart")
          }
      )

      r$piecewise[[i]]$sm <- sm
    }, error = function(e) { message("Warning: No breakpoint(s) found"); flush.console() })
  }

  r
}
