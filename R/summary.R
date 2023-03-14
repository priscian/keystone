#' @export
vu_summary <- function(
  f,
  p,
  subset = TRUE,
  envir = parent.frame(),
  test = FALSE, digits = 5L, overall = FALSE, exclude1 = FALSE, na_include = FALSE, continuous = 5L,
  verbose = TRUE,
  printout = TRUE,
  size = NULL,
  latex = FALSE,
  latex_corrections = list(
    latex_correct_insert_bottom,
    latex_correct_caption_position
  ),
  latex... = list(),
  return_latex = FALSE,
  summary... = list(),
  summary_formula = Hmisc::summaryM,
  print... = list(),
  evaluate_dots = TRUE,
  ...
)
{
  if (!inherits(p, "pointer"))
    stop("Function argument is not a pointer.")

  if (is.null(subset))
    subset <- TRUE

  dots <- get_dots(..., evaluate = evaluate_dots)

  if (!(identical(summary_formula, Hmisc::summaryM) || identical(summary_formula, "summaryM"))) {
    #f <- flip(f)
    summary...$method <- "reverse"
  }

  environment(f) <- environment()
  split_f <- split(f)

  summaryFormulaArgs = list(
    formula = f,
    data = ..(p),
    subset = subset,
    #method = "reverse", # Only for 'summary.formula()'.
    test = test,
    overall = overall,
    na.include = na_include,
    continuous = continuous
  )
  summaryFormulaArgs = modifyList(summaryFormulaArgs, summary...)

  s_ <- try(do.call(summary_formula, summaryFormulaArgs), silent = !verbose)

  dotsArguments <- dots$arguments
  if (evaluate_dots) dotsArguments <- dots$evaluated

  if (inherits(s_, "try-error"))
    return (NULL)
  else {
    if (!latex) {
      printSummaryFormulaArgs <- list(
        x = s_,
        long = TRUE,
        digits = digits,
        what = "%",
        exclude1 = exclude1,
        prtest = c("P")
      )
      ## Add '...' arguments to argument list.
      printSummaryFormulaArgs <-
        c(utils::modifyList(printSummaryFormulaArgs, dotsArguments[dots$dots_names != ""]),
          dotsArguments[dots$dots_names == ""], keep.null = TRUE)
      printSummaryFormulaArgs <- utils::modifyList(printSummaryFormulaArgs, print..., keep.null = TRUE)

      if (printout)
        do.call("print", printSummaryFormulaArgs)
    }
    else {
      latexArgs <- list(
        object = s_,
        file = "",
        booktabs = TRUE,
        ctable = FALSE,
        here = FALSE,
        na.blank = TRUE,
        size = size,
        center = "center",
        long = TRUE,
        digits = digits,
        what = "%",
        pctdig = 1L,
        exclude1 = exclude1,
        npct.size = "smaller[2]",
        Nsize = "smaller[1]",
        outer.size = "smaller[2]",
        legend.bottom = TRUE,
        prtest = c("P")
      )
      ## Add '...' arguments to argument list.
      latexArgs <-
        c(utils::modifyList(latexArgs, dotsArguments[dots$dots_names != ""]),
          dotsArguments[dots$dots_names == ""], keep.null = TRUE)
      latexArgs <- utils::modifyList(latexArgs, latex..., keep.null = TRUE)

      if (length(latex_corrections) > 0L) {
        l <- capture.output(rv <- do.call(Hmisc::latex, latexArgs))
        for (f in latex_corrections) {
          l <- do.call(f, list(l))
        }

        if (printout)
          cat(l, sep = "\n")
      }
      else {
        rv <- do.call(Hmisc::latex, latexArgs)
      }

      if (return_latex)
        return (rv)
    }
  }

  return (s_)
}
