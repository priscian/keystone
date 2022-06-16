### N.B. The functions in this file are likely to be eliminated at some point!

####################
##### Corresponding to "data-frame.R"
####################

## Subset a data set and clean it up a bit all at once.
#' @export
subset_plus <- function(x, subpart, exclusions, inclusions, ...)
{
  p <- x
  if (!inherits(p, "pointer"))
    p <- ptr(x)

  if (!missing(subpart))
    x <- subset(..(p), subpart)
  else
    x <- subset(..(p))

  if (!missing(inclusions))
    x <- x[, inclusions]
  if (!missing(exclusions))
    x <- x[, setdiff(names(x), exclusions)]

  for (i in seq(NCOL(x))) {
    if (inherits(x[, i], "mChoice")) {
      if (!missing(subpart))
        x[, i] <- update(x[, i], subpart)
    }
    else if (inherits(x[, i], "data.frame"))
      row.names(x[[names(x)[i]]]) <- NULL # Can't use x[, i] here, since it seems to pick out only one column.
  }

  drop_levels(ptr(x))
  renumber(ptr(x))

  return (x)
}


## Quickly select columns from a data frame or matrix.
#' @export
select_df <- function(
  x,
  ...,
  subpart = NULL,
  show_always = NULL,
  view = FALSE,
  sort = c(NA, "index", "name"),
  allow_duplicates = FALSE,
  drop = FALSE,
  do_search = TRUE
)
{
  # 'x' is a data frame or matrix.
  # '...' takes column names or column numbers of 'x' to be included in the output variable. If no column names or numbers are given, all the columns are kept. Unsyntactic column names should be enclosed in single or double quotes. Any expression evaluable in the calling context can be used; note that some expressions may need to be enclosed in backticks (`), and those enclosed in quotes will be processed as column names. Negative numbers will become offsets from the maximum column number, where -1 refers to ncol(x), -2 to ncol(x) - 1, -3 to ncol(x) - 2, etc. Ranges of column numbers such as 5:10 can be used; this is a consequence of the aforementioned expression evaluation.
  #   If 'p = "<regex>"' is given as a named (non-formal) argument, its value will be used to call 'pgrep("<regex>", x, silent = TRUE)' and put the result in situ among the other columns of '...'.
  # 'subpart' is a logical expression indicating elements or rows to keep, with missing values taken as false.
  # 'show.always' is a vector of column names that should always be included in the output variable whether they're explicitly named or not; should be set to null for no explicit inclusion.
  # 'view': logical; should results be displayed through the 'View()' function rather than returned?
  # 'sort' is the sort method for the selected column: NA for no sorting, "index" to order the columns as in 'x', or "name" to order the columns alphabetically.
  # 'allow.duplicates': should repeated column names/numbers be allowed? Note that the default behavior of the extraction operator '[.data.frame()' is to tack on sequential prefixes ".1", ".2", etc. to the name of subsequent instances of the column.
  # 'drop': logical, passed to '[.data.frame()'. If true the result is coerced to the lowest possible dimension. The default is to drop if only one column is left, but not to drop if only one row is left.
  # 'do.search': logical. If true, all the environments on the search path are checked for variables with the same names as the elements of '...'; matches are automatically 'deparse()'d without evaluation. If false, the time-consuming search is skipped, but the user must be careful to explicitly quote column names that have the same name as functions etc. in the search path, or risk errors.

  sort <- match.arg(sort)

  dots <- get_dots(...)

  cols <- seq_along(names(x))

  if (!is_invalid(dots$arguments)) {
    ## Which symbols can be evaluated as value types or in the calling context?
    evalFun <- function(a, b)
    {
      tryCatch({
        if (do_search) {
          if (length(find(deparse(a))) > 0L) # If 'deparse(a)' has the same name as any variable in the search path, skip evaluation.
            stop()
        }

        if (b == "p") {
          pgrep(a, x, silent = TRUE)
          #do.call(pgrep, list(pattern = a, x, silent = TRUE))
        }
        else
          eval(a)
      }, error = function(e) deparse(a))
    }

    cols <- mapply(evalFun, dots$arguments, dots$named_dots, SIMPLIFY = FALSE)

    if (!is.null(show_always))
      cols <- c(as.list(show_always), cols)

    ## Find the index of unevaluable symbols among the column names of 'x':
    cols <- unlist(sapply(cols, function(a) { r <- a; if (is.character(a)) r <- which(colnames(x) %in% a); r }))

    ## Eliminate any column numbers outside the range of '0:ncol(x)':
    cols <- cols[!(abs(cols) == 0L | abs(cols) > ncol(x))] # Not necessary to remove zeroes, though.

    ## Because negative and positive indices can't be mixed, turn negative values into offsets from the maximum column number:
    cols[cols < 0L] <- ncol(x) + cols[cols < 0L] + 1L

    if (!allow_duplicates)
      cols <- unique(cols)
  }

  if (!is.na(sort)) {
    if (sort == "index")
      cols <- sort(cols)
    else if (sort == "name")
      cols <- match(names(x)[cols][order(names(x)[cols])], names(x))
  }

  x <- x[, cols, drop = drop]

  if (!is.null(subpart))
    x <- subset_plus(x, subpart)

  if (view)
    View(x)
  else
    return (x)
}

#' @export
sdf <- select_df

## usage:
# x <- structure(list(patid = c("002-0085-001", "002-0085-002", "002-0085-003", "002-0085-004", "002-0085-005"), protcode = structure(c(1L, 1L, 1L, 1L, 1L), .Label = c("0085", "0120", "0123", "0165", "0191", "0219"), class = c("labelled", "factor"), label = "Protocol number"), rx = c(1L, 2L, 1L, 2L, 1L), race = structure(c(1L, 2L, 2L, 2L, 2L), .Label = c("White", "Black", "Hispanic", "Asian", "other"), class = c("labelled", "factor"), label = "Race/Ethnicity"), age = structure(c(55.4388546657541, 28.1670088980151, 46.3545516769336, 54.1875427789185, 53.5905772302076), label = "Age", units = "years", class = "labelled")), .Names = c("patid", "protcode", "rx", "race", "age"), row.names = c(NA, 5L), class = "data.frame")
# sdf(x, patid, race, -1, 2:3)
# sdf(x, patid, "race", -1, 2:3) # Quotes required only for unsyntactic column names.
# sdf(x, patid, race, -1, 2:3, frog, `1 + 1 <- 2`) # Last two columns are unevaluable and not in column names of 'x', so are dropped.
# sdf(x, patid, race, -1, 2:3, 1) # Duplicate "patid" is dropped.
# sdf(x, patid, race, -1, 2:3, 1, allow_duplicates = TRUE)
# sdf(x, patid, race, -1, 2:3, subpart = (seq(nrow(x)) %in% 2:4))
# sdf(x, patid, race, -1, 2:3, subpart = (x$race == "Black"))
# sdf(x, patid, age, pgrep("^r", x, silent = TRUE), protcode)
# sdf(x, patid, age, p = "^r", protcode) # Using special 'p' argument to call 'pgrep("^r", x, silent = TRUE)'; keeps argument ordering.
# sdf(x, patid, age, p = "^r", bad = "not.column", protcode, rx)
# sdf(x, patid, age, p = "^r", bad = "not.column", protcode, rx, allow_duplicates = TRUE)
# sdf(x, patid, age, p = "^r", bad = "not.column", protcode, rx, sort = "index")
# sdf(x, patid, age, p = "^r", bad = "not.column", protcode, rx, sort = "index", allow_duplicates = TRUE)
# sdf(x, patid, age, p = "^r", bad = "not.column", protcode, rx, sort = "name")
# sdf(x, patid, age, p = "^r", bad = "not.column", protcode, rx, sort = "name", allow_duplicates = TRUE)


## Quickly select columns from a data frame or matrix and display them using the 'View()' function.
#' @export
view_select_df <- function(x, ...)
{
  select_df(x, ..., view = TRUE)
}

#' @export
vdf <- view_select_df


#' @export
#' @importFrom gtools mixedsort
#' @importFrom pryr where
pgrep <- function(
  pattern = "",
  ...,
  v = "vs_",
  sort_names = FALSE,
  silent = FALSE,
  verbose = TRUE,
  value = TRUE,
  perl = TRUE,
  simplify = TRUE,
  envir = parent.frame())
{
  dots <- get_dots(...)

  namedArgs <- list()
  for (argName in dots$all_named_args) { # Get list of arguments to send into 'grep()'; no need to keep track of local args.
    if (!(argName %in% names(formals(grep)))) next
    val <- get(argName)
    if (is.null(val)) namedArgs[argName] <- list(NULL)
    else namedArgs[[argName]] <- val
  }

  parms <- dots$arguments
  parmNames <- dots$as_character

  rv <- list()

  for (i in seq_along(parms)) {
    p <- eval(parms[[i]], envir)

    obj <- NULL
    objName <- ""
    objEnv <- NULL

    if (is.numeric(p)) { # Removed GOG legacy code
      obj <- p
      objName <- parmNames[i]
    }
    else if (is.character(p)) {
      obj <- get(p)
      objName <- p
    }
    else if (is.pointer(p)) {
      obj <- ..(p)
      objName <- p$name
      objEnv <- environmentName(p$object)
    }
    else {
      obj <- p
      objName <- parmNames[i]
    }

    if (is.null(objEnv))
      objEnv <- environmentName(pryr::where(objName, envir))

    varNames <- names(obj)
    if (is.null(varNames))
      varNames <- character()
    if (sort_names) varNames <- gtools::mixedsort(varNames)

    namedArgs$x <- varNames

    out <- do.call("grep", namedArgs)
    rv[[objName]] <- out

    if (!silent) {
      if (verbose)
        catn("##### For object '" %_% objName %_% "' in <environment: " %_% objEnv %_% ">:")
      print(out)
    }
  }
  if (silent) {
    if (simplify && length(rv) == 1L)
      rv <- rv[[1L]]

    return (rv)
  }
}


####################
##### Corresponding to "latex.R"
####################

#' @export
make_sweave_document <- function(
  file_path,
  output_path = "./",
  graphics_path = "../images/",
  header,
  document = "",
  bib,
  template_path
)
{
  fileNameBase <- "output"
  if (missing(file_path))
    filepath_ <- "./" %_% fileNameBase %_% ".snw"
  else
    filepath_ <- file_path

  if (missing(template_path))
    templatePath <- system.file("templates", "report-template.snw", package = "keystone")
  else
    templatePath <- template_path

  hastitle_ <- "false"
  if (!missing(header)) {
    hastitle_ <- "true"

    if (!is.null(header$title)) title_ <- header$title
    else title_ <- ""

    if (!is.null(header$author)) author_ <- header$author
    else author_ <- ""

    if (!is.null(header$date)) date_ <- header$date
    else date_ <- "\\\\today"
  }

  ## Separate document parts with vertical whitespace.
  document_ <- document

  hasbib_ <- "false"
  bibstylepath_ <- "C:/common/tex/jjava"
  bibpath_ <- "C:/common/tex/gog"

  if (!missing(bib)) {
    hasbib_ <- "true"

    if (!is.logical(bib)) {
      if (!is.null(bib$bibstylepath)) bibstylepath_ <- bib$bibstylepath
      if (!is.null(bib$bibpath)) bibpath_ <- bib$bibpath
    }
    else {
      if (!bib) hasbib_ <- "false"
    }
  }

  ## Load template.
  s <- readLines(templatePath)

  ## Replace appropriate placeholders.
  s <- gsub("@@HASTITLE@@", hastitle_, s)
  if (hastitle_ == "true") {
    s <- gsub("@@TITLE@@", title_, s)
    s <- gsub("@@AUTHOR@@", author_, s)
    s <- gsub("@@DATE@@", date_, s)
  }

  w <- head(which(grepl("@@DOCUMENT@@", s)), 1L)
  s <- append(s, document_, w)[-w]

  s <- gsub("@@HASBIB@@", hasbib_, s)
  if (hasbib_ == "true") {
    s <- gsub("@@BIBSTYLEPATH@@", bibstylepath_, s)
    s <- gsub("@@BIBPATH@@", bibpath_, s)
  }

  s <- gsub("@@FILENAME@@", filepath_, s)
  s <- gsub("@@OUTPUT@@", output_path %_% basename(tools::file_path_sans_ext(filepath_)) %_% ".tex", s)

  s <- gsub("@@GRAPHICSPATH@@", graphics_path, s)

  ## Write output document.
  writeLines(s, filepath_)

  return (nop())
}


#' @export
make_latex_chunk <- function(
  contents = "",
  type = c("code", "source", "graphics"),
  caption,
  label
)
{
  type <- match.arg(type)

  if (missing(caption)) caption <- ""
  captionLatex <- "\\caption{" %_% caption
  captionLatex <- captionLatex %_% ifelse(!missing(label), "\\label{fig:" %_% label %_% "}}", "}")

  ## Separate chunk contents with vertical whitespace.
  contents <- c(contents, character(length(contents)))
  contents <- head(as.vector(matrix(contents, nrow = 2L, byrow = TRUE)), -1L)

  chunk <- switch(type,
    "source" = c(
      "<<echo=F, results=hide>>=",
      "  #tryCatch(source(\"" %_% contents[1L] %_% "\"), error = function(e) e)",
      "  source(\"" %_% contents[1L] %_% "\")",
      "@"),
    "graphics" = c(
      "\\begin{figure}[!ht]",
      "\\begin{center}",
      "<<echo=F, fig=T>>=",
      paste("  ", contents, sep = ""),
      "@",
      "  " %_% captionLatex,
      "\\end{center}",
      "\\end{figure}"),
    c( # Default option
      "<<echo=F, results=tex>>=",
      paste("  ", contents, sep = ""),
      "@")
  )
  chunk <- c(chunk, "")

  return (chunk)
}


#' @export
build_report_document <- function(
  file_path_base,
  study_title,
  study_code,
  investigator,
  output_path = "./report/",
  make_graphics_chunk = FALSE,
  ...
)
{
  if (missing(file_path_base)) file_path_base <- "studyName"
  if (missing(study_title)) study_title <- "Title of Study"
  if (missing(study_code)) study_code <- NULL
  if (missing(investigator)) investigator <- ""

  document <- c(
    make_latex_chunk(file_path_base %_% ".R", type = "source"),
    "%%%%% Tables", "",
    make_latex_chunk("show_patient_characteristics()")
  )
  if (make_graphics_chunk)
    document <- c(document, make_latex_chunk("OutputGraphics()", type = "graphics",
      caption = "Some graphical output.", label = "graphics_out"))

  header <- list(
    title = study_title %_% "\\\\\\\\" %_% ifelse(is.null(study_code), "",
      "{\\\\smaller[2](" %_% study_code %_% ")}"),
    author = "Investigator: " %_% investigator,
    date = NULL
  )

  make_sweave_document(file_path_base %_% ".snw", output_path = output_path,
    header = header, document = document, bib = FALSE, ...)
}
