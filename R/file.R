## Allow renaming of objects before 'save()'ing them.
## Expands on https://stackoverflow.com/questions/21248065/r-rename-r-object-while-save-ing-it/21248218#21248218
#' @export
saver <- function(
  ...,
  list = character(),
  file = stop("'file' must be specified"),
  envir = parent.frame(),
  save... = list()
)
{
  d <- get_dots(..., evaluate = FALSE)
  l <- c(structure(as.character(d$arguments), .Names = names(d$arguments)), list)
  if (is_invalid(names(l)))
    names(l) <- NA_character_
  names(l)[names(l) == "" | is.na(names(l))] <- l[names(l) == "" | is.na(names(l))]

  e <- new.env()
  plyr::l_ply(seq_along(l), function(s) assign(names(l)[s], get(l[s], envir = envir), envir = e))

  saveArgs <- list(
    list = ls(e, all.names = TRUE),
    file = file,
    #eval.promises = TRUE,
    envir = e
  )
  saveArgs <- utils::modifyList(saveArgs, save..., keep.null = TRUE)

  do.call("save", saveArgs)
}

## usage:
# foo <- list(beast = 666, "test", 3.14)
# frog <- "fish"; duh <- 333
# saver(bar = foo, frig = frog, duh, file = "hi.RData")
# saver(moo = frog, frig = frog, frog, list = c(durr = "duh"), file = "hi2.RData")


#' @export
append_rda <- function(file_path, objects = character(), envir = parent.frame(), remove = FALSE, ...)
{
  # 'file' is the name of the file where the data will be saved; if 'file' doesn't exist it will be created.
  # 'objects' is a character vector containing the names of objects to be appended to 'file'.
  # 'envir' is the environment to search for objects to be saved.
  # If 'remove' is true, remove all other objects from 'file' besides those named in 'objects'.

  e <- new.env()
  .null <- NULL # 'save()' requires storage of at least one variable, so fake it.

  if (!file.exists(file_path))
    save(.null, file = file_path)

  load(file_path, envir = e)

  if (is_invalid(names(objects)))
    names(objects) <- NA_character_
  names(objects)[names(objects) == "" | is.na(names(objects))] <-
    objects[names(objects) == "" | is.na(names(objects))]

  for (o in seq_along(objects))
    assign(names(objects)[o], get(objects[o], envir = envir), envir = e)

  if (remove) {
    rm(list = setdiff(ls(e, all.names = TRUE), names(objects)), envir = e)
    variables <- names(objects)
  }

  variables <- ls(e, all.names = TRUE)

  if (length(variables) == 0)
    variables <- ".null"

  save(list = variables, file = file_path, envir = e, ...)

  return (nop())
}

## usage:
# foo <- list(beast = 666, "test", 3.14)
# frog <- "fish"
# saver(moo = frog, frig = frog, frog, save... = list(list = c("foo")), file = "hi3.RData")
# duh <- 666
# append_rda("hi3.RData", objects = c(durr = "duh"))
# append_rda("hi3.RData", objects = c("duh"), remove = TRUE)


#' @export
clipwd <- function(use_dirname = TRUE, dir, source_files = TRUE, verbose = TRUE, ...)
{
  if (missing(dir))
    dir <- utils::readClipboard() # Windows only, try 'scan("clipboard", what="")[1]' otherwise.

  if (use_dirname)
    dir <- dirname(dir)

  setwd(dir, ...)

  if (source_files) {
    files <- choose.files(filters = Filters[c("R"), ])
    sourceCommands <- NULL
    for (f in files) {
      #sourceCommand <- "source(\"./" %_% basename(f) %_% "\", keep.source = FALSE)"
      sourceCommand <- "source(\"" %_% normalizePath(f, "/") %_% "\", keep.source = FALSE)"
      sourceCommands <- c(sourceCommands, sourceCommand)
      if (verbose)
        cat("Running command '" %_% sourceCommand %_% "'.... ")
      ## N.B. 'writeClipboard()' automatically ends character strings with '\n'; convert to raw to prevent this.
      tryCatch(source(f, keep.source = FALSE), # Need to add extra "raw" to raw string to prevent deletion of last character.
        finally = {
          b <- charToRaw(paste(sourceCommands, collapse = "\n"))
          b[length(b) + 1L] <- as.raw(0)
          utils::writeClipboard(b, format = 1L)
          ## Write command directly to history:
          timestamp(stamp = paste(sourceCommands, collapse = "\n"), prefix = "", suffix = "", quiet = verbose)
        }
      )
      if (verbose) { cat("Done.", fill = TRUE); flush.console() }
    }
  }
}


#' @export
clip_wd <- function(..., source_files = FALSE)
{
  clipwd(..., source_files = source_files)
}


## A drop-in revision of 'list.files()' w/ expanded regex support, largely taken from 'dirdf::dir2()'
#' @export
list_files <- function
(
  path = ".",
  pattern = NULL,
  all.files = FALSE,
  full.names = TRUE,
  recursive = FALSE,
  ignore.case = FALSE,
  include.dirs = FALSE,
  absolute = FALSE,
  negate = FALSE,
  files_expr = expression({ files <- files }),
  ...
)
{
  # 'path' is a character vector of paths.
  # 'pattern' is an optional regular expression. Full path names (including path and file names) that match the regular expression will be returned.
  # If 'all.files' is FALSE, non-visible files (prefixed with a .) are not returned. If TRUE, all files are returned.
  # If 'full.names' is TRUE or recursive = TRUE (or a positive number), full path names are returned, otherwise only the file names.
  # If 'recursive' is FALSE or 0, content of sub-directories is not included. if TRUE or +Inf, content of all sub-directories is recursively included. If a positive number depth, then sub-directories to that depth are recursively included.
  # If 'ignore.case' is TRUE, pattern matching is case-insensitive, otherwise not.
  # If 'include.dirs' is TRUE, directories are also returned, otherwise not.
  # If 'absolute' is TRUE or length(path) > 1, absolute path names are returned and used in the file pattern-matching.
  # If 'negate' is TRUE, return non-matching file names discovered by 'pattern'.
  # 'files_expr' is an expression to be evaluated just before returning the vector of file names/paths (perhaps further regexp winnowing).
  # '...' is passed to 'stringr::regex()'.

  path <- path[file_test("-d", path)]

  if (length(path) == 0)
    return(character(0))

  if (length(path) > 1) {
    files <- lapply(path, FUN = list_files, pattern = pattern,
      all.files = all.files, full.names = TRUE, recursive = recursive,
      ignore.case = ignore.case, include.dirs = include.dirs,
      absolute = absolute)
    files <- unlist(files, use.names = FALSE)

    return(files)
  }

  if (absolute) {
    path <- normalizePath(path, mustWork = TRUE)
  }
  else {
    # path <- sub("[/\\]+$", "", path)
    # path <- sub("/[/]+", "/", path)
    # path <- sub("\\[\\]+", "\\", path)
    path <- stringr::str_replace_all(path, r"--{[/\\]+$}--", "")
    path <- stringr::str_replace_all(path, "(?!^//)/[/]+", "/")
    path <- stringr::str_replace_all(path, r"--{(?!^\\\\)\\[\\]+}--", r"--{\\}--")
  }

  depth <- as.numeric(recursive)
  if (is.logical(recursive) && recursive)
    depth <- +Inf

  files <- dir(path = path, pattern = NULL, all.files = all.files,
    full.names = FALSE, recursive = FALSE, ignore.case = ignore.case,
    no.. = TRUE)

  if (length(files) == 0)
    return(character(0))

  if (path != ".")
    files <- file.path(path, files)

  is_dir <- utils::file_test("-d", files)
  dirs <- files[is_dir]
  if (depth > 0 && length(dirs) > 0) {
    files_depth <- lapply(dirs, FUN = list_files, pattern = NULL,
      all.files = all.files, full.names = TRUE, recursive = depth -
        1L, ignore.case = FALSE, include.dirs = include.dirs)
    files_depth <- unlist(files_depth, use.names = FALSE)
    files <- c(files, files_depth)
  }

  if (!include.dirs) {
    is_dir <- utils::file_test("-d", files)
    files <- files[!is_dir]
  }

  if (!absolute && !full.names) {
    prefix <- file.path(path, "")
    files <- gsub(sprintf("^%s", prefix), "", files)
  }

  ## Add extra regex support for pattern-matching
  if (!is.null(pattern)) {
    files_norm <- gsub("\\\\", "/", files)
    #idxs <- grep(pattern, files_norm, ignore.case = ignore.case)
    idxs <- stringr::str_detect(files_norm, stringr::regex(pattern, ignore_case = ignore.case, ...), negate = negate)
    files <- files[idxs]
  }

  poly_eval(files_expr)

  files
}
