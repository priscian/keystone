#' @export
get_dots <- function(..., evaluate = FALSE)
{
  caller <- sys.function(which = -1L)
  formalArguments <- NULL
  if (!is.null(caller)) {
    callerName <- as.list(sys.call(-1L))[[1L]]
    formalArguments <- names(formals(caller))
  }
  unevaluated <- eval(substitute(alist(...)))
  dotsNames <- names(unevaluated)
  if (is_invalid(dotsNames))
    dotsNames <- rep("", length(unevaluated))

  rv <- list()
  if (!is.null(sys.call(-2L)))
    rv$calling_function <- as.list(sys.call(-2L))[[1L]]
  rv$current_function <- callerName
  rv$current_formals <- formalArguments
  rv$arguments <- as.list(unevaluated)
  if (evaluate)
    rv$evaluated <- list(...)
  rv$dots_names <- dotsNames
  whichDots <- which(formalArguments == "...")
  if (length(whichDots) == 0L)
    whichDots <- ifelse(length(formalArguments) == 0L, 1L, length(formalArguments))
  temp <- append(formalArguments, dotsNames[dotsNames != ""], after = whichDots)
  rv$all_named_args <- temp[temp != "..."]

  return (rv)
}


## https://stackoverflow.com/a/47955845/931941
#' @export
get_all_args <- function(defaults = FALSE)
{
  ## Get formals of parent function.
  parentFormals <- formals(sys.function(sys.parent(n = 1)))

  ## Get names of assumed arguments.
  hasDots <- FALSE
  fnames <- names(parentFormals)
  if (any(fnames == "...")) {
    hasDots <- TRUE
    ## Remove '...' from list of parameter names.
    fnames <- fnames[-which(fnames == "...")]
  }

  ## Get current values for named variables in the parent frame.
  a <- evalq(as.list(environment()), envir = parent.frame())
  a <- a[fnames]

  ## Get the list of variables in '...'.
  if (hasDots)
    #a <- c(a, evalq(list(...), envir = parent.frame()))
    a <- c(a, evalq(get_dots(...)$arguments, envir = parent.frame()))

  if (defaults) {
    ## Get default values.
    defArgs <- as.list(parentFormals)
    defArgs <- defArgs[unlist(lapply(defArgs, FUN = function(x) class(x) != "name"))]
    a[names(defArgs)] <- defArgs
    setArgs <- evalq(as.list(match.call())[-1], envir = parent.frame())
    a[names(setArgs)] <- setArgs
  }

  a
}


## Add argument 'clear_1_cache = TRUE/FALSE' to a 'memoise()'d function;
##   this allows direct deletion of the function call's subcache file
## N.B. This patch only works for memoization to a disk cache.
#' @export
patch_memoised_for_subcaching <- function(
  fun, # A 'memoise()'d function
  name,
  key
)
{
  if (!memoise::is.memoised(fun)) {
    warning("Function is not 'memoise()'d")

    return (fun)
  }

  flit <- function() { }
  rlang::fn_body(flit) <- rlang::fn_body(fun)
  if (is_invalid(key)) {
    formals(flit) <- args(fun) %>% formals %>%
      #utils::modifyList(alist(clear_1_cache = FALSE, KEY =, SUFFIX = ""), keep.null = TRUE)
      utils::modifyList(alist(clear_1_cache = FALSE, KEY = NULL, SUFFIX = ""), keep.null = TRUE)
  } else {
    formals(flit) <- args(fun) %>% formals %>%
      utils::modifyList(list(clear_1_cache = FALSE, KEY = key, SUFFIX = ""), keep.null = TRUE)
  }

  ## Where does the 'key' assignment happen?
  i <- sapply(as.list(rlang::fn_body(flit)),
    function(a) { deparse(a) %>% stringr::str_detect("^\\s*key\\s*<-") %>% any },
      simplify = TRUE) %>% which
  ## Inject code to delete sub-cache of this function call
  rlang::fn_body(flit) <- rlang::fn_body(flit) %>% as.list %>%
    #append(quote({ browser() }), 1) %>%
    append(quote({
      if (is.null(KEY)) {
        key <- encl$`_hash`(c(encl$`_f_hash`))
      } else {
        key <- KEY
      }
      key <- paste0(key, SUFFIX)

      cache_path <- encl$`_cache`$keys %>% environment %>% `$`("path")
      ## Only proceed w/ subcache handling if function is memoised to a disk cache
      # cat(sprintf("'is.null(cache_path)' = %s", is.null(cache_path)), fill = TRUE) # Is function memoised to a disk cache?
      # cat(sprintf("'clear_1_cache' = %s", clear_1_cache), fill = TRUE) # For debugging parallel invocations
      if (!is.null(cache_path) || is.character(clear_1_cache)) {
        if (is.logical(clear_1_cache) && clear_1_cache) {
          cache_1_path <- cache_path %>% normalizePath(winslash = "/") %>%
            paste(key, sep = "/")
          if (file.exists(cache_1_path)) {
            file.remove(cache_1_path)
            message(sprintf("Cache file '%s' was deleted", key))
          } else {
            warning(sprintf("Cache file '%s' doesn't exist", key))
          }
        } else if (is.character(clear_1_cache)) {
          switch(clear_1_cache,
            skip = {
              message(sprintf("Skipping cache file '%s'", key))
              return (invisible(NULL))
            },
            run = {
              message(sprintf("Calling function without saving cache file '%s'", key))
              mc[[1L]] <- encl$`_f`
              res <- withVisible(eval(mc, parent.frame()))
              if (res$visible)
                return (res$value)
              else
                return (invisible(res$value))
            },
            {
              message(sprintf("Invalid string passed to 'clear_1_cache' for cache file '%s'", key))
              return (invisible(NULL))
            }
          )
        }
      } #else { message("Not a disk cache") }
      res <- encl$`_cache`$get(key)
      if (inherits(res, "key_missing")) {
        message(sprintf("Saving cache file '%s'", key))
      } else {
        message(sprintf("Loading cache file '%s'", key))
      }
    }), i) %>%
    append(quote({
      if (is.null(clear_1_cache)) {
        clear_1_cache <- FALSE
      }
      args$clear_1_cache <- NULL

      args$KEY <- NULL

      if (is.null(SUFFIX)) {
        SUFFIX <- ""
      }
      args$SUFFIX <- NULL

      mc <- as.list(mc) %>% `[<-`(c("clear_1_cache", "KEY", "SUFFIX"), NULL) %>% as.call
    }), i - 1) %>% as.call

  environment(flit) <- environment(fun)
  attributes(flit) <- attributes(fun)

  if (!missing(name))
    environment(flit)$`_f_hash` <- name

  flit
}


#' @export
memoise_package_function_ <- function(
  sym,
  env,
  cache,
  pattern = "^.*?namespace:(.*?)>",
  group = 1,
  str_extract... = list(),
  replace_env = globalenv(),
  ... # Other arguments to 'patch_memoised_for_subcaching()'
)
{
  sym <- as.name(sym)
  namespace <- NULL
  if (is.character(env)) {
    namespace <- env
    env <- asNamespace(env)
  }

  unlockBinding(sym, env)
  substitute(`__FUNSYM__` <<- memoise::memoise(`__FUNSYM__`, cache = cache) %>%
    keystone::patch_memoised_for_subcaching(name = as.character(sym), ...),
    list(`__FUNSYM__` = sym)) %>% eval()
  # substitute({ assign(as.character(sym), memoise::memoise(`__FUNSYM__`, cache = cache) %>%
  #   keystone::patch_memoised_for_subcaching(name = as.character(sym)) } %>%
  #   list(`__FUNSYM__` = sym)) %>% eval()
  lockBinding(sym, env)

  if (is_invalid(namespace)) {
    str_extractArgs <- list(
      string = capture.output(env),
      pattern = pattern,
      group = group
    )
    str_extractArgs <- utils::modifyList(str_extractArgs, str_extract..., keep.null = TRUE)
    namespace <- do.call(stringr::str_extract, str_extractArgs)
  }

  substitute(`__FUNSYM__` <<- get(`__FUN__`, asNamespace(`__NAMESPACE__`)),
    list(`__FUNSYM__` = sym, `__FUN__` = as.character(sym), `__NAMESPACE__` = namespace)) %>%
    eval(envir = replace_env)
}


## Evaluate function inside an environment & extract & save any useful variables from its body
#' @export
cordon <- function(
  fun, ...,
  arguments = list(),
  envir = environment(),
  file_path = NULL,
  variables = NULL,
  version = NA, version_fmt = "_v%03d",
  copy_args = FALSE,
  timestamp = TRUE, timestamp... = list(),
  action = c("run", "save", "load", "skip", "archive"),
  evaluate_dots = TRUE,
  create_path_dir = TRUE,
  verbose = TRUE
)
{
  action <- match.arg(action)
  run_ <- action == "run" || action == "save" || action == "load"
  save_ <- action == "save"
  load_ <- action == "load"
  archive_ <- action == "archive"

  version_ <- version
  rm(version)

  timestampArgs <- list(
    use_seconds = TRUE,
    seconds_sep = '+'
  )
  timestampArgs <- utils::modifyList(timestampArgs, timestamp..., keep.null = TRUE)

  if (archive_) {
    if (is.null(file_path))
      stop("Archive file path must be specified.")
    if (!(file.info(file_path)$isdir)) file_path <- dirname(file_path)

    if (verbose)
      cat(sprintf("Loading archive file \"%s\".... ", filePath))
    archive("load", file_path) # 'archive()' not implemented yet
    if (verbose) {
      cat("Done.", fill = TRUE);
      flush.console()
    }
  }
  else if (load_) {
    filePath <- file_path

    if (!is_invalid(version_)) local({
      ## Break up the file path & insert the version number
      firstPart <- tools::file_path_sans_ext(filePath)
      lastPart <- tools::file_ext(filePath)

      versionedPath <-
        sprintf("%s" %_% version_fmt %_% ".%s", firstPart, readr::parse_number(as.character(version_)), lastPart)
      filePath <<- versionedPath
    })

    if (timestamp) {
      ## Get list of files in directory of 'file_path'
      fileExt <- tools::file_ext(filePath)
      dirName <- dirname(filePath)
      timestampRe <- sprintf("_\\d{4}-\\d{2}-\\d{2}(?:\\%s\\d{5})?", timestampArgs$seconds_sep)
      ## Find all versions of the file according to their timestamp extensions
      filePaths <- sort(
        grep(
          sprintf("^.*?%s\\.%s$", timestampRe, fileExt),
          list.files(
            dirName,
            pattern =
              sprintf(
                "^%s%s\\.%s$",
                Hmisc::escapeRegex(tools::file_path_sans_ext(basename(filePath))),
                timestampRe, fileExt
              ),
            full.names = FALSE
          ),
          perl = TRUE, value = TRUE
        ),
        decreasing = TRUE
      ) %>%
      paste(dirName, ., sep = "/")

      if (length(filePaths) > 0L)
        ## Use the most recent version of the file according to its timestamp extension:
        filePath <- filePaths[1L]
    }

    if (verbose) cat(sprintf("Loading data file \"%s\".... ", filePath))
    load(file = filePath, envir = envir)
    if (verbose) { cat("Done.", fill = TRUE); flush.console() }
  }
  else if (run_) {
    temp <- fun
    #body(temp) <- as.call(c(as.name("{"), expression({ browser(); return (environment()) }))) # for debugging
    body(temp) <- as.call(c(as.name("{"), expression({ return (environment()) })))
    argList <- list()

    dots <- get_dots(..., evaluate = evaluate_dots)
    ## Add '...' arguments to argument list:
    dotsArguments <- dots$arguments
    if (evaluate_dots) dotsArguments <- dots$evaluated
    ## Replace duplicate named arguments with those from '...' and add new named arguments:
    argList <- utils::modifyList(argList, dotsArguments[dots$dots_names != ""], keep.null = TRUE)
    ## Tack on unnamed arguments from '...':
    argList <- c(argList, dotsArguments[dots$dots_names == ""])
    ## Add 'arguments' to 'argList'.
    ## Replace duplicate named arguments with those from 'arguments' and add new named arguments:
    argList <- utils::modifyList(argList, arguments[names(arguments) != ""], keep.null = TRUE)
    ## Tack on unnamed arguments from 'arguments':
    argList <- c(argList, arguments[names(arguments) == ""])

    evalEnv <- do.call(temp, argList)

    ## Evaluate the body of 'fun()' in the environment created
    eval(body(fun), envir = evalEnv)

    ## Pick out the variables to keep
    if (is.null(variables))
      variables <- setdiff(ls(evalEnv, all.names = TRUE), c(names(formals(fun))))

    if (is_invalid(names(variables)))
      names(variables) <- NA_character_
    names(variables)[names(variables) == "" | is.na(names(variables))] <-
      variables[names(variables) == "" | is.na(names(variables))]

    argEnv <- as.environment(argList[names(argList) != ""]) # Can only save named arguments
    if (!is.null(file_path)) {
      if (save_) {
        filePath <- file_path

        if (create_path_dir && !dir.exists(dirname(filePath)))
          dir.create(dirname(filePath), recursive = TRUE)

        if (!is_invalid(version_)) local({
          ## Break up the file path & insert the version number
          firstPart <- tools::file_path_sans_ext(filePath)
          lastPart <- tools::file_ext(filePath)

          versionedPath <-
            sprintf("%s" %_% version_fmt %_% ".%s", firstPart, readr::parse_number(as.character(version_)), lastPart)
          filePath <<- versionedPath
        })

        if (timestamp) {
          filePath <-
            sprintf("%s_%s.%s",
              tools::file_path_sans_ext(filePath),
              do.call(make_current_timestamp, timestampArgs),
              tools::file_ext(file_path)
            )
        }

        if (verbose) cat(sprintf("Saving data file \"%s\".... ", filePath))
        saver(list = variables, file = filePath, envir = evalEnv)
        if (copy_args)
          append_rda(filePath, objects = ls(argEnv, all = TRUE), envir = argEnv)
        if (verbose) { cat("Done.", fill = TRUE); flush.console() }
      }
    }

    for (v in seq_along(variables))
      assign(names(variables)[v],
        mget(variables[v], envir = evalEnv, ifnotfound = list(NULL))[[1]], envir = envir)

    if (copy_args) {
      for (a in ls(argEnv, all = TRUE))
        assign(a, get(a, envir = argEnv), envir = envir)
    }

    return (invisible(evalEnv))
  }
}


#' @export
eval_js <- function(
  ...,
  envir = parent.frame(),
  enclos = if(is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv()
)
{
  dots <- get_dots(..., evaluate = TRUE)
  expr <- unlist(dots$evaluated)

  if (is.list(expr)) {
    if (is.function(expr[[1L]])) # If first '...' argument is a function, execute it with other '...' arguments as its own.
      return (do.call(expr[[1L]], tail(expr, -1L)))

    for (i in expr) {
      if (is.expression(i) || is.language(i)) {
        return (eval(i, envir, enclos)) # Returns only the first expression found.
      }
    }
  }

  expr <- paste(expr, collapse = " ")

  if (typeof(expr) != "character")
    return (expr)

  expr <- parse(text = expr)
  eval(expr, envir, enclos)
}


## Allow 'what' argument of 'do.call()' to include package path.
## V. https://stackoverflow.com/questions/10022436/do-call-in-combination-with/10037475#10037475
#' @export
do_call <- function(what, args, ...)
{
  if (is.function(what)) {
    what <- deparse(as.list(match.call())$what)
  }
  myFunCall <- parse(text = what)[[1]]
  myCall <- as.call(c(list(myFunCall), args))

  return (eval(myCall, ...))
}


#' @export
nop <- function(x = NULL)
{
  return (invisible(x))
}
