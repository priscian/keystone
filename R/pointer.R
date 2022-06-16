## TODO: Using '.Internal(inspect(x))', keep a collection of all pointers,
##   and on deref/assignment update all same-referenced objects.
#' @export
ptr <- function(
  ...,
  pos = -1L,
  envir = as.environment(pos),
  named_list = TRUE,
  expand_character = FALSE
)
{
  dots <- get_dots(...)
  variableList <- dots$arguments

  if (length(variableList) == 0L)
    stop("Must supply reference object.")

  exclusions <- intersect(names(variableList), setdiff(dots$current_formals, "..."))
  for (exclusion in exclusions)
    variableList[[exclusion]] <- NULL

  if (length(variableList) == 0L)
    stop("Must supply reference object.")

  if (expand_character) {
    temp <- character()
    for (variable in variableList) {
      if (typeof(variable) == "character")
        temp <- c(temp, variable)
      else if (typeof(variable) == "symbol") {
        evaluatedVariable <- eval(variable)
        if (typeof(evaluatedVariable) == "character")
          temp <- c(temp, evaluatedVariable)
        else if (is.environment(evaluatedVariable)) {
          for (name in ls(evaluatedVariable))
            temp <- c(temp, variable %_% "$" %_% name)
        }
        else
          temp <- c(temp, as.character(variable))
      }
    }
    pointerNames <- temp
  }
  else
    pointerNames <- as.character(variableList)

  returnList <- list()
  for (pointerName in pointerNames) {
    e <- envir
    pName <- pointerName

    reEnv <- "^(.+?)\\$(.+?)$"
    envMatch <- regexec(reEnv, pointerName)
    envMatches <- NULL
    if (envMatch[[1L]][1L] != -1L) {
      envMatches <- regmatches(pointerName, envMatch)[[1L]][2L:3L]
      e <- get(envMatches[1L])
      pName <- envMatches[2L]
    }

    p <- list()
    p$object <- e
    p$name <- as.character(pName)
    class(p) <- "pointer"

    index <- length(returnList) + 1L
    if (named_list) index <- p$name

    returnList[[index]] <- p
  }

  if (length(returnList) == 1L)
    return (returnList[[1L]])

  return (returnList)
}

#' @export
pointer <- ptr


#' @export
as.pointer <- function(x)
{
  pointer(x)
}


#' @export
is.pointer <- function(x)
{
  return (inherits(x, "pointer"))
}


#' @export
.. <- function(x)
{
  if (is.environment(x)) return (x)
  else return (get(x$name, envir = x$object))
}

#' @export
deref <- ..


#' @export
`..<-` <- function(x, value)
{
  if (is.pointer(x)) assign(x$name, value, envir = x$object)

  return (x)
}

#' @export
`deref<-` <- `..<-`


#' @export
print.pointer <- function(x, ...)
{
  environment.name <- utils::capture.output(print(x$object))
  cat("Pointer to variable '", x$name, "' in ", environment.name, ":\n\n", sep = "")
  str(..(x), ...)
}

## usage:
# x <- list(frog = "frog", fish = "~frog")
# y <- x
# z <- pointer(x)
# ..(z)
# ..(z)$fish <- "trout"
# ..(z)
# x
# y
