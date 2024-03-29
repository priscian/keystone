#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%

## https://stat.ethz.ch/R-manual/R-devel/library/methods/html/BasicClasses.html
#' @export
R_basic_classes <- function()
{
  cl <- list(
    character = "character",
    complex = "complex",
    double = "double",
    expression = "expression",
    integer = "integer",
    list = "list",
    logical = "logical",
    numeric = "numeric",
    single = single(),
    raw = "raw",
    `NULL` = "NULL",
    `function` = "function",
    externalptr = "externalptr",
    namedList = "namedList"
  )

  sapply(cl, function(a) { if (is.character(a)) new(a) else a })
}


## Evaluate a number of different types of expressions
#' @export
poly_eval <- function(expr, envir = parent.frame(), env = rlang::caller_env(), ...)
{
  if (is.null(expr))
    return (NULL)

  if (is.function(expr)) {
    #expr(...) # Change to 'do.call()' to include 'envir'
    do.call(what = expr, args = get_dots(...)$arguments, envir = envir)
  } else if (rlang::is_expression(expr)) {
    rlang::eval_tidy(expr, env = env, ...)
  } else if (is.expression(expr)) {
    eval(expr, envir = envir, ...)
  } else {
    expr
  }
}


## Simplify 'substitute()'-ing into an expression. Also see '?substitute'.
#' @export
expr_sub <- function(
  expr,
  env
)
{
  as.expression(do.call(substitute, list(expr[[1]], env)))
}
