#' @export
split.formula <- function(
  x,
  re_plus_minus = NULL,
  remove_extra_parens = TRUE,
  ...
)
{
  a <- attributes(x)

  variables <- tail(as.character(attr(terms(x), "variables")), -1L)
  responseIndex <- attr(terms(x), "response")

  rePlusMinus <- ifelse(is.null(re_plus_minus), "\\s+(\\+|-)\\s+", re_plus_minus)
  exes <- tail(as.character(x), 1L)
  right <- exes
  operatorIndices <- gregexpr(rePlusMinus, exes, perl = TRUE)[[1L]]
  operatorLengths <- attr(operatorIndices, "match.length")
  operators <- NULL
  for (i in seq_along(operatorIndices))
    operators <- c(operators, substr(exes, operatorIndices[i], operatorIndices[i] + operatorLengths[i] - 1L))
  operators <- operators[operators != ""]
  exes <- trimws(strsplit(exes, rePlusMinus, perl = TRUE)[[1L]])
  exes <- exes[exes != ""]
  if (length(operators) < length(exes))
    #operators <- c("+", operators)
    operators <- c(operators, "")

  left <- NULL; independents <- exes
  if (responseIndex != 0L)
    left <- variables[responseIndex];

  ## 'update.formula()' sometimes parenthesizes backquoted variables; remove those parentheses.
  if (remove_extra_parens) {
    reRemoveParens <- "(?:^|\\s+)\\((`.+?`)\\)(?:\\s+|$)"
    independents <- gsub(reRemoveParens, "\\1", independents)
    left <- gsub(reRemoveParens, "\\1", left)
    right <- gsub(reRemoveParens, "\\1", right)
  }

  characters <- left %_% " ~ " %_% right

  rv <- list(
    left_side = left,
    right_side = right,
    as_character = characters,
    independent_terms = independents,
    operators = operators,
    all_vars = all.vars(x),
    has_intercept = attr(terms(x), "intercept") == 1,
    has_response = attr(terms(x), "response") == 1,
    terms = terms(x),
    attributes = a
  )

  return (rv)
}

## usage:
# split(x <- a ~ b + 1 + c + .d + offset(e) + b %in% a)


#' @export
flip  <- function(x, ...)
  UseMethod("flip")


#' @export
flip.formula <- function(x)
{
  if (!inherits(x, "formula"))
    stop("Function argument must be a formula.")

  a <- attributes(x)
  s <- split(x)
  x <- s$right_side %_% " ~ " %_% ifelse(is_invalid(s$left_side), "1", s$left_side)

  f <- as.formula(x)
  attributes(f) <- a

  f
}
