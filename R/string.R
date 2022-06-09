#' Concatenate Strings Easily
#'
#' Allows quick chaining together of character strings.
#'
#' @param a R object to be converted to a character vector.
#' @param b R object to be converted to a character vector.
#' @param sep A character string to separate the terms; passed to \code{\link[base]{paste}()}.
#'
#' @return The concatenation of \code{a} and \code{b}.
#'
#' @seealso \code{\link[base]{paste}}
#'
#' @examples
#' who <- "world"
#' "Hello " %_% who %_% "!"
#'
#' @export
`%_%` <- function(a, b, sep = "") paste(a, b, sep = sep)


#' @export
backtick <- function(x, ...)
{
  sapply(x, function(a) paste("`", as.character(a), "`", sep = ""), ...)
}


# ?base::tolower
#' @export
capwords <- function(s, strict = FALSE)
{
  cap <- function(s)
      paste(toupper(substring(s, 1L, 1L)), { s <- substring(s, 2L); if (strict) tolower(s) else s }, sep = "", collapse = " ")
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


#' @export
char_sort <- function(x, s)
{
  x[which(x %in% s)] <- s[which(s %in% x)]

  x
}


## V. '?base::grep' from the R command line. Probably unnecessary.
#' @export
parse_one <- function(res, result)
{
  m <- do.call(rbind, lapply(seq_along(res),
    function(i) {
      if (result[i] == -1) return("")
      st <- attr(result, "capture.start")[i, ]
      substring(res[i], st, st + attr(result, "capture.length")[i, ] - 1)
    }))
  colnames(m) <- attr(result, "capture.names")

  m
}


# http://stackoverflow.com/questions/28248457/gsub-in-r-with-unicode-replacement-give-different-results-under-windows-compared
#' @export
true_unicode <- function(x)
{
  packuni <- Vectorize(function(cp) {
    bv <- intToBits(cp)
    maxbit <- tail(which(bv != as.raw(0L)), 1L)
    if(maxbit < 8L)
      rawToChar(as.raw(codepoint))
    else if (maxbit < 12L)
      rawToChar(rev(packBits(c(bv[1L:6L], as.raw(c(0L, 1L)), bv[7L:11L], as.raw(c(0L, 1L, 1L))), "raw")))
    else if (maxbit < 17L)
      rawToChar(rev(packBits(c(bv[1L:6L], as.raw(c(0L, 1L)), bv[7L:12L], as.raw(c(0L, 1L)), bv[13L:16L], as.raw(c(0L, 1L, 1L, 1L))), "raw")))
    else
      stop("Too many bits.")
  })

  m <- gregexpr("<U\\+[0-9a-fA-F]{4}>", x)
  codes <- regmatches(x, m)
  chars <- lapply(codes, function(x) {
    codepoints <- strtoi(paste0("0x", substring(x, 4L, 7L)))
    packuni(codepoints)
  })
  regmatches(x, m) <- chars
  Encoding(x) <- "UTF-8"

  return (x)
}

#' @export
tu <- true_unicode
