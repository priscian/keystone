#' @import data.table

#' @export
dataframe <- function (
  ...,
  row.names = NULL,
  check.rows = FALSE,
  check.names = FALSE,
  fix.empty.names = FALSE,
  stringsAsFactors = FALSE
)
{
  data.frame(...,
    row.names = row.names,
    check.rows = check.rows,
    check.names = check.names,
    fix.empty.names = fix.empty.names,
    stringsAsFactors = stringsAsFactors)
}


#' @export
renumber  <- function(x, ...)
  UseMethod("renumber")


#' @export
renumber.pointer <- function(x, ...)
{
  if (inherits(..(x), "data.frame"))
    renumber.data.frame(x, ...)
  else
    stop("Pointer does not reference a relevant object type.")
}


#' @export
renumber.data.frame <- function(x, ...)
{
  row.names(..(x)) <- NULL
}


## Row insertion into data frames etc.
#' @export
insert_row  <- function(x, newrow, rnum, ...)
  UseMethod("insert_row")


#' @export
insert_row.data.frame <- function(x, newrow, rnum) {
  x[seq(rnum + 1L, nrow(x) + 1L), ] <- x[seq(rnum, nrow(x)), ]
  x[rnum, ] <- newrow

  return (x)
}


## Simplify 'read.table()' from 'textConnection()'s; like SAS's 'CARDS' statement.
#' @export
cards <- function(x, header = TRUE, as.is = TRUE, check.names = FALSE, stringsAsFactors = FALSE, ...)
{
  tab <-
    read.table(text = x, header = header, as.is = as.is, check.names = check.names,
      stringsAsFactors = stringsAsFactors, ...)

  return (tab)
}

## usage:
# x <- cards('
# A B C val
# 1 1 1 10
# 1 1 1 19
# 1 1 2 21
# 1 1 2 28
# 1 1 2 33
# 1 2 1 38
# 1 2 1 46
# 1 2 1 46
# 1 2 2 51
# 1 2 2 56
# 1 3 1 64
# 1 3 1 71
# 1 3 1 77
# 1 3 2 78
# 1 3 2 82
# 2 1 1 88
# 2 1 1 98
# 2 1 2 101
# 2 1 2 104
# 2 2 1 107
# 2 2 1 113
# 2 2 2 118
# 2 2 2 127
# 2 3 1 130
# 2 3 1 130
# 2 3 2 142
# 2 3 2 144
# 2 3 2 155
# ')


## 'cards()' for matrix values.
#' @export
mcards <- function(x, header = FALSE, ...)
{
  data.matrix(cards(x, header = header, ...))
}


## Copy of 'gdata::cbindX()' for binding columns of uneven length
#' @export
cbindx <- function(...)
{
  x <- list(...)
  test <- sapply(x, function(z) is.matrix(z) | is.data.frame(z))
  if (any(!test))
    stop("Only matrices and data frames can be used")
  tmp <- sapply(x, NROW)
  maxi <- which.max(tmp)
  test <- tmp < tmp[maxi]
  for (i in 1:length(tmp)) {
    if (test[i]) {
      add <- matrix(nrow = tmp[maxi] - tmp[i], ncol = NCOL(x[[i]]))
      if (is.data.frame(x[[i]])) {
        add <- as.data.frame(add)
      }
      colnames(add) <- colnames(x[[i]])
      x[[i]] <- rbind(x[[i]], add)
    }
  }
  ret <- x[[1]]
  for (i in 2:length(tmp)) {
    ret <- cbind(ret, x[[i]])
  }

  ret
}
