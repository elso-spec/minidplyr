#' Filter2
#'
#' Lets the user filter rows in data by a logical condition
#'
#' @param data A data frame
#' @param cond Logical condition
#'
#' @returns A data frame
#' @export
#'
#' @examples
#'
#' filter2(iris, Species == "setosa")
#' filter2(iris, Sepal.Length > 5.0)
#'
filter2 <- function(data, cond) {
  rows <- eval(substitute(cond), envir = data, enclos = parent.frame())
  if (!is.logical(rows)) stop("Condition must evaluate to a logical vector")

  return(data[rows, , drop = FALSE])
}


