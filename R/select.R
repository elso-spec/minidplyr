
#' Select
#'
#' @param data A data frame
#' @param vars Variables (character strings or column indices)
#'
#' @returns A subsetted data frame
#' @export
#'
#' @examples
select <- function(data, vars) {
  # data: a data frame
  # vars: character vector of column names OR numeric indices

  if (is.character(vars)) {
    if (!all(vars %in% names(data))) {
      stop("Some variables are not found in the data frame")
    }
    return(data[, vars, drop = FALSE])

  } else if (is.numeric(vars)) {
    if (any(vars < 1 | vars > ncol(data))) {
      stop("Some indices are out of range")
    }
    return(data[, vars, drop = FALSE])

  } else {
    stop("vars must be either character column names or numeric indices")
  }
}

minidplyr::select(iris, c(2:3))

