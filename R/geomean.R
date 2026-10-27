#' Geometric Mean
#'
#' Computes the geometric mean of a numeric vector
#'
#' @param x numeric vector
#' @return numeric
#'
#' @author Kyle Shertzer and Nikolai Klibansky
#'
#' @examples
#' x <- c(1,2,3)
#' y <- geomean(x) # y = prod(x)^(1 / nx)
#' print(y)
#'
#' @export

geomean <- function(x) {
  nx <- length(x)
  y <- prod(x)^(1 / nx)
  return(y)
}

