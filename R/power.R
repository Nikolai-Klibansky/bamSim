#' Power
#'
#' Computes the power of an independent variable
#'
#' @param x numeric vector or scalar
#' @param a coefficient
#' @param b exponent
#'
#' @author Kyle Shertzer and Nikolai Klibansky
#'
#' @aliases pow
#'
#' @return numeric vector or scalar
#'
#' @examples
#' x <- c(1,2,3)
#' a <- 2
#' b <- 3
#' y <- power(x,a,b) # y = a * x^b
#' print(y)
#'
#' @export

power <- function(x, a, b) {
  y <- a * x^b
  return(y)
}
