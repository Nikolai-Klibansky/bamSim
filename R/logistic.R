#' Logistic function
#'
#' Logistic function used for fisheries selectivity.
#'
#' @param x vector or scalar of independent variable
#' @param a slope parameter, default is 100 ("knife edge" for most applications)
#' @param b location parameter: x giving 50\% probability, default is 0
#'
#' @author Kyle Shertzer, and Nikolai Klibansky
#'
#' @return P = values of function at x
#'
#' @examples
#'
#'
#' @export

logistic <- function(x, a = 100, b = 0) {
  P <- 1 / (1 + exp(-a * (x - b)))
  return(P)
}
