#' Double logistic function
#'
#' Double logistic function used for fisheries selectivity.
#'
#' @param x vector or scalar of independent variable
#' @param a slope parameter, default is 100 ("knife edged" for most applications)
#' @param b location parameter: x giving 50\% probability, default is 0
#'
#' @return P = values of function at x
#'
#' @author Kyle Shertzer and Nikolai Klibansky
#'
#' @aliases double.logistic
#'
#' @examples
#' #example code
#'
#' @export

logistic_double <- function(x, a1 = 100, b1 = 0, a2 = 100, b2 = 0) {

  P1 <- 1 / (1 + exp(-a1 * (x - b1)))
  P2 <- 1 - (1 / (1 + exp(-a2 * (x - (b1 + b2)))))
  P <- P1 * P2
  P <- P / max(P)
  return(P)

}
