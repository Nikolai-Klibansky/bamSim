#' Lorenzen Mortality
#'
#' Function based on Lorenzen et al. 2022. Fisheries Research 106327
#'
#' @param length vector of length at age in cm
#' @param a intercept parameter
#' @param b slope parameter
#' @return Natural mortality (M) at age, which likely needs to be scaled for stock-specific values
#'
#' @author Kyle Shertzer and Nikolai Klibansky
#'
#' @aliases M.lorenzen
#'
#' @examples
#' # example code
#'
#' @export

M_Lorenzen <- function(length, a = 0, b = -1) {
  Mage <- exp(a + b * log(length))
  return(Mage)
}
