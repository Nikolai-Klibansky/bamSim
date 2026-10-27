#' Baranov Catch Equation
#'
#' Computes the ...
#'
#' @param F.age vector of fishing mortality rate at age
#' @param Z.age vector of total mortality rate at age including dead discards
#' @param N.age vector of abundance at age
#' @param wgt.age optional vector of weight at age
#' @return A list with total landings in numbers and optionally in weight.
#'
#' @author Kyle Shertzer and Nikolai Klibansky
#'
#' @examples
#' #example code
#'
#' @export

baranov <- function(F.age, Z.age, N.age, wgt.age = NULL) {
  L.age.n <- L.age.w <- rep(NA, times = length(N.age))

  L.age.n <- F.age * N.age * (1.0 - exp(-Z.age)) / Z.age
  L.n <- sum(L.age.n)

  if (!is.null(wgt.age)) {
    L.age.w <- wgt.age * L.age.n
    L.w <- sum(L.age.w)
  } else {
    L.w <- NA
  }

  return(list(L.N = L.n, L.W = L.w))
}
