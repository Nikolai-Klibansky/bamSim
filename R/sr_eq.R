#' SR equilibrium function
#'
#' Equilibrium spawning, conditional on F, with option for bias correction
#'
#' @param S vector or scalar of independent variable (spawners)
#' @param SR_switch (1=BevHolt, 2=Ricker, 3=Null)
#' @param BC bias correction, default is none (=1)
#' @param h steepness, default is 0.75
#' @param R0 recruitment of unfished popn or equilibrium recruitment for the Null model, default is 100
#' @param spr_F0 spawners per recruit of unfished popn, default is 1
#' @param spr_F spawners per recruit at fishing rate F
#'
#' @author Kyle Shertzer and Nikolai Klibansky
#'
#' @aliases SR.eq.fcn
#'
#' @return recruits = values of function at S
#'
#' @examples
#' # example code
#'
#' @export

sr_eq <- function(SR_switch, BC = 1, h = 0.75, R0 = 100, spr_F0 = 1, spr_F = 1) {
  recruits <- switch(SR_switch,
                     ((R0 / ((5.0 * h - 1.0) * spr_F)) * (BC * 4.0 * h * spr_F - spr_F0 * (1.0 - h))),
                     ((R0 / (spr_F / spr_F0) * (1.0 + log(BC * spr_F / spr_F0) / h))),
                     (BC * R0)
  )
  return(recruits)
}
