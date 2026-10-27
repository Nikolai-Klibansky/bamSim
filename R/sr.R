#' Stock-recruit function
#'
#' Beverton-Holt (B-H), Ricker, or Null Stock-Recruitment (S-R) Function
#'
#' @param S vector or scalar of independent variable (spawners)
#' @param SR.switch (1=BevHolt, 2=Ricker, 3=Null)
#' @param h steepness, default is 0.75
#' @param R0 recruitment of unfished popn or equilibrium recruitment for the Null model, default is 100
#' @param spr_F0 spawners per recruit of unfished popn, default is 1
#'
#' @author Kyle Shertzer, Erik Williams, and Nikolai Klibansky
#'
#' @aliases SR.fcn
#'
#'
#' @return recruits = values of function at S
#'
#' @examples
#' # example code
#'
#' @export

sr <- function(S, SR_switch, h = 0.75, R0 = 100, spr_F0 = 1) {
  recruits <- switch(SR_switch,
                     ((0.8 * R0 * h * S) / (0.2 * R0 * spr_F0 * (1 - h) + S * (h - 0.2))),
                     ((S / spr_F0) * exp(h * (1 - S / (R0 * spr_F0)))),
                     (R0)
  )
  return(recruits)
}

