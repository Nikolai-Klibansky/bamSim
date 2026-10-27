#' Compute Spawning Potential Ratio (SPR) reference points.
#'
#' Computes values for F30, F35, F40, F45, F50, and a user-supplied value.  Computes corresponding values of spawning output if provided equilibrium recruitment value.
#'
#' @param nages number of ages, starting with age 1
#' @param F_max the maximum F value over which to compute SPR, default is 1.0
#' @param R.eq allows computation of biomass benchmarks, if specified
#' @param SPR.input a user-supplied value if desired, as a decimal value, default is 0.4
#' @param sp.frac the fraction of year when peak spawning occurs, default is 0.0 (i.e., Jan 1)
#' @param reprod.age the reproductive contribution of each age (e.g., female sex ratio X female maturity X female weight)
#' @param M.age age dependent natural mortality
#' @param selex.age selectivity at age representing total mortality from fishing (including landings and discards)
#'
#' @author Kyle Shertzer and Nikolai Klibansky
#'
#' @aliases SPR.func
#'
#' @return Returns a list of vectors: F.pr = vector of F's used to compute SPR,
#' SPR = vector of SPR as a function of F,
#' s.per.rec = spawners per recruit as a function of F,
#' SSB.eq = reproductive output as a function of F, in units of reprod.age. If R.eq is not specified, this defaults to zero,
#' FX = F providing SPR=X,
#' SSB.FX = SSB corresponding FX. Applies only if R.eq is specified, else defaults to zero.
#'
#' @examples
#' #example code
#'
#' @export


SPR_calc <- function(nages, F_max = 1.0, R.eq = 0.0, SPR.input = 0.4, sp.frac = 0.0,
                     reprod.age, M.age, selex.age) {
  F.pr <- SPR <- sp.per.rec <- seq(0, F_max, by = 0.001) # F values over which to compute SPR
  SSB.eq <- seq(0, length.out = length(F.pr)) # equilibrium SSB associated with each F, conditional on R.eq
  F.age <- Z.age <- rep(0, nages) # F and Z at age
  N.pr <- N.pr.sp <- rep(1, nages) # N at age at start of year and at peak spawn time

  Fchoice <- SSB.F30 <- SSB.F35 <- SSB.F40 <- SSB.F45 <- SSB.F50 <- SSB.Fchoice <- NA

  for (i in 1:length(F.pr)) {
    F.age <- selex.age * F.pr[i]
    Z.age <- M.age + F.age

    for (iage in 2:nages) {
      N.pr[iage] <- N.pr[iage - 1] * exp(-Z.age[iage - 1])
    }
    N.pr[nages] <- N.pr[nages] / (1 - exp(-Z.age[nages])) # plus group
    N.pr.sp[1:(nages - 1)] <- N.pr[1:(nages - 1)] * exp(-sp.frac * Z.age[1:(nages - 1)])
    N.pr.sp[nages] <- N.pr.sp[(nages - 1)] * (exp(-(1 - sp.frac) * Z.age[(nages - 1)]) + sp.frac * Z.age[nages]) / (1 - exp(-Z.age[nages]))
    sp.per.rec[i] <- sum(N.pr.sp * reprod.age)

    SSB.eq[i] <- sp.per.rec[i] * R.eq

    SPR[i] <- sp.per.rec[i] / sp.per.rec[1]
  } # end i (F) loop

  if (SPR.input != 0.0) {
    Fchoice <- F.pr[which.min(abs(SPR - SPR.input))]
  }
  F30 <- F.pr[which.min(abs(SPR - 0.3))]
  F35 <- F.pr[which.min(abs(SPR - 0.35))]
  F40 <- F.pr[which.min(abs(SPR - 0.40))]
  F45 <- F.pr[which.min(abs(SPR - 0.45))]
  F50 <- F.pr[which.min(abs(SPR - 0.50))]

  SSB.F30 <- SSB.F35 <- SSB.F40 <- SSB.F45 <- SSB.F50 <- SSB.Fchoice <- 0.0
  if (R.eq != 0.0) {
    SSB.F30 <- SSB.eq[F.pr == F30]
    SSB.F35 <- SSB.eq[F.pr == F35]
    SSB.F40 <- SSB.eq[F.pr == F40]
    SSB.F45 <- SSB.eq[F.pr == F45]
    SSB.F50 <- SSB.eq[F.pr == F50]
    if (SPR.input != 0.0) {
      SSB.Fchoice <- SSB.eq[F.pr == Fchoice]
    }
  }
  return(list(
    F.pr = F.pr,
    SPR = SPR,
    s.per.rec = sp.per.rec,
    SSB.eq = SSB.eq,
    F30 = F30,
    F35 = F35,
    F40 = F40,
    F45 = F45,
    F50 = F50,
    Fproxy = Fchoice,
    SSB.F30 = SSB.F30,
    SSB.F35 = SSB.F35,
    SSB.F40 = SSB.F40,
    SSB.F45 = SSB.F45,
    SSB.F50 = SSB.F50,
    SSB.Fproxy = SSB.Fchoice
  ))
}

