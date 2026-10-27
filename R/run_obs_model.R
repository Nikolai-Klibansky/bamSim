#' Observation Model
#'
#' Adds lognormal observation error to landings and survey, and sampling error to age comps
#'
#' @param L1 error free time series of landings
#' @param L2 error free time series of landings
#' @param D error free time series of discards
#' @param survey error free time series of survey index
#' @param L1.age error free landings composition data from which to draw samples
#' @param L2.age error free landings composition data from which to draw samples
#' @param D.age error free discards composition data from which to draw samples
#' @param survey.age error free survey composition data from which to draw samples
#' @param CV.L1 CVs for landings time series
#' @param CV.L2 CVs for landings time series
#' @param CV.D CVs for discards time series
#' @param CV.survey CVs for survey time series
#' @param n.L1 annual sample sizes of age comps for L1
#' @param n.L2 annual sample sizes of age comps for L2
#' @param n.D annual sample sizes of age comps for D
#' @param n.survey annual sample sizes of age comps for survey
#' @param a1.yr1 first years of age comps for L1
#' @param a2.yr1 first years of age comps for L2
#' @param d.yr1 first years of age comps for D
#'
#' @author Kyle Shertzer and Nikolai Klibansky
#'
#' @aliases ObsModel
#'
#' @return time series with lognormal error
#' @return comp data with sampling error
#'
#' @examples
#' # example code
#'
#' @export

run_obs_model <- function(L1,
             L2,
             D,
             survey,
             L1.age,
             L2.age,
             D.len,
             survey.age,
             cv.L1,
             cv.L2,
             cv.D,
             cv.survey,
             n.L1,
             n.L2,
             n.D,
             n.survey,
             a1.yr1,
             a2.yr1,
             d.yr1) {

  # Lognormal error for time series
  nobs.L1 <- length(L1)
  nobs.L2 <- length(L2)
  nobs.D <- length(D)
  nobs.survey <- length(survey)

  # SD in log space, given CV in arithmetic space
  sd.L1 <- sqrt(log(1 + cv.L1^2))
  sd.L2 <- sqrt(log(1 + cv.L2^2))
  sd.D <- sqrt(log(1 + cv.D^2))
  sd.survey <- sqrt(log(1 + cv.survey^2))

  # log error
  ln.L1 <- rnorm(nobs.L1, mean = 0, sd = sd.L1)
  ln.L1 <- ln.L1 - mean(ln.L1)
  ln.L2 <- rnorm(nobs.L2, mean = 0, sd = sd.L2)
  ln.L2 <- ln.L2 - mean(ln.L2)
  ln.D <- rnorm(nobs.D, mean = 0, sd = sd.D)
  ln.D <- ln.D - mean(ln.D)
  ln.survey <- rnorm(nobs.survey, mean = 0, sd = sd.survey)
  ln.survey <- ln.survey - mean(ln.survey)

  # multiplicative lognormal error
  L1.obs <- L1 * exp(ln.L1)
  L2.obs <- L2 * exp(ln.L2)
  D.obs <- D * exp(ln.D)
  survey.obs <- survey * exp(ln.survey)

  #####################################################################################
  # Sampling of age comps for fleets 1 and 2 (rows=years, columns=ages)
  # Sampling of length comps for fleet 3 (discard)
  ### Fleet 1
  nages1 <- ncol(L1.age)
  nyr.L1.age <- nrow(L1.age)
  L1.age.obs <- matrix(0, nrow = nyr.L1.age, ncol = nages1)
  for (i in a1.yr1:nyr.L1.age) {
    probs <- L1.age[i, ] / sum(L1.age[i, ])
    L1.age.obs[i, ] <- rmultinom(n = 1, size = n.L1, prob = probs) / n.L1
  }

  ### Fleet 2
  nages2 <- ncol(L2.age)
  nyr.L2.age <- nrow(L2.age)
  L2.age.obs <- matrix(0, nrow = nyr.L2.age, ncol = nages2)
  for (i in a2.yr1:nyr.L2.age) {
    probs <- L2.age[i, ] / sum(L2.age[i, ])
    L2.age.obs[i, ] <- rmultinom(n = 1, size = n.L2, prob = probs) / n.L2
  }

  ### Fleet 3 (discards) length comps
  nlenbins <- ncol(D.len)
  nyr.D.len <- nrow(D.len)
  D.len.obs <- array(0, dim = c(nyr.D.len, nlenbins))
  for (i in d.yr1:nyr.D.len) {
    probs <- D.len[i, ] / sum(D.len[i, ])
    D.len.obs[i, ] <- rmultinom(n = 1, size = n.D, prob = probs) / n.D
  }

  ### Survey
  nages <- ncol(survey.age)
  ages <- 1:nages

  nyr.survey.age <- nrow(survey.age)
  survey.age.obs <- matrix(0, nrow = nyr.survey.age, ncol = nages)
  for (i in 1:nyr.survey.age) {
    probs <- survey.age[i, ] / sum(survey.age[i, ])
    survey.age.obs[i, ] <- rmultinom(n = 1, size = n.survey, prob = probs) / n.survey
  }

  #####################################################################################
  return(list(
    L1.obs = L1.obs, L2.obs = L2.obs, D.obs = D.obs, survey.obs = survey.obs,
    L1.age.obs = L1.age.obs, L2.age.obs = L2.age.obs, D.len.obs = D.len.obs,
    survey.age.obs = survey.age.obs
  ))
} # end ObsModel
