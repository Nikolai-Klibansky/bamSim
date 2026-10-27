#' Compute matrix of proportion of length at age
#'
#' Matrix has nages rows and nlenbins columns, where columns sum to one
#'
#' @param len.mu mean length at age
#' @param len.cv cv of len at age
#' @param len.bins length bins
#' @param len.binw bin width
#'
#' @return matrix of proportion of length at age
#'
#' @author Kyle Shertzer and Nikolai Klibansky
#'
#' @aliases lenprob.fcn
#'
#' @examples
#' lenprob(len.mu=c(1,2,3),len.cv=0.1,len.bins=c(1,2,3),len.binw=0.5)
#'
#' @export

lenprob <- function(len.mu, len.cv, len.bins, len.binw) {
  nages <- length(len.mu)
  nlenbins <- length(len.bins)
  len_sd <- array(NA, dim = c(nages))
  cprob_lenvec <- array(NA, dim = c(nlenbins))
  lenprob <- array(NA, dim = c(nages, nlenbins))
  for (i in 1:nages) {
    len_sd[i] <- len.cv * len.mu[i]
    zscore_lzero <- (0.0 - len.mu[i]) / len_sd[i]
    cprob_lzero <- pnorm(zscore_lzero)
    # first length bin
    zscore_len <- ((len.bins[1] + 0.5 * len.binw) - len.mu[i]) / len_sd[i]
    cprob_lenvec[1] <- pnorm(zscore_len)
    lenprob[i, 1] <- cprob_lenvec[1] - cprob_lzero
    # most other length bins
    for (l in 2:(nlenbins - 1)) {
      zscore_len <- ((len.bins[l] + 0.5 * len.binw) - len.mu[i]) / len_sd[i]
      cprob_lenvec[l] <- pnorm(zscore_len)
      lenprob[i, l] <- cprob_lenvec[l] - cprob_lenvec[l - 1]
    }
    # last length bin is a plus group
    zscore_len <- ((len.bins[nlenbins] - 0.5 * len.binw) - len.mu[i]) / len_sd[i]
    lenprob[i, nlenbins] <- 1.0 - pnorm(zscore_len)
    lenprob[i, ] <- lenprob[i, ] / (1.0 - cprob_lzero) # renormalize to account for any prob mass below 0
  }
  return(lenprob)
}

