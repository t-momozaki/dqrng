library(dqrng)
dqrng_dqrTruncNorm <- function(n, lb=-Inf, ub=Inf, mean=0, sd=1) {
  u <- dqrng::dqrunif(n)
  mean + sd * qnorm( pnorm(lb,mean,sd) + u * { pnorm(ub,mean,sd) - pnorm(lb,mean,sd) } )
}
