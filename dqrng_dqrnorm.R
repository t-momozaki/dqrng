library(dqrng)
dqrng_dqrnorm <- function(n, mean = 0, sd = 1) {
  u <- dqrunif(n)
  mean + sd * stats::qnorm(u)
}
