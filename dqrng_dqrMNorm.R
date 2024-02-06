library(dqrng)

dqrng_dqrMNorm <- function(mu = 0, Sigma1, Sigma2) {
  p <- ncol(Sigma1)
  n <- ncol(Sigma2)
  
  Ua <- base::chol(Sigma1, pivot = TRUE)
  Ua <- Ua[, order(attr(Ua, "pivot"))]
  
  Ub <- base::chol(Sigma2, pivot = TRUE)
  Ub <- Ub[, order(attr(Ub, "pivot"))]
  
  mu + t(Ub) %*% matrix(dqrng::dqrnorm(n*p),n,p) %*% Ua
}
