library(dqrng)
source("dqrng_dqrnorm.R")

dqrng_dqrMNorm <- function(mu = 0, Sigma1, Sigma2) {
  p <- ncol(Sigma1)
  n <- ncol(Sigma2)
  
  U <- base::chol(base::kronecker(Sigma1,Sigma2), pivot = TRUE)
  U <- U[, order(attr(U, "pivot"))]
  
  matrix(base::crossprod(U,dqrng_dqrnorm(n*p,as.vector(mu))),n,p)
}
