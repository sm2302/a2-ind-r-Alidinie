# Load libraries ---------------------------------------------------------------
library(tidyverse)              
# any other libraries...

# Q1 ---------------------------------------------------------------------------
pareto_pdf <- function(x, alpha, beta) {
  # creating a contradicting parameter where alpha and beta is less than or equal to zero
  if (alpha <= 0 || beta <= 0) 
  {stop("alpha or beta is less than or equal to zero")}
  # inserting the probability density function with condition
  if (x >= beta)
  {f <- (alpha * beta ^ alpha) / x ^ (alpha + 1)}
  else if (x < beta)
  {f <- 0}
  f
}

pareto_pdf(10, 1, 2)

# Q2 ---------------------------------------------------------------------------
pareto_dev <- function(alpha, beta, x) {
  sumlogf <- 0
  for (i in x)
  {logf <- log(pareto_pdf(i, alpha, beta))
  sumlogf <- sumlogf + logf
  }
  D <- -2*sumlogf
  print(D)
}

x <- c(1:100)
pareto_dev(2, 1, x)

#Q3-----------------------------------------------------------------------------
X <- read.table("20b9045.txt")
print(X)

beta_hat <- min(X)
alpha_hat <- n/ sum(log(X)) - log(beta_hat) 
# For derivative of alpha_hat, refer to the file uploaded named 'Q3.pdf'
                 

plot(density(X$V1)) #visualizing how the graph looks like

nll.normal <- function (X, par) {
  return (-sum(log(dnorm(X, alpha_hat = par[1], beta_hat = par[2] ))))
}

# Finding the parameter that maximize the likelihood function 
optim (par = c(1,2), nll.normal, data = X$V1)



N = 100            # The length of X
alpha_hat = 1
beta_hat= 2

log_like <- function(par, V1) {
  V1 <- as.matrix(V1)
  N <- nrow(V1)
  alpha_hat = par[1]
  beta_hat = par[2]
  loglik <- -2*log((alpha_hat * beta_hat ^ alpha_hat) / x ^ (alpha_hat + 1))
  return (-loglik)
}

MLE_estimates <- optim(fn = log_like,              # likelihood function
                       par = c(1,1),               # initial guess
                       lower = c(-Inf,-Inf),       # lower bound on parameters
                       upper = c(Inf, Inf),        # upper bound on parameters
                       hessian = TRUE,             # return hessian
                       method = "L-BFGS-B",
                       # inputs
                       V1 = X$V1)

#Q4-----------------------------------------------------------------------------
pareto_cdf <- function (x, alpha, beta)
  
# Refer to the file uploaded named 'Q4.pdf' for the in reference of CDF
  
  
F <- 1-(beta/x) ^ alpha

#Q5-----------------------------------------------------------------------------