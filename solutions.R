# Load libraries ---------------------------------------------------------------
library(tidyverse)              
# any other libraries...
usethis:: use_git_config(
  user.name = "Alidinie"
  user.email = "20b9045@ubd.edu.bn")

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
  print(f)
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
