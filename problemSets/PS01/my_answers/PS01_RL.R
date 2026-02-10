#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("dgof", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# Function
ks_test <- function(data) {
  n <- length(data)
  k <- 1:n
  
  # Create empirical distribution of observed data
  ECDF <- ecdf(data) # Empirical distribution function
  Fn <- ECDF(data)
  
  # Create theoretical distribution (normal distribution of data)
  F0 <- pnorm(data)
  
  # Generate the test statistic by comparing the similarity between the 
  # empirical  and theoretical CDFs
  d_plus <- max(Fn - F0)
  d_minus <- max(F0 - (Fn - 1/n))
  D <- max(d_plus, d_minus)
  
  # Scale D
  s <- n*D^2
  
  # Calculate the p-value
  if (s > 7.24 || (s > 3.76 && n > 99)) {
    p_value <- 2 * exp(-(2.000071 + .331/sqrt(n) + 1.409/n) * s)
  } else {
    return(message("n and/or s are too"))
  }
  
  # Print results
  print(paste("P-value = ", p_value))
  print(paste("D = ", D))
}

# Set seed to obtain reproducible values
set.seed(123)

# Generate data using 1000 samples from a Cauchy distribution
data <- rcauchy(1000, location = 0, scale = 1)

# Execute functions and compare my function with the ks.test() function
ks_test(data)
ks.test(data, "pnorm")

#####################
# Problem 2
#####################

# Set Seed
set.seed (123)

# Generate data
data <- data.frame(x = runif(200, 1, 10))

# Establish y
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# OLS Likelihood Function
ols_likelihood <- function(parameter, outcome, input) {
  n <- ncol(input) # Establish n
  beta <- parameter[1:n] # Starting values for beta
  sigma <- exp(parameter[1+n]) # Standard deviation using the error
  
   # Take the sum of normal density values for each data point
  -sum(dnorm(outcome, input %*% beta, sigma, log=TRUE))
}

# Run likelihood function in optim
results_norm <- optim(fn = ols_likelihood, 
                      outcome = data$y, 
                      input = cbind(1, data$x), 
                      par = c(1,1,1), # Initial values of parameters
                      hessian = TRUE, 
                      method = "BFGS")

# Run lm()
lm_result <- lm(data = data, data$y ~ data$x)

# Compare results
results_norm$par
lm_result
