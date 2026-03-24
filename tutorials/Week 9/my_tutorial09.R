#######################
# Tutorial 9: Poisson #
#######################

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

lapply(c("ggplot2", "tidyverse"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data: Research Productivity of Doctoral Students in Bio-chemistry (Long 1990) 
# Productivity of doctoral students in biochemistry during the last three yearsof their PhD programmes. 
# The response variables the number of articles published during this period (art)
# Explanatory variables include:
# - gender of the student (fem=1 for women, 0=men)
# - student’s marital status (mar= 1 if married, 0 otherwise)
# - student’s number of children five years old or younger (kid5); 
# - a rating of the prestige of the student’sPhD department (phd); 
# - number of articles published by the student’s mentor during the three-yearperiod (ment)

# Load data
long_data <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Long.txt", header=T)

# Make sure your data are in the correct format.
str(long_data)

# (a) Examine the distribution of the response variable. 
# Does least-squares linear regression appear a promising strategy for these data?
# Do we meet the OLS assumptions?
# Trying to understand what the dispersion is of our response variable
# look at the mean, variance, etc. for our explanatory variables
plot(long_data$phd, long_data$art)
hist(long_data$art)

mean(long_data$art)
var(long_data$art)
# Variance is bigger than the mean

ols_model <- lm(art ~., data = long_data)
summary(ols_model)
# predictions might end up being negative 
# constant variance is not validated, the residuals are not normal
# OLS is not doing a good job modeling our data

# (b) Perform a Poisson regression of number of articles published on the explanatory variables. 
# Do we meet assumptions for Poisson?
# What conclusions would you draw from this analysis (i.e. interpret your estimated coefficients)?
pois_model <- glm(art ~., data = long_data, family = "poisson")
summary(pois_model)

# Plot

# Conclusions
# Gender: For a women, holding all other variables constant, the expected mean number of articles
# published decreases by a multiplicative factor of exp(-.225) in comparison to men.
# expected number of articles goes down

# What is the predicted number of articles for a married male PhD researcher with 1 child at 2-rated institute whose PhD supervisor published 5 articles?
# Plot predictions vs count.
# Calculate pseudo R squared.
# Calculate RMSE.
# Should we add an interaction for gender with our covariates?

# Plot predicted vs count
pred <- predict(pois_model, type = "response")

plot(long_data$art, pred)
abline(0, 1, col = "red")

# (c) Consider the possibility of over-dispersion, either by fitting an over-dispersed Poisson model. 
# Is there evidence for over-dispersion? How, if at all, do the results change when over-dispersion is taken into account?
1 - pois_model$deviance / pois_model$null.deviance
# 10%

# rmse
rmse <- sqrt(mean((long_data$art - pred)^2))
rmse
# on average, the amount of articles that my model wrongly predicts

pois_int_model <- glm(art ~ fem * (ment + phd + mar + kid5), data = long_data, family = "poisson")
summary(pois_int_model)

anova(pois_model, pois_int_model, test = "LRT")

install.packages("AER")
library(AER)
dispersiontest(pois_model)
# reject that the dispersion is less than or equal to 1, meaning there is overdispersion

install.packages("pscl")
library(pscl)
mod.zip <- zeroinfl(art ~ ., data = long_data, dist = "poisson")
summary(mod.zip)

# count model coefficients = same poisson model but adjusting for all of the 0 cases
# zero-inflated model coefficients = publishing vs not publishing at all, y = 1 = non-published, binomial with logit link
# make interpretation for expected count given the count coefficients

# want to minimize the AIC
