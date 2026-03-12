##################
#### Stats II ####
##################

###############################
#### Tutorial 4: Logit ####
###############################

# In today's tutorial, we'll begin to explore logit regressions
#     1. Estimate logit regression in R using glm()
#     2. Practice makes inferences using logit regression
#     3. Compare logit models

#####################
# load libraries
# set wd
# clear global .envir
#####################

install.packages("forecast")
install.packages("zoo")

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

lapply(c("tidyverse"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Binary logits:

# Employing a sample of 1643 men between the ages of 20 and 24 from the U.S. National Longitudinal Survey of Youth.
# Powers and Xie (2000) investigate the relationship between high-school graduation and parents' education, race, family income, 
# number of siblings, family structure, and a test of academic ability. 

# The dataset contains the following variables:
# hsgrad: Whether the respondent was graduated from high school by 1985 (Yes or No)
# nonwhite: Whether the respondent is black or Hispanic (Yes or No)
# mhs: Whether the respondent’s mother is a high-school graduate (Yes or No)
# fhs: Whether the respondent’s father is a high-school graduate (Yes or No)
# income: Family income in 1979 (in $1000s) adjusted for family size
# asvab: Standardized score on the Armed Services Vocational Aptitude Battery test 
# nsibs: Number of siblings
# intact: Whether the respondent lived with both biological parents at age 14 (Yes or No)

graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt")

str(graduation)

# Convert yes/no variables into factor variables
yn_variables <- c("hsgrad", "nonwhite", "mhs", "fhs", "intact")
graduation[yn_variables] <- lapply(graduation[yn_variables], factor)

str(graduation)

# (a) Perform a logistic regression of hsgrad on the other variables in the data set.
full_model <- glm(data = graduation,
                  hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs + intact, 
                  family = binomial(link="logit"))

summary(full_model)

# Interpretations
# Intercept: When all the variables are equal to zero, the log-odds of graduating from high school are 0.932.
# Nonwhite: When someone is not white, and all other variables are held constant, the log-odds of graduating high school are 0.801 than for white people.

# Compute a likelihood-ratio test of the omnibus null hypothesis that none of the explanatory variables influences high-school graduation. 
null_model <- glm(data = graduation,
                  hsgrad ~ 1,
                  family = binomial)

anova(null_model, full_model, test="LRT")

# Interpretation: at least one of the variables in the model explains high school graduation rate (p-value very small)

# Then construct 95-percent confidence intervals for the coefficients of the seven explanatory variables. 
confint(full_model)

# What conclusions can you draw from these results? Finally, offer two brief, but concrete, interpretations of each of the estimated coefficients of income and intact.
# There is not a statistically significant relationship between whether the respondent's father is a high school graduation, and whether the person has siblings,
# due to the fact that the confidence interval contains 0. For the income, holding all other variables constant, for a one unit increase in the household income,
# the log-odds of graduating high school increase by 0.05309.

# (b) The logistic regression in the previous problem assumes that the partial relationship between the log-odds of high-school graduation and number of siblings is linear. 
# Test for nonlinearity by fitting a model that treats nsibs as a factor, performing an appropriate likelihood-ratio test. 
graduation["nsibs_factor"] <- lapply(graduation["nsibs"], factor)

factor_model <- glm(data = graduation,
                    hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs + intact + nsibs_factor,
                    family = binomial)

summary(factor_model)

anova(full_model, factor_model, test = "LRT")

# Linear representation of nsibs variable did not improve the model in comparison to it as a categorical variable

# In the course of working this problem, you should discover an issue in the data. 
unique(graduation$nsibs_factor)
table(graduation$nsibs_factor)

# Number of siblings = -3 is IMPOSSIBLE!
# Some values have very few observations, which leads to their high standard error so need to recategorize

# Deal with the issue in a reasonable manner. 
graduation_clean <- subset(graduation, nsibs >= 0) # Remove -3 cases

graduation_clean$nsibs_cat <- cut(
  graduation_clean$nsibs,
  breaks = c(-1, 1, 3, 5, 10, 20),
  labels = c("0-1", "2-3", "4-5", "6-10", "11+")
)
  
unique(graduation_clean$nsibs_cat)
table(graduation_clean$nsibs_cat)

# Does the result of the test change?
factor_model2 <- glm(
  data = graduation_clean,
  hsgrad ~ nonwhite + fhs + income + asvab + intact + nsibs_cat,
  family = binomial(link = "logit")
)

full_model2 <- glm(
  data = graduation_clean,
  hsgrad ~ nonwhite + fhs + income + asvab + nsibs + intact,
  family = binomial(link = "logit")
)

summary(factor_model2)
summary(full_model2)

anova(full_model2, factor_model2, test = "LRT")

# First model, nsibs = linear
# Factor model, nsibs = factor
# Changed value (factor) does not significantly change the relationship, so best to just keep the linear model
# for parsimony purposes