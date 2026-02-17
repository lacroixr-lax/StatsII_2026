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

lapply(c("tidyverse", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))

# visualize data
head(climateSupport)

str(climateSupport)

# run additive model
add_model <- glm(data = climateSupport,
                 choice ~ countries + sanctions,
                 family = binomial)
summary(add_model)

# create table
stargazer(add_model,
          type = "latex",
          title = "Table of Coefficients",
          column.labels = "Additive Model",
          covariate.labels = c("Countries", "Sanctions"),
          dep.var.labels = "Choice")

# Changing the variables to not be ordered factors and now just regular factors
climateSupport$countries <- factor(climateSupport$countries, ordered = FALSE)
climateSupport$sanctions <- factor(climateSupport$sanctions, ordered = FALSE)

# Run additive model
add_model2 <- glm(data = climateSupport,
                  choice ~ countries + sanctions,
                  family = binomial)

summary(add_model2)

# Create table
stargazer(add_model2,
          type = "latex",
          title = "Table of Coefficients",
          column.labels = "Additive Model",
          covariate.labels = c("Countries (80 of 192)", "Countries (160 of 192)", 
          "Sanctions (5%)", "Sanctions (15%)", "Sanctions (20%)"),
          dep.var.labels = "Choice")

#####################
# Problem 2
#####################

# 2a: Determining the log-odds difference between 5% and 15% sanctions.
coef(add_model2)["sanctions15%"] - coef(add_model2)["sanctions5%"]

# 2c: Estimated probability an individual will support the policy if there are 
# 80 out of 192 countries participating. 
probability <- exp(coef(add_model2)["(Intercept)"] + 
                     coef(add_model2)["countries80 of 192"])/
  (1 + exp(coef(add_model2)["(Intercept)"] + 
             coef(add_model2)["countries80 of 192"]))
print(probability)

#####################
# Problem 3
#####################

# Run interaction model
int_model <- glm(data = climateSupport,
                 choice ~ countries * sanctions,
                 family = binomial)
summary(int_model)

# Perform test on additive vs interaction model
anova(add_model2, int_model, test = "LRT")
