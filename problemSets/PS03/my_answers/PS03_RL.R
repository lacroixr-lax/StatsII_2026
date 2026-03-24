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

lapply(c("nnet", "MASS", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

## Data loading and prep

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)

# label GDPWdiff variable into levels "positive", "negative", and "no change"
gdp_data$GDPWdiff <- ifelse(gdp_data$GDPWdiff < 0, "negative",
                            ifelse(gdp_data$GDPWdiff == 0, "no change",
                                   ifelse(gdp_data$GDPWdiff > 0, "positive", NA)))

# factor variable
gdp_data$GDPWdiff <- factor(gdp_data$GDPWdiff,
                            levels = c("negative", "no change", "positive"))

## (1) Unordered multinomial logit

# make "no change" reference category
gdp_data$GDPWdiff <- relevel(gdp_data$GDPWdiff, ref = "no change")

# run model
unordered_model <- multinom(GDPWdiff ~ OIL + REG, data = gdp_data)
summary(unordered_model)

# create table
stargazer(unordered_model,
          type = "latex",
          title = "Table of Coefficients: Unordered Multinomial Logit",
          covariate.labels = c("Oil", "Democracy"))

## (2) Ordered 

# re-order the GDPWdiff variable
gdp_data$GDPWdiff <- factor(gdp_data$GDPWdiff,
                            levels = c("negative", "no change", "positive"),
                            ordered = TRUE)

# run model
ordered_model <- polr(GDPWdiff ~ OIL + REG, data = gdp_data, Hess = TRUE)
summary(ordered_model)

# create table
stargazer(ordered_model,
          add.lines = list(c("Threshold 1|2", round(ordered_model$zeta[1], 3)), 
                           c("Threshold 2|3", round(ordered_model$zeta[2], 3))),
          type = "latex",
          title = "Table of Coefficients: Ordered Multinomial Logit",
          style = "default",
          covariate.labels = c("Oil", "Democracy")
          )

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")

## (a) Run a poisson regression
# run model
poisson_model <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + 
                       PAN.governor.06, data = mexico_elections,
                     family = "poisson")
summary(poisson_model)

# create table
stargazer(poisson_model,
          report = "vctp*",
          type = "latex",
          title = "Table of Coefficients: Poisson Model",
          style = "default",
          covariate.labels = c("Competitive District", 
                               "Poverty",
                               "PAN-Affilitated Governor"))


## (c) estimate mean number of visits
# create dataframe with the hypothetical values
hypothetical_values <- data.frame(
  competitive.district = 1,
  marginality.06 = 0,
  PAN.governor.06 = 1)

# use predict function and hypothetical df to generate prediction
mean_visits <- predict(poisson_model, hypothetical_values, type = "response")

# calculation by hand
mean_visits_manual <- exp(-3.81023 - 0.08135*1 - 0.31158*1)

# compare
print(c(mean_visits, mean_visits_manual))
