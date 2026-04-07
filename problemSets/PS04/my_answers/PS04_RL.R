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

lapply(c("nnet", "MASS", "survival", "eha", "tidyverse", "ggfortify", 
         "stargazer", "VGAM", "sampleSelection", "texreg"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data on child mortality by mother's background and child gender
data("child", package = "eha")

# set survival of children
# start time, end time, whether the event happened or not
child_surv <- with(child, Surv(enter, exit, event))

# fit cox proportional hazard model
cox <- coxph(child_surv ~ m.age + sex, data = child)
summary(cox)

# output table
stargazer(cox, type = "latex",
          title = "Cox Proportional Hazard Model")

#####################
# Problem 2
#####################

# load data
disaster_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/refs/heads/main/datasets/disaster_response.csv")

# heckman selection model
heck <- heckit(
  selection = binContribution ~ occurrences + deathsEM + 
    normalizedDamageEMLogged,
  outcome = originalContributionMillionUSDLogged ~ occurrences + deathsEM + 
    normalizedDamageEMLogged,
  data = disaster_data
)
summary(heck)

# output table
texreg(heck, 
       caption = "Heckman Selection Model",
       label = "tab:heck")
