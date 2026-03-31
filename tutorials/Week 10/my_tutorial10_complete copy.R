###################################
# Tutorial 10: 
# Survival Analysis #
# Truncated Data #
###################################

# remove objects
rm(list=ls())

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer", "VGAM"),  pkgTest)

#### Survival Analysis
data(child)

#### Completed
# a)
# set survival of the children
# start time, end time, whether the event happened or not
child_surv <- with(child, Surv(enter, exit, event))

# km curve = how the survival rate changes over time
km <- survfit(child_surv ~ 1, data = child)
summary(km, times = seq(0, 15, 1))
plot(km, main = "Kaplan-Meier Plot", xlab = "Years", ylim = c(0.7, 1))
autoplot(km)

# survival rates by social class
km_socBranch <- survfit(child_surv ~ socBranch, data = child)
summary (km_socBranch)
autoplot(km_socBranch)

# b) Cox proportional hazard model
cox <- coxph(child_surv ~ sex + socBranch, data = child)
summary(cox)
drop1(cox, test = "Chisq")
stargazer(cox, type = "text")

# There is a 0.08 decrease in the expected log of the hazard for female babies compared to 
# male, holding socBranch constant. There is a 0.34 increase in the expected log of the hazard
# for babies of businessmen compared to officials, holding sex constant.

# exponentiate parameter estimates to obtain hazard ratios
exp(-0.083546)
# The hazard ratio of female babies is 0.92 that of male babies, i.e. female babies are less
# likely to die (92 female babies die for every 100 male babies; female deaths are 8% lower, etc.)

cox_fit <- survfit(cox)
autoplot(cox_fit)

newdat <- with(child, 
               data.frame(
                 sex = c("male", "female"), socBranch="official"
                 )
               )

plot(survfit(cox, newdata = newdat), xscale = 12,
     conf.int = T,
     ylim = c(0.6, 1),
     col = c("red", "blue"),
     xlab = "Time",
     ylab = "Survival proportion",
     main = "")
legend("bottomleft",
       legend=c("Male", "Female"),
       lty = 1, 
       col = c("red", "blue"),
       text.col = c("red", "blue"))
# Note: the confidence intervals on this plot are for the prediction, not
# the standard error of the terms in the model (the effect of sex in the 
# cox ph model was significant, here the CIs overlap. Always check your
# results and interpretation!)


# Adding an interaction
cox.int <- coxph(child_surv ~ sex * socBranch, data = child)
summary(cox.int)
# drop1 tests having each of the predictor variables in the model vs the null model
drop1(cox.int, test = "Chisq")
stargazer(cox.int, type = "text")

#### Truncated Data

education_data <- read.table("https://www.john-fox.ca/AppliedRegression/datasets/Long-PhDs.txt")

with(education_data, hist(job))

# OLS on original data
summary(m1 <- lm(job ~ ., education_data))

# Tobit 1
summary(m.tobit <- vglm(job ~ ., tobit(Lower = 1), education_data))
# a one-unit increase in phd increases the latent job score by 0.

# tobit 2
summary(m.tobit.2 <- vglm(job ~ ., tobit(Lower = 2), education_data))
# a one-unit increase in phd increases the latent job score by 0.330, accounting for censoring

## heckman
install.packages("sampleSelection")
library(sampleSelection)

new_data <- data("Mroz87")

# OLS 
# selection bias because it does not account for the people that are not apart of the labor force
summary(lm(wage ~ educ + exper + age + kids5, data = Mroz87))

# Heckman
heck <- selection(
  selection = lfp ~ educ + exper + age + kids5,
  outcome = wage ~ educ + exper + age + kids5,
  data = Mroz87
)

summary(heck)

# selection equation:
## what affects probability of working?
## more kids -> less likely to work