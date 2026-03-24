##############################################
# Tutorial 5 
# Ordered and Multinomial Logistic Regression
##############################################

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

lapply(c("MASS", "nnet", "ggplot2"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# This data set is analyzed by Long (1997).  The response variable has four ordered categories:
# Strongly Disagree, Disagree, Agree, and Strongly Agree in relation to the statement

# "A working mother can establish just as warm and secure a relationship with her children as a mother who does not work."

# The explanatory variables are:
# the year of the survey (1977 or 1989),
# the gender of the respondent,
# the race of the respondent (white or non-white),
# the respondent's age, and
# the prestige of the respondent's occupation (a quantitative variable)

# load data
workingMoms <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/WorkingMoms.txt", header=T)

# check basic cross tabs
summary(workingMoms)
ftable(xtabs(~ gender + year + attitude, data = workingMoms)) # build cross tab counting how many observations fall in each combo
# then, create a flat version of the multidimensional table created

# Re-label your outcome, gender, race, and year so they are legible
# i.e. "SD", "D", "A", "SA" to "Strongly Disagree", "Disagree", "Agree", "Strongly Agree" 
# 0,1 to "Non-white", "White"
# and "Year1977", "Year1989" to "1977", "1989"
# Re-label outcome variable
workingMoms$attitude <- factor(workingMoms$attitude,
                               levels = c("SD", "D", "A", "SA"),
                               labels = c("Strongly Disagree", "Disagree",
                                        "Agree", "Strongly Agree"))

# Factor the gender variable
workingMoms$gender <- as.factor(workingMoms$gender)

# Factor the race variable
workingMoms$race <- factor(workingMoms$race,
                           levels = c(0,1),
                           labels = c("Non-white", "White"))

# Factor the year variable
workingMoms$year <- factor(workingMoms$year,
                        levels = c("Year1977", "Year1989"),
                          labels = c("1977", "1989"))

# Create a new cross tabs table
ftable(xtabs(~ gender + year + attitude, data = workingMoms))

# Create a proportions table for female vs male
prop.table(table(workingMoms$gender, workingMoms$attitude), 1)

# Create a table of proportions for gender in 1977
prop.table(table(workingMoms$gender[workingMoms$year == "1977"],
                 workingMoms$attitude[workingMoms$year == "1977"]), 1)

# Create a table of proportions for gender in 1989
prop.table(table(workingMoms$gender[workingMoms$year == "1989"],
                 workingMoms$attitude[workingMoms$year == "1989"]), 1)

# Plot prestige (y-axis) by your outcome (x-axis) by gender ~ year
ggplot(workingMoms, aes(attitude, prestige, color = attitude)) +
  geom_boxplot() + # boxplot for each attitude group
  geom_jitter(alpha = 0.3) + # overlays data points but slightly scattered so not overlapping
  scale_x_discrete(labels=function(x)(sub("\\s", "\n", x))) + # wraps attitude labels
  theme(axis.text.x = element_text(angle = 45)) + # rotates x axis labels 
  facet_grid(gender ~ year) # x ~ y graph will get plotted for all levels of gender

# Shift in category can be seen more over time for women than men

# a) Perform an ordered (proportional odds) logistic regression
ord.log <- polr(attitude ~ ., data = workingMoms, Hess = TRUE)
summary(ord.log)

pp <- data.frame(fitted(ord.log))
head(data.frame(attitude = workingMoms$attitude,
                SD = pp$Strongly.Disagree,
                D = pp$Disagree,
                A = pp$Agree,
                SA = pp$Strongly.Agree))

# Calculate a p value
ctable <- coef(summary(ord.log)) # extract and store just coefficients
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # manually calculates p-values
ctable <- cbind(ctable, "p-value" = p) # attaches p-values as a new column to the coefficients table

# Calculate confidence intervals
ci <- confint(ord.log)

# Convert to odds ratio
exp(cbind(OR = coef(ord.log), ci))

# How do we interpret these coefficients?

## A one-unit increase in X changes the log-odds of being in a higher category by beta. 
## The same beta applies to every threshold. 

# Age: Holding all other variables constant, a one-unit increase in age decreases the log-odds of being in a more supportive
# category by 2.2%.
  # As people get older, they become less supportive of working mothers. 

# Education: Holding all other variables constant, a one-unit increase in education increases the log-odds of being in a 
# more supportive category by 7%.
  # The more educated people are, the more likely they are to be supportive of working mothers.

# Gender: Men have 52% lower odds of being in a more supportive category than women.

# Year1989: People surveyed in the year 1989 were 69% more likely than people in 1977 to be supportive of working mothers. 


# b) fit a multinomial logit model
# with Strongly Disagree as reference level for the outcome
workingMoms$attitude <- relevel(workingMoms$attitude, ref = "Strongly Disagree")
## Compare every category to strongly disagree

# run model
mult.log <- multinom(attitude ~ ., data = workingMoms)
summary(mult.log)
# For each predictor you have coefficients for each category
# So what are the odds of being in each category given your reference category 
# Main difference from ordered logit

# Interpretation
  # Agree vs strongly disagree (age = -0.025): A one-unit increase in age decreases the log-odds of agreeing 
  # (vs strongly disagreeing) by 0.025.

  # Strongly agree vs strongly disagree (education = 0.144): A one-unit increase in education increases the log-odds
  # of strongly agreeing vs strong disagreeing by 0.143.

exp(coef(mult.log))
# Age: A one year increase in age decreases the odds of agreeing by 0.05%.

# get p values
z <- summary(mult.log)$coefficients/summary(mult.log)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

# how do we interpret these coefficients?
## Interpretation

# Education: More education increases the odds of agreeing vs strongly disagreeing by 12%.
# Gender: Being a man vs a woman increases the log odds of disagreeing vs strongly disagreeing 
  # by 11%.

# calculate predicted probabilities to help our interpretation
pp <- data.frame(fitted(mult.log))
View(pp)

head(data.frame(attitude = workingMoms$attitude,
                SD = pp$Strongly.Disagree,
                D = pp$Disagree,
                A = pp$Agree,
                SA = pp$Strongly.Agree))

## Multinomial logit coefficients are log-odds, which are difficult to interpret.
## Predicted probabilities translate the model into something intuitive:
## "Given these probabilities, what is the probability of each attitude?"
## For example, instead of saying coefficient = 0.45, you can say "the model
## predicts a 45% of agreeing"

# c) Consider gender as an interaction with your other predictors
# i.e. consider that possibility that gender interacts with the other explanatory variables in influencing the response variable
mult.log.int <- multinom(attitude ~ gender * ., data = workingMoms)
summary(mult.log.int)

z.int <- summary(mult.log.int)$coefficients/summary(mult.log.int)$standard.errors
(p.int <- (1 - pnorm(abs(z.int), 0, 1)) * 2)

# Interpretation: Being a man results in a decrease in the effects of all covariates except for prestige
# on the likelihood of someone being supportive of working moms. 

pp.int <- data.frame(fitted(mult.log.int))
head(data.frame(attitude = workingMoms$attitude,
                SD = pp.int$Strongly.Disagree,
                D = pp.int$Disagree,
                A = pp$Agree,
                SA = pp$Strongly.Agree))

# What do you find?