##################
#### Stats II ####
##################

rm(list=ls())
###############################
#### Tutorial 2: GLMs ####
###############################

# In today's tutorial, we'll begin to explore GLMs
#     1. Import/wrangle data
#     2. Execute lm() and glm() of RQ
#     3. Compare models

#### Case study
# We're interested in central bank governors, specifically their occupational turnover, for almost all countries in the world starting from the year 1970

#### Create the dataset
# For this task, we first need data.
# 1. Go to https://kof.ethz.ch/en/data/data-on-central-bank-governors.html and download the data on Central Bank Governors
# https://ethz.ch/content/dam/ethz/special-interest/dual/kof-dam/documents/central_bank_governors/cbg_turnover_v23upload.xlsx
# 2. Gather necessary variables
#    codewdi: Country code or name
#    year
#    time to regular turnover	
#    regular turnover dummy	
#    irregular turnover dummy	
#    legal duration

# MAKE SURE THERE AREN'T MISSING VALUES!

# Now, you've got your dataset

#### Import the data
# Your csv file should now be in the desktop folder. Before opening it, we're going to
# load in uour libraries
pkgTest <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
  
lapply(c("tidyverse", "ggplot2", "readxl"), pkgTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()
## loading the data
data <- read.csv("data_for_tutorial2.csv")

#### Wrangling the data
# We should now have a dataset where our variables are at least of the correct type
# However, we need to do a bit of tidying to get the data into a more user-friendly
# format. 

data <- data %>%
  select(
    codewdi,
    country,
    year,
    `time.to.regular.turnover`,
    `regular.turnover.dummy`,
    `irregular.turnover.dummy`,
    `legal.duration`,
  ) %>%
  mutate(
    codewdi = as.factor(codewdi),
    country = as.factor(country),
    year = as.integer(year),
    time_to_regular_turnover = as.integer(`time.to.regular.turnover`),
    regular_turnover = as.integer(`regular.turnover.dummy`),
    irregular_turnover = as.integer(`irregular.turnover.dummy`),
    legal_duration = as.integer(`legal.duration`)
  ) %>%
  drop_na()

data <- data %>%
  select(
    codewdi,
    country,
    year,
    time_to_regular_turnover,
    regular_turnover,
    irregular_turnover,
    legal_duration)

str(data)

unique(data$year)
unique(data$time_to_regular_turnover)
unique(data$regular_turnover)
unique(data$irregular_turnover)
unique(data$legal_duration)

bad_codes <- c(-999, -666, -55, -881)

data <- data %>%
  mutate(
    across(
      c(year, time_to_regular_turnover, regular_turnover,
        irregular_turnover, legal_duration
    ),
    ~ replace(., . %in% bad_codes, NA)
  )
  )

data <- data %>% drop_na()

#### Descriptive patterns in turnover
# Compute the average turnover rate (mean of turnover) by country over the full sample period
country_turnover <- data %>%
  group_by(country) %>%
  summarize(
    avg_turnover = mean(irregular_turnover),
    n = n()
  )

# In a given year, how likely is it that the central bank governor completed their tenure and government changed

# (a) Which five countries have the highest average turnover rates?
country_turnover %>%
  arrange(desc(avg_turnover)) %>%
  slice(1:5)
  
# (b) Which five have the lowest average turnover rates?
country_turnover %>%
  arrange(avg_turnover) %>%
  slice(1:5)
  
# (c) Plot the distribution of country‑level average turnover rates (e.g. histogram or density) 
#     Briefly comment on whether high turnover is concentrated in a small set of countries
ggplot(country_turnover, aes(x = avg_turnover)) +
  geom_histogram()

####  Estimate a linear probability model (LPM) with OLS:
  
# (a) Fit lm() with:
  # Outcome: irregular turnover dummy
  # Covariates: 
  #   time to regular turnover	
  #   legal duration
formula <- lm(data = data, irregular_turnover ~ time_to_regular_turnover + legal_duration)
summary(formula)

# (b) For a “typical” observation  (e.g. median time to regular turnover & legal duration), compute the predicted probability
  
# (c) Identify at least one observation for which lm() prediction is below 0 or above 1 and explain why such predictions are problematic for a probability

# Using the full sample, construct a plot of predicted probability of turnover vs time to regular turnover:
  
#### Baseline logistic regression
  
# Estimate a logistic regression with governor turnover as the binary outcome and same covariates using glm(family = "binomial")
  
# (a) Report coefficient estimates and standard errors

# (b) Interpret the sign of each coefficient in terms of how they affect the probability of turnover

# (c) For the same “typical” observation used above, compute the predicted probability of turnover (type = "response"), and compare it to the lm() prediction

#### Compare lm() and glm()  

# (a) Use the lm() to compute fitted values across the observed range of time to regular turnoner, holding legal duration at median value

# (b) Use the logit model to compute fitted probabilities for the same legal duration values

# (c) Plot both curves on the same graph (e.g. blue for lm(), red for glm()) 
  
#### Country heterogeneity and fixed effects

# (a) Introduce country fixed effects into the logit specification using dummy variables 

# (b) Compare the estimated coefficients with and without country fixed effects. How does controlling for unobserved country characteristics affect the relationships w/ turnover?
  
# (c) What kinds of country‑specific factors might be absorbed by these fixed effects in this context
