##################
#### Stats II ####
##################

###############################
#### Tutorial 1: Refresher ####
###############################

# Today's tutorial is a refesher of the R skills we learnt in the first semester.
#     1. Importing data
#     2. Wrangling data
#     3. Analysing data
#     4. Communicating

#### Case study
# A fictional think-tank, the Alliance of Wealthy People who Dislike Tax, has asked
# you to study the relationship between tax, regulation and GDP per capita. They believe
# that countries with low tax and light regulation are wealthier, and they want you to 
# prove it using statistics!

# We're going to use variables such as: 
# Ease of doing business rank (1=most business-friendly regulations): IC.BUS.EASE.XQ
# GDP per capita (current US$): NY.GDP.PCAP.CD
# "tax revenue (% of GDP)": GC.TAX.TOTL.GD.ZS

#### Working Directory
getwd()

#### Importing the data
# Your csv file should now be in the desktop folder. Before opening it, we're going to
# load in our libraries.

library(tidyverse)
library(stargazer)

## loading the data
data <- read_csv("tutorial01_data.csv",
                 col_types = cols(
                   `Ease of doing business rank (1=most business-friendly regulations)` = col_double(),
                   `Tax revenue (% of GDP)` = col_double(),
                   `GDP per capita (current US$)` = col_double())) # This changed all of the data to numeric

str(data) # Check dataset

#### Wrangling the data
# We should now have a dataset where our variables are at least of the correct type.
# However, we need to do a bit of tidying to get the data into a more user-friendly
# format. 

# The data is in character format but it should be in numeric format
  
# 1. First, let's have a look at our data object. Use the functions we learned from last
#    term. 
str(data)
ls(data) # Check the variables
head(data)
summary(data)

# 2. Let's drop the rows and columns we don't need.
# We only have one year, so the two cols related to year can be dropped; also, we only
# really need one col for country name, so let's drop country code too.
data <- data %>% # Remove column that starts with time and the country code column
  select(-(starts_with("Time")), -(`Country Code`))
ls(data) # Check that you have the right variables

# 3. Let's also get rid of the variable code in square brackets
# Don't have to do this because the square brackets are not in the variable names
names(data) <- #hint: try using the function sub() with the regexp " \\[.*"
names(data) <- sub(" \\[.*", "", names(data)) # What to do if the brackets are there
  
#### Analysing the data
# Now that we have a dataset in the desired format, we can proceed to the analysis.

# 1. Let's perform some preliminary descriptive analysis using our visualisation skills.
#    Try using ggplot to create a plot of scatter showing GDP p/c vs Tax revenue. Add a
#    simple linear regression line.
ggplot(data = data, # Use back ticks when there is a space
       aes(`Tax revenue (% of GDP)`, `GDP per capita (current US$)`)) +
       geom_point() +
        geom_smooth(method = "lm")

# 2. Now let's try the same using GDP p/c vs Ease of Doing Business.
ggplot(data = data, 
  aes(`Ease of doing business rank (1=most business-friendly regulations)`, `GDP per capita (current US$)`)) +
  geom_point() +
  geom_smooth(method = "lm")

# 3. And, for the sake of argument, let's see what the relationship is between Tax and
#    Ease of Doing Business.
ggplot(data = data, 
  aes(`Ease of doing business rank (1=most business-friendly regulations)`, `Tax revenue (% of GDP)`)) +
  geom_point() +
  geom_smooth(method = "lm")

# 4. Let's think for a minute before we perform the multivariate regression: what kind
#    of interaction are we seeing with these three plots?
# First = positive linear relationship
# Second = negative linear relationship
# Third = Not really any significant relationship

# 5. Now let's run a regression!
formula <- `GDP per capita (current US$)` ~ `Tax revenue (% of GDP)` + `Ease of doing business rank (1=most business-friendly regulations)`
regression <- lm(formula, data)
summary(regression)

# How do we interpret these results?
# On average when tax revenue and ease of doing business rank are both 0, GDP per capita is 9256.
# On average when tax revenue increases by one unit, there is an associated GDP increase by 1458, holding ease of doing business constant.
# On average when ease of doing business increases by one unit, the GDP decreases by 223, holding tax revenue constant.

# Looking at the adjusted R-squared, 9% of the variability in the variables can be explained by one another???
# Based off of the f statistic, at least one of the variables is significant in explaining GDP.

#### Communicating
# The final task is to communicate our results. We're going to do this in pdf format 
# using latex, and then upload our results to github, just as we would with a problem
# set!

# 1. Visualisation
# We want a good visualisation of our results, including a title. We've seen that Ease 
# of Doing Business doesn't seem to have a very significant effect (statistically or
# substantively), so let's plot GDP vs Tax, and include Ease of Doing Business as
# either a size or alpha variable to our scatter points. Use the "export" option in the
# plots window to create a pdf of the plot below. Save it in the same folder as your 
# latex template.

pdf("graph2.pdf")
data %>%
  ggplot(aes(`Tax revenue (% of GDP)`, 
             `GDP per capita (current US$)`, 
             )) +
  geom_point() +
  geom_smooth(method = "lm")
dev.off()

# 2. Regression table
# We'll use stargazer to create the latex code for our regression table. Clear your 
# console, then run the code below.

stargazer(regression, type = "latex")

# Now all we need is to update the latex template and upload the pdf to github!