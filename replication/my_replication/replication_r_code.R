#####################
# Replication Project
#####################

# Clear environment and console
rm(list=ls())

# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", 
                      "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, 
                                  TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, 
                                                                    character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# load packages
lapply(c("tidyverse", "readstata13", "stargazer", "broom",
         "modelsummary", "tibble", "ggplot2", "performance",
         "see", "car", "corrr", "brms", "lme4"),  pkgTest)

# set working directory
setwd("/Users/rosalielacroix/Desktop/GitHub/StatsII_2026/replication/my_replication")

## DATA ##

# load dataset
df_all <- read.dta13("Kreiman_Masullo_Replication_LAPS_Final.dta")

# remove IVs that are not being used in regression and figures
df <- subset(df_all, select = -c(16:26, 28:44)) # columns 16-26 and 28-44 are 
                                                # not used in the regression

## REGRESSIONS ##

# create object that allows the departamentos to be easily placed at the end of each formula
all_depts <- grep("^departamento_", names(df), value = TRUE)

# model 1: farc attacks only
# find which reference category matches stata's intercept of 53.67 for model 1
for (ref in all_depts) { # iterate over each department dummy
  dept_vars_try <- paste(setdiff(all_depts, ref), collapse = " + ") # build formula without the current department dummy
  f_try <- as.formula(paste("ref_si ~ ln_atfarc_cede +", dept_vars_try)) # build full formula
  model_try <- lm(f_try, data = df) # try model without dummy
  intercept <- coef(model_try)["(Intercept)"] # isolate intercept
  if (abs(intercept - 53.67) < 0.01) {
    cat("Model 1 reference category:", ref, "\n") # if the intercept is within 0.01, this is the reference category
  }
}

# remove reference department from all departments
dept_vars_1 <- paste(setdiff(all_depts, "departamento_27"), collapse = " + ")

# run model 1
f1 <- as.formula(paste("ref_si ~ ln_atfarc_cede +", dept_vars_1)) # formula
model_1 <- lm(f1, data = df) # run regression
summary(model_1) # check regression coefficients are accurate

# model 2: paramilitary/BACRIM attacks only
# find which reference category matches stata's intercept for model 2
for (ref in all_depts) {
  dept_vars_try <- paste(setdiff(all_depts, ref), collapse = " + ")
  f_try <- as.formula(paste("ref_si ~ ln_atparabacrim_cede +", dept_vars_try))
  model_try <- lm(f_try, data = df)
  intercept <- coef(model_try)["(Intercept)"]
  if (abs(intercept - 60.89) < 0.01) {
    cat("Model 2 reference category:", ref, "\n")
  }
}

# remove reference department from all departments
dept_vars_2 <- paste(setdiff(all_depts, "departamento_5"), collapse = " + ")

# run model 2
f2 <- as.formula(paste("ref_si ~ ln_atparabacrim_cede +", dept_vars_2))
model_2 <- lm(f2, data = df)
summary(model_2)

# model 3: farc and para/BACRIM attacks only
# find which reference category matches stata's for model 3
for (ref in all_depts) {
  dept_vars_try <- paste(setdiff(all_depts, ref), collapse = " + ")
  f_try <- as.formula(paste("ref_si ~ ln_atfarc_cede + ln_atparabacrim_cede +", dept_vars_try))
  model_try <- lm(f_try, data = df)
  intercept <- coef(model_try)["(Intercept)"]
  if (abs(intercept - 69.81) < 0.01) {
    cat("Model 3 reference category:", ref, "\n")
  }
}

# remove reference department from all departments
dept_vars_3 <- paste(setdiff(all_depts, "departamento_32"), collapse = " + ")

# run model 3
f3 <- as.formula(paste("ref_si ~ ln_atfarc_cede + ln_atparabacrim_cede +", dept_vars_3))
model_3 <- lm(f3, data = df)
summary(model_3)

# model 4: farc attacks, para/BACRIM attacks, % participation in ref, 
# % support santos
# find which reference category matches stata's for model 4
for (ref in all_depts) {
  dept_vars_try <- paste(setdiff(all_depts, ref), collapse = " + ")
  f_try <- as.formula(paste("ref_si ~ ln_atfarc_cede + ln_atparabacrim_cede +
                       ref_partic + share_santos_2_100 +", dept_vars_try))
  model_try <- lm(f_try, data = df)
  intercept <- coef(model_try)["(Intercept)"]
  if (abs(intercept - 35.80) < 0.01) {
    cat("Model 4 reference category:", ref, "\n")
  }
}

# remove reference department from all departments
dept_vars_4 <- paste(setdiff(all_depts, "departamento_5"), collapse = " + ")

# run model 4
f4 <- as.formula(paste("ref_si ~ ln_atfarc_cede + ln_atparabacrim_cede + 
                       ref_partic + share_santos_2_100 +", dept_vars_4))
model_4 <- lm(f4, data = df)
summary(model_4)

# model 5: farc attacks, para/BACRIM attacks, % participation in ref, 
# % support santos, poverty, population, rural index
# find which reference category matches stata's for model 5 
for (ref in all_depts) {
  dept_vars_try <- paste(setdiff(all_depts, ref), collapse = " + ")
  f_try <- as.formula(paste("ref_si ~ ln_atfarc_cede + ln_atparabacrim_cede +
                       ref_partic + share_santos_2_100 +
                       incidence_multidim_pov_2005_100 + lpop + indrural2005 +", dept_vars_try))
  model_try <- lm(f_try, data = df)
  intercept <- coef(model_try)["(Intercept)"]
  if (abs(intercept - 30.22) < 0.01) {
    cat("Model 5 reference category:", ref, "\n")
  }
}

# remove reference department
dept_vars_5 <- paste(setdiff(all_depts, "departamento_5"), collapse = " + ")

# run model 5
f5 <- as.formula(paste("ref_si ~ ln_atfarc_cede + ln_atparabacrim_cede + 
                       ref_partic + share_santos_2_100 + 
                       incidence_multidim_pov_2005_100 + lpop + indrural2005 +",
                       dept_vars_5))
model_5 <- lm(f5, data = df)
summary(model_5)

# model 6: farc attacks, para/BACRIM attacks, % participation in ref, 
# % support santos, poverty, population, rural index, coca, oil
# find which reference category matches stata's for model 6
for (ref in all_depts) {
  dept_vars_try <- paste(setdiff(all_depts, ref), collapse = " + ")
  f_try <- as.formula(paste("ref_si ~ ln_atfarc_cede + ln_atparabacrim_cede +
                       ref_partic + share_santos_2_100 +
                       incidence_multidim_pov_2005_100 + lpop + indrural2005 +
                       cultivated100 + oil +", dept_vars_try))
  model_try <- lm(f_try, data = df)
  intercept <- coef(model_try)["(Intercept)"]
  if (abs(intercept - 20.01) < 0.01) {
    cat("Model 6 reference category:", ref, "\n")
  }
}

# remove reference department
dept_vars_6 <- paste(setdiff(all_depts, "departamento_3"), collapse = " + ")

# run model 6
f6 <- as.formula(paste("ref_si ~ ln_atfarc_cede + ln_atparabacrim_cede + 
                       ref_partic + share_santos_2_100 +
                       incidence_multidim_pov_2005_100 + lpop + indrural2005 +
                       cultivated100 + oil +", dept_vars_6))
model_6 <- lm(f6, data = df)
summary(model_6)

# model 7: farc attacks, para/BACRIM attacks, % participation in ref, 
# % support santos, poverty, population, rural index, coca, oil, elevation, 
# education coverage
# find which reference category matches stata's for model 7
for (ref in all_depts) {
  dept_vars_try <- paste(setdiff(all_depts, ref), collapse = " + ")
  f_try <- as.formula(paste("ref_si ~ ln_atfarc_cede + ln_atparabacrim_cede +
                       ref_partic + share_santos_2_100 +
                       incidence_multidim_pov_2005_100 + lpop + indrural2005 +
                       cultivated100 + oil + ln_altitud + 
                       coberturabrutaeduc +", dept_vars_try))
  model_try <- lm(f_try, data = df)
  intercept <- coef(model_try)["(Intercept)"]
  if (abs(intercept - 18.78) < 0.01) {
    cat("Model 7 reference category:", ref, "\n")
  }
}

# remove reference department
dept_vars_7 <- paste(setdiff(all_depts, "departamento_24"), collapse = " + ")

# run model 7
f7 <- as.formula(paste("ref_si ~ ln_atfarc_cede + ln_atparabacrim_cede +
                       ref_partic + share_santos_2_100 +
                       incidence_multidim_pov_2005_100 + lpop + indrural2005 +
                       cultivated100 + oil + ln_altitud + 
                       coberturabrutaeduc +", dept_vars_7))
model_7 <- lm(f7, data = df)
summary(model_7)

## FIGURES ##

## Table
# Split the models into two objects, with the first table containing models 1-4
models_a <- list("(1)" = model_1,
                 "(2)" = model_2,
                 "(3)" = model_3,
                 "(4)" = model_4)

# Define the extra rows for department fixed effects
fe_rows_a <- data.frame(
  term = "Department Fixed Effects",
  `(1)` = "$\\checkmark$",
  `(2)` = "$\\checkmark$",
  `(3)` = "$\\checkmark$",
  `(4)` = "$\\checkmark$",
  check.names = FALSE
)
attr(fe_rows_a, "position") <- 9 # place the row in the 9th row position

modelsummary(models_a,
             escape = FALSE,
             output = "table2a.tex",
             longtable = TRUE,
             title = "OLS Identity Perpetrator (Models 1--4)",
             coef_map = c("ln_atfarc_cede" = "(ln) FARC attacks",
                          "ln_atparabacrim_cede" = "(ln) Paramilitary/BACRIM attacks",
                          "ref_partic" = "\\% Participation in Referendum",
                          "share_santos_2_100" = "\\% Support Santos 2014",
                          "incidence_multidim_pov_2005_100" = "Poverty",
                          "lpop" = "(ln) Population",
                          "indrural2005" = "Rural Index",
                          "cultivated100" = "Coca",
                          "oil" = "Oil",
                          "ln_altitud" = "(ln) Elevation",
                          "coberturabrutaeduc" = "Education Coverage",
                          "(Intercept)" = "Constant"),
             add_rows = fe_rows_a,
             gof_omit = "AIC|BIC|Log.Lik.|RMSE",
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01))

# Second table will contain models 5-7
models_b <- list("(5)" = model_5,
                 "(6)" = model_6,
                 "(7)" = model_7)

fe_rows_b <- data.frame(
  term = "Department Fixed Effects",
  `(5)` = "$\\checkmark$",
  `(6)` = "$\\checkmark$",
  `(7)` = "$\\checkmark$"
)
attr(fe_rows_b, "position") <- 23

modelsummary(models_b,
             escape = FALSE,
             output = "table2b.tex",
             longtable = TRUE,
             title = "OLS Identity Perpetrator (Models 5--7)",
             coef_map = c("ln_atfarc_cede" = "(ln) FARC attacks",
                          "ln_atparabacrim_cede" = "(ln) Paramilitary/BACRIM attacks",
                          "ref_partic" = "\\% Participation in Referendum",
                          "share_santos_2_100" = "\\% Support Santos 2014",
                          "incidence_multidim_pov_2005_100" = "Poverty",
                          "lpop" = "(ln) Population",
                          "indrural2005" = "Rural Index",
                          "cultivated100" = "Coca",
                          "oil" = "Oil",
                          "ln_altitud" = "(ln) Elevation",
                          "coberturabrutaeduc" = "Education Coverage",
                          "(Intercept)" = "Constant"),
             add_rows = fe_rows_b,
             gof_omit = "AIC|BIC|Log.Lik.|RMSE",
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01))

## Plot

# The plot in the paper also includes the explanatory variables (ln) exposure
# total and (ln) exposure total 1000 pop, so I need to run the regression with 
# each of those included, and not include FARC and para/BACRIM attacks.

# Model with exposure total mean
f8 <- as.formula(paste("ref_si ~ ln_exposure_total_mean + 
                       ref_partic + share_santos_2_100 +
                       incidence_multidim_pov_2005_100 + lpop + indrural2005 +
                       cultivated100 + oil + ln_altitud + 
                       coberturabrutaeduc +", dept_vars))
model_8 <- lm(f8, data = df_all)
summary(model_8)

# Model with exposure total 1000 pop mean
f9 <- as.formula(paste("ref_si ~ ln_exposure_total_1000pop_mean + 
                       ref_partic + share_santos_2_100 +
                       incidence_multidim_pov_2005_100 + lpop + indrural2005 +
                       cultivated100 + oil + ln_altitud + 
                       coberturabrutaeduc +", dept_vars))
model_9 <- lm(f9, data = df_all)
summary(model_9)

# Make data frame that will be plotted
coef_data <- data.frame(
  variable = c("(ln) Exposure Total",
               "(ln) Exposure Total 1000 Pop.",
               "(ln) FARC Attacks",
               "(ln) Para/BACRIM Attacks"),
  estimate = c(model_8$coefficients["ln_exposure_total_mean"],
               model_9$coefficients["ln_exposure_total_1000pop_mean"],
               model_7$coefficients["ln_atfarc_cede"],
               model_7$coefficients["ln_atparabacrim_cede"]),
  se = c(0.34520, 0.34420, 0.263773, 0.445106)
) |>
  mutate(
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se,
    significant = ifelse(ci_low > 0 | ci_high < 0, "yes", "no"),
    variable = factor(variable, levels = c("(ln) Para/BACRIM Attacks",
                                           "(ln) FARC Attacks",
                                           "(ln) Exposure Total 1000 Pop.",
                                           "(ln) Exposure Total"))
  )

# create plot
pdf("figure_1.pdf")
figure_1 <- ggplot(coef_data, aes(x = estimate, y = variable)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  geom_segment(aes(x = ci_low, xend = ci_high, 
                   y = variable, yend = variable,
                   linetype = significant),
               color = "purple", linewidth = 0.8) +
  geom_point(aes(x = estimate), 
             size = 4, color = "purple") +
  labs(
    x = "DV: Support for the \"Yes\" in the 2016 Referendum",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 11)
  )
figure_1
dev.off()

## MY TWIST ##

# run model with only all departments
dept_vars_formula <- paste(all_depts, collapse = " + ") # paste all depts together
f_depts <- as.formula(paste("ref_si ~ +", dept_vars_formula)) # create formula
model_depts <- lm(f_depts, data = df) # run model
summary(model_depts)

diagnostic_plot_depts <- plot(check_model(model_depts, panel=FALSE)) # diagnostic plots
diagnostic_plot_depts[[5]] # collinearity plot

vif_depts <- vif(model_depts)

## multilevel model

# need to create a new column with the departments together and factor it
df$departamento <- factor(
  names(df[, all_depts])[max.col(df[, all_depts])]
)

# run all models
# model 1: FARC attacks only
multi_model_1 <- lmer(ref_si ~ ln_atfarc_cede + (1 | departamento), data = df)
summary(multi_model_1)

# model 2: Para/BACRIM attacks only
multi_model_2 <- lmer(ref_si ~ ln_atparabacrim_cede + (1 | departamento), 
                      data = df)
summary(multi_model_2)

# model 3: farc and para/BACRIM attacks only
multi_model_3 <- lmer(ref_si ~ ln_atfarc_cede + ln_atparabacrim_cede +
                        (1 | departamento), data = df)
summary(multi_model_3)

# model 4: farc attacks, para/BACRIM attacks, % participation in ref, 
# % support santos
multi_model_4 <- lmer(ref_si ~ ln_atfarc_cede + ln_atparabacrim_cede +
                        ref_partic + share_santos_2_100 + (1 | departamento),
                      data = df)
summary(multi_model_4)

# model 5: farc attacks, para/BACRIM attacks, % participation in ref, 
# % support santos, poverty, population, rural index
multi_model_5 <- lmer(ref_si ~ ln_atfarc_cede + ln_atparabacrim_cede +
                        ref_partic + share_santos_2_100 + 
                        incidence_multidim_pov_2005_100 +
                        lpop + indrural2005 + (1 | departamento),
                      data = df)
summary(multi_model_5)

# model 6: farc attacks, para/BACRIM attacks, % participation in ref, 
# % support santos, poverty, population, rural index, coca, oil
multi_model_6 <- lmer(ref_si ~ ln_atfarc_cede + ln_atparabacrim_cede +
                        ref_partic + share_santos_2_100 + 
                        incidence_multidim_pov_2005_100 +
                        lpop + indrural2005 + cultivated100 + oil + 
                        (1 | departamento),
                      data = df)
summary(multi_model_6)

# model 7: farc attacks, para/BACRIM attacks, % participation in ref, 
# % support santos, poverty, population, rural index, coca, oil, elevation, 
# education coverage
multi_model_7 <- lmer(ref_si ~ ln_atfarc_cede + ln_atparabacrim_cede +
                        ref_partic + share_santos_2_100 + 
                        incidence_multidim_pov_2005_100 +
                        lpop + indrural2005 + cultivated100 + oil + ln_altitud 
                      + coberturabrutaeduc + (1 | departamento),
                      data = df)
summary(multi_model_7)
