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
         "see", "car", "corrr", "brms", "lme4", "texreg"),  pkgTest)

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
f8 <- as.formula(paste(c("ref_si ~ ln_exposure_total_mean + 
                       ref_partic + share_santos_2_100 +
                       incidence_multidim_pov_2005_100 + lpop + indrural2005 +
                       cultivated100 + oil + ln_altitud + 
                       coberturabrutaeduc +", all_depts), collapse = " + "))
model_8 <- lm(f8, data = df_all)
summary(model_8)

# Model with exposure total 1000 pop mean
f9 <- as.formula(paste(c("ref_si ~ ln_exposure_total_1000pop_mean + 
                       ref_partic + share_santos_2_100 +
                       incidence_multidim_pov_2005_100 + lpop + indrural2005 +
                       cultivated100 + oil + ln_altitud + 
                       coberturabrutaeduc +", all_depts), collapse = " + "))
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
  se = c(0.34520, 0.34420, 0.263773, 0.445106) # taken from model summaries
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
                   y = variable, yend = variable),
               color = "purple", linewidth = 0.8) +
  geom_point(size = 3, color = "purple") +
  labs(
    x = "DV: Support for the \"Yes\" in the 2016 Referendum",
    y = NULL
  ) +
  coord_cartesian(xlim = c(min(coef_data$ci_low) - 0.2, 
                           max(coef_data$ci_high) + 0.2)) +
  scale_y_discrete(expand = expansion(mult = 0.15)) +
  theme_minimal() +
  theme(
    legend.position   = "none",           # remove the legend
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 11),
    plot.margin        = margin(10, 15, 10, 10)
  )
figure_1
dev.off()

## MY TWIST ##

## multilevel model

# need to create a new column with the departments together and factor it
df$departamento <- factor(
  names(df[, all_depts])[max.col(df[, all_depts])])

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

# determine and show shifted intercepts 
data.frame(coef(multi_model_7)$departamento)[1]

# table
texreg(list(multi_model_1, multi_model_2, multi_model_3, multi_model_4,
            multi_model_5, multi_model_6, multi_model_7), 
       file = "multi_model_table.tex",
       booktabs = TRUE,
       table = FALSE,
       include.variance = TRUE)

## Predicted probabilities

# histogram
hist(df$exp_atfarc_cede, breaks = 100,
     xlab = "FARC Attacks", main = "Frequency of FARC Attacks",
     col = "purple")

mean(df$exp_atfarc_cede, na.rm = TRUE)

hist(exp(df$ln_atparabacrim_cede), breaks = 100,
     xlab = "Para/BACRIM Attacks", main = "Frequency of Para/BACRIM Attacks",
     col = "blue")

mean(exp(df$ln_atparabacrim_cede), na.rm = TRUE)

# find predicted probabilities of various levels of farc attacks

# repeat the first 100 rows of the original dataframe
prediction_data_farc <- df[rep(1, 100), ]

# fill all department columns with 0
dept_cols <- grep("^departamento_", names(prediction_data_farc), value = TRUE)
prediction_data_farc[, dept_cols] <- 0

# fill every other variable, make all values equal to the mean
prediction_data_farc$ln_atparabacrim_cede = mean(df$ln_atparabacrim_cede, na.rm = TRUE)
prediction_data_farc$ref_partic = mean(df$ref_partic, na.rm = TRUE)
prediction_data_farc$share_santos_2_100 = mean(df$share_santos_2_100, na.rm = TRUE)
prediction_data_farc$incidence_multidim_pov_2005_100 = mean(df$incidence_multidim_pov_2005_100, na.rm = TRUE)
prediction_data_farc$lpop = mean(df$lpop, na.rm = TRUE)
prediction_data_farc$indrural2005 = mean(df$indrural2005, na.rm = TRUE)
prediction_data_farc$cultivated100 = mean(df$cultivated100, na.rm = TRUE)
prediction_data_farc$oil = mean(df$oil, na.rm = TRUE)
prediction_data_farc$ln_altitud = mean(df$ln_altitud, na.rm = TRUE)
prediction_data_farc$coberturabrutaeduc = mean(df$coberturabrutaeduc, na.rm = TRUE)

# have FARC attacks variable vary amongst ranges between the min and max values
prediction_data_farc$ln_atfarc_cede <- as.numeric(seq(min(df$ln_atfarc_cede, na.rm = TRUE), 
                                       max(df$ln_atfarc_cede, na.rm = TRUE),
                                       length.out = 100))

# predict new data using model_7 on the prediction data created
preds_farc <- predict(model_7, newdata = prediction_data_farc, type = "response",
                 interval = "confidence")

# unlist predicted column
prediction_data_farc$predicted <- preds_farc[, "fit"]

# confidence intervals around predicted probabilities
prediction_data_farc$lower_95 <- preds_farc[, "lwr"]
prediction_data_farc$upper_95 <- preds_farc[, "upr"]

# plot predicted % Yes votes in the referendum against farc attacks
pdf("pred_plot.pdf")
pred_plot <- ggplot(prediction_data_farc, aes(x = exp(ln_atfarc_cede), y = predicted)) +
  geom_ribbon(aes(ymin=lower_95, ymax=upper_95), alpha = 0.2) +
  geom_point() +
  geom_rug(data = df, aes(x=exp_atfarc_cede),
           inherit.aes = FALSE, alpha = 0.4, sides = "b") +
  labs(y = "Predicted % Yes Votes in Referendum",
       x = "FARC Attacks",
       title = "FARC Attacks vs Predicted Votes in Referendum") +
  theme_minimal()
pred_plot
dev.off()

# find predicted probabilities for varying levels of para/bacrim attacks
# repeat the first 100 rows of the original dataframe
prediction_data_para <- df[rep(1, 100), ]

# fill all department columns with 0
dept_cols <- grep("^departamento_", names(prediction_data_para), value = TRUE)
prediction_data_para[, dept_cols] <- 0

# fill every other variable, make all values equal to the mean
prediction_data_para$ln_atfarc_cede = mean(df$ln_atfarc_cede, na.rm = TRUE)
prediction_data_para$ref_partic = mean(df$ref_partic, na.rm = TRUE)
prediction_data_para$share_santos_2_100 = mean(df$share_santos_2_100, na.rm = TRUE)
prediction_data_para$incidence_multidim_pov_2005_100 = mean(df$incidence_multidim_pov_2005_100, na.rm = TRUE)
prediction_data_para$lpop = mean(df$lpop, na.rm = TRUE)
prediction_data_para$indrural2005 = mean(df$indrural2005, na.rm = TRUE)
prediction_data_para$cultivated100 = mean(df$cultivated100, na.rm = TRUE)
prediction_data_para$oil = mean(df$oil, na.rm = TRUE)
prediction_data_para$ln_altitud = mean(df$ln_altitud, na.rm = TRUE)
prediction_data_para$coberturabrutaeduc = mean(df$coberturabrutaeduc, na.rm = TRUE)

# have FARC attacks variable vary amongst ranges between the min and max values
prediction_data_para$ln_atparabacrim_cede <- as.numeric(seq(min(df$ln_atparabacrim_cede, na.rm = TRUE), 
                                                      max(df$ln_atparabacrim_cede, na.rm = TRUE),
                                                      length.out = 100))

# predict new data using model_7 on the prediction data created
preds_para <- predict(model_7, newdata = prediction_data_para, type = "response",
                      interval = "confidence")

# unlist predicted column
prediction_data_para$predicted <- preds_para[, "fit"]

# confidence intervals around predicted probabilities
prediction_data_para$lower_95 <- preds_para[, "lwr"]
prediction_data_para$upper_95 <- preds_para[, "upr"]

# plot predicted % Yes votes in the referendum against farc attacks
pdf("pred_plot_para.pdf")
pred_plot_para <- ggplot(prediction_data_para, aes(x = exp(ln_atparabacrim_cede), y = predicted)) +
  geom_ribbon(aes(ymin=lower_95, ymax=upper_95), alpha = 0.2) +
  geom_point() +
  geom_rug(data = df, aes(x=exp(ln_atparabacrim_cede)),
           inherit.aes = FALSE, alpha = 0.4, sides = "b") +
  labs(y = "Predicted % Yes Votes in Referendum",
       x = "Para/BACRIM",
       title = "Para/BACRIM Attacks vs Predicted Votes in Referendum") +
  theme_minimal()
pred_plot_para
dev.off()
