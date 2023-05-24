# Libraries
library(survival)
library(eha)
library(ggfortify)
library(survminer)
library(rms)
library(smoothHR)
library(stargazer)
library(muhaz)
library(bshazard)

# Load data
df_final <- read.csv("treefall_final_table.csv")

# Make "male" as factor
df_final$male <- ifelse(df_final$male == 1, "Male", "Female")
df_final$male <- as.factor(df_final$male)
class(df_final$male)

############################## MODELS ##########################################
################################################################################

# A description to get an understanding of what is going on!

# Cox model is h(t) = h0(t) x exp(b1x1 + b2x2 ... + bnxn)
# Recall that h(t) is hazard function, it is the probability of experiencing the
# risk at that instant. Quantities exp(b) are called hazard ratios. If b = 0,
# hazard ratio is 1, called baseline hazard. Parameter estimates are logarithms
# of risk ratios relative to the baseline hazard. A value of coefficient greater
# than zero, or equivalently a hazard ratio greater than one, indicates that as
# the value of the ith covariate increases, the event hazard increases and thus
# the length of survival decreases.
# Put another way, a hazard ratio above 1 indicates a covariate that is positiv-
# -ely associated with the event probability, and thus negatively associa-
# -ted with the length of survival.

# Dependent variable is the incidence rate of a specific event and the independ-
# ent variables are risk factors or predictors that the investigators use to ex-
# plain or predict the study endpoint.

################################ MODEL 1 #######################################
# Sex as predictor
model1 <- coxreg(Surv(enter, exit, event) ~ male, data = df_final)
summary(model1) # Gives coefficient of -0.159 which means that males
# have lower risk of tree fall exposure by a factor of exp(-0.159) = 0.853

# Testing Proportional Hazards
# Log-Rank Test: It is a powerful test against proportional hazards alternatives
# H0: In terms of survivability, there is no difference between the two groups
# H1: There is a survival differential between the two groups
model1_lrtest <- eha::logrank(Surv(enter, exit, event), group = male,
                              data = df_final)
model1_lrtest # H0 cannot be rejected, so there is no difference between the two
# survdiff(Surv(exit, event) ~ male, df_final) # Corroborates the above result

# Plots for Model 1
# For some reason strata() gives plot for categories but not the previous fit
model1_p <- coxreg(Surv(enter, exit, event) ~ strata(male), data = df_final)
# Survival function
plot(model1_p, fn = "sur", xlab = "Age in years",
     ylab = "Proportion of individuals not experienced risk",
     main = "Tree Fall")
# Cumulative hazard function
plot(model1_p, fn = "cum", xlab = "Age in years",
     ylab = "Cumulative hazard", main = "Tree Fall")

# Export results in table
stargazer(model1, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model1.tex",
          dep.var.labels = "Hazard Rate", covariate.labels = "Male")






# Regression with sex and other risk as predictor
model2a <- coxreg(Surv(exit, event) ~ male + sickness.during.interval,
                  data = df_final)
summary(model2a)
stargazer(model2a, type = "latex",
          title = "Regression with sex and sickness as predictor",
          out = "model2a.tex")
