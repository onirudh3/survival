# Libraries
library(survival)
library(eha)

# Load data
df_final <- read.csv("treefall_final_table.csv")

############################ NON-PARAMETRICS ###################################
# (Kaplan-Meier) Survival function
with(df_final, plot(Surv(enter, exit, event),
                main = "Tree Fall (Non-parametric)",
                xlab = "Age", ylab = "Survival"))

?Surv()

# (Nelson-Aalen) Cumulative hazards function
with(df_final, plot(Surv(enter, exit, event), fun = "cumhaz",
                main = "Tree Fall (Non-parametric)",
                xlab = "Age", ylab = "Cumulative Hazard"))

################################################################################
############################ PARAMETRICS #######################################
fit.w <- phreg(Surv(enter, exit, event) ~ 1, data = df_final)

# Survival function
plot(fit.w, fn = "sur", main = "Tree Fall (Parametric)")

# Cumulative hazards (parametric proportional hazards model with no covariates)
plot(fit.w, fn = "cum", main = "Tree Fall (Parametric)")


########################## MODELS ##############################################
# Cox model is h(t) = h0(t) x exp(b1x1 + b2x2 ... + bnxn)
# Recall that h(t) is hazard function, it is the probability of experiencing the risk at that instant
# Quantities exp(b) are called hazard ratios. If b = 0, hazard ratio is 1, called baseline hazard.
# Parameter estimates are logarithms of risk ratios relative to the baseline hazard.
# A value of coefficient greater than zero, or equivalently a hazard ratio greater than
# one, indicates that as the value of the ith covariate increases, the event hazard
# increases and thus the length of survival decreases.
# Put another way, a hazard ratio above 1 indicates a covariate that is positively
# associated with the event probability, and thus negatively associated with the length of survival.


# Regression with co-occurrence of sickness as predictor
res <- coxreg(Surv(exit, event) ~ sickness.during.interval, data = df_final)
summary(res) # Gives coefficient of -1.468 which means that those who experience
# sickness have lower risk of tree fall exposure by a factor of exp(-1.468) = 0.2303
