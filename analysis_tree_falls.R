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
# Regression with co-occurrence of sickness as predictor
res <- coxreg(Surv(exit, event) ~ df_final$sickness.co_occurrence, data = df_final)
summary(res)
