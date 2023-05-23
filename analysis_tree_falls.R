# Libraries
library(survival)
library(eha)
library(ggfortify)
library(survminer)
library(rms)
library(smoothHR)
library(stargazer)

# Load data
df_final <- read.csv("treefall_final_table.csv")

################################################################################
############################ NON-PARAMETRICS ###################################
################################################################################

################### (Kaplan-Meier) Survival function ###########################

# with(df_final, plot(Surv(enter, exit, event),
#                     main = "TREE FALL",
#                     xlab = "Age", ylab = "Proportion of individuals not experienced risk"))

# help(autoplot.survfit)
fit <- survfit(Surv(enter, exit, event) ~ 1, data = df_final) # survfit() for survival curves
p <- autoplot(fit, censor.shape = '|', censor.colour = "orange", surv.colour = "orange") +
  theme_classic() +
  ggtitle("TREE FALL") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age in years") +
  ylab("Proportion of individuals not experienced risk")
p
# View(layer_data(p, 1)) # 47.2% of individuals don't experience the risk of tree fall by age 75.08830

######################## SURVIVAL FUNCTION BY GENDER ###########################

# I'm not able to change the legend title from "strata" for some reason
# df_final$male <- ifelse(df_final$male == 1, "Male", "Female")
fit2 <- survfit(Surv(enter, exit, event) ~ male, data = df_final) # survfit() for survival curves
# autoplot(fit2, legTitle = "Gender") +
#   theme_classic() +
#   ggtitle("TREE FALL") +
#   theme(plot.title = element_text(size = 20)) +
#   xlab("Age in years") +
#   ylab("Proportion of individuals not experienced risk")

ggsurvplot(fit2, font.title = c("30"), conf.int = TRUE, legend = "right", surv.scale = "percent", legend.labs = c("Female", "Male"),
           legend.title = "Sex", palette = c("orchid2", "dodgerblue2"),
           title = "TREE FALL", ylim = c(0.4, 1)) +
  xlab("Age in years") +
  ylab("Proportion of individuals not experienced risk")

################ (Nelson-Aalen) Cumulative hazards function ####################

# with(df_final, plot(Surv(enter, exit, event), fun = "cumhaz",
#                     main = "Tree Fall (Non-parametric)",
#                     xlab = "Age", ylab = "Cumulative Hazard"))

autoplot(fit, fun = "cumhaz", censor.shape = '|', censor.colour = "orange", surv.colour = "orange") +
  theme_classic() +
  ggtitle("TREE FALL") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age in years") +
  ylab("Cumulative hazard")

# By gender
ggsurvplot(fit2, font.title = c("30"), conf.int = TRUE, legend = "right", legend.labs = c("Female", "Male"),
           legend.title = "Sex", palette = c("orchid2", "dodgerblue2"),
           title = "TREE FALL", fun = "cumhaz") +
  xlab("Age in years") +
  ylab("Cumulative hazard")

############################ HAZARD FUNCTION ###################################






################################################################################
############################ PARAMETRICS #######################################
################################################################################
fit.w <- phreg(Surv(enter, exit, event) ~ 1, data = df_final)

# Survival function
plot(fit.w, fn = "sur", main = "TREE FALL", xlab = "Age in years", ylab = "NULL")

# Hazard function
plot(fit.w, fn = "haz", main = "TREE FALL", xlab = "Age in years", ylab = "Probability of experiencing risk")

# Cumulative hazards (parametric proportional hazards model with no covariates)
plot(fit.w, fn = "cum", main = "TREE FALL", xlab = "Age in years")

################################################################################
############################## MODELS ##########################################
################################################################################

# A description to get an understanding of what is going on!

# Cox model is h(t) = h0(t) x exp(b1x1 + b2x2 ... + bnxn)
# Recall that h(t) is hazard function, it is the probability of experiencing the risk at that instant
# Quantities exp(b) are called hazard ratios. If b = 0, hazard ratio is 1, called baseline hazard.
# Parameter estimates are logarithms of risk ratios relative to the baseline hazard.
# A value of coefficient greater than zero, or equivalently a hazard ratio greater than
# one, indicates that as the value of the ith covariate increases, the event hazard
# increases and thus the length of survival decreases.
# Put another way, a hazard ratio above 1 indicates a covariate that is positively
# associated with the event probability, and thus negatively associated with the length of survival.

# Regression with sex as predictor
model1 <- coxreg(Surv(exit, event) ~ male, data = df_final)
summary(model1) # Gives coefficient of -0.2 which means that those who experience
# sickness have lower risk of tree fall exposure by a factor of exp(-0.2) = 0.819

stargazer(model1, type = "latex", title = "model1.tex", notes = "Standard errors in parentheses")

# Regression with sex and other risk as predictor
model2a <- coxreg(Surv(exit, event) ~ male + sickness.during.interval, data = df_final)
summary(model2a)
stargazer(model2a, type = "latex", title = "Regression with sex and sickness as predictor", out = "model2a.tex")
