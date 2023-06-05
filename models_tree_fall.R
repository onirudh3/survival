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
library(xtable)

# Load data
df_final <- read.csv("treefall_final_table.csv")

# Make "male" as factor
# df_final$male <- ifelse(df_final$male == 1, "Male", "Female")
# df_final$male <- as.factor(df_final$male)
# class(df_final$male)

# Make region as factor
df_final$region <- as.factor(df_final$region)

# Make dummies for region
df_final$Forest <- ifelse(df_final$region == "Forest", 1, 0)
df_final$Near.San.Borja <- ifelse(df_final$region == "Near San Borja", 1, 0)
df_final$Upriver <- ifelse(df_final$region == "Upriver", 1, 0)

# Creating column for risk years
df_final$risk.years <- df_final$exit - df_final$enter
sum(df_final$risk.years) # 13254.94
# Basically adding the ages of everyone


############################## MODELS ##########################################

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

################################ MODEL 0 #######################################
# No covariate
model0 <- coxreg(Surv(enter, exit, event) ~ 1, data = df_final)
summary(model0)
plot(model0)

# model0 <- coxph(Surv(enter, exit, event) ~ 1, data = df_final)
# basehaz(model0)
# plot(basehaz(model0))

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
          dep.var.labels = "Hazard Rate", covariate.labels = "Male",
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "568"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

################################ MODEL 2 #######################################
# Regressions with sex and other risk as predictor
################################ MODEL 2a ######################################
# Sex + Sickness
model2a <- coxreg(Surv(enter, exit, event) ~ male + sickness.during.interval,
                  data = df_final)
summary(model2a)

# Plot Model 2a
plot(coxreg(Surv(enter, exit, event) ~ male + strata(sickness.during.interval),
            data = df_final), xlab = "Age in years",
     ylab = "Cumulative Hazard",
     main = "Tree Fall")

# Basically same plot as without covariates, only values change

# Export results in table
stargazer(model2a, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model2a.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Sickness"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "568"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

################################ MODEL 2b ######################################
# Sex + Snake Bite
model2b <- coxreg(Surv(enter, exit, event) ~ male + bite.during.interval,
                  data = df_final)
summary(model2b)

# Plot Model 2b
plot(coxreg(Surv(enter, exit, event) ~ male + strata(bite.during.interval),
            data = df_final), xlab = "Age in years",
     ylab = "Cumulative Hazard",
     main = "Tree Fall")

# Export results in table
stargazer(model2b, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model2b.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Snake/Ray Bite"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "568"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

################################ MODEL 2c ######################################
# Sex + Fought
model2c <- coxreg(Surv(enter, exit, event) ~ male + fought.during.interval,
                  data = df_final)
summary(model2c)

# Plot Model 2c
plot(coxreg(Surv(enter, exit, event) ~ male + strata(fought.during.interval),
            data = df_final), xlab = "Age in years",
     ylab = "Cumulative Hazard",
     main = "Tree Fall")

# Export results in table
stargazer(model2c, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model2c.tex",
          dep.var.labels = "Hazard Rate", covariate.labels = c("Male", "Fight"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "568"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))


################################ MODEL 2d ######################################
# Sex + Animal Attack
model2d <- coxreg(Surv(enter, exit, event) ~ male +
                    animal.attack.during.interval,
                  data = df_final)
summary(model2d)

# Plot Model 2d
plot(coxreg(Surv(enter, exit, event) ~ male +
              strata(animal.attack.during.interval),
            data = df_final), xlab = "Age in years",
     ylab = "Cumulative Hazard",
     main = "Tree Fall")

# Export results in table
stargazer(model2d, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model2d.tex",
          dep.var.labels = "Hazard Rate", covariate.labels = c("Male",
                                                               "Animal Attack"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "568"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

################################ MODEL 2e ######################################
# Sex + Canoe Capsize
model2e <- coxreg(Surv(enter, exit, event) ~ male +
                    canoe.capsize.during.interval,
                  data = df_final)
summary(model2e)

# Plot Model 2e
plot(coxreg(Surv(enter, exit, event) ~ male +
              strata(canoe.capsize.during.interval),
            data = df_final), xlab = "Age in years",
     ylab = "Cumulative Hazard",
     main = "Tree Fall")

# Export results in table
stargazer(model2e, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model2e.tex",
          dep.var.labels = "Hazard Rate", covariate.labels = c("Male",
                                                               "Canoe Capsize"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "568"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

################################ MODEL 2f ######################################
# Sex + Canoe Capsize
model2f <- coxreg(Surv(enter, exit, event) ~ male + cut.self.during.interval,
                  data = df_final)
summary(model2f)

# Plot Model 2f
plot(coxreg(Surv(enter, exit, event) ~ male + strata(cut.self.during.interval),
            data = df_final), xlab = "Age in years",
     ylab = "Cumulative Hazard",
     main = "Tree Fall")

# Export results in table
stargazer(model2f, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model2f.tex",
          dep.var.labels = "Hazard Rate", covariate.labels = c("Male",
                                                               "Cut Self"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "568"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))


################################ MODEL 3 #######################################
# Regressions with sex, region and other risk as predictor
################################ MODEL 3a ######################################
# Sex + Region + Sickness
# model3a <- coxreg(Surv(enter, exit, event) ~ male + Forest + Near.San.Borja +
#                     Upriver + sickness.during.interval, data = df_final)
model3a <- coxreg(Surv(enter, exit, event) ~ male + region +
                    sickness.during.interval, data = df_final)
summary(model3a)

# Plot Model 3a
plot(coxreg(Surv(enter, exit, event) ~ male + region +
              strata(sickness.during.interval),
            data = df_final), xlab = "Age in years",
     ylab = "Cumulative Hazard",
     main = "Tree Fall")
# Basically same plot as without covariates, only values change

# Export results in table
stargazer(model3a, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model3a.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver", "Sickness"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "568"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

################################ MODEL 3b ######################################
# Sex + Region + Bite
# model3b <- coxreg(Surv(enter, exit, event) ~ male + Forest + Near.San.Borja +
#                     Upriver + bite.during.interval, data = df_final)
model3b <- coxreg(Surv(enter, exit, event) ~ male + region +
                    bite.during.interval, data = df_final)
summary(model3b)

# Plot Model 3b
plot(coxreg(Surv(enter, exit, event) ~ male + region +
              strata(bite.during.interval),
            data = df_final), xlab = "Age in years",
     ylab = "Cumulative Hazard",
     main = "Tree Fall")
# Basically same plot as without covariates, only values change

# Export results in table
stargazer(model3b, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model3b.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Snake/Ray Bite"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "568"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

################################ MODEL 3c ######################################
# Sex + Region + Fought
# model3c <- coxreg(Surv(enter, exit, event) ~ male + Forest + Near.San.Borja +
#                     Upriver + fought.during.interval, data = df_final)
model3c <- coxreg(Surv(enter, exit, event) ~ male + region +
                    fought.during.interval, data = df_final)
summary(model3c)

# Plot Model 3c
plot(coxreg(Surv(enter, exit, event) ~ male + region +
              strata(fought.during.interval),
            data = df_final), xlab = "Age in years",
     ylab = "Cumulative Hazard",
     main = "Tree Fall")
# Basically same plot as without covariates, only values change

# Export results in table
stargazer(model3c, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model3c.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver", "Fight"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "568"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

################################ MODEL 3d ######################################
# Sex + Region + Animal Attack
# model3d <- coxreg(Surv(enter, exit, event) ~ male + Forest + Near.San.Borja +
#                     Upriver + animal.attack.during.interval, data = df_final)
model3d <- coxreg(Surv(enter, exit, event) ~ male + region +
                    animal.attack.during.interval, data = df_final)
summary(model3d)

# Plot Model 3d
plot(coxreg(Surv(enter, exit, event) ~ male + region +
              strata(animal.attack.during.interval),
            data = df_final), xlab = "Age in years",
     ylab = "Cumulative Hazard",
     main = "Tree Fall")
# Basically same plot as without covariates, only values change

# Export results in table
stargazer(model3d, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model3d.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Animal Attack"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "568"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

################################ MODEL 3e ######################################
# Sex + Region + Canoe Capsize
# model3e <- coxreg(Surv(enter, exit, event) ~ male + Forest + Near.San.Borja +
#                     Upriver + canoe.capsize.during.interval, data = df_final)
model3e <- coxreg(Surv(enter, exit, event) ~ male + region +
                    canoe.capsize.during.interval, data = df_final)
summary(model3e)

# Plot Model 3e
plot(coxreg(Surv(enter, exit, event) ~ male + region +
              strata(canoe.capsize.during.interval),
            data = df_final), xlab = "Age in years",
     ylab = "Cumulative Hazard",
     main = "Tree Fall")
# Basically same plot as without covariates, only values change

# Export results in table
stargazer(model3e, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model3e.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Canoe Capsize"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "568"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

################################ MODEL 3f ######################################
# Sex + Region + Cut Self
# model3f <- coxreg(Surv(enter, exit, event) ~ male + Forest + Near.San.Borja +
#                     Upriver + cut.self.during.interval, data = df_final)
model3f <- coxreg(Surv(enter, exit, event) ~ male + region +
                    cut.self.during.interval, data = df_final)
summary(model3f)

# Plot Model 3f
plot(coxreg(Surv(enter, exit, event) ~ male + region +
              strata(cut.self.during.interval),
            data = df_final), xlab = "Age in years",
     ylab = "Cumulative Hazard",
     main = "Tree Fall")
# Basically same plot as without covariates, only values change

# Export results in table
stargazer(model3f, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model3f.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver", "Cut Self"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "568"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))



################################ MODEL 4 #######################################
# Regressions with sex, region, length of previous tree fall, and other risk as
# predictor
################################ MODEL 4a ######################################
# Create new df with no NAs in length.of.last.fall
new_df <- df_final %>%
  tidyr::drop_na(length.of.last.fall)

# Total risk years in new_df
sum(new_df$risk.years) # 2,630.692

model4a <- coxreg(Surv(enter, exit, event) ~ length.of.last.fall,
                  data = new_df)
summary(model4a)

# Export results in table
stargazer(model4a, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model4a.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Tree Fall Interval"),
          add.lines = list(c("No. of Individuals", "160"),
                           c("No. of Intervals", "180"),
                           c("Total No. of Risk Years", "2,630.692")),
          omit.stat = c("ll", "n"))

################################ MODEL 4b ######################################
# Length of Prior Tree Fall + Sex
new_df <- df_final %>%
  tidyr::drop_na(length.of.last.fall)

model4b <- coxreg(Surv(enter, exit, event) ~ length.of.last.fall + male,
                  data = new_df)
summary(model4b)

# Export results in table
stargazer(model4b, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model4b.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Tree Fall Interval", "Male"),
          add.lines = list(c("No. of Individuals", "160"),
                           c("No. of Intervals", "180"),
                           c("Total No. of Risk Years", "2,630.692")),
          omit.stat = c("ll", "n"))

################################ MODEL 4c ######################################
# Length of Prior Tree Fall + Sex + Region
new_df <- df_final %>%
  tidyr::drop_na(length.of.last.fall)

model4c <- coxreg(Surv(enter, exit, event) ~ length.of.last.fall + male +
                    region, data = new_df)
summary(model4c)

# Export results in table
stargazer(model4c, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model4c.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Tree Fall Interval", "Male",
                               "Near San Borja", "Upriver"),
          add.lines = list(c("No. of Individuals", "160"),
                           c("No. of Intervals", "180"),
                           c("Total No. of Risk Years", "2,630.692")),
          omit.stat = c("ll", "n"))

################################ MODEL 4d ######################################
# Length of Prior Tree Fall + Sickness
new_df <- df_final %>%
  tidyr::drop_na(length.of.last.fall)

model4d <- coxreg(Surv(enter, exit, event) ~ length.of.last.fall +
                    sickness.during.interval, data = new_df)
summary(model4d)

# Export results in table
stargazer(model4d, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model4d.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Tree Fall Interval",
                               "Sickness"),
          add.lines = list(c("No. of Individuals", "160"),
                           c("No. of Intervals", "180"),
                           c("Total No. of Risk Years", "2,630.692")),
          omit.stat = c("ll", "n"))

################################ MODEL 4e ######################################
# Length of Prior Tree Fall + Snake/Ray Bite
new_df <- df_final %>%
  tidyr::drop_na(length.of.last.fall)

model4e <- coxreg(Surv(enter, exit, event) ~ length.of.last.fall +
                    bite.during.interval, data = new_df)
summary(model4e)

# Export results in table
stargazer(model4e, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model4e.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Tree Fall Interval",
                               "Snake/Ray Bite"),
          add.lines = list(c("No. of Individuals", "160"),
                           c("No. of Intervals", "180"),
                           c("Total No. of Risk Years", "2,630.692")),
          omit.stat = c("ll", "n"))

################################ MODEL 4f ######################################
# Length of Prior Tree Fall + Fight
new_df <- df_final %>%
  tidyr::drop_na(length.of.last.fall)

model4f <- coxreg(Surv(enter, exit, event) ~ length.of.last.fall +
                    fought.during.interval, data = new_df)
summary(model4f)

# Export results in table
stargazer(model4f, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model4f.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Tree Fall Interval", "Fight"),
          add.lines = list(c("No. of Individuals", "160"),
                           c("No. of Intervals", "180"),
                           c("Total No. of Risk Years", "2,630.692")),
          omit.stat = c("ll", "n"))

################################ MODEL 4g ######################################
# Length of Prior Tree Fall + Animal Attack
new_df <- df_final %>%
  tidyr::drop_na(length.of.last.fall)

model4g <- coxreg(Surv(enter, exit, event) ~ length.of.last.fall +
                    animal.attack.during.interval, data = new_df)
summary(model4g)

# Export results in table
stargazer(model4g, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model4g.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Tree Fall Interval",
                               "Animal Attack"),
          add.lines = list(c("No. of Individuals", "160"),
                           c("No. of Intervals", "180"),
                           c("Total No. of Risk Years", "2,630.692")),
          omit.stat = c("ll", "n"))

################################ MODEL 4h ######################################
# Length of Prior Tree Fall + Canoe Capsize
new_df <- df_final %>%
  tidyr::drop_na(length.of.last.fall)

model4h <- coxreg(Surv(enter, exit, event) ~ length.of.last.fall +
                    canoe.capsize.during.interval, data = new_df)
summary(model4h)

# Export results in table
stargazer(model4h, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model4h.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Tree Fall Interval",
                               "Canoe Capsize"),
          add.lines = list(c("No. of Individuals", "160"),
                           c("No. of Intervals", "180"),
                           c("Total No. of Risk Years", "2,630.692")),
          omit.stat = c("ll", "n"))

################################ MODEL 4i ######################################
# Length of Prior Tree Fall + Cut Self
new_df <- df_final %>%
  tidyr::drop_na(length.of.last.fall)

model4i <- coxreg(Surv(enter, exit, event) ~ length.of.last.fall +
                    cut.self.during.interval, data = new_df)
summary(model4i)

# Export results in table
stargazer(model4i, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model4i.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Tree Fall Interval",
                               "Cut Self"),
          add.lines = list(c("No. of Individuals", "160"),
                           c("No. of Intervals", "180"),
                           c("Total No. of Risk Years", "2,630.692")),
          omit.stat = c("ll", "n"))
