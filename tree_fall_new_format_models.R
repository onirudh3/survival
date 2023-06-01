####################### LIBRARIES AND IMPORT DATA ##############################
library(survival)
library(eha)
library(tidyverse)
library(ggfortify)
library(stargazer)

# Import data
df <- read.csv("tree_fall_new_format.csv")

# Make region as factor
df$region <- as.factor(df$region)

############################## Model 1: Sex ####################################
model1 <- coxreg(Surv(enter, exit, event) ~ male, data = df)
summary(model1)
# Gives coefficient of -0.148 which means that males have lower risk of tree
# fall exposure by a factor of exp(-0.148) = 0.862

# Testing Proportional Hazards
# Log-Rank Test: It is a powerful test against proportional hazards alternatives
# H0: In terms of survival, there is no difference between the two groups
# H1: There is a survival differential between the two groups
model1_lrtest <- eha::logrank(Surv(enter, exit, event), group = male, data = df)
model1_lrtest
# H0 cannot be rejected, so there is no difference between the two groups.
# survdiff(Surv(exit, event) ~ male, df) # Corroborates this result

# Plots for model 1
# For some reason strata() gives plot for categories but not the previous fit
model1_p <- coxreg(Surv(enter, exit, event) ~ strata(male), data = df)
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
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

############################# Model 2: Sex + Risk ##############################
#* Model 2a: Sex + Sickness ####
model2a <- coxreg(Surv(enter, exit, event) ~ male + sickness.during.interval,
                  data = df)
summary(model2a)

# Export results in table
stargazer(model2a, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model2a.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Sickness"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

#* Model 2b: Sex + Snake Bite ####
model2b <- coxreg(Surv(enter, exit, event) ~ male + bite.during.interval,
                  data = df)
summary(model2b)

# Export results in table
stargazer(model2b, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model2b.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Snake/Ray Bite"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

#* Model 2c: Sex + Fought ####
model2c <- coxreg(Surv(enter, exit, event) ~ male + fought.during.interval,
                  data = df)
summary(model2c)

# Export results in table
stargazer(model2c, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model2c.tex",
          dep.var.labels = "Hazard Rate", covariate.labels = c("Male", "Fight"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

#* Model 2d: Sex + Animal Attack ####
model2d <- coxreg(Surv(enter, exit, event) ~ male +
                    animal.attack.during.interval, data = df)
summary(model2d)

# Export results in table
stargazer(model2d, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model2d.tex",
          dep.var.labels = "Hazard Rate", covariate.labels = c("Male",
                                                               "Animal Attack"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

#* Model 2e: Sex + Canoe Capsize ####
model2e <- coxreg(Surv(enter, exit, event) ~ male +
                    canoe.capsize.during.interval, data = df)
summary(model2e)

# Export results in table
stargazer(model2e, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model2e.tex",
          dep.var.labels = "Hazard Rate", covariate.labels = c("Male",
                                                               "Canoe Capsize"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

#* Model 2f: Sex + Cut Self ####
model2f <- coxreg(Surv(enter, exit, event) ~ male + cut.self.during.interval,
                  data = df)
summary(model2f)

# Export results in table
stargazer(model2f, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model2f.tex",
          dep.var.labels = "Hazard Rate", covariate.labels = c("Male",
                                                               "Cut Self"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

######################### Model 3: Sex + Region + Risk #########################
#* Model 3a: Sex + Region + Sickness ####
model3a <- coxreg(Surv(enter, exit, event) ~ male + region +
                    sickness.during.interval, data = df)
summary(model3a)

# Export results in table
stargazer(model3a, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model3a.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver", "Sickness"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

#* Model 3b: Sex + Region + Snake/Ray Bite ####
model3b <- coxreg(Surv(enter, exit, event) ~ male + region +
                    bite.during.interval, data = df)
summary(model3b)

# Export results in table
stargazer(model3b, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model3b.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Snake/Ray Bite"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

#* Model 3c: Sex + Region + Fought ####
model3c <- coxreg(Surv(enter, exit, event) ~ male + region +
                    fought.during.interval, data = df)
summary(model3c)

# Export results in table
stargazer(model3c, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model3c.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver", "Fight"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

#* Model 3d: Sex + Region + Animal Attack ####
model3d <- coxreg(Surv(enter, exit, event) ~ male + region +
                    animal.attack.during.interval, data = df)
summary(model3d)

# Export results in table
stargazer(model3d, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model3d.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Animal Attack"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

#* Model 3e: Sex + Region + Canoe Capsize ####
model3e <- coxreg(Surv(enter, exit, event) ~ male + region +
                    canoe.capsize.during.interval, data = df)
summary(model3e)

# Export results in table
stargazer(model3e, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model3e.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Canoe Capsize"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

#* Model 3f: Sex + Region + Cut Self ####
model3f <- coxreg(Surv(enter, exit, event) ~ male + region +
                    cut.self.during.interval, data = df)
summary(model3f)

# Export results in table
stargazer(model3f, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model3f.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver", "Cut Self"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))


################## Model 4: Sex + Region + Risk + Sex*Risk #####################
#* Model 4a: Sex + Region + Sickness + Sex*Sickness ####
model4a <- coxreg(Surv(enter, exit, event) ~ male + region +
                    sickness.during.interval + male * sickness.during.interval,
                  data = df)
summary(model4a)

# Export results in table
stargazer(model4a, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model4a.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver", "Sickness",
                               "Male \\texttimes\\ Sickness"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

#* Model 4b: Sex + Region + Sickness + Sex*Snake/Ray Bite ####
model4b <- coxreg(Surv(enter, exit, event) ~ male + region +
                    bite.during.interval + male * bite.during.interval,
                  data = df)
summary(model4b)

# Export results in table
stargazer(model4b, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model4b.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Snake/Ray Bite",
                               "Male \\texttimes\\ Snake/Ray Bite"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

#* Model 4c: Sex + Region + Sickness + Sex*Fought ####
model4c <- coxreg(Surv(enter, exit, event) ~ male + region +
                    fought.during.interval + male * fought.during.interval,
                  data = df)
summary(model4c)

# Export results in table
stargazer(model4c, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model4c.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Fight",
                               "Male \\texttimes\\ Fight"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

#* Model 4d: Sex + Region + Sickness + Sex*animal.attack ####
model4d <- coxreg(Surv(enter, exit, event) ~ male + region +
                    animal.attack.during.interval + male * animal.attack.during.interval,
                  data = df)
summary(model4d)

# Export results in table
stargazer(model4d, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model4d.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Animal Attack",
                               "Male \\texttimes\\ Animal Attack"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

#* Model 4e: Sex + Region + Sickness + Sex*canoe.capsize ####
model4e <- coxreg(Surv(enter, exit, event) ~ male + region +
                    canoe.capsize.during.interval + male * canoe.capsize.during.interval,
                  data = df)
summary(model4e)

# Export results in table
stargazer(model4e, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model4e.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Canoe Capsize",
                               "Male \\texttimes\\ Canoe Capsize"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

#* Model 4f: Sex + Region + Sickness + Sex*cut.self ####
model4f <- coxreg(Surv(enter, exit, event) ~ male + region +
                    cut.self.during.interval + male * cut.self.during.interval,
                  data = df)
summary(model4f)

# Export results in table
stargazer(model4f, type = "latex", title = "Tree Fall \\vspace{-1.4em}",
          notes = "Standard errors in parentheses", out = "model4f.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Cut Self",
                               "Male \\texttimes\\ Cut Self"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))





