
# Libraries and Data Import -----------------------------------------------
library(tidyverse)
library(survival)
library(eha)
library(stargazer)
library(bshazard)
library(muhaz)
library(biostat3)
library(ggfortify)
library(rms)
library(Greg)
library(survminer)
library(moonBook)

# Import data
df <- read.csv("data_new_format.csv") # format with 13,451 intervals
# df_long <- read.csv("Animal_Attack_merged_final_table.csv") # longer interval format

# Make region as factor
df$region <- as.factor(df$region)
# df_long$region <- as.factor(df_long$region)

# Model 1: Sex ------------------------------------------------------------
model1 <- coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male, data = df)
summary(model1)

# Testing Proportional Hazards
eha::logrank(Surv(enter, exit, Animal_Attack.during.interval), group = male, data = df)

# Export results in table
stargazer(model1, type = "latex", title = "Animal Attack (c) \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Combined Tables/model1.tex",
          dep.var.labels = "Hazard Rate", covariate.labels = "Male",
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

# Model 2: Sex + Risk -----------------------------------------------------

## Model 2a: Sex + Tree Fall ----
model2a <- coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male +
                    tree.fall.during.interval, data = df)
summary(model2a)
stargazer(model2a, type = "latex", title = "Animal Attack (c) \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Combined Tables/model2a.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Tree Fall"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 2b: Sex + Sickness ----
model2b <- coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male +
                    sickness.during.interval, data = df)
summary(model2b)
stargazer(model2b, type = "latex", title = "Animal Attack (c) \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Combined Tables/model2b.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Sickness"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 2c: Sex + Fight ----
model2c <- coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male +
                    fought.during.interval, data = df)
summary(model2c)
stargazer(model2c, type = "latex", title = "Animal Attack (c) \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Combined Tables/model2c.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Fight"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 2d: Sex + Canoe Capsize ----
model2d <- coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male +
                    canoe.capsize.during.interval, data = df)
summary(model2d)
stargazer(model2d, type = "latex", title = "Animal Attack (c) \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Combined Tables/model2d.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Canoe Capsize"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 2e: Sex + Cut Self ----
model2e <- coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male +
                    cut.self.during.interval, data = df)
summary(model2e)
stargazer(model2e, type = "latex", title = "Animal Attack (c) \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Combined Tables/model2e.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Cut Self"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

# Model 3: Sex + Region + Risk --------------------------------------------

## Model 3a: Sex + Region + Tree Fall ----
model3a <- coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male + region +
                    tree.fall.during.interval, data = df)
summary(model3a)
stargazer(model3a, type = "latex", title = "Animal Attack (c) \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Combined Tables/model3a.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Tree Fall"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

# Plot
pdf(file = "Animal Attack Combined Plots/model3a.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male + region +
              strata(tree.fall.during.interval), data = df),
     main = "Animal Attack (c) (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Tree Fall Did Not Occur", "Tree Fall Occurred"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3b: Sex + Region + Sickness ----
model3b <- coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male + region +
                    sickness.during.interval, data = df)
summary(model3b)
stargazer(model3b, type = "latex", title = "Animal Attack (c) \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Combined Tables/model3b.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Sickness"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

# Plot
pdf(file = "Animal Attack Combined Plots/model3b.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male + region +
              strata(sickness.during.interval), data = df),
     main = "Animal Attack (c) (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Sickness Did Not Occur", "Sickness Occurred"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3c: Sex + Region + Fight ----
model3c <- coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male + region +
                    fought.during.interval, data = df)
summary(model3c)

stargazer(model3c, type = "latex", title = "Animal Attack (c) \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Combined Tables/model3c.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Fight"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

# Plot
pdf(file = "Animal Attack Combined Plots/model3c.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male + region +
              strata(fought.during.interval), data = df),
     main = "Animal Attack (c) (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Fight Did Not Occur", "Fight Occurred"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3d: Sex + Region + Canoe Capsize ----
model3d <- coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male + region +
                    canoe.capsize.during.interval, data = df)
summary(model3d)
stargazer(model3d, type = "latex", title = "Animal Attack (c) \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Combined Tables/model3d.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Canoe Capsize"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

# Plot
pdf(file = "Animal Attack Combined Plots/model3d.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male + region +
              strata(canoe.capsize.during.interval), data = df),
     main = "Animal Attack (c) (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Canoe Capsize Did Not Occur", "Canoe Capsize Occurred"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3e: Sex + Region + Cut Self ----
model3e <- coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male + region +
                    cut.self.during.interval, data = df)
summary(model3e)
stargazer(model3e, type = "latex", title = "Animal Attack (c) \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Combined Tables/model3e.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver", "Cut Self"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

# Plot
pdf(file = "Animal Attack Combined Plots/model3e.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male + region +
              strata(cut.self.during.interval), data = df),
     main = "Animal Attack (c) (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Cut Self Did Not Occur", "Cut Self Occurred"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

# Model 4: Sex + Region + Risk + Sex*Risk ---------------------------------

## Model 4a: Sex + Region + Tree Fall + Sex*Tree Fall ----
model4a <- coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male + region +
                    tree.fall.during.interval +
                    male * tree.fall.during.interval, data = df)
summary(model4a)
stargazer(model4a, type = "latex", title = "Animal Attack (c) \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Combined Tables/model4a.tex", dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver", "Tree Fall",
                               "Male \\texttimes\\ Tree Fall"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 4b: Sex + Region + Sickness + Sex*Sickness ----
model4b <- coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male + region +
                    sickness.during.interval + male * sickness.during.interval,
                  data = df)
summary(model4b)
stargazer(model4b, type = "latex", title = "Animal Attack (c) \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Combined Tables/model4b.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Sickness",
                               "Male \\texttimes\\ Sickness"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 4c: Sex + Region + Fight + Sex*Fight ----
model4c <- coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male + region +
                    fought.during.interval +
                    male * fought.during.interval,
                  data = df)
summary(model4c)
stargazer(model4c, type = "latex", title = "Animal Attack (c) \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Combined Tables/model4c.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Fight",
                               "Male \\texttimes\\ Fight"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 4d: Sex + Region + Canoe Capsize + Sex*Canoe Capsize ----
model4d <- coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male + region +
                    canoe.capsize.during.interval +
                    male * canoe.capsize.during.interval,
                  data = df)
summary(model4d)
stargazer(model4d, type = "latex", title = "Animal Attack (c) \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Combined Tables/model4d.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Canoe Capsize",
                               "Male \\texttimes\\ Canoe Capsize"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 4e: Sex + Region + Cut Self + Sex*Cut Self ----
model4e <- coxreg(Surv(enter, exit, Animal_Attack.during.interval) ~ male + region +
                    cut.self.during.interval + male * cut.self.during.interval,
                  data = df)
summary(model4e)
stargazer(model4e, type = "latex", title = "Animal Attack (c) \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Combined Tables/model4e.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Cut Self",
                               "Male \\texttimes\\ Cut Self"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))
