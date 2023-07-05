
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
library(coxme)
library(xtable)

# Import data
df <- read.csv("data_new_format.csv") # format with 13,451 intervals
df_long <- read.csv("animal_attack_final_table.csv") # longer interval format
df_first <- read.csv("animal_attack_time_to_first_risk_short_interval.csv") # time to first risk short interval format


# Make region as factor
df$region <- as.factor(df$region)
df_long$region <- as.factor(df_long$region)
df_first$region <- as.factor(df_first$region)


# Model 1: Sex ------------------------------------------------------------
model1 <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male, data = df)
summary(model1)

# Testing Proportional Hazards
eha::logrank(Surv(enter, exit, animal.attack.during.interval), group = male, data = df)

# Testing Proportional Hazards for region
eha::logrank(Surv(enter, exit, animal.attack.during.interval), group = region, data = df)

# Export results in table
stargazer(model1, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model1.tex",
          dep.var.labels = "Hazard Rate", covariate.labels = "Male",
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

# Model 2: Sex + Risk -----------------------------------------------------

## Model 2a: Sex + Tree Fall ----
model2a <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male +
                    tree.fall.during.interval, data = df)
summary(model2a)
stargazer(model2a, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model2a.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Tree Fall"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 2b: Sex + Sickness ----
model2b <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male +
                    sickness.during.interval, data = df)
summary(model2b)
stargazer(model2b, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model2b.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Sickness"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 2c: Sex + Snake/Ray Bite ----
model2c <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male +
                    bite.during.interval, data = df)
summary(model2c)
stargazer(model2c, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model2c.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Snake/Ray Bite"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 2d: Sex + Fight ----
model2d <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male +
                    fought.during.interval, data = df)
summary(model2d)
stargazer(model2d, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model2d.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Fight"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 2e: Sex + Canoe Capsize ----
model2e <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male +
                    canoe.capsize.during.interval, data = df)
summary(model2e)
stargazer(model2e, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model2e.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Canoe Capsize"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 2f: Sex + Cut Self ----
model2f <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male +
                    cut.self.during.interval, data = df)
summary(model2f)
stargazer(model2f, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model2f.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Cut Self"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))


# Model 3: Sex + Region + Risk --------------------------------------------

## Model 3a: Sex + Region + Tree Fall ----
model3a <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
                    tree.fall.during.interval, data = df)
summary(model3a)
stargazer(model3a, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model3a.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Tree Fall"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

# Plot
pdf(file = "Animal Attack Plots/model3a.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
              strata(tree.fall.during.interval), data = df),
     main = "Animal Attack (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Tree Fall Did Not Occur", "Tree Fall Occurred"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3b: Sex + Region + Sickness ----
model3b <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
                    sickness.during.interval, data = df)
summary(model3b)
stargazer(model3b, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model3b.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Sickness"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

# Plot
pdf(file = "Animal Attack Plots/model3b.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
              strata(sickness.during.interval), data = df),
     main = "Animal Attack (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Sickness Did Not Occur", "Sickness Occurred"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3c: Sex + Region + Snake/Ray Bite ----
model3c <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
                    bite.during.interval, data = df)
summary(model3c)
stargazer(model3c, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model3c.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver", "Snake/Ray Bite"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

# Plot: Error in x[[i]][1, 1] : subscript out of bounds
# pdf(file = "Animal Attack Plots/model3c.pdf", height = 5, width = 5)
# plot(coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
#               strata(bite.during.interval), data = df),
#      main = "Animal Attack (Controlling for Sex and Region)",
#      xlab = "Age in Years",
#      ylab = "Cumulative Hazard",
#      printLegend = F,
#      col = 1:2)
# legend("topleft", legend = c("Snake/Ray Bite Did Not Occur", "Snake/Ray Bite Occurred"),
#        col = 1:2, lty = 1, bty = "n")
# dev.off()

## Model 3d: Sex + Region + Fight ----
model3d <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
                    fought.during.interval, data = df)
summary(model3d)

stargazer(model3d, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model3d.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Fight"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

# Plot
pdf(file = "Animal Attack Plots/model3d.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
              strata(fought.during.interval), data = df),
     main = "Animal Attack (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Fight Did Not Occur", "Fight Occurred"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3e: Sex + Region + Canoe Capsize ----
model3e <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
                    canoe.capsize.during.interval, data = df)
summary(model3e)
stargazer(model3e, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model3e.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Canoe Capsize"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

# Plot
pdf(file = "Animal Attack Plots/model3e.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
              strata(canoe.capsize.during.interval), data = df),
     main = "Animal Attack (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Canoe Capsize Did Not Occur", "Canoe Capsize Occurred"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3f: Sex + Region + Cut Self ----
model3f <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
                    cut.self.during.interval, data = df)
summary(model3f)
stargazer(model3f, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model3f.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver", "Cut Self"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

# Plot
pdf(file = "Animal Attack Plots/model3f.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
              strata(cut.self.during.interval), data = df),
     main = "Animal Attack (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Cut Self Did Not Occur", "Cut Self Occurred"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

# Model 4: Sex + Region + Risk + Sex*Risk ---------------------------------

## Model 4a: Sex + Region + Tree Fall + Sex*Tree Fall ----
model4a <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
                    tree.fall.during.interval +
                    male * tree.fall.during.interval, data = df)
summary(model4a)
stargazer(model4a, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model4a.tex", dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver", "Tree Fall",
                               "Male \\texttimes\\ Tree Fall"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 4b: Sex + Region + Sickness + Sex*Sickness ----
model4b <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
                    sickness.during.interval + male * sickness.during.interval,
                  data = df)
summary(model4b)
stargazer(model4b, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model4b.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Sickness",
                               "Male \\texttimes\\ Sickness"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 4c: Sex + Region + Snake/Ray Bite + Sex*Snake/Ray Bite ----
model4c <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
                    bite.during.interval + male * bite.during.interval,
                  data = df)
summary(model4c)
stargazer(model4c, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model4c.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Snake/Ray Bite",
                               "Male \\texttimes\\ Snake/Ray Bite"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 4d: Sex + Region + Fight + Sex*Fight ----
model4d <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
                    fought.during.interval +
                    male * fought.during.interval,
                  data = df)
summary(model4d)
stargazer(model4d, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model4d.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Fight",
                               "Male \\texttimes\\ Fight"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 4e: Sex + Region + Canoe Capsize + Sex*Canoe Capsize ----
model4e <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
                    canoe.capsize.during.interval +
                    male * canoe.capsize.during.interval,
                  data = df)
summary(model4e)
stargazer(model4e, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model4e.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Canoe Capsize",
                               "Male \\texttimes\\ Canoe Capsize"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 4f: Sex + Region + Cut Self + Sex*Cut Self ----
model4f <- coxreg(Surv(enter, exit, animal.attack.during.interval) ~ male + region +
                    cut.self.during.interval + male * cut.self.during.interval,
                  data = df)
summary(model4f)
stargazer(model4f, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model4f.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Cut Self",
                               "Male \\texttimes\\ Cut Self"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,451"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))



# Model 5: Sex + Region + Length of Last Animal Attack + Risk ------------------

# Create new df with no NAs in length.of.last.animal.attack
new_df <- df_long %>%
  tidyr::drop_na(length.of.last.animal.attack)

# Total risk years in new_df
x <- new_df[!duplicated(new_df$pid), ]
sum(x$age)

# Number of Individuals
plyr::count(new_df$pid)

## Model 5a: Length of Last Animal Attack ----
model5a <- coxreg(Surv(enter, exit, event) ~ length.of.last.animal.attack,
                  data = new_df)
summary(model5a)
stargazer(model5a, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model5a.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Animal Attack Interval"),
          add.lines = list(c("No. of Individuals", "25"),
                           c("No. of Intervals", "30"),
                           c("Total No. of Risk Years", "907.95")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 5b: Length of Prior Animal Attack + Sex ----
model5b <- coxreg(Surv(enter, exit, event) ~ length.of.last.animal.attack + male,
                  data = new_df)
summary(model5b)
stargazer(model5b, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model5b.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Animal Attack Interval", "Male"),
          add.lines = list(c("No. of Individuals", "25"),
                           c("No. of Intervals", "30"),
                           c("Total No. of Risk Years", "907.95")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 5c: Length of Prior Animal Attack + Sex + Region ----
model5c <- coxreg(Surv(enter, exit, event) ~ length.of.last.animal.attack + male +
                    region, data = new_df)
summary(model5c)
stargazer(model5c, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model5c.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Animal Attack Interval", "Male",
                               "Near San Borja", "Upriver"),
          add.lines = list(c("No. of Individuals", "25"),
                           c("No. of Intervals", "30"),
                           c("Total No. of Risk Years", "907.95")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 5d: Length of Prior Animal Attack + Tree Fall ----
model5d <- coxreg(Surv(enter, exit, event) ~ length.of.last.animal.attack +
                    tree.fall.during.interval, data = new_df)
summary(model5d)
stargazer(model5d, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model5d.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Animal Attack Interval",
                               "Tree Fall"),
          add.lines = list(c("No. of Individuals", "25"),
                           c("No. of Intervals", "30"),
                           c("Total No. of Risk Years", "907.95")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 5e: Length of Prior Animal Attack + Sickness ----
model5e <- coxreg(Surv(enter, exit, event) ~ length.of.last.animal.attack +
                    sickness.during.interval, data = new_df)
summary(model5e)
stargazer(model5e, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model5e.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Animal Attack Interval",
                               "Sickness"),
          add.lines = list(c("No. of Individuals", "25"),
                           c("No. of Intervals", "30"),
                           c("Total No. of Risk Years", "907.95")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 5f: Length of Prior Animal Attack + Snake/Ray Bite ----
model5f <- coxreg(Surv(enter, exit, event) ~ length.of.last.animal.attack +
                    bite.during.interval, data = new_df)
summary(model5f)
stargazer(model5f, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model5f.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Animal Attack Interval", "Snake/Ray Bite"),
          add.lines = list(c("No. of Individuals", "25"),
                           c("No. of Intervals", "30"),
                           c("Total No. of Risk Years", "907.95")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 5g: Length of Prior Animal Attack + Fight ----
model5g <- coxreg(Surv(enter, exit, event) ~ length.of.last.animal.attack +
                    fought.during.interval, data = new_df)
summary(model5g)
stargazer(model5g, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model5g.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Animal Attack Interval",
                               "Fight"),
          add.lines = list(c("No. of Individuals", "25"),
                           c("No. of Intervals", "30"),
                           c("Total No. of Risk Years", "907.95")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 5h: Length of Prior Animal Attack + Canoe Capsize ----
model5h <- coxreg(Surv(enter, exit, event) ~ length.of.last.animal.attack +
                    canoe.capsize.during.interval, data = new_df)
summary(model5h)
stargazer(model5h, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model5h.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Animal Attack Interval",
                               "Canoe Capsize"),
          add.lines = list(c("No. of Individuals", "25"),
                           c("No. of Intervals", "30"),
                           c("Total No. of Risk Years", "907.95")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))

## Model 5i: Length of Prior Animal Attack + Cut Self ----
model5i <- coxreg(Surv(enter, exit, event) ~ length.of.last.animal.attack +
                    cut.self.during.interval, data = new_df)
summary(model5i)
stargazer(model5i, type = "latex", report = "vcsp", single.row = T, title = "Animal Attack \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Animal Attack Tables/model5i.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Animal Attack Interval",
                               "Cut Self"),
          add.lines = list(c("No. of Individuals", "25"),
                           c("No. of Intervals", "30"),
                           c("Total No. of Risk Years", "907.95")),
          omit.stat = c("ll", "n", "rsq", "max.rsq", "wald", "lr", "logrank"))


# Model 6: Mixed effects --------------------------------------------------

## Null Model
model6 <- coxph(Surv(exit, animal.attack.during.interval) ~ 1, data = df_first)
# summary(model6)

## Model 6a: Null with random intercept for pid ----
model6a <- coxme(Surv(exit, animal.attack.during.interval) ~ 1 + (1 | pid),
                 data = df_first)
# summary(model6a)

## Model 6b: Null with random intercepts for pid and house.id ----
model6b <- coxme(Surv(exit, animal.attack.during.interval) ~ 1 + (1 | pid) +
                   (1 | house.id), data = df_first)
# summary(model6b)

## Model 6c: Null with random intercepts for pid, house.id and region ----
model6c <- coxme(Surv(exit, animal.attack.during.interval) ~ 1 + (1 | pid) +
                   (1 | house.id) + (1 | region), data = df_first)
# summary(model6c)

## Model 6d: Null with region FE and pid, house.id RE ----
model6d <- coxme(Surv(exit, animal.attack.during.interval) ~ 1 + (1 | pid) +
                   (1 | house.id) + region, data = df_first)
# summary(model6d)
# Note that animal attack violates proportional hazards for region, test is
# not significant

## Model 6e: Nested RE ----
model6e <- coxme(Surv(exit, animal.attack.during.interval) ~ 1 + (1 | pid) + (1 | region/house.id),
                 data = df_first)
# summary(model6e)

## Comparing model fit ----
aov_test <- anova(model6, model6a, model6b, model6c, model6d, model6e)
aov_test

aic_test <- AIC(model6, model6a, model6b, model6c, model6d, model6e)
aic_test

## Tabulating ----
df_coxme <- cbind(aov_test, aic_test)
df_coxme <- subset(df_coxme, select = -c(Df))
df_coxme <- data.frame(t(df_coxme))
colnames(df_coxme) <- c("1", "2", "3", "4", "5", "6")

addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 5
addtorow$pos[[3]] <- 5
addtorow$pos[[4]] <- 5
addtorow$pos[[5]] <- 5
addtorow$pos[[6]] <- 5
addtorow$pos[[7]] <- 5
addtorow$command <- c('\\hline',
                      '\\hline',
                      'PID RE & No & Yes & Yes & Yes & Yes & Yes \\\\\n',
                      'House ID RE & No & No & Yes & Yes & Yes & Nested \\\\\n',
                      'Region RE & No & No & No & Yes & No & Yes \\\\\n',
                      'Region FE & No & No & No & No & Yes & No \\\\\n',
                      '\\hline')
print(xtable(df_coxme, caption = "Animal Attack Mixed Effects Specifications"), add.to.row = addtorow,
      caption.placement = "top", file = "Animal Attack Tables/model6.tex")
rm(aic_test, aov_test)
# rm(addtorow)


# Descriptive Plots -------------------------------------------------------
# Make age.cat as factor
df$age.cat <- factor(df$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))

df_long$male <- ifelse(df_long$male == 1, "Male", "Female")
df$male <- ifelse(df$male == 1, "Male", "Female")

## Percentage of intervals where Animal attack occurred by age category ----
# Bar plot
pdf(file = "Animal Attack Plots/descriptive_plot1.pdf", height = 5,
    width = 7)
df %>%
  count(age.cat, animal.attack.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(animal.attack.during.interval == 1) %>%
  ggplot() +
  geom_col(aes(x = age.cat, y = prop), fill = "pink") +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), nudge_y = 0.0001, size = 2.5) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals (13,451 Intervals)")
dev.off()

# Line plot
pdf(file = "Animal Attack Plots/descriptive_plot2.pdf", height = 5,
    width = 7)
df %>%
  count(age.cat, animal.attack.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(animal.attack.during.interval == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "pink",
            size = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals (13,451 Intervals)")
dev.off()

# By gender
df_male <- subset(df, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, animal.attack.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(animal.attack.during.interval == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df, male == "Female")
df_female_plot <- df_female %>%
  count(age.cat, animal.attack.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(animal.attack.during.interval == 1)
df_female_plot$male <- "Female"
df_female_plot <- df_female_plot[c("age.cat", "male", "prop")]

df_gender_plot <- rbind(df_male_plot, df_female_plot)

df_gender_plot$prop <- round(df_gender_plot$prop, digits = 4)

df_gender_plot$age.cat <- factor(df_gender_plot$age.cat,
                                 levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))

df_gender_plot <- complete(df_gender_plot, age.cat, male)

pdf(file = "Animal Attack Plots/descriptive_plot3.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Animal Attack") +
  labs(subtitle = "388 Individuals, 13,451 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.8))
dev.off()

## Age Interval Plots ----

### Tree Fall ----
# Distribution of co-occurring Tree Fall within Fight age intervals
pdf(file = "Animal Attack Plots/co_occurrence1a.pdf", height = 5, width = 7)
df_long %>%
  count(tree.fall.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(tree.fall.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = tree.fall.co_occurrence.interval, y = prop),
           fill = "#7c9b99", width = 0.9, show.legend = F) +
  geom_text(aes(x = tree.fall.co_occurrence.interval, y = prop,
                label = scales::percent(prop)), vjust = 0, nudge_y = .001,
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("ANIMAL ATTACK and TREE FALL") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (543 Intervals)")
dev.off()

# By gender
pdf(file = "Animal Attack Plots/co_occurrence1b.pdf", height = 5, width = 10)
df_long %>%
  count(tree.fall.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(tree.fall.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = tree.fall.co_occurrence.interval, y = prop, fill = male),
           width = 0.9, show.legend = F) +
  geom_text(aes(x = tree.fall.co_occurrence.interval, y = prop,
                label = scales::percent(prop)), vjust = 0, nudge_y = .001,
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("ANIMAL ATTACK and TREE FALL") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (543 Intervals)") +
  facet_wrap(~male) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.background = element_blank())
dev.off()

### Sickness ----
# Distribution of co-occurring Sickness within Fight age intervals
pdf(file = "Animal Attack Plots/co_occurrence2a.pdf", height = 5, width = 7)
df_long %>%
  count(sickness.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(sickness.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = sickness.co_occurrence.interval, y = prop), fill = "#7c9b99", width = 0.9, show.legend = F) +
  geom_text(aes(x = sickness.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("ANIMAL ATTACK and SICKNESS") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (543 Intervals)")
dev.off()

# By gender
pdf(file = "Animal Attack Plots/co_occurrence2b.pdf", height = 5, width = 10)
df_long %>%
  count(sickness.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(sickness.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = sickness.co_occurrence.interval, y = prop, fill = male), width = 0.9, show.legend = F) +
  geom_text(aes(x = sickness.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("ANIMAL ATTACK and SICKNESS") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (543 Intervals)") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  theme(strip.background = element_blank())
dev.off()

### Snake/Ray Bite ----
# Distribution of co-occurring Snake/Ray Bite within Fight age intervals
pdf(file = "Animal Attack Plots/co_occurrence3a.pdf", height = 5, width = 8)
df_long %>%
  count(bite.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(bite.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = bite.co_occurrence.interval, y = prop), fill = "#7c9b99", width = 0.9, show.legend = F) +
  geom_text(aes(x = bite.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("ANIMAL ATTACK and
  SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (543 Intervals)")
dev.off()

# By gender
pdf(file = "Animal Attack Plots/co_occurrence3b.pdf", height = 5, width = 10)
df_long %>%
  count(bite.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(bite.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = bite.co_occurrence.interval, y = prop, fill = male), width = 0.9, show.legend = F) +
  geom_text(aes(x = bite.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("ANIMAL ATTACK and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (543 Intervals)") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  theme(strip.background = element_blank())
dev.off()

### Fight ----
# Distribution of co-occurring animal attack within Fight age intervals
pdf(file = "Animal Attack Plots/co_occurrence4a.pdf", height = 5, width = 7.5)
df_long %>%
  count(fought.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(fought.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = fought.co_occurrence.interval, y = prop), fill = "#7c9b99", width = 0.9, show.legend = F) +
  geom_text(aes(x = fought.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent, limits = c(0,NA)) +
  theme_classic(base_size = 15) +
  ggtitle("ANIMAL ATTACK and FIGHT") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (543 Intervals)")
dev.off()

# By gender
pdf(file = "Animal Attack Plots/co_occurrence4b.pdf", height = 5, width = 10)
df_long %>%
  count(fought.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(fought.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = fought.co_occurrence.interval, y = prop, fill = male), width = 0.9, show.legend = F) +
  geom_text(aes(x = fought.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent, limits = c(0,NA)) +
  theme_classic(base_size = 15) +
  ggtitle("ANIMAL ATTACK and FIGHT") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (543 Intervals)") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  theme(strip.background = element_blank())
dev.off()

### Canoe Capsize ----
# Distribution of co-occurring canoe capsize within Fight age intervals
pdf(file = "Animal Attack Plots/co_occurrence5a.pdf", height = 5, width = 7.5)
df_long %>%
  count(canoe.capsize.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(canoe.capsize.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = canoe.capsize.co_occurrence.interval, y = prop), fill = "#7c9b99", width = 0.9, show.legend = F) +
  geom_text(aes(x = canoe.capsize.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("ANIMAL ATTACK and
  CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (543 Intervals)")
dev.off()

# By gender
pdf(file = "Animal Attack Plots/co_occurrence5b.pdf", height = 5, width = 10)
df_long %>%
  count(canoe.capsize.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(canoe.capsize.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = canoe.capsize.co_occurrence.interval, y = prop, fill = male), width = 0.9, show.legend = F) +
  geom_text(aes(x = canoe.capsize.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .0001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("ANIMAL ATTACK and CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (543 Intervals)") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  theme(strip.background = element_blank())
dev.off()

### Cut Self ----
# Distribution of co-occurring cut self within Fight age intervals
pdf(file = "Animal Attack Plots/co_occurrence6a.pdf", height = 5, width = 7)
df_long %>%
  count(cut.self.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(cut.self.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = cut.self.co_occurrence.interval, y = prop), fill = "#7c9b99", width = 0.9, show.legend = F) +
  geom_text(aes(x = cut.self.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("ANIMAL ATTACK and
  CUT SELF") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (543 Intervals)")
dev.off()

# By gender
pdf(file = "Animal Attack Plots/co_occurrence6b.pdf", height = 5, width = 10)
df_long %>%
  count(cut.self.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(cut.self.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = cut.self.co_occurrence.interval, y = prop, fill = male), width = 0.9, show.legend = F) +
  geom_text(aes(x = cut.self.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .0005, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("ANIMAL ATTACK and CUT SELF") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (543 Intervals)") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  theme(strip.background = element_blank())
dev.off()


## Percentage co-occurrence of risks ----

# Changing the 0's and 1's of the variables for the plots to be more understandable
df_long$event <- ifelse(df_long$event == 1, "Animal Attack
Occurred", "Animal Attack Did
Not Occur")
df_long$tree.fall.during.interval <- ifelse(df_long$tree.fall.during.interval == 1, "Tree Fall
Occured", "Tree Fall Did
Not Occur")
df_long$sickness.during.interval <- ifelse(df_long$sickness.during.interval == 1, "Sickness
Occured", "Sickness Did
Not Occur")
df_long$bite.during.interval <- ifelse(df_long$bite.during.interval == 1, "Snake/Ray Bite
Occured", "Snake/Ray Bite
Did Not Occur")
df_long$fought.during.interval <- ifelse(df_long$fought.during.interval == 1, "Fight
Occured", "Fight Did
Not Occur")
df_long$canoe.capsize.during.interval <- ifelse(df_long$canoe.capsize.during.interval == 1, "Canoe Capsize
Occured", "Canoe Capsize Did
Not Occur")
df_long$cut.self.during.interval <- ifelse(df_long$cut.self.during.interval == 1, "Cut Self
Occured", "Cut Self Did
Not Occur")


### Tree Fall ----
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which tree fall occurs
pdf(file = "Animal Attack Plots/co_occurrence1c.pdf", height = 4, width = 6.5)
df_long %>%
  count(tree.fall.during.interval, event) %>%
  group_by(tree.fall.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(tree.fall.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("ANIMAL ATTACK and TREE FALL") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Animal Attack Plots/co_occurrence1d.pdf", height = 4, width = 7)
df_long %>%
  count(tree.fall.during.interval, event, male) %>%
  group_by(tree.fall.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(tree.fall.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("ANIMAL ATTACK and TREE FALL") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Animal Attack Plots/co_occurrence1e.pdf", height = 4.5, width = 7)
df_long %>%
  count(tree.fall.during.interval, event, region) %>%
  group_by(tree.fall.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(tree.fall.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 13) +
  ggtitle("ANIMAL ATTACK and TREE FALL") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  labs(fill = "") +
  facet_wrap(~region) + theme(strip.text.x = element_text(size = 15)) +
  theme(strip.background = element_blank()) +
  guides(x =  guide_axis(angle = 90))
dev.off()

### Sickness ----
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which sickness occurs
pdf(file = "Animal Attack Plots/co_occurrence2c.pdf", height = 4, width = 6.5)
df_long %>%
  count(sickness.during.interval, event) %>%
  group_by(sickness.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(sickness.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("ANIMAL ATTACK and SICKNESS") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Animal Attack Plots/co_occurrence2d.pdf", height = 4, width = 7)
df_long %>%
  count(sickness.during.interval, event, male) %>%
  group_by(sickness.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(sickness.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("ANIMAL ATTACK and SICKNESS") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Animal Attack Plots/co_occurrence2e.pdf", height = 4, width = 7.5)
df_long %>%
  count(sickness.during.interval, event, region) %>%
  group_by(sickness.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(sickness.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("ANIMAL ATTACK and SICKNESS") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  labs(fill = "") +
  facet_wrap(~region) + theme(strip.text.x = element_text(size = 15)) +
  theme(strip.background = element_blank()) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  guides(x =  guide_axis(angle = 90))
dev.off()

### Snake/Ray Bite ----
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which Snake/Ray Bite
pdf(file = "Animal Attack Plots/co_occurrence3c.pdf", height = 4, width = 7)
df_long %>%
  count(bite.during.interval, event) %>%
  group_by(bite.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(bite.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("ANIMAL ATTACK and
  SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Animal Attack Plots/co_occurrence3d.pdf", height = 4, width = 7)
df_long %>%
  count(bite.during.interval, event, male) %>%
  group_by(bite.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(bite.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("ANIMAL ATTACK and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Animal Attack Plots/co_occurrence3e.pdf", height = 4, width = 7)
df_long %>%
  count(bite.during.interval, event, region) %>%
  group_by(bite.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(bite.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("ANIMAL ATTACK and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  labs(fill = "") +
  facet_wrap(~region) + theme(strip.text.x = element_text(size = 15)) +
  theme(strip.background = element_blank()) +
  guides(x =  guide_axis(angle = 90))
dev.off()

### Fight ----
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which fight occurred
pdf(file = "Animal Attack Plots/co_occurrence4c.pdf", height = 4, width = 6)
df_long %>%
  count(fought.during.interval, event) %>%
  group_by(fought.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(fought.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("ANIMAL ATTACK and FIGHT") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Animal Attack Plots/co_occurrence4d.pdf", height = 4, width = 7.5)
df_long %>%
  count(fought.during.interval, event, male) %>%
  group_by(fought.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(fought.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("ANIMAL ATTACK and FIGHT") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Animal Attack Plots/co_occurrence4e.pdf", height = 4.5, width = 7)
df_long %>%
  count(fought.during.interval, event, region) %>%
  group_by(fought.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(fought.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("ANIMAL ATTACK and FIGHT") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  labs(fill = "") +
  facet_wrap(~region) + theme(strip.text.x = element_text(size = 15)) +
  theme(strip.background = element_blank()) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  guides(x =  guide_axis(angle = 90))
dev.off()

### Canoe Capsize ----
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which canoe capsize
pdf(file = "Animal Attack Plots/co_occurrence5c.pdf", height = 4, width = 6)
df_long %>%
  count(canoe.capsize.during.interval, event) %>%
  group_by(canoe.capsize.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(canoe.capsize.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("ANIMAL ATTACK and 
  CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Animal Attack Plots/co_occurrence5d.pdf", height = 4, width = 8)
df_long %>%
  count(canoe.capsize.during.interval, event, male) %>%
  group_by(canoe.capsize.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(canoe.capsize.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("ANIMAL ATTACK and CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Animal Attack Plots/co_occurrence5e.pdf", height = 4.5, width = 7.5)
df_long %>%
  count(canoe.capsize.during.interval, event, region) %>%
  group_by(canoe.capsize.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(canoe.capsize.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("ANIMAL ATTACK and CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  labs(fill = "") +
  facet_wrap(~region) + theme(strip.text.x = element_text(size = 15)) +
  theme(strip.background = element_blank()) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  guides(x =  guide_axis(angle = 90))
dev.off()

### Cut Self ----
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which cut self
pdf(file = "Animal Attack Plots/co_occurrence6c.pdf", height = 4, width = 6)
df_long %>%
  count(cut.self.during.interval, event) %>%
  group_by(cut.self.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(cut.self.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("ANIMAL ATTACK and
  CUT SELF") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Animal Attack Plots/co_occurrence6d.pdf", height = 4, width = 7)
df_long %>%
  count(cut.self.during.interval, event, male) %>%
  group_by(cut.self.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(cut.self.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("ANIMAL ATTACK and CUT SELF") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Animal Attack Plots/co_occurrence6e.pdf", height = 4.5, width = 7)
df_long %>%
  count(cut.self.during.interval, event, region) %>%
  group_by(cut.self.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(cut.self.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("ANIMAL ATTACK and CUT SELF") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (543 Intervals)") +
  labs(fill = "") +
  facet_wrap(~region) + theme(strip.text.x = element_text(size = 15)) + theme(strip.text.x = element_text(size = 15)) +
  theme(strip.background = element_blank()) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  guides(x =  guide_axis(angle = 90))
dev.off()


## Survival Function ----
fit <- survfit(Surv(enter, exit, animal.attack.during.interval) ~ 1, data = df)
pdf(file = "Animal Attack Plots/survival_function.pdf", height = 5, width = 6)
autoplot(fit, censor.shape = '|', censor.colour = "pink",
         surv.colour = "pink") +
  theme_classic(base_size = 12) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age in years") +
  ylab("Proportion of individuals not experienced risk") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  labs(subtitle = "388 Individuals, 13,451 Intervals")
dev.off()

# By gender
fit2 <- survfit(Surv(enter, exit, animal.attack.during.interval) ~ male, data = df)
pdf(file = "Animal Attack Plots/survival_function_by_gender.pdf", height = 5)
ggsurvplot(fit2, font.title = c("30"), conf.int = TRUE, legend = "right",
           surv.scale = "percent", legend.labs = c("Female", "Male"),
           legend.title = "Sex", palette = c("orchid2", "dodgerblue2"),
           title = "Animal Attack", axes.offset = F, break.x.by = 5,
           subtitle = "388 Individuals, 13,451 Intervals") +
  xlab("Age in years") +
  ylab("Proportion of individuals not experienced risk") # Cannot change legend size for some reason
dev.off()

## Hazard Function ----
### bshazard ----
fit <- bshazard(Surv(enter, exit, animal.attack.during.interval) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
pdf(file = "Animal Attack Plots/hazard_function1.pdf", height = 5)
ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "pink4") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "pink") +
  theme_classic(base_size = 12) +
  xlab("Age in Years") +
  ylab("Probability of experiencing risk") +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(subtitle = "388 Individuals, 13,451 Intervals")
dev.off()

### muhaz ----
pdf(file = "Animal Attack Plots/hazard_function2.pdf", height = 5)
mfit <- muhaz(df$exit, df$animal.attack.during.interval)
plot(mfit, main = "Animal Attack")
dev.off()

pdf(file = "Animal Attack Plots/hazard_function3.pdf", height = 5)
kfit <- kphaz.fit(df$exit, df$animal.attack.during.interval)
kphaz.plot(kfit, main = "Animal Attack")
dev.off()

# By gender
fit <- bshazard(Surv(enter, exit, animal.attack.during.interval) ~ 1, data = subset(df, male == "Male"))
summary(fit)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"

fit2 <- bshazard(Surv(enter, exit, animal.attack.during.interval) ~ 1, data = subset(df, male == "Female"))
summary(fit2)
df_surv2 <- data.frame(time = fit2$time, hazard = fit2$hazard,
                       lower.ci = fit2$lower.ci, upper.ci = fit2$upper.ci)
df_surv2$Sex <- "Female"

df_surv3 <- bind_rows(df_surv, df_surv2)

pdf(file = "Animal Attack Plots/hazard_function4.pdf", height = 5)
ggplot(df_surv3, aes(x = time, y = hazard, fill = Sex)) +
  geom_line(aes(color = Sex)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2) +
  theme_classic(base_size = 12) +
  xlab("Age in Years") +
  ylab("Probability of experiencing risk") +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(subtitle = "388 Individuals, 13,451 Intervals")
dev.off()

# fit <- stpm2(Surv(enter, exit, animal.attack.during.interval) ~ male, data = df)
# plot(fit, newdata = data.frame(male = 1), type = "hr", var = "male")

### Male hazard over female hazard ratio ----
df_surv_male <- df_surv[c("time", "hazard")]
df_surv_male <- df_surv_male %>%  dplyr::rename("hazard_m" = "hazard")
df_surv_female <- df_surv2[c("time", "hazard")]
df_surv_female <- df_surv_female %>%  dplyr::rename("hazard_f" = "hazard")
df_surv4 <- left_join(df_surv_female, df_surv_male)
df_surv4$hazard_m_by_f <- df_surv4$hazard_m / df_surv4$hazard_f
df_surv4 <- subset(df_surv4, !is.na(df_surv4$hazard_m_by_f))
pdf(file = "Animal Attack Plots/hazard_function5.pdf", height = 5)
ggplot(df_surv4) +
  geom_line(aes(x = time, y = hazard_m_by_f), color = "green4") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack") +
  xlab("Age in Years") +
  ylab("Ratio of Instantaneous Risk (Male/Female)") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(subtitle = "388 Individuals, 13,451 Intervals") +
  geom_segment(aes(x = 0, y = 1, xend = 75, yend = 1), lty = 2, col = "lavender")
dev.off()

## Cumulative hazard function ----
fit <- survfit(Surv(enter, exit, animal.attack.during.interval) ~ 1, data = df)
pdf(file = "Animal Attack Plots/cumhaz_plot1.pdf", height = 5)
autoplot(fit, censor.shape = '|', censor.colour = "pink",
         surv.colour = "pink", fun = "cumhaz") +
  theme_classic(base_size = 12) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age in years") +
  ylab("Cumulative hazard") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(subtitle = "388 Individuals, 13,451 Intervals")
dev.off()

# By gender
fit2 <- survfit(Surv(enter, exit, animal.attack.during.interval) ~ male, data = df)
pdf(file = "Animal Attack Plots/cumhaz_plot2.pdf", height = 5)
ggsurvplot(fit2, font.title = c("30"), conf.int = TRUE, legend = "right",
           legend.labs = c("Female", "Male"),
           legend.title = "Sex", palette = c("orchid2", "dodgerblue2"),
           title = "Animal Attack", fun = "cumhaz", axes.offset = F, break.x.by = 5,
           subtitle = "388 Individuals, 13,451 Intervals") +
  xlab("Age in years") +
  ylab("Cumulative hazard")
dev.off()
