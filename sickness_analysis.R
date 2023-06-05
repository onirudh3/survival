
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

# Import data, format with 13,541 intervals
df <- read.csv("data_new_format.csv")
df_long <- read.csv("sickness_final_table.csv")

# Make region as factor
df$region <- as.factor(df$region)
df_long$region <- as.factor(df_long$region)

# Model 1: Sex ------------------------------------------------------------
model1 <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male, data = df)
summary(model1)

# Testing Proportional Hazards
eha::logrank(Surv(enter, exit, sickness.during.interval), group = male,
             data = df)

# Export results in table
stargazer(model1, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model1.tex",
          dep.var.labels = "Hazard Rate", covariate.labels = "Male",
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

# Model 2: Sex + Risk -----------------------------------------------------

## Model 2a: Sex + Tree Fall ----
model2a <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male +
                    tree.fall.during.interval,
                  data = df)
summary(model2a)
stargazer(model2a, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model2a.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Tree Fall"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 2b: Sex + Snake/Ray Bite ----
model2b <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male +
                    bite.during.interval, data = df)
summary(model2b)
stargazer(model2b, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model2b.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Snake/Ray Bite"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 2c: Sex + Fight ----
model2c <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male +
                    fought.during.interval, data = df)
summary(model2c)
stargazer(model2c, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model2c.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Fight"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 2d: Sex + Animal Attack ----
model2d <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male +
                    animal.attack.during.interval, data = df)
summary(model2d)
stargazer(model2d, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model2d.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Animal Attack"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 2e: Sex + Canoe Capsize ----
model2e <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male +
                    canoe.capsize.during.interval, data = df)
summary(model2e)
stargazer(model2e, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model2e.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Canoe Capsize"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 2f: Sex + Cut Self ----
model2f <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male +
                    cut.self.during.interval, data = df)
summary(model2f)
stargazer(model2f, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model2f.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Cut Self"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))


# Model 3: Sex + Region + Risk --------------------------------------------

## Model 3a: Sex + Region + Tree Fall ----
model3a <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    tree.fall.during.interval, data = df)
summary(model3a)
stargazer(model3a, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model3a.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Tree Fall"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

# Plot
pdf(file = "Sickness Plots/model3a.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
              strata(tree.fall.during.interval), data = df),
     main = "SICKNESS (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Tree Fall Occurred", "Tree Fall Did Not Occur"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3b: Sex + Region + Snake/Ray Bite ----
model3b <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    bite.during.interval, data = df)
summary(model3b)
stargazer(model3b, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model3b.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Snake/Ray Bite"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

# Plot
pdf(file = "Sickness Plots/model3b.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
              strata(bite.during.interval), data = df),
     main = "SICKNESS (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Snake/Ray Bite Occurred", "Snake/Ray Bite Did Not Occur"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3c: Sex + Region + Fight ----
model3c <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    fought.during.interval, data = df)
summary(model3c)
stargazer(model3c, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model3c.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver", "Fight"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

# Plot
pdf(file = "Sickness Plots/model3c.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
              strata(fought.during.interval), data = df),
     main = "SICKNESS (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Fight Occurred", "Fight Did Not Occur"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3d: Sex + Region + Animal Attack ----
model3d <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    animal.attack.during.interval, data = df)
summary(model3d)
stargazer(model3d, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model3d.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Animal Attack"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

# Plot
pdf(file = "Sickness Plots/model3d.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
              strata(animal.attack.during.interval), data = df),
     main = "SICKNESS (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Animal Attack Occurred", "Animal Attack Did Not Occur"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3e: Sex + Region + Canoe Capsize ----
model3e <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    canoe.capsize.during.interval, data = df)
summary(model3e)
stargazer(model3e, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model3e.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Canoe Capsize"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

# Plot
pdf(file = "Sickness Plots/model3e.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
              strata(canoe.capsize.during.interval), data = df),
     main = "SICKNESS (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Canoe Capsize Occurred", "Canoe Capsize Did Not Occur"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3f: Sex + Region + Cut Self ----
model3f <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    cut.self.during.interval, data = df)
summary(model3f)
stargazer(model3f, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model3f.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver", "Cut Self"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

# Plot
pdf(file = "Sickness Plots/model3f.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
              strata(cut.self.during.interval), data = df),
     main = "SICKNESS (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Cut Self Occurred", "Cut Self Did Not Occur"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

# Model 4: Sex + Region + Risk + Sex*Risk ---------------------------------

## Model 4a: Sex + Region + Tree Fall + Sex*Tree Fall ----
model4a <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    tree.fall.during.interval +
                    male * tree.fall.during.interval, data = df)
summary(model4a)
stargazer(model4a, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model4a.tex", dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver", "Tree Fall",
                               "Male \\texttimes\\ Tree Fall"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 4b: Sex + Region + Snake/Ray Bite + Sex*Snake/Ray Bite ----
model4b <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    bite.during.interval + male * bite.during.interval,
                  data = df)
summary(model4b)
stargazer(model4b, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model4b.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Snake/Ray Bite",
                               "Male \\texttimes\\ Snake/Ray Bite"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 4c: Sex + Region + Fight + Sex*Fight ----
model4c <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    fought.during.interval + male * fought.during.interval,
                  data = df)
summary(model4c)
stargazer(model4c, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model4c.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Fight",
                               "Male \\texttimes\\ Fight"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 4d: Sex + Region + Animal Attack + Sex*Animal Attack ----
model4d <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    animal.attack.during.interval +
                    male * animal.attack.during.interval,
                  data = df)
summary(model4d)
stargazer(model4d, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model4d.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Animal Attack",
                               "Male \\texttimes\\ Animal Attack"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 4e: Sex + Region + Canoe Capsize + Sex*Canoe Capsize ----
model4e <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    canoe.capsize.during.interval +
                    male * canoe.capsize.during.interval,
                  data = df)
summary(model4e)
stargazer(model4e, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model4e.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Canoe Capsize",
                               "Male \\texttimes\\ Canoe Capsize"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))

## Model 4f: Sex + Region + Cut Self + Sex*Cut Self ----
model4f <- coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    cut.self.during.interval + male * cut.self.during.interval,
                  data = df)
summary(model4f)
stargazer(model4f, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model4f.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Male", "Near San Borja", "Upriver",
                               "Cut Self",
                               "Male \\texttimes\\ Cut Self"),
          add.lines = list(c("No. of Individuals", "388"),
                           c("No. of Intervals", "13,541"),
                           c("Total No. of Risk Years", "13,254.94")),
          omit.stat = c("ll", "n"))



# Model 5: Sex + Region + Length of Last Sickness + Risk ------------------

# Create new df with no NAs in length.of.last.sickness
new_df <- df_long %>%
  tidyr::drop_na(length.of.last.sickness)

# Total risk years in new_df
# x <- new_df[!duplicated(new_df$pid), ]
# sum(x$age)

## Model 5a: Length of Last Sickness ----
model5a <- coxreg(Surv(enter, exit, event) ~ length.of.last.sickness,
                  data = new_df)
summary(model5a)
stargazer(model5a, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model5a.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Sickness Interval"),
          add.lines = list(c("No. of Individuals", "325"),
                           c("No. of Intervals", "475"),
                           c("Total No. of Risk Years", "11,197.42")),
          omit.stat = c("ll", "n"))

## Model 5b: Length of Prior Sickness + Sex ----
model5b <- coxreg(Surv(enter, exit, event) ~ length.of.last.sickness + male,
                  data = new_df)
summary(model5b)
stargazer(model5b, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model5b.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Sickness Interval", "Male"),
          add.lines = list(c("No. of Individuals", "325"),
                           c("No. of Intervals", "475"),
                           c("Total No. of Risk Years", "11,197.42")),
          omit.stat = c("ll", "n"))

## Model 5c: Length of Prior Sickness + Sex + Region ----
model5c <- coxreg(Surv(enter, exit, event) ~ length.of.last.sickness + male +
                    region, data = new_df)
summary(model5c)
stargazer(model5c, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model5c.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Sickness Interval", "Male",
                               "Near San Borja", "Upriver"),
          add.lines = list(c("No. of Individuals", "325"),
                           c("No. of Intervals", "475"),
                           c("Total No. of Risk Years", "11,197.42")),
          omit.stat = c("ll", "n"))

## Model 5d: Length of Prior Sickness + Tree Fall ----
model5d <- coxreg(Surv(enter, exit, event) ~ length.of.last.sickness +
                    tree.fall.during.interval, data = new_df)
summary(model5d)
stargazer(model5d, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model5d.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Sickness Interval",
                               "Tree Fall"),
          add.lines = list(c("No. of Individuals", "325"),
                           c("No. of Intervals", "475"),
                           c("Total No. of Risk Years", "11,197.42")),
          omit.stat = c("ll", "n"))

## Model 5e: Length of Prior Sickness + Snake/Ray Bite ----
model5e <- coxreg(Surv(enter, exit, event) ~ length.of.last.sickness +
                    bite.during.interval, data = new_df)
summary(model5e)
stargazer(model5e, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model5e.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Sickness Interval",
                               "Snake/Ray Bite"),
          add.lines = list(c("No. of Individuals", "325"),
                           c("No. of Intervals", "475"),
                           c("Total No. of Risk Years", "11,197.42")),
          omit.stat = c("ll", "n"))

## Model 5f: Length of Prior Sickness + Fight ----
model5f <- coxreg(Surv(enter, exit, event) ~ length.of.last.sickness +
                    fought.during.interval, data = new_df)
summary(model5f)
stargazer(model5f, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model5f.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Sickness Interval", "Fight"),
          add.lines = list(c("No. of Individuals", "325"),
                           c("No. of Intervals", "475"),
                           c("Total No. of Risk Years", "11,197.42")),
          omit.stat = c("ll", "n"))

## Model 5g: Length of Prior Sickness + Animal Attack ----
model5g <- coxreg(Surv(enter, exit, event) ~ length.of.last.sickness +
                    animal.attack.during.interval, data = new_df)
summary(model5g)
stargazer(model5g, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model5g.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Sickness Interval",
                               "Animal Attack"),
          add.lines = list(c("No. of Individuals", "325"),
                           c("No. of Intervals", "475"),
                           c("Total No. of Risk Years", "11,197.42")),
          omit.stat = c("ll", "n"))

## Model 5h: Length of Prior Sickness + Canoe Capsize ----
model5h <- coxreg(Surv(enter, exit, event) ~ length.of.last.sickness +
                    canoe.capsize.during.interval, data = new_df)
summary(model5h)
stargazer(model5h, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model5h.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Sickness Interval",
                               "Canoe Capsize"),
          add.lines = list(c("No. of Individuals", "325"),
                           c("No. of Intervals", "475"),
                           c("Total No. of Risk Years", "11,197.42")),
          omit.stat = c("ll", "n"))

## Model 5i: Length of Prior Sickness + Cut Self ----
model5i <- coxreg(Surv(enter, exit, event) ~ length.of.last.sickness +
                    cut.self.during.interval, data = new_df)
summary(model5i)
stargazer(model5i, type = "latex", title = "Sickness \\vspace{-1.4em}",
          notes = "Standard errors in parentheses",
          out = "Sickness Tables/model5i.tex",
          dep.var.labels = "Hazard Rate",
          covariate.labels = c("Length of Prior Sickness Interval",
                               "Cut Self"),
          add.lines = list(c("No. of Individuals", "325"),
                           c("No. of Intervals", "475"),
                           c("Total No. of Risk Years", "11,197.42")),
          omit.stat = c("ll", "n"))



# Descriptive Plots -------------------------------------------------------
# Make age.cat as factor
df$age.cat <- factor(df$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))

## Percentage of intervals where sickness occurred by age category ----
# Bar plot
pdf(file = "Sickness Plots/sickness_descriptive_plot1.pdf", height = 5,
    width = 7)
df %>%
  count(age.cat, sickness.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(sickness.during.interval == 1) %>%
  ggplot() +
  geom_col(aes(x = age.cat, y = prop), fill = "darkgoldenrod3") +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), nudge_y = 0.0001, size = 2.5) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("SICKNESS") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals (13,541 Intervals)")
dev.off()

# Line plot
pdf(file = "Sickness Plots/sickness_descriptive_plot2.pdf", height = 5,
    width = 7)
df %>%
  count(age.cat, sickness.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(sickness.during.interval == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "darkgoldenrod3",
            size = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("SICKNESS") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals (13,541 Intervals)")
dev.off()


## Age Interval Plots ----

df_long$male <- ifelse(df_long$male == 1, "Male", "Female")

### Tree Fall ----
# Distribution of co-occurring sickness within SICKNESS age intervals
pdf(file = "Sickness Plots/co_occurrence1a.pdf", height = 5, width = 7)
df_long %>%
  count(tree.fall.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(tree.fall.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = tree.fall.co_occurrence.interval, y = prop),
           fill = "lightblue", width = 0.9) +
  geom_text(aes(x = tree.fall.co_occurrence.interval, y = prop,
                label = scales::percent(prop)), vjust = 0, nudge_y = .001,
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and TREE FALL") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (843 Intervals)")
dev.off()

# By gender
pdf(file = "Sickness Plots/co_occurrence1b.pdf", height = 5, width = 10)
df_long %>%
  count(tree.fall.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(tree.fall.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = tree.fall.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.9) +
  geom_text(aes(x = tree.fall.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and TREE FALL") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (843 Intervals)") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())
dev.off()

### Snake/Ray Bite ----
# Distribution of co-occurring snake/ray bite within SICKNESS age intervals
pdf(file = "Sickness Plots/co_occurrence2a.pdf", height = 5, width = 7)
df_long %>%
  count(bite.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(bite.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = bite.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.9) +
  geom_text(aes(x = bite.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (843 Intervals)")
dev.off()

# By gender
pdf(file = "Sickness Plots/co_occurrence2b.pdf", height = 5, width = 10)
df_long %>%
  count(bite.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(bite.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = bite.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.9) +
  geom_text(aes(x = bite.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (843 Intervals)") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())
dev.off()

### Fight ----
# Distribution of co-occurring fought within SICKNESS age intervals
pdf(file = "Sickness Plots/co_occurrence3a.pdf", height = 5, width = 7)
df_long %>%
  count(fought.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(fought.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = fought.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.9) +
  geom_text(aes(x = fought.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and FOUGHT") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (843 Intervals)")
dev.off()

# By gender
pdf(file = "Sickness Plots/co_occurrence3b.pdf", height = 5, width = 10)
df_long %>%
  count(fought.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(fought.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = fought.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.9) +
  geom_text(aes(x = fought.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and FOUGHT") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (843 Intervals)") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())
dev.off()

### Animal Attack ----
# Distribution of co-occurring animal attack within SICKNESS age intervals
pdf(file = "Sickness Plots/co_occurrence4a.pdf", height = 5, width = 7)
df_long %>%
  count(animal.attack.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(animal.attack.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = animal.attack.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.9) +
  geom_text(aes(x = animal.attack.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .0001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and ANIMAL ATTACK") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (843 Intervals)")
dev.off()

# By gender
pdf(file = "Sickness Plots/co_occurrence4b.pdf", height = 5, width = 10)
df_long %>%
  count(animal.attack.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(animal.attack.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = animal.attack.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.9) +
  geom_text(aes(x = animal.attack.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .0001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and ANIMAL ATTACK") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (843 Intervals)") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())
dev.off()

### Canoe Capsize ----
# Distribution of co-occurring canoe capsize within SICKNESS age intervals
pdf(file = "Sickness Plots/co_occurrence5a.pdf", height = 5, width = 7)
df_long %>%
  count(canoe.capsize.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(canoe.capsize.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = canoe.capsize.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.9) +
  geom_text(aes(x = canoe.capsize.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (843 Intervals)")
dev.off()

# By gender
pdf(file = "Sickness Plots/co_occurrence5b.pdf", height = 5, width = 10)
df_long %>%
  count(canoe.capsize.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(canoe.capsize.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = canoe.capsize.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.9) +
  geom_text(aes(x = canoe.capsize.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .0001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (843 Intervals)") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())
dev.off()

### Cut Self ----
# Distribution of co-occurring cut self within SICKNESS age intervals
pdf(file = "Sickness Plots/co_occurrence6a.pdf", height = 5, width = 7)
df_long %>%
  count(cut.self.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(cut.self.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = cut.self.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.9) +
  geom_text(aes(x = cut.self.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and CUT SELF") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (843 Intervals)")
dev.off()

# By gender
pdf(file = "Sickness Plots/co_occurrence6b.pdf", height = 5, width = 10)
df_long %>%
  count(cut.self.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(cut.self.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = cut.self.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.9) +
  geom_text(aes(x = cut.self.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .0005, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and CUT SELF") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (843 Intervals)") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())
dev.off()


## Percentage co-occurrence of risks ----

# Changing the 0's and 1's of the variables for the plots to be more understandable
df_long$event <- ifelse(df_long$event == 1, "Sickness Occurred", "Sickness Did Not Occur")
df_long$tree.fall.during.interval <- ifelse(df_long$tree.fall.during.interval == 1, "Tree Fall Occured", "Tree Fall Did Not Occur")
df_long$bite.during.interval <- ifelse(df_long$bite.during.interval == 1, "Snake/Ray Bite Occured", "Snake/Ray Bite Did Not Occur")
df_long$fought.during.interval <- ifelse(df_long$fought.during.interval == 1, "Fight Occured", "Fight Did Not Occur")
df_long$animal.attack.during.interval <- ifelse(df_long$animal.attack.during.interval == 1, "Animal Attack Occured", "Animal Attack Did Not Occur")
df_long$canoe.capsize.during.interval <- ifelse(df_long$canoe.capsize.during.interval == 1, "Canoe Capsize Occured", "Canoe Capsize Did Not Occur")
df_long$cut.self.during.interval <- ifelse(df_long$cut.self.during.interval == 1, "Cut Self Occured", "Cut Self Not Occur")


### Tree Fall ----
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which SICKNESS occurs
pdf(file = "Sickness Plots/co_occurrence1c.pdf", height = 4, width = 6)
df_long %>%
  count(tree.fall.during.interval, event) %>%
  group_by(tree.fall.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(tree.fall.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 11) +
  ggtitle("SICKNESS and TREE FALL") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Sickness Plots/co_occurrence1d.pdf", height = 4, width = 8)
df_long %>%
  count(tree.fall.during.interval, event, male) %>%
  group_by(tree.fall.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(tree.fall.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 11) +
  ggtitle("SICKNESS and TREE FALL") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Sickness Plots/co_occurrence1e.pdf", height = 5, width = 8)
df_long %>%
  count(tree.fall.during.interval, event, region) %>%
  group_by(tree.fall.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(tree.fall.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 11) +
  ggtitle("SICKNESS and TREE FALL") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~region) +
  theme(strip.background = element_blank()) +
  guides(x =  guide_axis(angle = 90))
dev.off()

### Snake/Ray Bite ----
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which snake/ray bite occurs
pdf(file = "Sickness Plots/co_occurrence2c.pdf", height = 4, width = 6)
df_long %>%
  count(bite.during.interval, event) %>%
  group_by(bite.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(bite.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 11) +
  ggtitle("SICKNESS and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Sickness Plots/co_occurrence2d.pdf", height = 4, width = 9)
df_long %>%
  count(bite.during.interval, event, male) %>%
  group_by(bite.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(bite.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 10) +
  ggtitle("SICKNESS and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Sickness Plots/co_occurrence2e.pdf", height = 5, width = 8)
df_long %>%
  count(bite.during.interval, event, region) %>%
  group_by(bite.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(bite.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 11) +
  ggtitle("SICKNESS and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~region) +
  theme(strip.background = element_blank()) +
  guides(x =  guide_axis(angle = 90))
dev.off()

### Fight ----
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which fought
pdf(file = "Sickness Plots/co_occurrence3c.pdf", height = 4, width = 6)
df_long %>%
  count(fought.during.interval, event) %>%
  group_by(fought.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(fought.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 11) +
  ggtitle("SICKNESS and FOUGHT") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Sickness Plots/co_occurrence3d.pdf", height = 4, width = 8)
df_long %>%
  count(fought.during.interval, event, male) %>%
  group_by(fought.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(fought.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 11) +
  ggtitle("SICKNESS and FOUGHT") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Sickness Plots/co_occurrence3e.pdf", height = 5, width = 8)
df_long %>%
  count(fought.during.interval, event, region) %>%
  group_by(fought.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(fought.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 11) +
  ggtitle("SICKNESS and FOUGHT") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~region) +
  theme(strip.background = element_blank()) +
  guides(x =  guide_axis(angle = 90))
dev.off()

### Animal Attack ----
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which animal attack
pdf(file = "Sickness Plots/co_occurrence4c.pdf", height = 4, width = 6)
df_long %>%
  count(animal.attack.during.interval, event) %>%
  group_by(animal.attack.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(animal.attack.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 11) +
  ggtitle("SICKNESS and ANIMAL ATTACK") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Sickness Plots/co_occurrence4d.pdf", height = 4, width = 9)
df_long %>%
  count(animal.attack.during.interval, event, male) %>%
  group_by(animal.attack.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(animal.attack.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 10) +
  ggtitle("SICKNESS and ANIMAL ATTACK") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Sickness Plots/co_occurrence4e.pdf", height = 5, width = 8)
df_long %>%
  count(animal.attack.during.interval, event, region) %>%
  group_by(animal.attack.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(animal.attack.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 11) +
  ggtitle("SICKNESS and ANIMAL ATTACK") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~region) +
  theme(strip.background = element_blank()) +
  guides(x =  guide_axis(angle = 90))
dev.off()

# View(df_long[(df_long$region == "Upriver"),])

### Canoe Capsize ----
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which canoe capsize
pdf(file = "Sickness Plots/co_occurrence5c.pdf", height = 4, width = 6)
df_long %>%
  count(canoe.capsize.during.interval, event) %>%
  group_by(canoe.capsize.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(canoe.capsize.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 11) +
  ggtitle("SICKNESS and CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Sickness Plots/co_occurrence5d.pdf", height = 4, width = 9)
df_long %>%
  count(canoe.capsize.during.interval, event, male) %>%
  group_by(canoe.capsize.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(canoe.capsize.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 10) +
  ggtitle("SICKNESS and CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Sickness Plots/co_occurrence5e.pdf", height = 5, width = 8)
df_long %>%
  count(canoe.capsize.during.interval, event, region) %>%
  group_by(canoe.capsize.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(canoe.capsize.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 11) +
  ggtitle("SICKNESS and CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~region) +
  theme(strip.background = element_blank()) +
  guides(x =  guide_axis(angle = 90))
dev.off()

### Cut Self ----
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which cut self
pdf(file = "Sickness Plots/co_occurrence6c.pdf", height = 4, width = 6)
df_long %>%
  count(cut.self.during.interval, event) %>%
  group_by(cut.self.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(cut.self.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 11) +
  ggtitle("SICKNESS and CUT SELF") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Sickness Plots/co_occurrence6d.pdf", height = 4, width = 8)
df_long %>%
  count(cut.self.during.interval, event, male) %>%
  group_by(cut.self.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(cut.self.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 11) +
  ggtitle("SICKNESS and CUT SELF") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Sickness Plots/co_occurrence6e.pdf", height = 5, width = 8)
df_long %>%
  count(cut.self.during.interval, event, region) %>%
  group_by(cut.self.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(cut.self.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 11) +
  ggtitle("SICKNESS and CUT SELF") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~region) +
  theme(strip.background = element_blank()) +
  guides(x =  guide_axis(angle = 90))
dev.off()


## Survival Function ----
fit <- survfit(Surv(enter, exit, sickness.during.interval) ~ 1, data = df)
pdf(file = "Sickness Plots/sickness_survival_function.pdf", height = 5,
    width = 7)
autoplot(fit, censor.shape = '|', censor.colour = "darkgoldenrod3",
         surv.colour = "darkgoldenrod3") +
  theme_classic() +
  ggtitle("SICKNESS") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age in years") +
  ylab("Proportion of individuals not experienced risk")
dev.off()

# By gender
fit2 <- survfit(Surv(enter, exit, sickness.during.interval) ~ male, data = df)
pdf(file = "Sickness Plots/sickness_survival_function_by_gender.pdf",
    height = 5)
ggsurvplot(fit2, font.title = c("30"), conf.int = TRUE, legend = "right",
           surv.scale = "percent", legend.labs = c("Female", "Male"),
           legend.title = "Sex", palette = c("orchid2", "dodgerblue2"),
           title = "SICKNESS") +
  xlab("Age in years") +
  ylab("Proportion of individuals not experienced risk")
dev.off()

## Hazard Function ----
fit <- bshazard(Surv(enter, exit, sickness.during.interval) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
pdf(file = "Sickness Plots/sickness_hazard_function1.pdf", height = 5)
ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "darkgoldenrod3") +
  geom_segment(aes(x = 0, xend = max(time), y = 0, yend = 0), linetype = 2) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "darkgoldenrod3", color = "darkgoldenrod3") +
  theme_classic() +
  xlab("Age in Years") +
  ylab("Probability of experiencing risk") +
  ggtitle("SICKNESS") +
  theme(plot.title = element_text(size = 30))
dev.off()

# By gender
pdf(file = "Sickness Plots/sickness_hazard_function2.pdf", height = 5)
plot(muhaz2(Surv(exit, sickness.during.interval) ~ male, data = df), lty = 1,
     main = "SICKNESS", xlab = "Age in Years",
     ylab = "Probability of experiencing risk",
     legend.args = list(x = "topleft", legend = c("Female", "Male"), col = 1:2,
                        lty = 1, bty = "n"), bty = "L", cex.main = "3")
dev.off()


## Hazard Ratio, male over female ----
df$male <- as.numeric(df$male)
df$sickness.during.interval <- as.numeric(df$sickness.during.interval)
pdf(file = "Sickness Plots/sickness_hazard_ratio_plot.pdf", height = 5)
hazard.ratio.plot(df$male, Surv(df$exit, df$sickness.during.interval),
                  antilog = T,
                  smooth = T,
                  type = "l",
                  lwd = 1.5,
                  pr = F,
                  bty = "L",
                  main = "SICKNESS",
                  xlab = "Age in Years",
                  ylab = "Hazard Ratio (Male/Female)",
                  col = 6,
                  legendloc = "none",
                  cex.main = "3")
legend("topleft", inset = c(0.01, -0.03), legend = c("Hazard Ratio", "95% CI",
                                                     "Smoothed Estimate"),
       bty = "n", lty = 1:3,
       col = c(6, 1, 1),
       lwd = 1.5,
       cex = 0.7)
dev.off()
# fit <- coxph(Surv(enter, exit, sickness.during.interval) ~ male, data = df)
# plotHR(fit, term = "exit", xlim = c(0,80))



## Cumulative hazard function ----
fit <- survfit(Surv(enter, exit, sickness.during.interval) ~ 1, data = df)
pdf(file = "Sickness Plots/sickness_cumhaz_plot1.pdf", height = 5)
autoplot(fit, censor.shape = '|', censor.colour = "darkgoldenrod3",
         surv.colour = "darkgoldenrod3", fun = "cumhaz") +
  theme_classic(base_size = 12) +
  ggtitle("SICKNESS") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age in years") +
  ylab("Cumulative hazard")
dev.off()

# By gender
fit2 <- survfit(Surv(enter, exit, sickness.during.interval) ~ male, data = df)
pdf(file = "Sickness Plots/sickness_cumhaz_plot2.pdf", height = 5)
ggsurvplot(fit2, font.title = c("30"), conf.int = TRUE, legend = "right",
           legend.labs = c("Female", "Male"),
           legend.title = "Sex", palette = c("orchid2", "dodgerblue2"),
           title = "SICKNESS", fun = "cumhaz") +
  xlab("Age in years") +
  ylab("Cumulative hazard")
dev.off()
