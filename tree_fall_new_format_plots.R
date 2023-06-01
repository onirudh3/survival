# Libraries and Data Import -----------------------------------------------
library(survival)
library(eha)
library(ggfortify)
library(muhaz)

# Import data
df <- read.csv("tree_fall_new_format.csv")

# Make region as factor
df$region <- as.factor(df$region)

# Plots -------------------------------------------------------------------
# Adding age category columns
df <- df %>%
  mutate(age.cat = case_when(exit >= 0 & exit < 5 ~ "0-5",
                             exit >= 5 & exit < 10 ~ "5-10",
                             exit >= 10 & exit < 15 ~ "10-15",
                             exit >= 15 & exit < 20 ~ "15-20",
                             exit >= 20 & exit < 25 ~ "20-25",
                             exit >= 25 & exit < 30 ~ "25-30",
                             exit >= 30 & exit < 35 ~ "30-35",
                             exit >= 35 & exit < 40 ~ "35-40",
                             exit >= 40 & exit < 45 ~ "40-45",
                             exit >= 45 & exit < 50 ~ "45-50",
                             exit >= 50 & exit < 55 ~ "50-55",
                             exit >= 55 & exit < 60 ~ "55-60",
                             exit >= 60 ~ "60+"))

# Model 3 Plots
# 3a
fit <- coxreg(Surv(enter, exit, event) ~ male + region + strata(sickness.during.interval), data = df)
plot(fit,
     main = "TREE FALL (Controlling for sex and region)",
     col = c("blue", "red"),
     xlab = "Age in Years",
     ylab = "Cumulative Hazard")

# 3b
fit <- coxreg(Surv(enter, exit, event) ~ male + region + strata(bite.during.interval), data = df)
plot(fit,
     main = "TREE FALL (Controlling for sex and region)",
     col = c("blue", "red"),
     xlab = "Age in Years",
     ylab = "Cumulative Hazard")

# 3c
fit <- coxreg(Surv(enter, exit, event) ~ male + region + strata(fought.during.interval), data = df)
plot(fit,
     main = "TREE FALL (Controlling for sex and region)",
     col = c("blue", "red"),
     xlab = "Age in Years",
     ylab = "Cumulative Hazard")

# 3d
fit <- coxreg(Surv(enter, exit, event) ~ male + region + strata(animal.attack.during.interval), data = df)
plot(fit,
     main = "TREE FALL (Controlling for sex and region)",
     col = c("blue", "red"),
     xlab = "Age in Years",
     ylab = "Cumulative Hazard")

# 3e
fit <- coxreg(Surv(enter, exit, event) ~ male + region + strata(canoe.capsize.during.interval), data = df)
plot(fit,
     main = "TREE FALL (Controlling for sex and region)",
     col = c("blue", "red"),
     xlab = "Age in Years",
     ylab = "Cumulative Hazard")

# 3f
fit <- coxreg(Surv(enter, exit, event) ~ male + region + strata(cut.self.during.interval), data = df)
plot(fit,
     main = "TREE FALL (Controlling for sex and region)",
     col = c("blue", "red"),
     xlab = "Age in Years",
     ylab = "Cumulative Hazard")

