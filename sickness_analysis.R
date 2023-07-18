
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
library(xfun)
library(coxme)
library(xtable)

# Import data, format with 13,451 intervals
df <- read.csv("data_new_format.csv")
df_long <- read.csv("sickness_final_table.csv")
df_first <- read.csv("sickness_time_to_first_risk_long_interval.csv") # time to first risk long interval format


# Make region as factor
df$region <- as.factor(df$region)
df_long$region <- as.factor(df_long$region)
df_first$region <- as.factor(df_first$region)

# Model 1: Sex ------------------------------------------------------------
model1 <- coxph(Surv(enter, exit, sickness.during.interval) ~ male, data = df)
summary(model1)

# Testing Proportional Hazards
eha::logrank(Surv(enter, exit, sickness.during.interval), group = male,
             data = df)

# Testing Proportional Hazards for region
eha::logrank(Surv(enter, exit, sickness.during.interval), group = region, data = df)

# Export results in table
results <- data.frame("Coefficient" = summary(model1)$coefficients[,1],
                      "SE" = summary(model1)$coefficients[,3],
                      "p" = summary(model1)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "t.results.")
results$Male <- as.numeric(results$Male)
results$Male <- round(results$Male, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))

addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 4
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model1.tex")

# Model 2: Sex + Risk -----------------------------------------------------

## Model 2a: Sex + Tree Fall ----
model2a <- coxph(Surv(enter, exit, sickness.during.interval) ~ male +
                    tree.fall.during.interval,
                  data = df)
results <- data.frame("Coefficient" = summary(model2a)$coefficients[,1],
                      "SE" = summary(model2a)$coefficients[,3],
                      "p" = summary(model2a)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male")
results <- results %>% rename("Tree Fall" = "tree.fall.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 5
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model2a.tex")

## Model 2b: Sex + Snake/Ray Bite ----
model2b <- coxph(Surv(enter, exit, sickness.during.interval) ~ male +
                    bite.during.interval, data = df)
results <- data.frame("Coefficient" = summary(model2b)$coefficients[,1],
                      "SE" = summary(model2b)$coefficients[,3],
                      "p" = summary(model2b)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male")
results <- results %>% rename("Snake/Ray Bite" = "bite.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 5
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model2b.tex")

## Model 2c: Sex + Fight ----
model2c <- coxph(Surv(enter, exit, sickness.during.interval) ~ male +
                    fought.during.interval, data = df)
results <- data.frame("Coefficient" = summary(model2c)$coefficients[,1],
                      "SE" = summary(model2c)$coefficients[,3],
                      "p" = summary(model2c)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male")
results <- results %>% rename("Fight" = "fought.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 5
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model2c.tex")

## Model 2d: Sex + Animal Attack ----
model2d <- coxph(Surv(enter, exit, sickness.during.interval) ~ male +
                    animal.attack.during.interval, data = df)
results <- data.frame("Coefficient" = summary(model2d)$coefficients[,1],
                      "SE" = summary(model2d)$coefficients[,3],
                      "p" = summary(model2d)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male")
results <- results %>% rename("Animal Attack" = "animal.attack.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 5
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model2d.tex")

## Model 2e: Sex + Canoe Capsize ----
model2e <- coxph(Surv(enter, exit, sickness.during.interval) ~ male +
                    canoe.capsize.during.interval, data = df)
results <- data.frame("Coefficient" = summary(model2e)$coefficients[,1],
                      "SE" = summary(model2e)$coefficients[,3],
                      "p" = summary(model2e)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male")
results <- results %>% rename("Canoe Capsize" = "canoe.capsize.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 5
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model2e.tex")

## Model 2f: Sex + Cut Self ----
model2f <- coxph(Surv(enter, exit, sickness.during.interval) ~ male +
                    cut.self.during.interval, data = df)
results <- data.frame("Coefficient" = summary(model2f)$coefficients[,1],
                      "SE" = summary(model2f)$coefficients[,3],
                      "p" = summary(model2f)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male")
results <- results %>% rename("Cut Self" = "cut.self.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 5
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model2f.tex")

## Model 2g: Sex + Animal Attack (c) ----
model2g <- coxph(Surv(enter, exit, sickness.during.interval) ~ male +
                    Animal_Attack.during.interval, data = df)
results <- data.frame("Coefficient" = summary(model2g)$coefficients[,1],
                      "SE" = summary(model2g)$coefficients[,3],
                      "p" = summary(model2g)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male")
results <- results %>% rename("Animal Attack (c)" = "Animal_Attack.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 5
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model2g.tex")


# Model 3: Sex + Region + Risk --------------------------------------------

## Model 3a: Sex + Region + Tree Fall ----
model3a <- coxph(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    tree.fall.during.interval, data = df)
results <- data.frame("Coefficient" = summary(model3a)$coefficients[,1],
                      "SE" = summary(model3a)$coefficients[,3],
                      "p" = summary(model3a)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male",
                              "Near San Borja" = "regionNear.San.Borja",
                              "Upriver" = "regionUpriver",
                              "Tree Fall" = "tree.fall.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 4
addtorow$pos[[3]] <- 7
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model3a.tex")

# Plot
pdf(file = "Sickness Plots/model3a.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
              strata(tree.fall.during.interval), data = df),
     main = "SICKNESS (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Tree Fall Did Not Occur", "Tree Fall Occured"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3b: Sex + Region + Snake/Ray Bite ----
model3b <- coxph(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    bite.during.interval, data = df)
results <- data.frame("Coefficient" = summary(model3b)$coefficients[,1],
                      "SE" = summary(model3b)$coefficients[,3],
                      "p" = summary(model3b)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male",
                              "Near San Borja" = "regionNear.San.Borja",
                              "Upriver" = "regionUpriver",
                              "Snake/Ray Bite" = "bite.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 4
addtorow$pos[[3]] <- 7
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model3b.tex")

# Plot
pdf(file = "Sickness Plots/model3b.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
              strata(bite.during.interval), data = df),
     main = "SICKNESS (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Snake/Ray Bite Did Not Occur", "Snake/Ray Bite Occured"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3c: Sex + Region + Fight ----
model3c <- coxph(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    fought.during.interval, data = df)
results <- data.frame("Coefficient" = summary(model3c)$coefficients[,1],
                      "SE" = summary(model3c)$coefficients[,3],
                      "p" = summary(model3c)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male",
                              "Near San Borja" = "regionNear.San.Borja",
                              "Upriver" = "regionUpriver",
                              "Fight" = "fought.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 4
addtorow$pos[[3]] <- 7
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model3c.tex")

# Plot
pdf(file = "Sickness Plots/model3c.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
              strata(fought.during.interval), data = df),
     main = "SICKNESS (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Fight Did Not Occur", "Fight Occured"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3d: Sex + Region + Animal Attack ----
model3d <- coxph(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    animal.attack.during.interval, data = df)
results <- data.frame("Coefficient" = summary(model3d)$coefficients[,1],
                      "SE" = summary(model3d)$coefficients[,3],
                      "p" = summary(model3d)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male",
                              "Near San Borja" = "regionNear.San.Borja",
                              "Upriver" = "regionUpriver",
                              "Animal Attack" = "animal.attack.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 4
addtorow$pos[[3]] <- 7
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model3d.tex")

# Plot
pdf(file = "Sickness Plots/model3d.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
              strata(animal.attack.during.interval), data = df),
     main = "SICKNESS (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Animal Attack Did Not Occur", "Animal Attack Occured"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3e: Sex + Region + Canoe Capsize ----
model3e <- coxph(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    canoe.capsize.during.interval, data = df)
results <- data.frame("Coefficient" = summary(model3e)$coefficients[,1],
                      "SE" = summary(model3e)$coefficients[,3],
                      "p" = summary(model3e)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male",
                              "Near San Borja" = "regionNear.San.Borja",
                              "Upriver" = "regionUpriver",
                              "Canoe Capsize" = "canoe.capsize.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 4
addtorow$pos[[3]] <- 7
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model3e.tex")

# Plot
pdf(file = "Sickness Plots/model3e.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
              strata(canoe.capsize.during.interval), data = df),
     main = "SICKNESS (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Canoe Capsize Did Not Occur", "Canoe Capsize Occured"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3f: Sex + Region + Cut Self ----
model3f <- coxph(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    cut.self.during.interval, data = df)
results <- data.frame("Coefficient" = summary(model3f)$coefficients[,1],
                      "SE" = summary(model3f)$coefficients[,3],
                      "p" = summary(model3f)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male",
                              "Near San Borja" = "regionNear.San.Borja",
                              "Upriver" = "regionUpriver",
                              "Cut Self" = "cut.self.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 4
addtorow$pos[[3]] <- 7
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model3f.tex")

# Plot
pdf(file = "Sickness Plots/model3f.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
              strata(cut.self.during.interval), data = df),
     main = "SICKNESS (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Cut Self Did Not Occur", "Cut Self Occured"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

## Model 3g: Sex + Region + Animal Attack (c) ----
model3g <- coxph(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    Animal_Attack.during.interval, data = df)
results <- data.frame("Coefficient" = summary(model3g)$coefficients[,1],
                      "SE" = summary(model3g)$coefficients[,3],
                      "p" = summary(model3g)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male",
                              "Near San Borja" = "regionNear.San.Borja",
                              "Upriver" = "regionUpriver",
                              "Animal Attack (c)" = "Animal_Attack.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 4
addtorow$pos[[3]] <- 7
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model3g.tex")

# Plot
pdf(file = "Sickness Plots/model3g.pdf", height = 5, width = 5)
plot(coxreg(Surv(enter, exit, sickness.during.interval) ~ male + region +
              strata(Animal_Attack.during.interval), data = df),
     main = "SICKNESS (Controlling for Sex and Region)",
     xlab = "Age in Years",
     ylab = "Cumulative Hazard",
     printLegend = F,
     col = 1:2)
legend("topleft", legend = c("Animal Attack (c) Did Not Occur", "Animal Attack (c) Occured"),
       col = 1:2, lty = 1, bty = "n")
dev.off()

# Model 4: Sex + Region + Risk + Sex*Risk ---------------------------------

## Model 4a: Sex + Region + Tree Fall + Sex*Tree Fall ----
model4a <- coxph(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    tree.fall.during.interval +
                    male * tree.fall.during.interval, data = df)
results <- data.frame("Coefficient" = summary(model4a)$coefficients[,1],
                      "SE" = summary(model4a)$coefficients[,3],
                      "p" = summary(model4a)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male",
                              "Near San Borja" = "regionNear.San.Borja",
                              "Upriver" = "regionUpriver",
                              "Tree Fall" = "tree.fall.during.interval",
                              "Male x Tree Fall" = "male.tree.fall.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 5
addtorow$pos[[3]] <- 8
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model4a.tex")

## Model 4b: Sex + Region + Snake/Ray Bite + Sex*Snake/Ray Bite ----
model4b <- coxph(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    bite.during.interval + male * bite.during.interval,
                  data = df)
results <- data.frame("Coefficient" = summary(model4b)$coefficients[,1],
                      "SE" = summary(model4b)$coefficients[,3],
                      "p" = summary(model4b)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male",
                              "Near San Borja" = "regionNear.San.Borja",
                              "Upriver" = "regionUpriver",
                              "Snake/Ray Bite" = "bite.during.interval",
                              "Male x Snake/Ray Bite" = "male.bite.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 5
addtorow$pos[[3]] <- 8
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model4b.tex")

## Model 4c: Sex + Region + Fight + Sex*Fight ----
model4c <- coxph(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    fought.during.interval + male * fought.during.interval,
                  data = df)
results <- data.frame("Coefficient" = summary(model4c)$coefficients[,1],
                      "SE" = summary(model4c)$coefficients[,3],
                      "p" = summary(model4c)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male",
                              "Near San Borja" = "regionNear.San.Borja",
                              "Upriver" = "regionUpriver",
                              "Fight" = "fought.during.interval",
                              "Male x Fight" = "male.fought.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 5
addtorow$pos[[3]] <- 8
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model4c.tex")

## Model 4d: Sex + Region + Animal Attack + Sex*Animal Attack ----
model4d <- coxph(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    animal.attack.during.interval +
                    male * animal.attack.during.interval,
                  data = df)
results <- data.frame("Coefficient" = summary(model4d)$coefficients[,1],
                      "SE" = summary(model4d)$coefficients[,3],
                      "p" = summary(model4d)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male",
                              "Near San Borja" = "regionNear.San.Borja",
                              "Upriver" = "regionUpriver",
                              "Animal Attack" = "animal.attack.during.interval",
                              "Male x Animal Attack" = "male.animal.attack.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 5
addtorow$pos[[3]] <- 8
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model4d.tex")

## Model 4e: Sex + Region + Canoe Capsize + Sex*Canoe Capsize ----
model4e <- coxph(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    canoe.capsize.during.interval +
                    male * canoe.capsize.during.interval,
                  data = df)
results <- data.frame("Coefficient" = summary(model4e)$coefficients[,1],
                      "SE" = summary(model4e)$coefficients[,3],
                      "p" = summary(model4e)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male",
                              "Near San Borja" = "regionNear.San.Borja",
                              "Upriver" = "regionUpriver",
                              "Canoe Capsize" = "canoe.capsize.during.interval",
                              "Male x Canoe Capsize" = "male.canoe.capsize.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 5
addtorow$pos[[3]] <- 8
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model4e.tex")

## Model 4f: Sex + Region + Cut Self + Sex*Cut Self ----
model4f <- coxph(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    cut.self.during.interval + male * cut.self.during.interval,
                  data = df)
results <- data.frame("Coefficient" = summary(model4f)$coefficients[,1],
                      "SE" = summary(model4f)$coefficients[,3],
                      "p" = summary(model4f)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male",
                              "Near San Borja" = "regionNear.San.Borja",
                              "Upriver" = "regionUpriver",
                              "Cut Self" = "cut.self.during.interval",
                              "Male x Cut Self" = "male.cut.self.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 5
addtorow$pos[[3]] <- 8
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model4f.tex")

## Model 4g: Sex + Region + Animal Attack (c) + Sex*Animal Attack (c) ----
model4g <- coxph(Surv(enter, exit, sickness.during.interval) ~ male + region +
                    Animal_Attack.during.interval + male * Animal_Attack.during.interval,
                  data = df)
results <- data.frame("Coefficient" = summary(model4g)$coefficients[,1],
                      "SE" = summary(model4g)$coefficients[,3],
                      "p" = summary(model4g)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male",
                              "Near San Borja" = "regionNear.San.Borja",
                              "Upriver" = "regionUpriver",
                              "Animal Attack (c)" = "Animal_Attack.during.interval",
                              "Male x Animal Attack (c)" = "male.Animal_Attack.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 5
addtorow$pos[[3]] <- 8
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model4g.tex")

# Model 5: Sex + Region + Length of Last Sickness + Risk ------------------

# Create new df with no NAs in length.of.last.sickness
new_df <- df_long %>%
  tidyr::drop_na(length.of.last.sickness)

# Total risk years in new_df
# x <- new_df[!duplicated(new_df$pid), ]
# sum(x$age)

## Model 5a: Length of Last Sickness ----
model5a <- coxph(Surv(enter, exit, event) ~ length.of.last.sickness,
                  data = new_df)
results <- data.frame("Coefficient" = summary(model5a)$coefficients[,1],
                      "SE" = summary(model5a)$coefficients[,3],
                      "p" = summary(model5a)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Length of Prior Sickness Interval" = "t.results.")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 4
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model5a.tex")

## Model 5b: Length of Prior Sickness + Sex ----
model5b <- coxph(Surv(enter, exit, event) ~ length.of.last.sickness + male,
                  data = new_df)
results <- data.frame("Coefficient" = summary(model5b)$coefficients[,1],
                      "SE" = summary(model5b)$coefficients[,3],
                      "p" = summary(model5b)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male",
                              "Length of Prior Sickness Interval" = "length.of.last.sickness")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 5
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model5b.tex")

## Model 5c: Length of Prior Sickness + Sex + Region ----
model5c <- coxph(Surv(enter, exit, event) ~ length.of.last.sickness + male +
                    region, data = new_df)
results <- data.frame("Coefficient" = summary(model5c)$coefficients[,1],
                      "SE" = summary(model5c)$coefficients[,3],
                      "p" = summary(model5c)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Male" = "male",
                              "Length of Prior Sickness Interval" = "length.of.last.sickness",
                              "Near San Borja" = "regionNear.San.Borja",
                              "Upriver" = "regionUpriver")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 4
addtorow$pos[[3]] <- 7
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model5c.tex")

## Model 5d: Length of Prior Sickness + Tree Fall ----
model5d <- coxph(Surv(enter, exit, event) ~ length.of.last.sickness +
                    tree.fall.during.interval, data = new_df)
results <- data.frame("Coefficient" = summary(model5d)$coefficients[,1],
                      "SE" = summary(model5d)$coefficients[,3],
                      "p" = summary(model5d)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Length of Prior Sickness Interval" = "length.of.last.sickness",
                              "Tree Fall" = "tree.fall.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 5
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model5d.tex")

## Model 5e: Length of Prior Sickness + Snake/Ray Bite ----
model5e <- coxph(Surv(enter, exit, event) ~ length.of.last.sickness +
                    bite.during.interval, data = new_df)
results <- data.frame("Coefficient" = summary(model5e)$coefficients[,1],
                      "SE" = summary(model5e)$coefficients[,3],
                      "p" = summary(model5e)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Length of Prior Sickness Interval" = "length.of.last.sickness",
                              "Snake/Ray Bite" = "bite.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 5
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model5e.tex")

## Model 5f: Length of Prior Sickness + Fight ----
model5f <- coxph(Surv(enter, exit, event) ~ length.of.last.sickness +
                    fought.during.interval, data = new_df)
results <- data.frame("Coefficient" = summary(model5f)$coefficients[,1],
                      "SE" = summary(model5f)$coefficients[,3],
                      "p" = summary(model5f)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Length of Prior Sickness Interval" = "length.of.last.sickness",
                              "Fight" = "fought.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 5
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model5f.tex")

## Model 5g: Length of Prior Sickness + Animal Attack ----
model5g <- coxph(Surv(enter, exit, event) ~ length.of.last.sickness +
                    animal.attack.during.interval, data = new_df)
results <- data.frame("Coefficient" = summary(model5g)$coefficients[,1],
                      "SE" = summary(model5g)$coefficients[,3],
                      "p" = summary(model5g)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Length of Prior Sickness Interval" = "length.of.last.sickness",
                              "Animal Attack" = "animal.attack.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 5
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model5g.tex")

## Model 5h: Length of Prior Sickness + Canoe Capsize ----
model5h <- coxph(Surv(enter, exit, event) ~ length.of.last.sickness +
                    canoe.capsize.during.interval, data = new_df)
results <- data.frame("Coefficient" = summary(model5h)$coefficients[,1],
                      "SE" = summary(model5h)$coefficients[,3],
                      "p" = summary(model5h)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Length of Prior Sickness Interval" = "length.of.last.sickness",
                              "Canoe Capsize" = "canoe.capsize.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 5
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model5h.tex")

## Model 5i: Length of Prior Sickness + Cut Self ----
model5i <- coxph(Surv(enter, exit, event) ~ length.of.last.sickness +
                    cut.self.during.interval, data = new_df)
results <- data.frame("Coefficient" = summary(model5i)$coefficients[,1],
                      "SE" = summary(model5i)$coefficients[,3],
                      "p" = summary(model5i)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Length of Prior Sickness Interval" = "length.of.last.sickness",
                              "Cut Self" = "cut.self.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 5
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model5i.tex")

## Model 5j: Length of Prior Sickness + Animal Attack (c) ----
model5j <- coxph(Surv(enter, exit, event) ~ length.of.last.sickness +
                    Animal_Attack.during.interval, data = new_df)
results <- data.frame("Coefficient" = summary(model5j)$coefficients[,1],
                      "SE" = summary(model5j)$coefficients[,3],
                      "p" = summary(model5j)$coefficients[,5])
results <- data.frame(t(results))
results <- results %>% rename("Length of Prior Sickness Interval" = "length.of.last.sickness",
                              "Animal Attack (c)" = "Animal_Attack.during.interval")
results <- round(results, 3)
results <- results %>%
  mutate("No. of Individuals" = c("", "388", ""),
         "No. of Intervals" = c("", "13,451", ""),
         "No. of Risk Years" = c("", "13,254.94", ""))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 5
addtorow$command <- c('\\hline ',
                      '\\hline ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model5j.tex")


# Model 6: Mixed effects --------------------------------------------------
df_first <- read.csv("sickness_time_to_first_risk_short_interval.csv") # time to first risk short interval format

## Null Model
model6 <- coxph(Surv(exit, sickness.during.interval) ~ 1, data = df_first)
# summary(model6)

# Proportional hazards test
c <- eha::logrank(Surv(exit, sickness.during.interval), male, df_first)
c <- data.frame("Chisq" = c$test.statistic, "df" = c$df, "p" = c$p.value)
stargazer(c, summary = F, title = "Sickness Log-rank Test Results for Sex", rownames = F,
          out = "Sickness Tables/logranktest3.tex")

d <- eha::logrank(Surv(exit, sickness.during.interval), region, df_first)
d <- data.frame("Chisq" = d$test.statistic, "df" = d$df, "p" = d$p.value)
stargazer(d, summary = F, title = "Sickness Log-rank Test Results for Region", rownames = F,
          out = "Sickness Tables/logranktest4.tex")

## Model 6a: Null with random intercept for pid ----
model6a <- coxme(Surv(exit, sickness.during.interval) ~ 1 + (1 | pid),
                 data = df_first)
# summary(model6a)

## Model 6b: Null with random intercepts for pid and house.id ----
model6b <- coxme(Surv(exit, sickness.during.interval) ~ 1 + (1 | pid) +
                   (1 | house.id), data = df_first)
# summary(model6b)

## Model 6c: Null with random intercepts for pid, house.id and region ----
model6c <- coxme(Surv(exit, sickness.during.interval) ~ 1 + (1 | pid) +
                   (1 | house.id) + (1 | region), data = df_first)
# summary(model6c)

## Model 6d: Null with region FE and pid, house.id RE ----
model6d <- coxme(Surv(exit, sickness.during.interval) ~ 1 + (1 | pid) +
                   (1 | house.id) + region, data = df_first)
# summary(model6d)

## Model 6e: Nested RE ----
model6e <- coxme(Surv(exit, sickness.during.interval) ~ 1 + (1 | pid) + (1 | region/house.id),
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
x <- xtable(df_coxme, caption = "Sickness Mixed Effects Specifications")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model6.tex")

## Model 6f: Tree Fall ----
model6f <- coxme(Surv(exit, sickness.during.interval) ~ strata(male) + (1 | pid) +
                   (1 | house.id) + (1 | region) + tree.fall.during.interval, df_first)
saveRDS(model6f, file = "Sickness Tables/coxme_tree_fall.RDS")

extract_coxme_table <- function (mod){
  beta <- mod$coefficients
  exp_beta <- exp(beta)
  nvar <- length(beta)
  nfrail <- nrow(mod$var) - nvar
  se <- sqrt(diag(mod$var)[nfrail + 1:nvar])
  p<- signif(1 - pchisq((beta/se)^2, 1), 2)
  table = data.frame(cbind(beta, exp_beta, se, p))
  return(table)
}

results <- extract_coxme_table(model6f)
b <- data.frame(confint(model6f))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Tree Fall")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model6f.tex")

# Plot Schoenfeld residuals
pdf(file = "Sickness Plots/schoenfeld_res1.pdf", height = 5, width = 7)
ggcoxzph(cox.zph(model6f))
dev.off()


## Model 6g: Snake/Ray Bite ----
model6g <- coxme(Surv(exit, sickness.during.interval) ~ strata(male) + (1 | pid) +
                   (1 | house.id) + (1 | region) + bite.during.interval, df_first)
saveRDS(model6g, file = "Sickness Tables/coxme_snake_ray_bite.RDS")

results <- extract_coxme_table(model6g)
b <- data.frame(confint(model6g))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Snake/Ray Bite")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model6g.tex")

# Plot Schoenfeld residuals
pdf(file = "Sickness Plots/schoenfeld_res2.pdf", height = 5, width = 7)
ggcoxzph(cox.zph(model6g))
dev.off()


## Model 6h: Fight ----
model6h <- coxme(Surv(exit, sickness.during.interval) ~ strata(male) + (1 | pid) +
                   (1 | house.id) + (1 | region) + fought.during.interval, df_first)
saveRDS(model6h, file = "Sickness Tables/coxme_fight.RDS")

results <- extract_coxme_table(model6h)
b <- data.frame(confint(model6h))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Fight")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model6h.tex")

# Plot Schoenfeld residuals
pdf(file = "Sickness Plots/schoenfeld_res3.pdf", height = 5, width = 7)
ggcoxzph(cox.zph(model6h))
dev.off()


## Model 6i: Canoe Capsize ----
model6i <- coxme(Surv(exit, sickness.during.interval) ~ strata(male) + (1 | pid) +
                   (1 | house.id) + (1 | region) + canoe.capsize.during.interval, df_first)
saveRDS(model6i, file = "Sickness Tables/coxme_canoe_capsize.RDS")

results <- extract_coxme_table(model6i)
b <- data.frame(confint(model6i))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Canoe Capsize")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model6i.tex")

# Plot Schoenfeld residuals
pdf(file = "Sickness Plots/schoenfeld_res4.pdf", height = 5, width = 7)
ggcoxzph(cox.zph(model6i))
dev.off()


## Model 6j: Animal Attack ----
model6j <- coxme(Surv(exit, sickness.during.interval) ~ strata(male) + (1 | pid) +
                   (1 | house.id) + (1 | region) + animal.attack.during.interval, df_first)
saveRDS(model6j, file = "Sickness Tables/coxme_animal_attack.RDS")

results <- extract_coxme_table(model6j)
b <- data.frame(confint(model6j))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Animal Attack")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model6j.tex")

# Plot Schoenfeld residuals
pdf(file = "Sickness Plots/schoenfeld_res5.pdf", height = 5, width = 7)
ggcoxzph(cox.zph(model6j))
dev.off()


## Model 6k: Animal Attack (c) ----
model6k <- coxme(Surv(exit, sickness.during.interval) ~ strata(male) + (1 | pid) +
                   (1 | house.id) + (1 | region) + Animal_Attack.during.interval, df_first)
saveRDS(model6k, file = "Sickness Tables/coxme_animal_attack_combined.RDS")

results <- extract_coxme_table(model6k)
b <- data.frame(confint(model6k))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Animal Attack (c)")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model6k.tex")

# Plot Schoenfeld residuals
pdf(file = "Sickness Plots/schoenfeld_res6.pdf", height = 5, width = 7)
ggcoxzph(cox.zph(model6k))
dev.off()


## Model 6l: Cut Self ----
model6l <- coxme(Surv(exit, sickness.during.interval) ~ strata(male) + (1 | pid) +
                   (1 | house.id) + (1 | region) + cut.self.during.interval, df_first)
saveRDS(model6l, file = "Sickness Tables/coxme_cut_self.RDS")

results <- extract_coxme_table(model6l)
b <- data.frame(confint(model6l))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Cut Self")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model6l.tex")

# Plot Schoenfeld residuals
pdf(file = "Sickness Plots/schoenfeld_res7.pdf", height = 5, width = 7)
ggcoxzph(cox.zph(model6l))
dev.off()


# Building mixed effects hazard plot for all risks ------------------------

df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Sickness Tables/coxme_tree_fall.RDS")), data.frame(confint(readRDS("Sickness Tables/coxme_tree_fall.RDS")))),
                            cbind(extract_coxme_table(readRDS("Sickness Tables/coxme_fight.RDS")), data.frame(confint(readRDS("Sickness Tables/coxme_fight.RDS")))),
                            # cbind(extract_coxme_table(readRDS("Sickness Tables/coxme_animal_attack.RDS")), data.frame(confint(readRDS("Sickness Tables/coxme_animal_attack.RDS")))),
                            cbind(extract_coxme_table(readRDS("Sickness Tables/coxme_animal_attack_combined.RDS")), data.frame(confint(readRDS("Sickness Tables/coxme_animal_attack_combined.RDS")))),
                            cbind(extract_coxme_table(readRDS("Sickness Tables/coxme_cut_self.RDS")), data.frame(confint(readRDS("Sickness Tables/coxme_cut_self.RDS")))),
                            cbind(extract_coxme_table(readRDS("Sickness Tables/coxme_canoe_capsize.RDS")), data.frame(confint(readRDS("Sickness Tables/coxme_canoe_capsize.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Sickness Tables/coxme_tree_fall.RDS"))$table),
                   data.frame(cox.zph(readRDS("Sickness Tables/coxme_fight.RDS"))$table),
                   data.frame(cox.zph(readRDS("Sickness Tables/coxme_animal_attack_combined.RDS"))$table),
                   data.frame(cox.zph(readRDS("Sickness Tables/coxme_cut_self.RDS"))$table),
                   data.frame(cox.zph(readRDS("Sickness Tables/coxme_canoe_capsize.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "tree.fall.during.interval" ~ "Tree Fall",
                               covariate == "fought.during.interval" ~ "Fight",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack",
                               covariate == "cut.self.during.interval" ~ "Cut Self",
                               covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize",))

pdf(file = "Sickness Plots/coxme_plot.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = reorder(covariate, -exp_beta), y = exp_beta, fill = p.y), stat = "identity") +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, size = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID, House ID, and Region") +
  geom_segment(aes(x = 0, y = 1, xend = 5.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) +
  ggtitle("Outcome Variable: Sickness") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 10.5))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Sickness Plots/coxme_plot.RDS")


# Model 7: coxme with calendar year ---------------------------------------

## Model 7a: Tree Fall ----
model7a <- coxme(Surv(exit, sickness.during.interval) ~ strata(male) + strata(year) + (1 | pid) +
                   (1 | house.id) + (1 | region) + tree.fall.during.interval, df_first)
saveRDS(model7a, file = "Sickness Tables/coxme_tree_fall1.RDS")

results <- extract_coxme_table(model7a)
b <- data.frame(confint(model7a))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Tree Fall")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model7a.tex")

# Plot Schoenfeld residuals
pdf(file = "Sickness Plots/schoenfeld_res1_.pdf", height = 5, width = 7)
ggcoxzph(cox.zph(model7a))
dev.off()


## Model 7b: Fight ----
model7b <- coxme(Surv(exit, sickness.during.interval) ~ strata(male) + strata(year) + (1 | pid) +
                   (1 | house.id) + (1 | region) + fought.during.interval, df_first)
saveRDS(model7b, file = "Sickness Tables/coxme_fight1.RDS")

results <- extract_coxme_table(model7b)
b <- data.frame(confint(model7b))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Fight")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model7b.tex")

# Plot Schoenfeld residuals
pdf(file = "Sickness Plots/schoenfeld_res2_.pdf", height = 5, width = 7)
ggcoxzph(cox.zph(model7b))
dev.off()


## Model 7c: Canoe Capsize ----
model7c <- coxme(Surv(exit, sickness.during.interval) ~ strata(male) + strata(year) + (1 | pid) +
                   (1 | house.id) + (1 | region) + canoe.capsize.during.interval, df_first)
saveRDS(model7c, file = "Sickness Tables/coxme_canoe_capsize1.RDS")

results <- extract_coxme_table(model7c)
b <- data.frame(confint(model7c))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Canoe Capsize")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model7c.tex")

# Plot Schoenfeld residuals
pdf(file = "Sickness Plots/schoenfeld_res3_.pdf", height = 5, width = 7)
ggcoxzph(cox.zph(model7c))
dev.off()


## Model 7d: Animal Attack (c) ----
model7d <- coxme(Surv(exit, sickness.during.interval) ~ strata(male) + strata(year) + (1 | pid) +
                   (1 | house.id) + (1 | region) + Animal_Attack.during.interval, df_first)
saveRDS(model7d, file = "Sickness Tables/coxme_animal_attack_combined1.RDS")

results <- extract_coxme_table(model7d)
b <- data.frame(confint(model7d))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Animal Attack (c)")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model7d.tex")

# Plot Schoenfeld residuals
pdf(file = "Sickness Plots/schoenfeld_res4_.pdf", height = 5, width = 7)
ggcoxzph(cox.zph(model7d))
dev.off()


## Model 7e: Cut Self ----
model7e <- coxme(Surv(exit, sickness.during.interval) ~ strata(male) + strata(year) + (1 | pid) +
                   (1 | house.id) + (1 | region) + cut.self.during.interval, df_first)
saveRDS(model7e, file = "Sickness Tables/coxme_cut_self1.RDS")

results <- extract_coxme_table(model7e)
b <- data.frame(confint(model7e))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Cut Self")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Sickness \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Sickness Tables/model7e.tex")

# Plot Schoenfeld residuals
pdf(file = "Sickness Plots/schoenfeld_res5_.pdf", height = 5, width = 7)
ggcoxzph(cox.zph(model7e))
dev.off()

# Building mixed effects hazard plot for all risks ------------------------

df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Sickness Tables/coxme_tree_fall1.RDS")), data.frame(confint(readRDS("Sickness Tables/coxme_tree_fall.RDS")))),
                            cbind(extract_coxme_table(readRDS("Sickness Tables/coxme_fight1.RDS")), data.frame(confint(readRDS("Sickness Tables/coxme_fight.RDS")))),
                            # cbind(extract_coxme_table(readRDS("Sickness Tables/coxme_animal_attack.RDS")), data.frame(confint(readRDS("Sickness Tables/coxme_animal_attack.RDS")))),
                            cbind(extract_coxme_table(readRDS("Sickness Tables/coxme_animal_attack_combined1.RDS")), data.frame(confint(readRDS("Sickness Tables/coxme_animal_attack_combined.RDS")))),
                            cbind(extract_coxme_table(readRDS("Sickness Tables/coxme_cut_self1.RDS")), data.frame(confint(readRDS("Sickness Tables/coxme_cut_self.RDS")))),
                            cbind(extract_coxme_table(readRDS("Sickness Tables/coxme_canoe_capsize1.RDS")), data.frame(confint(readRDS("Sickness Tables/coxme_canoe_capsize.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Sickness Tables/coxme_tree_fall1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Sickness Tables/coxme_fight1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Sickness Tables/coxme_animal_attack_combined1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Sickness Tables/coxme_cut_self1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Sickness Tables/coxme_canoe_capsize1.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "tree.fall.during.interval" ~ "Tree Fall",
                               covariate == "fought.during.interval" ~ "Fight",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack",
                               covariate == "cut.self.during.interval" ~ "Cut Self",
                               covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize",))

pdf(file = "Sickness Plots/coxme_plot_.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = reorder(covariate, -exp_beta), y = exp_beta, fill = p.y), stat = "identity") +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, size = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID, House ID, and Region") +
  geom_segment(aes(x = 0, y = 1, xend = 5.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) +
  ggtitle("Outcome Variable: Sickness") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 13))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Sickness Plots/coxme_plot_.RDS")

# Descriptive Plots -------------------------------------------------------
# Make age.cat as factor
df$age.cat <- factor(df$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))

df_long$male <- ifelse(df_long$male == 1, "Male", "Female")
df$male <- ifelse(df$male == 1, "Male", "Female")

## Percentage of intervals where sickness occurred by age category ----
# Bar plot
pdf(file = "Sickness Plots/descriptive_plot1.pdf", height = 5,
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
  ylab("Percentage of Intervals (13,451 Intervals)")
dev.off()

# Line plot
pdf(file = "Sickness Plots/descriptive_plot2.pdf", height = 5,
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
  ylab("Percentage of Intervals (13,451 Intervals)")
dev.off()

# By gender
df_male <- subset(df, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, sickness.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(sickness.during.interval == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df, male == "Female")
df_female_plot <- df_female %>%
  count(age.cat, sickness.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(sickness.during.interval == 1)
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


pdf(file = "Sickness Plots/descriptive_plot3.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Sickness") +
  labs(subtitle = "388 Individuals, 13,451 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.5))
dev.off()


## Age Interval Plots ----


### Tree Fall ----
# Distribution of co-occurring Tree Fall within Sickness age intervals
pdf(file = "Sickness Plots/co_occurrence1a.pdf", height = 5, width = 7)
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
  ggtitle("SICKNESS and TREE FALL") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (863 Intervals)")
dev.off()

# By gender
pdf(file = "Sickness Plots/co_occurrence1b.pdf", height = 5, width = 10)
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
  ggtitle("SICKNESS and TREE FALL") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (863 Intervals)") +
  facet_wrap(~male) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.background = element_blank())
dev.off()

### Fight ----
# Distribution of co-occurring Fight within Sickness age intervals
pdf(file = "Sickness Plots/co_occurrence2a.pdf", height = 5, width = 7)
df_long %>%
  count(fought.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(fought.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = fought.co_occurrence.interval, y = prop), fill = "#7c9b99", width = 0.9, show.legend = F) +
  geom_text(aes(x = fought.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and FIGHT") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (863 Intervals)")
dev.off()

# By gender
pdf(file = "Sickness Plots/co_occurrence2b.pdf", height = 5, width = 10)
df_long %>%
  count(fought.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(fought.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = fought.co_occurrence.interval, y = prop, fill = male), width = 0.9, show.legend = F) +
  geom_text(aes(x = fought.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and FIGHT") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (863 Intervals)") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  theme(strip.background = element_blank())
dev.off()

### Snake/Ray Bite ----
# Distribution of co-occurring Snake/Ray Bite within Sickness age intervals
pdf(file = "Sickness Plots/co_occurrence3a.pdf", height = 5, width = 7)
df_long %>%
  count(bite.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(bite.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = bite.co_occurrence.interval, y = prop), fill = "#7c9b99", width = 0.9, show.legend = F) +
  geom_text(aes(x = bite.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (863 Intervals)")
dev.off()

# By gender
pdf(file = "Sickness Plots/co_occurrence3b.pdf", height = 5, width = 10)
df_long %>%
  count(bite.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(bite.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = bite.co_occurrence.interval, y = prop, fill = male), width = 0.9, show.legend = F) +
  geom_text(aes(x = bite.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (863 Intervals)") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  theme(strip.background = element_blank())
dev.off()

### Animal Attack ----
# Distribution of co-occurring animal attack within Sickness age intervals
pdf(file = "Sickness Plots/co_occurrence4a.pdf", height = 5, width = 7.5)
df_long %>%
  count(animal.attack.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(animal.attack.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = animal.attack.co_occurrence.interval, y = prop), fill = "#7c9b99", width = 0.9, show.legend = F) +
  geom_text(aes(x = animal.attack.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and ANIMAL ATTACK") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (863 Intervals)")
dev.off()

# By gender
pdf(file = "Sickness Plots/co_occurrence4b.pdf", height = 5, width = 10)
df_long %>%
  count(animal.attack.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(animal.attack.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = animal.attack.co_occurrence.interval, y = prop, fill = male), width = 0.9, show.legend = F) +
  geom_text(aes(x = animal.attack.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and ANIMAL ATTACK") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (863 Intervals)") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  theme(strip.background = element_blank())
dev.off()

### Canoe Capsize ----
# Distribution of co-occurring canoe capsize within Sickness age intervals
pdf(file = "Sickness Plots/co_occurrence5a.pdf", height = 5, width = 7.5)
df_long %>%
  count(canoe.capsize.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(canoe.capsize.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = canoe.capsize.co_occurrence.interval, y = prop), fill = "#7c9b99", width = 0.9, show.legend = F) +
  geom_text(aes(x = canoe.capsize.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (863 Intervals)")
dev.off()

# By gender
pdf(file = "Sickness Plots/co_occurrence5b.pdf", height = 5, width = 10)
df_long %>%
  count(canoe.capsize.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(canoe.capsize.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = canoe.capsize.co_occurrence.interval, y = prop, fill = male), width = 0.9, show.legend = F) +
  geom_text(aes(x = canoe.capsize.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .0001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (863 Intervals)") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  theme(strip.background = element_blank())
dev.off()

### Cut Self ----
# Distribution of co-occurring cut self within Sickness age intervals
pdf(file = "Sickness Plots/co_occurrence6a.pdf", height = 5, width = 7)
df_long %>%
  count(cut.self.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(cut.self.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = cut.self.co_occurrence.interval, y = prop), fill = "#7c9b99", width = 0.9, show.legend = F) +
  geom_text(aes(x = cut.self.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and CUT SELF") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (863 Intervals)")
dev.off()

# By gender
pdf(file = "Sickness Plots/co_occurrence6b.pdf", height = 5, width = 10)
df_long %>%
  count(cut.self.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(cut.self.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = cut.self.co_occurrence.interval, y = prop, fill = male), width = 0.9, show.legend = F) +
  geom_text(aes(x = cut.self.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .0005, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and CUT SELF") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (863 Intervals)") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  theme(strip.background = element_blank())
dev.off()

### ANIMAL ATTACK (c) ----
# Distribution of co-occurring ANIMAL ATTACK (c) within Sickness age intervals
pdf(file = "Sickness Plots/co_occurrence7a.pdf", height = 5, width = 7)
df_long %>%
  count(Animal_Attack.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(Animal_Attack.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = Animal_Attack.co_occurrence.interval, y = prop), fill = "#7c9b99", width = 0.9, show.legend = F) +
  geom_text(aes(x = Animal_Attack.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and
  ANIMAL ATTACK (c)") +
  theme(plot.title = element_text(size = 25)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (863 Intervals)")
dev.off()

# By gender
pdf(file = "Sickness Plots/co_occurrence7b.pdf", height = 5, width = 10)
df_long %>%
  count(Animal_Attack.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(Animal_Attack.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = Animal_Attack.co_occurrence.interval, y = prop, fill = male), width = 0.9, show.legend = F) +
  geom_text(aes(x = Animal_Attack.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .0005, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  ggtitle("SICKNESS and
  ANIMAL ATTACK (c)") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals (863 Intervals)") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  theme(strip.background = element_blank())
dev.off()

## Percentage co-occurrence of risks ----

# Changing the 0's and 1's of the variables for the plots to be more understandable
df_long$event <- ifelse(df_long$event == 1, "Sickness
Occurred", "Sickness Did
Not Occur")
df_long$tree.fall.during.interval <- ifelse(df_long$tree.fall.during.interval == 1, "Tree Fall
Occured", "Tree Fall Did
Not Occur")
df_long$fought.during.interval <- ifelse(df_long$fought.during.interval == 1, "Fight
Occured", "Fight Did
Not Occur")
df_long$bite.during.interval <- ifelse(df_long$bite.during.interval == 1, "Snake/Ray Bite
Occured", "Snake/Ray Bite
Did Not Occur")
df_long$animal.attack.during.interval <- ifelse(df_long$animal.attack.during.interval == 1, "Animal Attack
Occured", "Animal Attack Did
Not Occur")
df_long$canoe.capsize.during.interval <- ifelse(df_long$canoe.capsize.during.interval == 1, "Canoe Capsize
Occured", "Canoe Capsize
Did Not Occur")
df_long$cut.self.during.interval <- ifelse(df_long$cut.self.during.interval == 1, "Cut Self
Occured", "Cut Self Did
Not Occur")
df_long$Animal_Attack.during.interval <- ifelse(df_long$Animal_Attack.during.interval == 1, "Animal Attack (c)
Occured", "Animal Attack (c)
Did Not Occur")

### Tree Fall ----
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which tree fall occurs
pdf(file = "Sickness Plots/co_occurrence1c.pdf", height = 4, width = 6.5)
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
  ggtitle("SICKNESS and TREE FALL") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Sickness Plots/co_occurrence1d.pdf", height = 4, width = 7)
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
  ggtitle("SICKNESS and TREE FALL") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Sickness Plots/co_occurrence1e.pdf", height = 4.5, width = 7)
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
  ggtitle("SICKNESS and TREE FALL") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  labs(fill = "") +
  facet_wrap(~region) + theme(strip.text.x = element_text(size = 15)) +
  theme(strip.background = element_blank()) +
  guides(x =  guide_axis(angle = 90))
dev.off()

### Fight ----
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which fight occurs
pdf(file = "Sickness Plots/co_occurrence2c.pdf", height = 4, width = 6.5)
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
  ggtitle("SICKNESS and FIGHT") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Sickness Plots/co_occurrence2d.pdf", height = 4, width = 7)
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
  ggtitle("SICKNESS and FIGHT") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Sickness Plots/co_occurrence2e.pdf", height = 4, width = 7.5)
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
  ggtitle("SICKNESS and FIGHT") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~region) + theme(strip.text.x = element_text(size = 15)) +
  theme(strip.background = element_blank()) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  guides(x =  guide_axis(angle = 90))
dev.off()

### Snake/Ray Bite ----
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which Snake/Ray Bite
pdf(file = "Sickness Plots/co_occurrence3c.pdf", height = 4, width = 6)
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
  ggtitle("SICKNESS and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Sickness Plots/co_occurrence3d.pdf", height = 4, width = 7)
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
  ggtitle("SICKNESS and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Sickness Plots/co_occurrence3e.pdf", height = 4, width = 7)
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
  ggtitle("SICKNESS and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  labs(fill = "") +
  facet_wrap(~region) + theme(strip.text.x = element_text(size = 15)) +
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
  theme_classic(base_size = 12) +
  ggtitle("SICKNESS and 
      ANIMAL ATTACK") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Sickness Plots/co_occurrence4d.pdf", height = 4, width = 7.5)
df_long %>%
  count(animal.attack.during.interval, event, male) %>%
  group_by(animal.attack.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(animal.attack.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("SICKNESS and ANIMAL ATTACK") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Sickness Plots/co_occurrence4e.pdf", height = 4.5, width = 7)
df_long %>%
  count(animal.attack.during.interval, event, region) %>%
  group_by(animal.attack.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(animal.attack.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("SICKNESS and ANIMAL ATTACK") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~region) + theme(strip.text.x = element_text(size = 15)) +
  theme(strip.background = element_blank()) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  guides(x =  guide_axis(angle = 90))
dev.off()

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
  theme_classic(base_size = 12) +
  ggtitle("SICKNESS and 
      CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Sickness Plots/co_occurrence5d.pdf", height = 4, width = 8)
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
  ggtitle("SICKNESS and CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Sickness Plots/co_occurrence5e.pdf", height = 4.5, width = 7.5)
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
  ggtitle("SICKNESS and CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~region) + theme(strip.text.x = element_text(size = 15)) +
  theme(strip.background = element_blank()) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
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
  theme_classic(base_size = 12) +
  ggtitle("SICKNESS and CUT SELF") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Sickness Plots/co_occurrence6d.pdf", height = 4, width = 7)
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
  ggtitle("SICKNESS and CUT SELF") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Sickness Plots/co_occurrence6e.pdf", height = 4.5, width = 7)
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
  ggtitle("SICKNESS and CUT SELF") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~region) + theme(strip.text.x = element_text(size = 15)) + theme(strip.text.x = element_text(size = 15)) +
  theme(strip.background = element_blank()) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  guides(x =  guide_axis(angle = 90))
dev.off()

### ANIMAL ATTACK (c) ----
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which ANIMAL ATTACK (c)
pdf(file = "Sickness Plots/co_occurrence7c.pdf", height = 4, width = 6)
df_long %>%
  count(Animal_Attack.during.interval, event) %>%
  group_by(Animal_Attack.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(Animal_Attack.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("SICKNESS and 
  ANIMAL ATTACK (c)") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  labs(fill = "")
dev.off()

# For males and females
pdf(file = "Sickness Plots/co_occurrence7d.pdf", height = 4, width = 7)
df_long %>%
  count(Animal_Attack.during.interval, event, male) %>%
  group_by(Animal_Attack.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(Animal_Attack.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("SICKNESS and
  ANIMAL ATTACK (c)") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~male) + theme(strip.text.x = element_text(size = 20)) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  theme(strip.background = element_blank())
dev.off()

# By region
pdf(file = "Sickness Plots/co_occurrence7e.pdf", height = 4.5, width = 7)
df_long %>%
  count(Animal_Attack.during.interval, event, region) %>%
  group_by(Animal_Attack.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(Animal_Attack.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  ggtitle("SICKNESS and
  ANIMAL ATTACK (c)") +
  theme(plot.title = element_text(size = 25)) +
  xlab("") +
  ylab("Percentage of Intervals (863 Intervals)") +
  labs(fill = "") +
  facet_wrap(~region) + theme(strip.text.x = element_text(size = 15)) + theme(strip.text.x = element_text(size = 15)) +
  theme(strip.background = element_blank()) +
  scale_fill_manual(values = c("lightcyan2", "lightcoral")) +
  guides(x = guide_axis(angle = 90))
dev.off()

## Survival Function ----
fit <- survfit(Surv(enter, exit, sickness.during.interval) ~ 1, data = df)
pdf(file = "Sickness Plots/survival_function.pdf", height = 5, width = 7)
autoplot(fit, censor.shape = '|', censor.colour = "darkgoldenrod3",
         surv.colour = "darkgoldenrod3") +
  theme_classic(base_size = 14) +
  ggtitle("SICKNESS") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age in years") +
  ylab("Proportion of individuals not experienced risk") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  labs(subtitle = "388 Individuals, 13,451 Intervals")
dev.off()


# Survival risk table
fit <- survfit(Surv(enter, exit, event) ~ 1, df_first)
gg <- ggsurvtable(fit, break.time.by = 10, data = df_first)
gg <- data.frame(gg[["risk.table"]][["data"]])
gg <- gg[c("time", "n.risk", "cum.n.event", "pct.risk")]
gg$pct.risk <- gg$pct.risk / 100
gg <- gg %>% rename("Age (Years)" = "time",
                    "No. at Risk" = "n.risk",
                    "No. of Events" = "cum.n.event",
                    "Proportion at Risk" = "pct.risk")
stargazer(gg, summary = F, out = "Sickness Tables/risk_table.tex",
          title = "Sickness \\vspace{-1.4em}", rownames = F,
          digits = 2)

# By gender
fit2 <- survfit(Surv(enter, exit, sickness.during.interval) ~ male, data = df)
pdf(file = "Sickness Plots/survival_function_by_gender.pdf", height = 5)
ggsurvplot(fit2, font.title = c("30"), conf.int = TRUE, legend = "right",
           surv.scale = "percent", legend.labs = c("Female", "Male"),
           legend.title = "Sex", palette = c("orchid2", "dodgerblue2"),
           title = "SICKNESS", axes.offset = F, break.x.by = 5,
           subtitle = "388 Individuals, 13,451 Intervals") +
  xlab("Age in years") +
  ylab("Proportion of individuals not experienced risk")
dev.off()

## Hazard Function ----
### bshazard ----
fit <- bshazard(Surv(enter, exit, sickness.during.interval) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
pdf(file = "Sickness Plots/hazard_function1.pdf", height = 5)
ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "darkgoldenrod3") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "darkgoldenrod3", color = "darkgoldenrod3") +
  theme_classic() +
  xlab("Age in Years") +
  ylab("Probability of experiencing risk") +
  ggtitle("SICKNESS") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(subtitle = "388 Individuals, 13,451 Intervals")
dev.off()

# Hazard table
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df_first)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv <- round(df_surv, 3) # here we understand hazards begin from age 1 onward
df_surv$time <- plyr::round_any(df_surv$time, 10, f = floor)
df_surv <- subset(df_surv, subset = time %in% c(10, 20, 30, 40, 50, 60, 70))
df_surv <- distinct(df_surv, time, .keep_all = T)
df_surv <- df_surv %>% add_row(time = 0, hazard = NA, lower.ci = NA, upper.ci = NA, .before = 1)
df_surv <- df_surv %>% rename("Age (Years)" = "time",
                              "Hazard" = "hazard",
                              "Lower CI" = "lower.ci",
                              "Upper CI" = "upper.ci")
stargazer(df_surv, summary = F, out = "Sickness Tables/hazard_table.tex",
          title = "Sickness \\vspace{-1.4em}", rownames = F,
          notes = "\\tiny Note: Sickness hazard begins from the age interval 0-1 onward.")

### muhaz ----
pdf(file = "Sickness Plots/hazard_function2.pdf", height = 5)
mfit <- muhaz(df$exit, df$sickness.during.interval)
plot(mfit, main = "SICKNESS")
dev.off()

pdf(file = "Sickness Plots/hazard_function3.pdf", height = 5)
kfit <- kphaz.fit(df$exit, df$sickness.during.interval)
kphaz.plot(kfit, main = "SICKNESS")
dev.off()

# By gender
fit <- bshazard(Surv(enter, exit, sickness.during.interval) ~ 1, data = subset(df, male == "Male"))
summary(fit)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"

fit2 <- bshazard(Surv(enter, exit, sickness.during.interval) ~ 1, data = subset(df, male == "Female"))
summary(fit2)
df_surv2 <- data.frame(time = fit2$time, hazard = fit2$hazard,
                       lower.ci = fit2$lower.ci, upper.ci = fit2$upper.ci)
df_surv2$Sex <- "Female"

df_surv3 <- bind_rows(df_surv, df_surv2)

pdf(file = "Sickness Plots/hazard_function4.pdf", height = 5)
ggplot(df_surv3, aes(x = time, y = hazard, fill = Sex)) +
  geom_line(aes(color = Sex)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2) +
  theme_classic(base_size = 14) +
  xlab("Age in Years") +
  ylab("Probability of experiencing risk") +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(subtitle = "388 Individuals, 13,451 Intervals")
dev.off()

### Ratio of male hazard over female hazard ----
df_surv_male <- df_surv[c("time", "hazard")]
df_surv_male <- df_surv_male %>%  dplyr::rename("hazard_m" = "hazard")
df_surv_female <- df_surv2[c("time", "hazard")]
df_surv_female <- df_surv_female %>%  dplyr::rename("hazard_f" = "hazard")
df_surv4 <- left_join(df_surv_female, df_surv_male)
df_surv4$hazard_m_by_f <- df_surv4$hazard_m / df_surv4$hazard_f
df_surv4 <- subset(df_surv4, !is.na(df_surv4$hazard_m_by_f))
pdf(file = "Sickness Plots/hazard_function5.pdf", height = 5)
ggplot(df_surv4) +
  geom_line(aes(x = time, y = hazard_m_by_f), color = "darkgoldenrod4") +
  theme_classic(base_size = 14) +
  ggtitle("Sickness") +
  xlab("Age in Years") +
  ylab("Ratio of Instantaneous Risk (Male/Female)") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(subtitle = "388 Individuals, 13,451 Intervals") +
  geom_segment(aes(x = 0, y = 1, xend = 75, yend = 1), lty = 2, col = "lavender")
dev.off()

## Cumulative hazard function ----
fit <- survfit(Surv(enter, exit, sickness.during.interval) ~ 1, data = df)
pdf(file = "Sickness Plots/cumhaz_plot1.pdf", height = 5)
autoplot(fit, censor.shape = '|', censor.colour = "lightgreen",
         surv.colour = "lightgreen", fun = "cumhaz") +
  theme_classic(base_size = 14) +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age in years") +
  ylab("Cumulative hazard") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(subtitle = "388 Individuals, 13,451 Intervals")
dev.off()

# By gender
fit2 <- survfit(Surv(enter, exit, sickness.during.interval) ~ male, data = df)
pdf(file = "Sickness Plots/cumhaz_plot2.pdf", height = 5)
ggsurvplot(fit2, font.title = c("30"), conf.int = TRUE, legend = "right",
           legend.labs = c("Female", "Male"),
           legend.title = "Sex", palette = c("orchid2", "dodgerblue2"),
           title = "Sickness", fun = "cumhaz", axes.offset = F, break.x.by = 5,
           subtitle = "388 Individuals, 13,451 Intervals") +
  xlab("Age in years") +
  ylab("Cumulative hazard")
dev.off()
