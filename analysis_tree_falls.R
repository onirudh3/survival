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

# Load data
df_final <- read.csv("treefall_final_table.csv")


################################################################################
############################ NON-PARAMETRICS ###################################
################################################################################

################### (Kaplan-Meier) Survival function ###########################

# with(df_final, plot(Surv(enter, exit, event),
#                     main = "TREE FALL",
#                     xlab = "Age", ylab = "Proportion of individuals not
#                                           experienced risk"))

# help(autoplot.survfit)
fit <- survfit(Surv(enter, exit, event) ~ 1, data = df_final) # survfit() for
# survival curves
p <- autoplot(fit, censor.shape = '|', censor.colour = "orange",
              surv.colour = "orange") +
  theme_classic() +
  ggtitle("TREE FALL") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age in years") +
  ylab("Proportion of individuals not experienced risk")
p
# View(layer_data(p, 1)) # 47.2% of individuals don't experience the risk of
# tree fall by age 75.08830

######################## SURVIVAL FUNCTION BY GENDER ###########################

# I'm not able to change the legend title from "strata" for some reason
# df_final$male <- ifelse(df_final$male == 1, "Male", "Female")
fit2 <- survfit(Surv(enter, exit, event) ~ male, data = df_final)
# autoplot(fit2, legTitle = "Gender") +
#   theme_classic() +
#   ggtitle("TREE FALL") +
#   theme(plot.title = element_text(size = 20)) +
#   xlab("Age in years") +
#   ylab("Proportion of individuals not experienced risk")

ggsurvplot(fit2, font.title = c("30"), conf.int = TRUE, legend = "right",
           surv.scale = "percent", legend.labs = c("Female", "Male"),
           legend.title = "Sex", palette = c("orchid2", "dodgerblue2"),
           title = "TREE FALL", ylim = c(0.4, 1)) +
  xlab("Age in years") +
  ylab("Proportion of individuals not experienced risk")


################ (Nelson-Aalen) Cumulative hazards function ####################

# with(df_final, plot(Surv(enter, exit, event), fun = "cumhaz",
#                     main = "Tree Fall (Non-parametric)",
#                     xlab = "Age", ylab = "Cumulative Hazard"))

autoplot(fit, fun = "cumhaz", censor.shape = '|', censor.colour = "orange",
         surv.colour = "orange") +
  theme_classic() +
  ggtitle("TREE FALL") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age in years") +
  ylab("Cumulative hazard")

# By gender
ggsurvplot(fit2, font.title = c("30"), conf.int = TRUE, legend = "right",
           legend.labs = c("Female", "Male"),
           legend.title = "Sex", palette = c("orchid2", "dodgerblue2"),
           title = "TREE FALL", fun = "cumhaz") +
  xlab("Age in years") +
  ylab("Cumulative hazard")

############################ HAZARD FUNCTION ###################################
fit <- bshazard(Surv(exit, event) ~ strata(male), data = df_final)
plot(fit)

df_surv <- data.frame(time = fit$time,
                      hazard = fit$hazard,
                      lower.ci = fit$lower.ci,
                      upper.ci = fit$upper.ci)
ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "darkgreen") +
  geom_segment(aes(x = 0, xend = max(time), y = 0, yend = 0),
               linetype = 2) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci),
              alpha = 0.2,
              fill = "darkgreen",
              color = "darkgreen") +
  theme_classic()

ff <- muhaz(df_final$exit, df_final$event)
plot(ff)



################################################################################
############################ PARAMETRICS #######################################
################################################################################
fit.w <- phreg(Surv(enter, exit, event) ~ 1, data = df_final)

# Survival function
plot(fit.w, fn = "sur", main = "TREE FALL", xlab = "Age in years",
     ylab = "NULL")

# Hazard function
plot(fit.w, fn = "haz", main = "TREE FALL", xlab = "Age in years",
     ylab = "Probability of experiencing risk")

# Cumulative hazards (parametric proportional hazards model with no covariates)
plot(fit.w, fn = "cum", main = "TREE FALL", xlab = "Age in years")

