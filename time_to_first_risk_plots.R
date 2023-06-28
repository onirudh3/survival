
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggfortify)


# Tree Fall ---------------------------------------------------------------

df <- read.csv("tree_fall_time_to_first_risk_long_interval.csv")

# SURVIVAL CURVES (Kaplan-Meier estimator)
# null model
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
summary(m1)
autoplot(m1, censor.shape = '|', censor.colour = "green3",
         surv.colour = "green3") +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced tree fall") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
# For Anirudh to do, change axis labels and scale; y-axis from 0-100, labeled
# 'Proportion not having experienced tree fall'; x-axis labeled 'Age [years]';
# add horizontal line at 50%; delete 'strata' from legend


# Plot hazard function of null model
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "green4") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "lightgreen") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))


# Male covariate
m1a <- survfit(Surv(exit, event) ~ male, data = df, conf.type = "log-log")
summary(m1a)
autoplot(m1a) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced tree fall") +
  scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
#For Anirudh to do, change axis labels and scale; y-axis from 0-100, labeled
# 'Proportion not having experienced tree fall'; x-axis labeled 'Age [years]';
# add horizontal line at 50%; delete 'strata' from legend


# Region covariate
m1b <- survfit(Surv(exit, event) ~ region, data = df, conf.type = "log-log")
summary(m1b)
autoplot(m1b) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced tree fall") +
  # scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
#For Anirudh to do, change axis labels and scale; y-axis from 0-100, labeled
# 'Proportion not having experienced tree fall'; x-axis labeled 'Age [years]';
# add horizontal line at 50%; delete 'strata' from legend


# Male and region covariates
m1c <- survfit(Surv(exit, event) ~ male + region, data = df, conf.type = "log-log")
summary(m1c)
autoplot(m1c) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced tree fall") +
  scale_color_hue(labels = c("Female, Forest", "Female, Near San Borja", "Female, Upriver",
                             "Male, Forest", "Male, Near San Borja", "Male, Upriver")) + # Make this line a comment to verify if labels are correct
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


