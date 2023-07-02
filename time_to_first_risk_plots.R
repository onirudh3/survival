
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(survival)
library(bshazard)




# Tree Fall ---------------------------------------------------------------

df <- read.csv("tree_fall_time_to_first_risk_long_interval.csv")

df$male <- as.factor(df$male)

## Survival Curves ----
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
                             "Male, Forest", "Male, Near San Borja", "Male, Upriver")) +
  # Make the above line into comment and run the plot to verify if labels are correct
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))

# Hazard Curves ----
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

# Plot hazard by gender
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = male)) + geom_line(aes(col = male)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = male), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30)) +
  scale_color_hue(labels = c("Female", "Male")) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Ratio of male hazard over female hazard
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == 1))
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"

fit2 <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == 0))
df_surv2 <- data.frame(time = fit2$time, hazard = fit2$hazard,
                       lower.ci = fit2$lower.ci, upper.ci = fit2$upper.ci)
df_surv2$Sex <- "Female"

df_surv_male <- df_surv[c("time", "hazard")]
df_surv_male <- df_surv_male %>%  dplyr::rename("hazard_m" = "hazard")
df_surv_female <- df_surv2[c("time", "hazard")]
df_surv_female <- df_surv_female %>%  dplyr::rename("hazard_f" = "hazard")
df_surv3 <- left_join(df_surv_female, df_surv_male)
df_surv3$hazard_m_by_f <- df_surv3$hazard_m / df_surv3$hazard_f
df_surv3 <- subset(df_surv3, !is.na(df_surv3$hazard_m_by_f))
ggplot(df_surv3) +
  geom_line(aes(x = time, y = hazard_m_by_f), color = "green") +
  theme_classic(base_size = 14) +
  ggtitle("Tree Fall") +
  xlab("Age in Years") +
  ylab("Ratio of Hazards (Male/Female)") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_segment(aes(x = 0, y = 1, xend = 75, yend = 1), lty = 2, col = "lavender")

# Plot hazard by region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = region)) + geom_line(aes(col = region)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = region), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Tree Fall") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Plot hazard by sex and region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = interaction(male, region))) +
  geom_line(aes(col = interaction(male, region))) +
  # geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = interaction(male, region)), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Tree Fall") +
  guides(fill = F) +
  scale_color_hue(labels = c("Female, Forest", "Male, Forest", "Female, Near San Borja",
                             "Male, Near San Borja", "Female, Upriver", "Male, Upriver")) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.1))




# Snake/Ray Bite ----------------------------------------------------------

df <- read.csv("snake_ray_bite_time_to_first_risk_long_interval.csv")

df$male <- as.factor(df$male)

## Survival Curves ----
# null model
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
summary(m1)
autoplot(m1, censor.shape = '|', censor.colour = "lightseagreen",
         surv.colour = "lightseagreen") +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  ggtitle("Snake/Ray Bite") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced snake/ray bite") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Male covariate
m1a <- survfit(Surv(exit, event) ~ male, data = df, conf.type = "log-log")
summary(m1a)
autoplot(m1a) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced snake/ray bite") +
  scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Snake/Ray Bite") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Region covariate
m1b <- survfit(Surv(exit, event) ~ region, data = df, conf.type = "log-log")
summary(m1b)
autoplot(m1b) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced snake/ray bite") +
  # scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Snake/Ray Bite") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Male and region covariates
m1c <- survfit(Surv(exit, event) ~ male + region, data = df, conf.type = "log-log")
summary(m1c)
autoplot(m1c) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced snake/ray bite") +
  scale_color_hue(labels = c("Female, Forest", "Female, Near San Borja", "Female, Upriver",
                             "Male, Forest", "Male, Near San Borja", "Male, Upriver")) +
  # Make the above line into comment and run the plot to verify if labels are correct
  ggtitle("Snake/Ray Bite") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))

# Hazard Curves ----
# Plot hazard function of null model
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "lightseagreen") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "lightseagreen") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Snake/Ray Bite") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Plot hazard by gender
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = male)) + geom_line(aes(col = male)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = male), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Snake/Ray Bite") +
  theme(plot.title = element_text(size = 30)) +
  scale_color_hue(labels = c("Female", "Male")) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Ratio of male hazard over female hazard
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == 1))
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"

fit2 <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == 0))
df_surv2 <- data.frame(time = fit2$time, hazard = fit2$hazard,
                       lower.ci = fit2$lower.ci, upper.ci = fit2$upper.ci)
df_surv2$Sex <- "Female"

df_surv_male <- df_surv[c("time", "hazard")]
df_surv_male <- df_surv_male %>%  dplyr::rename("hazard_m" = "hazard")
df_surv_female <- df_surv2[c("time", "hazard")]
df_surv_female <- df_surv_female %>%  dplyr::rename("hazard_f" = "hazard")
df_surv3 <- left_join(df_surv_female, df_surv_male)
df_surv3$hazard_m_by_f <- df_surv3$hazard_m / df_surv3$hazard_f
df_surv3 <- subset(df_surv3, !is.na(df_surv3$hazard_m_by_f))
ggplot(df_surv3) +
  geom_line(aes(x = time, y = hazard_m_by_f), color = "lightseagreen") +
  theme_classic(base_size = 14) +
  ggtitle("Snake/Ray Bite") +
  xlab("Age in Years") +
  ylab("Ratio of Hazards (Male/Female)") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_segment(aes(x = 0, y = 1, xend = 75, yend = 1), lty = 2, col = "lavender")

# Plot hazard by region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = region)) + geom_line(aes(col = region)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = region), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Snake/Ray Bite") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Plot hazard by sex and region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = interaction(male, region))) +
  geom_line(aes(col = interaction(male, region))) +
  # geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = interaction(male, region)), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Snake/Ray Bite") +
  guides(fill = F) +
  scale_color_hue(labels = c("Female, Forest", "Male, Forest", "Female, Near San Borja",
                             "Male, Near San Borja", "Female, Upriver", "Male, Upriver")) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.1))





# Fight -------------------------------------------------------------------

df <- read.csv("fought_time_to_first_risk_long_interval.csv")

df$male <- as.factor(df$male)

## Survival Curves (Kaplan-Meier estimator) ----
# null model
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
summary(m1)
autoplot(m1, censor.shape = '|', censor.colour = "turquoise2",
         surv.colour = "turquoise2") +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced fight") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Male covariate
m1a <- survfit(Surv(exit, event) ~ male, data = df, conf.type = "log-log")
summary(m1a)
autoplot(m1a) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced fight") +
  scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Region covariate
m1b <- survfit(Surv(exit, event) ~ region, data = df, conf.type = "log-log")
summary(m1b)
autoplot(m1b) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced fight") +
  # scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Male and region covariates
m1c <- survfit(Surv(exit, event) ~ male + region, data = df, conf.type = "log-log")
summary(m1c)
autoplot(m1c) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced fight") +
  scale_color_hue(labels = c("Female, Forest", "Female, Near San Borja", "Female, Upriver",
                             "Male, Forest", "Male, Near San Borja", "Male, Upriver")) +
  # Make the above line into comment and run the plot to verify if labels are correct
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))

# Hazard Curves ----
# Plot hazard function of null model
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "turquoise2") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "turquoise2") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Plot hazard by gender
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = male)) + geom_line(aes(col = male)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = male), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30)) +
  scale_color_hue(labels = c("Female", "Male")) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Ratio of male hazard over female hazard
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == 1))
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"

fit2 <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == 0))
df_surv2 <- data.frame(time = fit2$time, hazard = fit2$hazard,
                       lower.ci = fit2$lower.ci, upper.ci = fit2$upper.ci)
df_surv2$Sex <- "Female"

df_surv_male <- df_surv[c("time", "hazard")]
df_surv_male <- df_surv_male %>%  dplyr::rename("hazard_m" = "hazard")
df_surv_female <- df_surv2[c("time", "hazard")]
df_surv_female <- df_surv_female %>%  dplyr::rename("hazard_f" = "hazard")
df_surv3 <- left_join(df_surv_female, df_surv_male)
df_surv3$hazard_m_by_f <- df_surv3$hazard_m / df_surv3$hazard_f
df_surv3 <- subset(df_surv3, !is.na(df_surv3$hazard_m_by_f))
ggplot(df_surv3) +
  geom_line(aes(x = time, y = hazard_m_by_f), color = "turquoise2") +
  theme_classic(base_size = 14) +
  ggtitle("Fight") +
  xlab("Age in Years") +
  ylab("Ratio of Hazards (Male/Female)") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_segment(aes(x = 0, y = 1, xend = 75, yend = 1), lty = 2, col = "lavender")

# Plot hazard by region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = region)) + geom_line(aes(col = region)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = region), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Fight") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Plot hazard by sex and region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = interaction(male, region))) +
  geom_line(aes(col = interaction(male, region))) +
  # geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = interaction(male, region)), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Fight") +
  guides(fill = F) +
  scale_color_hue(labels = c("Female, Forest", "Male, Forest", "Female, Near San Borja",
                             "Male, Near San Borja", "Female, Upriver", "Male, Upriver")) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))





# Sickness -------------------------------------------------------------------

df <- read.csv("sickness_time_to_first_risk_long_interval.csv")

df$male <- as.factor(df$male)

## Survival Curves (Kaplan-Meier estimator) ----
# null model
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
summary(m1)
autoplot(m1, censor.shape = '|', censor.colour = "goldenrod2",
         surv.colour = "goldenrod2") +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced sickness") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Male covariate
m1a <- survfit(Surv(exit, event) ~ male, data = df, conf.type = "log-log")
summary(m1a)
autoplot(m1a) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced sickness") +
  scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Region covariate
m1b <- survfit(Surv(exit, event) ~ region, data = df, conf.type = "log-log")
summary(m1b)
autoplot(m1b) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced sickness") +
  # scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Male and region covariates
m1c <- survfit(Surv(exit, event) ~ male + region, data = df, conf.type = "log-log")
summary(m1c)
autoplot(m1c) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced sickness") +
  scale_color_hue(labels = c("Female, Forest", "Female, Near San Borja", "Female, Upriver",
                             "Male, Forest", "Male, Near San Borja", "Male, Upriver")) +
  # Make the above line into comment and run the plot to verify if labels are correct
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))

# Hazard Curves ----
# Plot hazard function of null model
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "goldenrod2") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "goldenrod2") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Plot hazard by gender
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = male)) + geom_line(aes(col = male)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = male), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30)) +
  scale_color_hue(labels = c("Female", "Male")) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Ratio of male hazard over female hazard
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == 1))
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"

fit2 <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == 0))
df_surv2 <- data.frame(time = fit2$time, hazard = fit2$hazard,
                       lower.ci = fit2$lower.ci, upper.ci = fit2$upper.ci)
df_surv2$Sex <- "Female"

df_surv_male <- df_surv[c("time", "hazard")]
df_surv_male <- df_surv_male %>%  dplyr::rename("hazard_m" = "hazard")
df_surv_female <- df_surv2[c("time", "hazard")]
df_surv_female <- df_surv_female %>%  dplyr::rename("hazard_f" = "hazard")
df_surv3 <- left_join(df_surv_female, df_surv_male)
df_surv3$hazard_m_by_f <- df_surv3$hazard_m / df_surv3$hazard_f
df_surv3 <- subset(df_surv3, !is.na(df_surv3$hazard_m_by_f))
ggplot(df_surv3) +
  geom_line(aes(x = time, y = hazard_m_by_f), color = "goldenrod2") +
  theme_classic(base_size = 14) +
  ggtitle("Sickness") +
  xlab("Age in Years") +
  ylab("Ratio of Hazards (Male/Female)") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_segment(aes(x = 0, y = 1, xend = 75, yend = 1), lty = 2, col = "lavender")

# Plot hazard by region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = region)) + geom_line(aes(col = region)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = region), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Sickness") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Plot hazard by sex and region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = interaction(male, region))) +
  geom_line(aes(col = interaction(male, region))) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = interaction(male, region)), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Sickness") +
  guides(fill = F) +
  scale_color_hue(labels = c("Female, Forest", "Male, Forest", "Female, Near San Borja",
                             "Male, Near San Borja", "Female, Upriver", "Male, Upriver")) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))


# Animal Attack -----------------------------------------------------------

df <- read.csv("animal_attack_time_to_first_risk_long_interval.csv")

df$male <- as.factor(df$male)

## Survival Curves (Kaplan-Meier estimator) ----
# null model
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
summary(m1)
autoplot(m1, censor.shape = '|', censor.colour = "pink4",
         surv.colour = "pink4") +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced animal attack") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Male covariate
m1a <- survfit(Surv(exit, event) ~ male, data = df, conf.type = "log-log")
summary(m1a)
autoplot(m1a) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced animal attack") +
  scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Region covariate
m1b <- survfit(Surv(exit, event) ~ region, data = df, conf.type = "log-log")
summary(m1b)
autoplot(m1b) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced animal attack") +
  # scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Male and region covariates
m1c <- survfit(Surv(exit, event) ~ male + region, data = df, conf.type = "log-log")
summary(m1c)
autoplot(m1c) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced animal attack") +
  scale_color_hue(labels = c("Female, Forest", "Female, Near San Borja", "Female, Upriver",
                             "Male, Forest", "Male, Near San Borja", "Male, Upriver")) +
  # Make the above line into comment and run the plot to verify if labels are correct
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))

# Hazard Curves ----
# Plot hazard function of null model
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "pink4") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "pink4") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Plot hazard by gender
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = male)) + geom_line(aes(col = male)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = male), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30)) +
  scale_color_hue(labels = c("Female", "Male")) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Ratio of male hazard over female hazard
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == 1))
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"
fit2 <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == 0))
df_surv2 <- data.frame(time = fit2$time, hazard = fit2$hazard,
                       lower.ci = fit2$lower.ci, upper.ci = fit2$upper.ci)
df_surv2$Sex <- "Female"
df_surv_male <- df_surv[c("time", "hazard")]
df_surv_male <- df_surv_male %>%  dplyr::rename("hazard_m" = "hazard")
df_surv_female <- df_surv2[c("time", "hazard")]
df_surv_female <- df_surv_female %>%  dplyr::rename("hazard_f" = "hazard")
df_surv3 <- left_join(df_surv_female, df_surv_male)
df_surv3$hazard_m_by_f <- df_surv3$hazard_m / df_surv3$hazard_f
df_surv3 <- subset(df_surv3, !is.na(df_surv3$hazard_m_by_f))
ggplot(df_surv3) +
  geom_line(aes(x = time, y = hazard_m_by_f), color = "pink4") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack") +
  xlab("Age in Years") +
  ylab("Ratio of Hazards (Male/Female)") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_segment(aes(x = 0, y = 1, xend = 75, yend = 1), lty = 2, col = "lavender")

# Plot hazard by region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = region)) + geom_line(aes(col = region)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = region), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Plot hazard by sex and region ERROR SHOWN
# as.data.frame.bshazard <- function(x, ...) {
#   with(x, data.frame(time, hazard, lower.ci, upper.ci))
# }
# hazards <- group_by(df, male, region) %>%
#   do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
#   ungroup()
# ggplot(hazards, aes(x = time, y = hazard, group = interaction(male, region))) +
#   geom_line(aes(col = interaction(male, region))) +
#   # geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = interaction(male, region)), alpha = 0.3) +
#   labs(color = "", x = "Age [years]", y = "Hazard") +
#   theme_classic(base_size = 14) +
#   ggtitle("Animal Attack") +
#   guides(fill = F) +
#   scale_color_hue(labels = c("Female, Forest", "Male, Forest", "Female, Near San Borja",
#                              "Male, Near San Borja", "Female, Upriver", "Male, Upriver")) +
#   theme(plot.title = element_text(size = 30)) +
#   scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA))


# Canoe Capsize -----------------------------------------------------------

df <- read.csv("canoe_capsize_time_to_first_risk_long_interval.csv")

df$male <- as.factor(df$male)

## Survival Curves (Kaplan-Meier estimator) ----
# null model
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
summary(m1)
autoplot(m1, censor.shape = '|', censor.colour = "purple",
         surv.colour = "purple") +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced canoe capsize") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Male covariate
m1a <- survfit(Surv(exit, event) ~ male, data = df, conf.type = "log-log")
summary(m1a)
autoplot(m1a) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced canoe capsize") +
  scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Region covariate
m1b <- survfit(Surv(exit, event) ~ region, data = df, conf.type = "log-log")
summary(m1b)
autoplot(m1b) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced canoe capsize") +
  # scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Male and region covariates
m1c <- survfit(Surv(exit, event) ~ male + region, data = df, conf.type = "log-log")
summary(m1c)
autoplot(m1c) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced canoe capsize") +
  scale_color_hue(labels = c("Female, Forest", "Female, Near San Borja", "Female, Upriver",
                             "Male, Forest", "Male, Near San Borja", "Male, Upriver")) +
  # Make the above line into comment and run the plot to verify if labels are correct
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))

# Hazard Curves ----
# Plot hazard function of null model
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "purple") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "purple") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Plot hazard by gender
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = male)) + geom_line(aes(col = male)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = male), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30)) +
  scale_color_hue(labels = c("Female", "Male")) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Ratio of male hazard over female hazard
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == 1))
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"
fit2 <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == 0))
df_surv2 <- data.frame(time = fit2$time, hazard = fit2$hazard,
                       lower.ci = fit2$lower.ci, upper.ci = fit2$upper.ci)
df_surv2$Sex <- "Female"
df_surv_male <- df_surv[c("time", "hazard")]
df_surv_male <- df_surv_male %>%  dplyr::rename("hazard_m" = "hazard")
df_surv_female <- df_surv2[c("time", "hazard")]
df_surv_female <- df_surv_female %>%  dplyr::rename("hazard_f" = "hazard")
df_surv3 <- left_join(df_surv_female, df_surv_male)
df_surv3$hazard_m_by_f <- df_surv3$hazard_m / df_surv3$hazard_f
df_surv3 <- subset(df_surv3, !is.na(df_surv3$hazard_m_by_f))
ggplot(df_surv3) +
  geom_line(aes(x = time, y = hazard_m_by_f), color = "purple") +
  theme_classic(base_size = 14) +
  ggtitle("Canoe Capsize") +
  xlab("Age in Years") +
  ylab("Ratio of Hazards (Male/Female)") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_segment(aes(x = 0, y = 1, xend = 75, yend = 1), lty = 2, col = "lavender")

# Plot hazard by region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = region)) + geom_line(aes(col = region)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = region), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Canoe Capsize") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Plot hazard by sex and region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = interaction(male, region))) +
  geom_line(aes(col = interaction(male, region))) +
  # geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = interaction(male, region)), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Canoe Capsize") +
  guides(fill = F) +
  scale_color_hue(labels = c("Female, Forest", "Male, Forest", "Female, Near San Borja",
                             "Male, Near San Borja", "Female, Upriver", "Male, Upriver")) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))


# Cut Self ----------------------------------------------------------------

df <- read.csv("cut_self_time_to_first_risk_long_interval.csv")

df$male <- as.factor(df$male)

## Survival Curves (Kaplan-Meier estimator) ----
# null model
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
summary(m1)
autoplot(m1, censor.shape = '|', censor.colour = "orange",
         surv.colour = "orange") +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced cut self") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Male covariate
m1a <- survfit(Surv(exit, event) ~ male, data = df, conf.type = "log-log")
summary(m1a)
autoplot(m1a) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced cut self") +
  scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Region covariate
m1b <- survfit(Surv(exit, event) ~ region, data = df, conf.type = "log-log")
summary(m1b)
autoplot(m1b) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced cut self") +
  # scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Male and region covariates
m1c <- survfit(Surv(exit, event) ~ male + region, data = df, conf.type = "log-log")
summary(m1c)
autoplot(m1c) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced cut self") +
  scale_color_hue(labels = c("Female, Forest", "Female, Near San Borja", "Female, Upriver",
                             "Male, Forest", "Male, Near San Borja", "Male, Upriver")) +
  # Make the above line into comment and run the plot to verify if labels are correct
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))

# Hazard Curves ----
# Plot hazard function of null model
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "orange") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "orange") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Plot hazard by gender
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = male)) + geom_line(aes(col = male)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = male), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30)) +
  scale_color_hue(labels = c("Female", "Male")) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Ratio of male hazard over female hazard
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == 1))
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"
fit2 <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == 0))
df_surv2 <- data.frame(time = fit2$time, hazard = fit2$hazard,
                       lower.ci = fit2$lower.ci, upper.ci = fit2$upper.ci)
df_surv2$Sex <- "Female"
df_surv_male <- df_surv[c("time", "hazard")]
df_surv_male <- df_surv_male %>%  dplyr::rename("hazard_m" = "hazard")
df_surv_female <- df_surv2[c("time", "hazard")]
df_surv_female <- df_surv_female %>%  dplyr::rename("hazard_f" = "hazard")
df_surv3 <- left_join(df_surv_female, df_surv_male)
df_surv3$hazard_m_by_f <- df_surv3$hazard_m / df_surv3$hazard_f
df_surv3 <- subset(df_surv3, !is.na(df_surv3$hazard_m_by_f))
ggplot(df_surv3) +
  geom_line(aes(x = time, y = hazard_m_by_f), color = "orange") +
  theme_classic(base_size = 14) +
  ggtitle("Cut Self") +
  xlab("Age in Years") +
  ylab("Ratio of Hazards (Male/Female)") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_segment(aes(x = 0, y = 1, xend = 75, yend = 1), lty = 2, col = "lavender")

# Plot hazard by region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = region)) + geom_line(aes(col = region)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = region), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Cut Self") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Plot hazard by sex and region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = interaction(male, region))) +
  geom_line(aes(col = interaction(male, region))) +
  # geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = interaction(male, region)), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Cut Self") +
  guides(fill = F) +
  scale_color_hue(labels = c("Female, Forest", "Male, Forest", "Female, Near San Borja",
                             "Male, Near San Borja", "Female, Upriver", "Male, Upriver")) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))


# Cut Self ----------------------------------------------------------------

df <- read.csv("Animal_Attack_combined_time_to_first_risk_long_interval.csv")

df$male <- as.factor(df$male)

## Survival Curves (Kaplan-Meier estimator) ----
# null model
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
summary(m1)
autoplot(m1, censor.shape = '|', censor.colour = "lightcoral",
         surv.colour = "lightcoral") +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack (c)") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced animal attack (c)") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Male covariate
m1a <- survfit(Surv(exit, event) ~ male, data = df, conf.type = "log-log")
summary(m1a)
autoplot(m1a) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced animal attack (c)") +
  scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Animal Attack (c)") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Region covariate
m1b <- survfit(Surv(exit, event) ~ region, data = df, conf.type = "log-log")
summary(m1b)
autoplot(m1b) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced animal attack (c)") +
  # scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Animal Attack (c)") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Male and region covariates
m1c <- survfit(Surv(exit, event) ~ male + region, data = df, conf.type = "log-log")
summary(m1c)
autoplot(m1c) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced animal attack (c)") +
  scale_color_hue(labels = c("Female, Forest", "Female, Near San Borja", "Female, Upriver",
                             "Male, Forest", "Male, Near San Borja", "Male, Upriver")) +
  # Make the above line into comment and run the plot to verify if labels are correct
  ggtitle("Animal Attack (c)") +
  theme(plot.title = element_text(size = 30)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))

# Hazard Curves ----
# Plot hazard function of null model
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "lightcoral") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "lightcoral") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Animal Attack (c)") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Plot hazard by gender
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = male)) + geom_line(aes(col = male)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = male), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack (c)") +
  theme(plot.title = element_text(size = 30)) +
  scale_color_hue(labels = c("Female", "Male")) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Ratio of male hazard over female hazard
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == 1))
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"
fit2 <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == 0))
df_surv2 <- data.frame(time = fit2$time, hazard = fit2$hazard,
                       lower.ci = fit2$lower.ci, upper.ci = fit2$upper.ci)
df_surv2$Sex <- "Female"
df_surv_male <- df_surv[c("time", "hazard")]
df_surv_male <- df_surv_male %>%  dplyr::rename("hazard_m" = "hazard")
df_surv_female <- df_surv2[c("time", "hazard")]
df_surv_female <- df_surv_female %>%  dplyr::rename("hazard_f" = "hazard")
df_surv3 <- left_join(df_surv_female, df_surv_male)
df_surv3$hazard_m_by_f <- df_surv3$hazard_m / df_surv3$hazard_f
df_surv3 <- subset(df_surv3, !is.na(df_surv3$hazard_m_by_f))
ggplot(df_surv3) +
  geom_line(aes(x = time, y = hazard_m_by_f), color = "lightcoral") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack (c)") +
  xlab("Age in Years") +
  ylab("Ratio of Hazards (Male/Female)") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_segment(aes(x = 0, y = 1, xend = 75, yend = 1), lty = 2, col = "lavender")

# Plot hazard by region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = region)) + geom_line(aes(col = region)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = region), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack (c)") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Plot hazard by sex and region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
ggplot(hazards, aes(x = time, y = hazard, group = interaction(male, region))) +
  geom_line(aes(col = interaction(male, region))) +
  # geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = interaction(male, region)), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack (c)") +
  guides(fill = F) +
  scale_color_hue(labels = c("Female, Forest", "Male, Forest", "Female, Near San Borja",
                             "Male, Near San Borja", "Female, Upriver", "Male, Upriver")) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
