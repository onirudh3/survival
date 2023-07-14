
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(survival)
library(bshazard)
library(ggfortify)


# Tree Fall ---------------------------------------------------------------

df <- read.csv("tree_fall_time_to_first_risk_long_interval.csv")
df_short <- read.csv("tree_fall_time_to_first_risk_short_interval.csv")

df$male <- ifelse(df$male == 1, "Male", "Female")
df_short$male <- ifelse(df_short$male == 1, "Male", "Female")

df$male <- as.factor(df$male)
df_short$male <- as.factor(df_short$male)

## Descriptive Plots ----

### Long Interval ----
# Make age.cat as factor
df$age.cat <- factor(df$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))

# Line plot
pdf(file = "Tree Fall Plots/descriptive_plot4.pdf", height = 5, width = 7)
df %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "green3",
            linewidth = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Tree Fall") +
  labs(subtitle = "388 Individuals, 388 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")
dev.off()


# By gender
df_male <- subset(df, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df, male == "Female")
df_female_plot <- df_female %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1)
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


pdf(file = "Tree Fall Plots/descriptive_plot5.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Tree Fall") +
  labs(subtitle = "388 Individuals, 388 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.5))
dev.off()


### Short Interval ----
# Make age.cat as factor
df_short$age.cat <- factor(df_short$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))

# Line plot
pdf(file = "Tree Fall Plots/descriptive_plot6.pdf", height = 5, width = 7)
df_short %>%
  count(age.cat, tree.fall.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(tree.fall.during.interval == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "green3",
            linewidth = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Tree Fall") +
  labs(subtitle = "388 Individuals, 10,738 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")
dev.off()

# By gender
df_male <- subset(df_short, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, tree.fall.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(tree.fall.during.interval == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df_short, male == "Female")
df_female_plot <- df_female %>%
  count(age.cat, tree.fall.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(tree.fall.during.interval == 1)
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


pdf(file = "Tree Fall Plots/descriptive_plot7.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Tree Fall") +
  labs(subtitle = "388 Individuals, 10,738 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.5))
dev.off()



## Survival Curves ----
# null model
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
summary(m1)
pdf(file = "Tree Fall Plots/survival_function_time_to_first_risk.pdf", height = 5)
p <- autoplot(m1, censor.shape = '|', censor.colour = "orange2",
         surv.colour = "pink3") +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced tree fall") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Tree Fall Plots/survival_function_time_to_first_risk.RDS")
# For Anirudh to do, change axis labels and scale; y-axis from 0-100, labeled
# 'Proportion not having experienced tree fall'; x-axis labeled 'Age [years]';
# add horizontal line at 50%; delete 'strata' from legend


# Male covariate
m1a <- survfit(Surv(exit, event) ~ male, data = df, conf.type = "log-log")
summary(m1a)
pdf(file = "Tree Fall Plots/survival_function_time_to_first_risk_by_gender.pdf", height = 5)
p <- autoplot(m1a) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced tree fall") +
  scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Tree Fall Plots/survival_function_time_to_first_risk_by_gender.RDS")
#For Anirudh to do, change axis labels and scale; y-axis from 0-100, labeled
# 'Proportion not having experienced tree fall'; x-axis labeled 'Age [years]';
# add horizontal line at 50%; delete 'strata' from legend


# Region covariate
m1b <- survfit(Surv(exit, event) ~ region, data = df, conf.type = "log-log")
summary(m1b)
pdf(file = "Tree Fall Plots/survival_function_time_to_first_risk_by_region.pdf", height = 5)
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
dev.off()
#For Anirudh to do, change axis labels and scale; y-axis from 0-100, labeled
# 'Proportion not having experienced tree fall'; x-axis labeled 'Age [years]';
# add horizontal line at 50%; delete 'strata' from legend


# Male and region covariates
m1c <- survfit(Surv(exit, event) ~ male + region, data = df, conf.type = "log-log")
summary(m1c)
pdf(file = "Tree Fall Plots/survival_function_time_to_first_risk_by_gender_and_region.pdf", height = 5)
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
dev.off()

# Hazard Curves ----
# Plot hazard function of null model
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
pdf(file = "Tree Fall Plots/hazard_function_time_to_first_risk.pdf", height = 5)
p <- ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "orange2") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "pink3") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.15))
p
dev.off()
saveRDS(p, file = "Tree Fall Plots/hazard_function_time_to_first_risk.RDS")

# Plot hazard by gender
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Tree Fall Plots/hazard_function_time_to_first_risk_by_gender.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = male)) + geom_line(aes(col = male)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = male), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_color_hue(labels = c("Female", "Male")) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.28))
p
dev.off()
saveRDS(p, file = "Tree Fall Plots/hazard_function_time_to_first_risk_by_gender.RDS")

# Ratio of male hazard over female hazard
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == "Male"))
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"

fit2 <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == "Female"))
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
pdf(file = "Tree Fall Plots/hazard_ratio.pdf", height = 5)
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
dev.off()

# Plot hazard by region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Tree Fall Plots/hazard_function_time_to_first_risk_by_region.pdf", height = 5)
ggplot(hazards, aes(x = time, y = hazard, group = region)) + geom_line(aes(col = region)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = region), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Tree Fall") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
dev.off()

# Plot hazard by sex and region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Tree Fall Plots/hazard_function_time_to_first_risk_by_gender_and_region.pdf", height = 5)
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
dev.off()



# Snake/Ray Bite ----------------------------------------------------------

df <- read.csv("snake_ray_bite_time_to_first_risk_long_interval.csv")
df_short <- read.csv("snake_ray_bite_time_to_first_risk_short_interval.csv")

df$male <- ifelse(df$male == 1, "Male", "Female")
df_short$male <- ifelse(df_short$male == 1, "Male", "Female")

df$male <- as.factor(df$male)
df_short$male <- as.factor(df_short$male)

## Descriptive Plots ----

### Long Interval ----
# Make age.cat as factor
df$age.cat <- factor(df$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))

# Line plot
pdf(file = "Snake Bite Plots/descriptive_plot4.pdf", height = 5, width = 7)
df %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "lightseagreen",
            linewidth = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Snake/Ray Bite") +
  labs(subtitle = "388 Individuals, 388 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")
dev.off()


# By gender
df_male <- subset(df, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df, male == "Female")
df_female_plot <- df_female %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1)
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


pdf(file = "Snake Bite Plots/descriptive_plot5.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Snake/Ray Bite") +
  labs(subtitle = "388 Individuals, 388 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.5))
dev.off()


### Short Interval ----
# Make age.cat as factor
df_short$age.cat <- factor(df_short$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                                        "20-25", "25-30", "30-35", "35-40",
                                                        "40-45", "45-50", "50-55", "55-60",
                                                        "60+"))

# Line plot
pdf(file = "Snake Bite Plots/descriptive_plot6.pdf", height = 5, width = 7)
df_short %>%
  count(age.cat, bite.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(bite.during.interval == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "lightseagreen",
            linewidth = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Snake/Ray Bite") +
  labs(subtitle = "388 Individuals, 10,681 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")
dev.off()

# By gender
df_male <- subset(df_short, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, bite.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(bite.during.interval == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df_short, male == "Female")
df_female_plot <- df_female %>%
  count(age.cat, bite.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(bite.during.interval == 1)
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


pdf(file = "Snake Bite Plots/descriptive_plot7.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Snake/Ray Bite") +
  labs(subtitle = "388 Individuals, 10,681 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.5))
dev.off()

## Survival Curves ----
# null model
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
summary(m1)
pdf(file = "Snake Bite Plots/survival_function_time_to_first_risk.pdf", height = 5)
p <- autoplot(m1, censor.shape = '|', censor.colour = "orange2",
         surv.colour = "pink3") +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  ggtitle("Snake/Ray Bite") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced snake/ray bite") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Snake Bite Plots/survival_function_time_to_first_risk.RDS")

# Male covariate
m1a <- survfit(Surv(exit, event) ~ male, data = df, conf.type = "log-log")
summary(m1a)
pdf(file = "Snake Bite Plots/survival_function_time_to_first_risk_by_gender.pdf", height = 5)
p <- autoplot(m1a) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced snake/ray bite") +
  scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Snake/Ray Bite") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Snake Bite Plots/survival_function_time_to_first_risk_by_gender.RDS")


# Region covariate
m1b <- survfit(Surv(exit, event) ~ region, data = df, conf.type = "log-log")
summary(m1b)
pdf(file = "Snake Bite Plots/survival_function_time_to_first_risk_by_region.pdf", height = 5)
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
dev.off()

# Male and region covariates
m1c <- survfit(Surv(exit, event) ~ male + region, data = df, conf.type = "log-log")
summary(m1c)
pdf(file = "Snake Bite Plots/survival_function_time_to_first_risk_by_gender_and_region.pdf", height = 5)
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
dev.off()

# Hazard Curves ----
# Plot hazard function of null model
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
pdf(file = "Snake Bite Plots/hazard_function_time_to_first_risk.pdf", height = 5)
p <- ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "orange2") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "pink3") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Snake/Ray Bite") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.15))
p
dev.off()
saveRDS(p, file = "Snake Bite Plots/hazard_function_time_to_first_risk.RDS")

# Plot hazard by gender
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Snake Bite Plots/hazard_function_time_to_first_risk_by_gender.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = male)) + geom_line(aes(col = male)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = male), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Snake/Ray Bite") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_color_hue(labels = c("Female", "Male")) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.28))
p
dev.off()
saveRDS(p, file = "Snake Bite Plots/hazard_function_time_to_first_risk_by_gender.RDS")

# Ratio of male hazard over female hazard
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == "Male"))
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"

fit2 <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == "Female"))
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
pdf(file = "Snake Bite Plots/hazard_ratio.pdf", height = 5)
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
dev.off()

# Plot hazard by region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Snake Bite Plots/hazard_function_time_to_first_risk_by_region.pdf", height = 5)
ggplot(hazards, aes(x = time, y = hazard, group = region)) + geom_line(aes(col = region)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = region), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Snake/Ray Bite") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
dev.off()

# Plot hazard by sex and region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Snake Bite Plots/hazard_function_time_to_first_risk_by_gender_and_region.pdf", height = 5)
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
dev.off()


# Fight -------------------------------------------------------------------

df <- read.csv("fought_time_to_first_risk_long_interval.csv")
df_short <- read.csv("fought_time_to_first_risk_short_interval.csv")

df$male <- ifelse(df$male == 1, "Male", "Female")
df_short$male <- ifelse(df_short$male == 1, "Male", "Female")

df$male <- as.factor(df$male)
df_short$male <- as.factor(df_short$male)

## Descriptive Plots ----

### Long Interval ----
# Make age.cat as factor
df$age.cat <- factor(df$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))

# Line plot
pdf(file = "Fight Plots/descriptive_plot4.pdf", height = 5, width = 7)
df %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "turquoise2",
            linewidth = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Fight") +
  labs(subtitle = "388 Individuals, 388 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")
dev.off()


# By gender
df_male <- subset(df, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df, male == "Female")
df_female_plot <- df_female %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1)
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


pdf(file = "Fight Plots/descriptive_plot5.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Fight") +
  labs(subtitle = "388 Individuals, 388 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.5))
dev.off()


### Short Interval ----
# Make age.cat as factor
df_short$age.cat <- factor(df_short$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                                        "20-25", "25-30", "30-35", "35-40",
                                                        "40-45", "45-50", "50-55", "55-60",
                                                        "60+"))

# Line plot
pdf(file = "Fight Plots/descriptive_plot6.pdf", height = 5, width = 7)
df_short %>%
  count(age.cat, fought.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(fought.during.interval == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "turquoise2",
            linewidth = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Fight") +
  labs(subtitle = "388 Individuals, 12,117 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")
dev.off()

# By gender
df_male <- subset(df_short, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, fought.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(fought.during.interval == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df_short, male == "Female")
df_female_plot <- df_female %>%
  count(age.cat, fought.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(fought.during.interval == 1)
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


pdf(file = "Fight Plots/descriptive_plot7.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Fight") +
  labs(subtitle = "388 Individuals, 12,117 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.5))
dev.off()

## Survival Curves (Kaplan-Meier estimator) ----
# null model
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
summary(m1)
pdf(file = "Fight Plots/survival_function_time_to_first_risk.pdf", height = 5)
p <- autoplot(m1, censor.shape = '|', censor.colour = "orange2",
         surv.colour = "pink3") +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced fight") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Fight Plots/survival_function_time_to_first_risk.RDS")


# Male covariate
m1a <- survfit(Surv(exit, event) ~ male, data = df, conf.type = "log-log")
summary(m1a)
pdf(file = "Fight Plots/survival_function_time_to_first_risk_by_gender.pdf", height = 5)
p <- autoplot(m1a) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced fight") +
  scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Fight Plots/survival_function_time_to_first_risk_by_gender.RDS")


# Region covariate
m1b <- survfit(Surv(exit, event) ~ region, data = df, conf.type = "log-log")
summary(m1b)
pdf(file = "Fight Plots/survival_function_time_to_first_risk_by_region.pdf", height = 5)
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
dev.off()

# Male and region covariates
m1c <- survfit(Surv(exit, event) ~ male + region, data = df, conf.type = "log-log")
summary(m1c)
pdf(file = "Fight Plots/survival_function_time_to_first_risk_by_gender_and_region.pdf", height = 5)
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
dev.off()

# Hazard Curves ----
# Plot hazard function of null model
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
pdf(file = "Fight Plots/hazard_function_time_to_first_risk.pdf", height = 5)
p <- ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "orange2") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "pink3") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.15))
p
dev.off()
saveRDS(p, file = "Fight Plots/hazard_function_time_to_first_risk.RDS")

# Plot hazard by gender
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Fight Plots/hazard_function_time_to_first_risk_by_gender.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = male)) + geom_line(aes(col = male)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = male), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_color_hue(labels = c("Female", "Male")) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.28))
p
dev.off()
saveRDS(p, file = "Fight Plots/hazard_function_time_to_first_risk_by_gender.RDS")

# Ratio of male hazard over female hazard
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == "Male"))
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"

fit2 <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == "Female"))
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
pdf(file = "Fight Plots/hazard_ratio.pdf", height = 5)
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
dev.off()

# Plot hazard by region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Fight Plots/hazard_function_time_to_first_risk_by_region.pdf", height = 5)
ggplot(hazards, aes(x = time, y = hazard, group = region)) + geom_line(aes(col = region)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = region), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Fight") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
dev.off()

# Plot hazard by sex and region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Fight Plots/hazard_function_time_to_first_risk_by_gender_and_region.pdf", height = 5)
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
dev.off()




# Sickness -------------------------------------------------------------------

df <- read.csv("sickness_time_to_first_risk_long_interval.csv")
df_short <- read.csv("sickness_time_to_first_risk_short_interval.csv")

df$male <- ifelse(df$male == 1, "Male", "Female")
df_short$male <- ifelse(df_short$male == 1, "Male", "Female")

df$male <- as.factor(df$male)
df_short$male <- as.factor(df_short$male)

## Descriptive Plots ----

### Long Interval ----
# Make age.cat as factor
df$age.cat <- factor(df$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))

# Line plot
pdf(file = "Sickness Plots/descriptive_plot4.pdf", height = 5, width = 7)
df %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "goldenrod2",
            linewidth = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Sickness") +
  labs(subtitle = "388 Individuals, 388 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")
dev.off()


# By gender
df_male <- subset(df, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df, male == "Female")
df_female_plot <- df_female %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1)
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


pdf(file = "Sickness Plots/descriptive_plot5.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Sickness") +
  labs(subtitle = "388 Individuals, 388 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.5))
dev.off()


### Short Interval ----
# Make age.cat as factor
df_short$age.cat <- factor(df_short$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                                        "20-25", "25-30", "30-35", "35-40",
                                                        "40-45", "45-50", "50-55", "55-60",
                                                        "60+"))

# Line plot
pdf(file = "Sickness Plots/descriptive_plot6.pdf", height = 5, width = 7)
df_short %>%
  count(age.cat, sickness.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(sickness.during.interval == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "goldenrod2",
            linewidth = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Sickness") +
  labs(subtitle = "388 Individuals, 8,430 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")
dev.off()

# By gender
df_male <- subset(df_short, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, sickness.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(sickness.during.interval == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df_short, male == "Female")
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


pdf(file = "Sickness Plots/descriptive_plot7.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Sickness") +
  labs(subtitle = "388 Individuals, 8,430 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.5))
dev.off()

## Survival Curves (Kaplan-Meier estimator) ----
# null model
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
summary(m1)
pdf(file = "Sickness Plots/survival_function_time_to_first_risk.pdf", height = 5)
p <- autoplot(m1, censor.shape = '|', censor.colour = "orange2",
         surv.colour = "pink3") +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced sickness") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Sickness Plots/survival_function_time_to_first_risk.RDS")


# Male covariate
m1a <- survfit(Surv(exit, event) ~ male, data = df, conf.type = "log-log")
summary(m1a)
pdf(file = "Sickness Plots/survival_function_time_to_first_risk_by_gender.pdf", height = 5)
p <- autoplot(m1a) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced sickness") +
  scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Sickness Plots/survival_function_time_to_first_risk_by_gender.RDS")


# Region covariate
m1b <- survfit(Surv(exit, event) ~ region, data = df, conf.type = "log-log")
summary(m1b)
pdf(file = "Sickness Plots/survival_function_time_to_first_risk_by_region.pdf", height = 5)
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
dev.off()


# Male and region covariates
m1c <- survfit(Surv(exit, event) ~ male + region, data = df, conf.type = "log-log")
summary(m1c)
pdf(file = "Sickness Plots/survival_function_time_to_first_risk_by_gender_and_region.pdf", height = 5)
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
dev.off()

# Hazard Curves ----
# Plot hazard function of null model
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
pdf(file = "Sickness Plots/hazard_function_time_to_first_risk.pdf", height = 5)
p <- ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "orange2") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "pink3") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.15))
p
dev.off()
saveRDS(p, file = "Sickness Plots/hazard_function_time_to_first_risk.RDS")

# Plot hazard by gender
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Sickness Plots/hazard_function_time_to_first_risk_by_gender.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = male)) + geom_line(aes(col = male)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = male), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_color_hue(labels = c("Female", "Male")) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.28))
p
dev.off()
saveRDS(p, file = "Sickness Plots/hazard_function_time_to_first_risk_by_gender.RDS")

# Ratio of male hazard over female hazard
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == "Male"))
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"

fit2 <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == "Female"))
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
pdf(file = "Sickness Plots/hazard_ratio.pdf", height = 5)
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
dev.off()

# Plot hazard by region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Sickness Plots/hazard_function_time_to_first_risk_by_region.pdf", height = 5)
ggplot(hazards, aes(x = time, y = hazard, group = region)) + geom_line(aes(col = region)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = region), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Sickness") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
dev.off()

# Plot hazard by sex and region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Sickness Plots/hazard_function_time_to_first_risk_by_gender_and_region.pdf", height = 5)
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
dev.off()

# Animal Attack -----------------------------------------------------------

df <- read.csv("animal_attack_time_to_first_risk_long_interval.csv")
df_short <- read.csv("animal_attack_time_to_first_risk_short_interval.csv")

df$male <- ifelse(df$male == 1, "Male", "Female")
df_short$male <- ifelse(df_short$male == 1, "Male", "Female")

df$male <- as.factor(df$male)
df_short$male <- as.factor(df_short$male)

## Descriptive Plots ----

### Long Interval ----
# Make age.cat as factor
df$age.cat <- factor(df$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))

# Line plot
pdf(file = "Animal Attack Plots/descriptive_plot4.pdf", height = 5, width = 7)
df %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "pink4",
            linewidth = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Animal Attack") +
  labs(subtitle = "388 Individuals, 388 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")
dev.off()


# By gender
df_male <- subset(df, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df, male == "Female")
df_female_plot <- df_female %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1)
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


pdf(file = "Animal Attack Plots/descriptive_plot5.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Animal Attack") +
  labs(subtitle = "388 Individuals, 388 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.5))
dev.off()


### Short Interval ----
# Make age.cat as factor
df_short$age.cat <- factor(df_short$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                                        "20-25", "25-30", "30-35", "35-40",
                                                        "40-45", "45-50", "50-55", "55-60",
                                                        "60+"))

# Line plot
pdf(file = "Animal Attack Plots/descriptive_plot6.pdf", height = 5, width = 7)
df_short %>%
  count(age.cat, animal.attack.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(animal.attack.during.interval == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "pink4",
            linewidth = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Animal Attack") +
  labs(subtitle = "388 Individuals, 13,104 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")
dev.off()

# By gender
df_male <- subset(df_short, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, animal.attack.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(animal.attack.during.interval == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df_short, male == "Female")
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


pdf(file = "Animal Attack Plots/descriptive_plot7.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Animal Attack") +
  labs(subtitle = "388 Individuals, 13,104 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.5))
dev.off()

## Survival Curves (Kaplan-Meier estimator) ----
# null model
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
summary(m1)
pdf(file = "Animal Attack Plots/survival_function_time_to_first_risk.pdf", height = 5)
p <- autoplot(m1, censor.shape = '|', censor.colour = "orange2",
         surv.colour = "pink3") +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced animal attack") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Animal Attack Plots/survival_function_time_to_first_risk.RDS")


# Male covariate
m1a <- survfit(Surv(exit, event) ~ male, data = df, conf.type = "log-log")
summary(m1a)
pdf(file = "Animal Attack Plots/survival_function_time_to_first_risk_by_gender.pdf", height = 5)
p <- autoplot(m1a) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced animal attack") +
  scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Animal Attack Plots/survival_function_time_to_first_risk_by_gender.RDS")


# Region covariate
m1b <- survfit(Surv(exit, event) ~ region, data = df, conf.type = "log-log")
summary(m1b)
pdf(file = "Animal Attack Plots/survival_function_time_to_first_risk_by_region.pdf", height = 5)
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
dev.off()


# Male and region covariates
m1c <- survfit(Surv(exit, event) ~ male + region, data = df, conf.type = "log-log")
summary(m1c)
pdf(file = "Animal Attack Plots/survival_function_time_to_first_risk_by_gender_and_region.pdf", height = 5)
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
dev.off()

# Hazard Curves ----
# Plot hazard function of null model
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
pdf(file = "Animal Attack Plots/hazard_function_time_to_first_risk.pdf", height = 5)
p <- ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "orange2") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "pink3") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.15))
p
dev.off()
saveRDS(p, file = "Animal Attack Plots/hazard_function_time_to_first_risk.RDS")

# Plot hazard by gender
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Animal Attack Plots/hazard_function_time_to_first_risk_by_gender.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = male)) + geom_line(aes(col = male)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = male), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_color_hue(labels = c("Female", "Male")) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.28))
p
dev.off()
saveRDS(p, file = "Animal Attack Plots/hazard_function_time_to_first_risk_by_gender.RDS")

# Ratio of male hazard over female hazard
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == "Male"))
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"
fit2 <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == "Female"))
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
pdf(file = "Animal Attack Plots/hazard_ratio.pdf", height = 5)
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
dev.off()

# Plot hazard by region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Animal Attack Plots/hazard_function_time_to_first_risk_by_region.pdf", height = 5)
ggplot(hazards, aes(x = time, y = hazard, group = region)) + geom_line(aes(col = region)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = region), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
dev.off()

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
df_short <- read.csv("canoe_capsize_time_to_first_risk_short_interval.csv")

df$male <- ifelse(df$male == 1, "Male", "Female")
df_short$male <- ifelse(df_short$male == 1, "Male", "Female")

df$male <- as.factor(df$male)
df_short$male <- as.factor(df_short$male)

## Descriptive Plots ----

### Long Interval ----
# Make age.cat as factor
df$age.cat <- factor(df$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))

# Line plot
pdf(file = "Canoe Capsize Plots/descriptive_plot4.pdf", height = 5, width = 7)
df %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "purple",
            linewidth = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Canoe Capsize") +
  labs(subtitle = "388 Individuals, 388 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")
dev.off()


# By gender
df_male <- subset(df, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df, male == "Female")
df_female_plot <- df_female %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1)
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


pdf(file = "Canoe Capsize Plots/descriptive_plot5.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Canoe Capsize") +
  labs(subtitle = "388 Individuals, 388 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.5))
dev.off()


### Short Interval ----
# Make age.cat as factor
df_short$age.cat <- factor(df_short$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                                        "20-25", "25-30", "30-35", "35-40",
                                                        "40-45", "45-50", "50-55", "55-60",
                                                        "60+"))

# Line plot
pdf(file = "Canoe Capsize Plots/descriptive_plot6.pdf", height = 5, width = 7)
df_short %>%
  count(age.cat, canoe.capsize.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(canoe.capsize.during.interval == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "purple",
            linewidth = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Canoe Capsize") +
  labs(subtitle = "388 Individuals, 11,822 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")
dev.off()

# By gender
df_male <- subset(df_short, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, canoe.capsize.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(canoe.capsize.during.interval == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df_short, male == "Female")
df_female_plot <- df_female %>%
  count(age.cat, canoe.capsize.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(canoe.capsize.during.interval == 1)
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


pdf(file = "Canoe Capsize Plots/descriptive_plot7.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Canoe Capsize") +
  labs(subtitle = "388 Individuals, 11,822 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.5))
dev.off()

## Survival Curves (Kaplan-Meier estimator) ----
# null model
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
summary(m1)
pdf(file = "Canoe Capsize Plots/survival_function_time_to_first_risk.pdf", height = 5)
p <- autoplot(m1, censor.shape = '|', censor.colour = "orange2",
         surv.colour = "pink3") +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced canoe capsize") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Canoe Capsize Plots/survival_function_time_to_first_risk.RDS")


# Male covariate
m1a <- survfit(Surv(exit, event) ~ male, data = df, conf.type = "log-log")
summary(m1a)
pdf(file = "Canoe Capsize Plots/survival_function_time_to_first_risk_by_gender.pdf", height = 5)
p <- autoplot(m1a) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced canoe capsize") +
  scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Canoe Capsize Plots/survival_function_time_to_first_risk_by_gender.RDS")


# Region covariate
m1b <- survfit(Surv(exit, event) ~ region, data = df, conf.type = "log-log")
summary(m1b)
pdf(file = "Canoe Capsize Plots/survival_function_time_to_first_risk_by_region.pdf", height = 5)
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
dev.off()


# Male and region covariates
m1c <- survfit(Surv(exit, event) ~ male + region, data = df, conf.type = "log-log")
summary(m1c)
pdf(file = "Canoe Capsize Plots/survival_function_time_to_first_risk_by_gender_and_region.pdf", height = 5)
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
dev.off()

# Hazard Curves ----
# Plot hazard function of null model
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
pdf(file = "Canoe Capsize Plots/hazard_function_time_to_first_risk.pdf", height = 5)
p <- ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "orange2") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "pink3") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.15))
p
dev.off()
saveRDS(p, file = "Canoe Capsize Plots/hazard_function_time_to_first_risk.RDS")

# Plot hazard by gender
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Canoe Capsize Plots/hazard_function_time_to_first_risk_by_gender.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = male)) + geom_line(aes(col = male)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = male), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_color_hue(labels = c("Female", "Male")) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.28))
p
dev.off()
saveRDS(p, file = "Canoe Capsize Plots/hazard_function_time_to_first_risk_by_gender.RDS")

# Ratio of male hazard over female hazard
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == "Male"))
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"
fit2 <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == "Female"))
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
pdf(file = "Canoe Capsize Plots/hazard_ratio.pdf", height = 5)
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
dev.off()

# Plot hazard by region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Canoe Capsize Plots/hazard_function_time_to_first_risk_by_region.pdf", height = 5)
ggplot(hazards, aes(x = time, y = hazard, group = region)) + geom_line(aes(col = region)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = region), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Canoe Capsize") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
dev.off()

# Plot hazard by sex and region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Canoe Capsize Plots/hazard_function_time_to_first_risk_by_gender_and_region.pdf", height = 5)
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
dev.off()

# Cut Self ----------------------------------------------------------------

df <- read.csv("cut_self_time_to_first_risk_long_interval.csv")
df_short <- read.csv("cut_self_time_to_first_risk_short_interval.csv")

df$male <- ifelse(df$male == 1, "Male", "Female")
df_short$male <- ifelse(df_short$male == 1, "Male", "Female")

df$male <- as.factor(df$male)
df_short$male <- as.factor(df_short$male)

## Descriptive Plots ----

### Long Interval ----
# Make age.cat as factor
df$age.cat <- factor(df$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))

# Line plot
pdf(file = "Cut Self Plots/descriptive_plot4.pdf", height = 5, width = 7)
df %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "orange",
            linewidth = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Cut Self") +
  labs(subtitle = "388 Individuals, 388 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")
dev.off()


# By gender
df_male <- subset(df, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df, male == "Female")
df_female_plot <- df_female %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1)
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


pdf(file = "Cut Self Plots/descriptive_plot5.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Cut Self") +
  labs(subtitle = "388 Individuals, 388 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.5))
dev.off()


### Short Interval ----
# Make age.cat as factor
df_short$age.cat <- factor(df_short$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                                        "20-25", "25-30", "30-35", "35-40",
                                                        "40-45", "45-50", "50-55", "55-60",
                                                        "60+"))

# Line plot
pdf(file = "Cut Self Plots/descriptive_plot6.pdf", height = 5, width = 7)
df_short %>%
  count(age.cat, cut.self.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(cut.self.during.interval == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "orange",
            linewidth = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Cut Self") +
  labs(subtitle = "388 Individuals, 9,275 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")
dev.off()

# By gender
df_male <- subset(df_short, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, cut.self.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(cut.self.during.interval == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df_short, male == "Female")
df_female_plot <- df_female %>%
  count(age.cat, cut.self.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(cut.self.during.interval == 1)
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


pdf(file = "Cut Self Plots/descriptive_plot7.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Cut Self") +
  labs(subtitle = "388 Individuals, 9,275 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.5))
dev.off()

## Survival Curves (Kaplan-Meier estimator) ----
# null model
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
summary(m1)
pdf(file = "Cut Self Plots/survival_function_time_to_first_risk.pdf", height = 5)
p <- autoplot(m1, censor.shape = '|', censor.colour = "orange2",
         surv.colour = "pink3") +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced cut self") +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Cut Self Plots/survival_function_time_to_first_risk.RDS")


# Male covariate
m1a <- survfit(Surv(exit, event) ~ male, data = df, conf.type = "log-log")
summary(m1a)
pdf(file = "Cut Self Plots/survival_function_time_to_first_risk_by_gender.pdf", height = 5)
p <- autoplot(m1a) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced cut self") +
  scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Cut Self Plots/survival_function_time_to_first_risk_by_gender.RDS")


# Region covariate
m1b <- survfit(Surv(exit, event) ~ region, data = df, conf.type = "log-log")
summary(m1b)
pdf(file = "Cut Self Plots/survival_function_time_to_first_risk_by_region.pdf", height = 5)
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
dev.off()


# Male and region covariates
m1c <- survfit(Surv(exit, event) ~ male + region, data = df, conf.type = "log-log")
summary(m1c)
pdf(file = "Cut Self Plots/survival_function_time_to_first_risk_by_gender_and_region.pdf", height = 5)
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
dev.off()

# Hazard Curves ----
# Plot hazard function of null model
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
pdf(file = "Cut Self Plots/hazard_function_time_to_first_risk.pdf", height = 5)
p <- ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "orange2") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "pink3") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.15))
p
dev.off()
saveRDS(p, file = "Cut Self Plots/hazard_function_time_to_first_risk.RDS")

# Plot hazard by gender
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Cut Self Plots/hazard_function_time_to_first_risk_by_gender.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = male)) + geom_line(aes(col = male)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = male), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_color_hue(labels = c("Female", "Male")) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.28))
p
dev.off()
saveRDS(p, file = "Cut Self Plots/hazard_function_time_to_first_risk_by_gender.RDS")

# Ratio of male hazard over female hazard
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == "Male"))
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"
fit2 <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == "Female"))
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
pdf(file = "Cut Self Plots/hazard_ratio.pdf", height = 5)
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
dev.off()

# Plot hazard by region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Cut Self Plots/hazard_function_time_to_first_risk_by_region.pdf", height = 5)
ggplot(hazards, aes(x = time, y = hazard, group = region)) + geom_line(aes(col = region)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = region), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Cut Self") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
dev.off()

# Plot hazard by sex and region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Cut Self Plots/hazard_function_time_to_first_risk_by_gender_and_region.pdf", height = 5)
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
dev.off()


# Animal Attack (c) -------------------------------------------------------

df <- read.csv("Animal_Attack_combined_time_to_first_risk_long_interval.csv")
df_short <- read.csv("Animal_Attack_combined_time_to_first_risk_short_interval.csv")

df$male <- ifelse(df$male == 1, "Male", "Female")
df_short$male <- ifelse(df_short$male == 1, "Male", "Female")

df$male <- as.factor(df$male)
df_short$male <- as.factor(df_short$male)

## Descriptive Plots ----

### Long Interval ----
# Make age.cat as factor
df$age.cat <- factor(df$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))

# Line plot
pdf(file = "Animal Attack Combined Plots/descriptive_plot4.pdf", height = 5, width = 7)
df %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "orange",
            linewidth = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Animal Attack (c)") +
  labs(subtitle = "388 Individuals, 388 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")
dev.off()


# By gender
df_male <- subset(df, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df, male == "Female")
df_female_plot <- df_female %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1)
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


pdf(file = "Animal Attack Combined Plots/descriptive_plot5.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Animal Attack (c)") +
  labs(subtitle = "388 Individuals, 388 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.5))
dev.off()


### Short Interval ----
# Make age.cat as factor
df_short$age.cat <- factor(df_short$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                                        "20-25", "25-30", "30-35", "35-40",
                                                        "40-45", "45-50", "50-55", "55-60",
                                                        "60+"))

# Line plot
pdf(file = "Animal Attack Combined Plots/descriptive_plot6.pdf", height = 5, width = 7)
df_short %>%
  count(age.cat, Animal_Attack.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(Animal_Attack.during.interval == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "orange",
            linewidth = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 12) +
  ggtitle("Animal Attack (c)") +
  labs(subtitle = "388 Individuals, 10,478 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")
dev.off()

# By gender
df_male <- subset(df_short, male == "Male")
df_male_plot <- df_male %>%
  count(age.cat, Animal_Attack.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(Animal_Attack.during.interval == 1)
df_male_plot$male <- "Male"
df_male_plot <- df_male_plot[c("age.cat", "male", "prop")]

df_female <- subset(df_short, male == "Female")
df_female_plot <- df_female %>%
  count(age.cat, Animal_Attack.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(Animal_Attack.during.interval == 1)
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


pdf(file = "Animal Attack Combined Plots/descriptive_plot7.pdf", height = 5, width = 7.5)
ggplot(df_gender_plot, aes(x = age.cat, y = prop, group = male, color = male)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.5) +
  geom_text(aes(label = scales::percent(prop)), color = "black", size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 13) +
  ggtitle("Animal Attack (c)") +
  labs(subtitle = "388 Individuals, 10,478 Intervals") +
  theme(plot.title = element_text(size = 30)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.5))
dev.off()

## Survival Curves (Kaplan-Meier estimator) ----
# null model
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
summary(m1)
pdf(file = "Animal Attack Combined Plots/survival_function_time_to_first_risk.pdf", height = 5)
p <- autoplot(m1, censor.shape = '|', censor.colour = "orange2",
         surv.colour = "pink3") +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced animal attack (c)") +
  # labs(subtitle = "(All Animal Attacks)") +
  # theme(plot.subtitle = element_text(hjust = 0.5, size = 5)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Animal Attack Combined Plots/survival_function_time_to_first_risk.RDS")


# Male covariate
m1a <- survfit(Surv(exit, event) ~ male, data = df, conf.type = "log-log")
summary(m1a)
pdf(file = "Animal Attack Combined Plots/survival_function_time_to_first_risk_by_gender.pdf", height = 5)
p <- autoplot(m1a) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced animal attack (c)") +
  scale_color_hue(labels = c("Female", "Male")) +
  ggtitle("Animal Attack (c)") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Animal Attack Combined Plots/survival_function_time_to_first_risk_by_gender.RDS")



# Region covariate
m1b <- survfit(Surv(exit, event) ~ region, data = df, conf.type = "log-log")
summary(m1b)
pdf(file = "Animal Attack Combined Plots/survival_function_time_to_first_risk_by_region.pdf", height = 5)
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
dev.off()


# Male and region covariates
m1c <- survfit(Surv(exit, event) ~ male + region, data = df, conf.type = "log-log")
summary(m1c)
pdf(file = "Animal Attack Combined Plots/survival_function_time_to_first_risk_by_gender_and_region.pdf", height = 5)
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
dev.off()

# Hazard Curves ----
# Plot hazard function of null model
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
pdf(file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk.pdf", height = 5)
p <- ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "orange2") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "pink3") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.15))
p
dev.off()
saveRDS(p, file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk.RDS")


# Plot hazard by gender
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_gender.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = male)) + geom_line(aes(col = male)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = male), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_color_hue(labels = c("Female", "Male")) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.28))
p
dev.off()
saveRDS(p, file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_gender.RDS")


# Ratio of male hazard over female hazard
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == "Male"))
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
df_surv$Sex <- "Male"
fit2 <- bshazard(Surv(enter, exit, event) ~ 1, data = subset(df, male == "Female"))
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
pdf(file = "Animal Attack Combined Plots/hazard_ratio.pdf", height = 5)
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
dev.off()

# Plot hazard by region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_region.pdf", height = 5)
ggplot(hazards, aes(x = time, y = hazard, group = region)) + geom_line(aes(col = region)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = region), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack (c)") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
dev.off()

# Plot hazard by sex and region
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, male, region) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_gender_and_region.pdf", height = 5)
ggplot(hazards, aes(x = time, y = hazard, group = interaction(male, region))) +
  geom_line(aes(col = interaction(male, region))) +
  # geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = interaction(male, region)), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack") +
  guides(fill = F) +
  scale_color_hue(labels = c("Female, Forest", "Male, Forest", "Female, Near San Borja",
                             "Male, Near San Borja", "Female, Upriver", "Male, Upriver")) +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
dev.off()


# Panel plot for survival curves ------------------------------------------

figure <- ggarrange(readRDS("Sickness Plots/survival_function_time_to_first_risk.RDS") + rremove("ylab") + rremove("xlab"),
          readRDS("Cut Self Plots/survival_function_time_to_first_risk.RDS") + rremove("ylab") + rremove("xlab"),
          readRDS("Animal Attack Combined Plots/survival_function_time_to_first_risk.RDS") + rremove("ylab") + rremove("xlab"),
          readRDS("Tree Fall Plots/survival_function_time_to_first_risk.RDS") + rremove("ylab") + rremove("xlab"),
          readRDS("Fight Plots/survival_function_time_to_first_risk.RDS") + rremove("ylab") + rremove("xlab"),
          readRDS("Canoe Capsize Plots/survival_function_time_to_first_risk.RDS") + rremove("ylab") + rremove("xlab"))

pdf(file = "Panel Plots/survival.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Percentage Not Experienced Risk", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()



# Panel plot for survival curves by gender --------------------------------

figure <- ggarrange(readRDS("Sickness Plots/survival_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Cut Self Plots/survival_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Animal Attack Combined Plots/survival_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Tree Fall Plots/survival_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Fight Plots/survival_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Canoe Capsize Plots/survival_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab"),
                    common.legend = T, legend = "bottom")

pdf(file = "Panel Plots/survival_by_gender.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Percentage Not Experienced Risk", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()



# Panel plot for hazard curves --------------------------------------------

figure <- ggarrange(readRDS("Sickness Plots/hazard_function_time_to_first_risk.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Cut Self Plots/hazard_function_time_to_first_risk.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Animal Attack Combined Plots/hazard_function_time_to_first_risk.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Tree Fall Plots/hazard_function_time_to_first_risk.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Fight Plots/hazard_function_time_to_first_risk.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Canoe Capsize Plots/hazard_function_time_to_first_risk.RDS") + rremove("ylab") + rremove("xlab"))

pdf(file = "Panel Plots/hazard.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Hazard", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()

# Panel plot for hazard curves by gender ----------------------------------

figure <- ggarrange(readRDS("Sickness Plots/hazard_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Cut Self Plots/hazard_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Tree Fall Plots/hazard_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Fight Plots/hazard_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Canoe Capsize Plots/hazard_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab"),
                    common.legend = T, legend = "bottom")

pdf(file = "Panel Plots/hazard_by_gender.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Hazard", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()


# Panel plot for cox mixed effects models ---------------------------------
ph_legend <- as_ggplot(get_legend(readRDS("Animal Attack Combined Plots/coxme_plot.RDS") +
                                    guides(fill = guide_legend(nrow = 1))))

figure <- ggarrange(readRDS("Sickness Plots/coxme_plot.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Cut Self Plots/coxme_plot.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Animal Attack Combined Plots/coxme_plot.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Tree Fall Plots/coxme_plot.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Fight Plots/coxme_plot.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Canoe Capsize Plots/coxme_plot.RDS") + rremove("ylab") + rremove("xlab"),
                    "",
                    ph_legend,
                    legend = "none",
                    heights = c(1, 1, 0.2, 1, 1, 0.2, 1, 1))
pdf(file = "Panel Plots/coxme.pdf", height = 15, width = 15)
annotate_figure(figure, left = textGrob("Hazard Ratio", rot = 90, vjust = 1, gp = gpar(cex = 1.7)),
                bottom = grid::textGrob("Time-varying covariate: occurrence of event", gp = gpar(cex = 1.7)))
dev.off()
