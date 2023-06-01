# Libraries and Data Import -----------------------------------------------
library(survival)
library(tidyverse)
library(eha)
library(ggfortify)
library(muhaz)
library(Hmisc)

# Import data
df <- read.csv("tree_fall_new_format.csv")

# Make region as factor
df$region <- as.factor(df$region)

# Plots -------------------------------------------------------------------
# Categorizing the age in the interval for an individual
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

# Make age.cat as factor
df$age.cat <- factor(df$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))

## Model 3 Plots ----
# 3a
fit <- coxreg(Surv(enter, exit, event) ~ male + region +
                strata(sickness.during.interval), data = df)
plot(fit,
     main = "TREE FALL (Controlling for sex and region)",
     col = c("blue", "red"),
     xlab = "Age in Years",
     ylab = "Cumulative Hazard")

# 3b
fit <- coxreg(Surv(enter, exit, event) ~ male + region +
                strata(bite.during.interval), data = df)
plot(fit,
     main = "TREE FALL (Controlling for sex and region)",
     col = c("blue", "red"),
     xlab = "Age in Years",
     ylab = "Cumulative Hazard")

# 3c
fit <- coxreg(Surv(enter, exit, event) ~ male + region +
                strata(fought.during.interval), data = df)
plot(fit,
     main = "TREE FALL (Controlling for sex and region)",
     col = c("blue", "red"),
     xlab = "Age in Years",
     ylab = "Cumulative Hazard")

# 3d
fit <- coxreg(Surv(enter, exit, event) ~ male + region +
                strata(animal.attack.during.interval), data = df)
plot(fit,
     main = "TREE FALL (Controlling for sex and region)",
     col = c("blue", "red"),
     xlab = "Age in Years",
     ylab = "Cumulative Hazard")

# 3e
fit <- coxreg(Surv(enter, exit, event) ~ male + region +
                strata(canoe.capsize.during.interval), data = df)
plot(fit,
     main = "TREE FALL (Controlling for sex and region)",
     col = c("blue", "red"),
     xlab = "Age in Years",
     ylab = "Cumulative Hazard")

# 3f
fit <- coxreg(Surv(enter, exit, event) ~ male + region +
                strata(cut.self.during.interval), data = df)
plot(fit,
     main = "TREE FALL (Controlling for sex and region)",
     col = c("blue", "red"),
     xlab = "Age in Years",
     ylab = "Cumulative Hazard")





# Descriptive Plots -------------------------------------------------------
# Percentage of intervals in which tree fall occurs, by tree fall age
# Bar plot
df %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1) %>%
  ggplot() +
  geom_col(aes(x = age.cat, y = prop), fill = "lightseagreen") +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), nudge_y = 0.0001, size = 5) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")

# Line plot
df %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "lightseagreen") +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 5) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals")

# Percentage of individuals experiencing tree fall, by tree fall age
# Bar plot
df2 <- df %>% filter(event == 1)

df2 %>%
  count(age.cat, pid) %>%
  mutate(prop = prop.table(n)) %>%
  ggplot() +
  geom_col(aes(x = age.cat, y = prop))

balls <- df %>% filter(event == 1 & age.cat == "10-15")
plyr::count(balls$pid)




# Trying to plot all risks
# df <- df %>% dplyr::rename("tree.fall.during.interval" = "event")
# m <- pivot_longer(df,
#                   cols = c("tree.fall.during.interval",
#                            "sickness.during.interval", "bite.during.interval",
#                            "fought.during.interval",
#                            "animal.attack.during.interval",
#                            "canoe.capsize.during.interval",
#                            "cut.self.during.interval")) %>%
#   ungroup()












tree_fall_df <- df %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1)
tree_fall_df$event <- ifelse(tree_fall_df$event == 1, "Tree Fall", tree_fall_df$event)

sickness_df <- df %>%
  count(age.cat, sickness.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(sickness.during.interval == 1)
sickness_df <- sickness_df %>% dplyr::rename("event" = "sickness.during.interval")
sickness_df$event <- ifelse(sickness_df$event == 1, "Sickness", sickness_df$event)

bite_df <- df %>%
  count(age.cat, bite.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(bite.during.interval == 1)
bite_df <- bite_df %>% dplyr::rename("event" = "bite.during.interval")
bite_df$event <- ifelse(bite_df$event == 1, "Snake/Ray Bite", bite_df$event)

fought_df <- df %>%
  count(age.cat, fought.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(fought.during.interval == 1)
fought_df <- fought_df %>% dplyr::rename("event" = "fought.during.interval")
fought_df$event <- ifelse(fought_df$event == 1, "Fight", fought_df$event)

animal_attack_df <- df %>%
  count(age.cat, animal.attack.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(animal.attack.during.interval == 1)
animal_attack_df <- animal_attack_df %>% dplyr::rename("event" = "animal.attack.during.interval")
animal_attack_df$event <- ifelse(animal_attack_df$event == 1, "Animal Attack", animal_attack_df$event)

canoe_capsize_df <- df %>%
  count(age.cat, canoe.capsize.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(canoe.capsize.during.interval == 1)
canoe_capsize_df <- canoe_capsize_df %>% dplyr::rename("event" = "canoe.capsize.during.interval")
canoe_capsize_df$event <- ifelse(canoe_capsize_df$event == 1, "Canoe Capsize", canoe_capsize_df$event)

cut_self_df <- df %>%
  count(age.cat, cut.self.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(cut.self.during.interval == 1)
cut_self_df <- cut_self_df %>% dplyr::rename("event" = "cut.self.during.interval")
cut_self_df$event <- ifelse(cut_self_df$event == 1, "Cut Self", cut_self_df$event)

df2 <- rbind(tree_fall_df, sickness_df, bite_df, fought_df, animal_attack_df, canoe_capsize_df, cut_self_df)

ggplot(df2, aes(x = age.cat, y = prop, group = event, col = event)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "Type of Risk")







# p <- ggplot(df2) +
#   geom_line(aes(x = age.cat, y = prop, group = 1), color = "lightseagreen") +
#   theme_classic()
#
#
# df3 <- df %>%
#   count(age.cat, sickness.during.interval) %>%
#   mutate(prop = prop.table(n)) %>%
#   filter(sickness.during.interval == 1)
#
# q <- ggplot(df3) + geom_line()
#
# p + q























