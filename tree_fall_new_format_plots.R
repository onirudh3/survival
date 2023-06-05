# Libraries and Data Import -----------------------------------------------
library(survival)
library(tidyverse)
library(eha)
library(ggfortify)
library(muhaz)
library(Hmisc)
library(rstpm2)

# Import data
df <- read.csv("data_new_format.csv")

# Model 3 Plots -----------------------------------------------------------
# 3a
df$sickness.during.interval <- as.factor(df$sickness.during.interval)
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


# Trying hazard ratio plot ------------------------------------------------


mod_tvc <- stpm2(Surv(enter, exit, tree.fall.during) ~ male, data = df)

plot(mod_tvc, newdata = data.frame(arm = 0), type = “hr”,
     var = “arm”, ci = TRUE, rug = FALSE,
     main = “Time dependent hazard ratio”, xlim=c(1,24), ylim=c(0,2.5),
     ylab = “Hazard ratio”, xlab = “Time”)


# Descriptive Plots -------------------------------------------------------
## Percentage of intervals in which tree fall occurs, by tree fall age ----
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
  ylab("Percentage of Intervals (13,541 Intervals)") +
  theme(plot.title = element_text(hjust = 0.5))

# Line plot
df %>%
  count(age.cat, event) %>%
  mutate(prop = prop.table(n)) %>%
  filter(event == 1) %>%
  ggplot() +
  geom_line(aes(x = age.cat, y = prop, group = 1), color = "lightseagreen",
            size = 2) +
  geom_text(aes(x = age.cat, y = prop,
                label = scales::percent(prop)), size = 5) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals (13,541 Intervals)")

## Percentage of individuals experiencing tree fall, by tree fall age -----
# Bar plot





## Percentage of intervals in which all risks occur by age of occurrence ----
tree_fall_df <- df %>%
  count(age.cat, tree.fall.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(tree.fall.during.interval == 1)
tree_fall_df <- tree_fall_df %>%
  dplyr::rename("event" = "tree.fall.during.interval")
tree_fall_df$event <- ifelse(tree_fall_df$event == 1, "Tree Fall",
                             tree_fall_df$event)

sickness_df <- df %>%
  count(age.cat, sickness.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(sickness.during.interval == 1)
sickness_df <- sickness_df %>%
  dplyr::rename("event" = "sickness.during.interval")
sickness_df$event <- ifelse(sickness_df$event == 1,
                            "Sickness", sickness_df$event)

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
animal_attack_df <- animal_attack_df %>%
  dplyr::rename("event" = "animal.attack.during.interval")
animal_attack_df$event <- ifelse(animal_attack_df$event == 1, "Animal Attack",
                                 animal_attack_df$event)

canoe_capsize_df <- df %>%
  count(age.cat, canoe.capsize.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(canoe.capsize.during.interval == 1)
canoe_capsize_df <- canoe_capsize_df %>%
  dplyr::rename("event" = "canoe.capsize.during.interval")
canoe_capsize_df$event <- ifelse(canoe_capsize_df$event == 1, "Canoe Capsize",
                                 canoe_capsize_df$event)

cut_self_df <- df %>%
  count(age.cat, cut.self.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(cut.self.during.interval == 1)
cut_self_df <- cut_self_df %>%
  dplyr::rename("event" = "cut.self.during.interval")
cut_self_df$event <- ifelse(cut_self_df$event == 1, "Cut Self",
                            cut_self_df$event)

df2 <- rbind(tree_fall_df, sickness_df, bite_df, fought_df, animal_attack_df,
             canoe_capsize_df, cut_self_df)
rm(tree_fall_df, sickness_df, bite_df, fought_df, animal_attack_df,
   canoe_capsize_df, cut_self_df)


df2$event <- factor(df2$event, levels = c("Cut Self", "Sickness",
                                         "Snake/Ray Bite", "Tree Fall", "Fight",
                                         "Canoe Capsize", "Animal Attack"))

ggplot(df2, aes(x = age.cat, y = prop, group = event, col = event)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1.6) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals (13,541 Intervals)") +
  labs(color = "Type")























