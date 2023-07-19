
# Libraries and data import -----------------------------------------------

library(tidyverse)

# Tree Fall ----

df1 <- read.csv("tree_fall_time_to_first_risk_short_interval.csv")

df1 <- df1 %>%
  mutate(tree_fall_days_disabled_1 =
           case_when(!is.na(tree_fall_days_disabled_2) ~
                       tree_fall_days_disabled_1 +
                       tree_fall_days_disabled_2,
                     T ~ tree_fall_days_disabled_1))

df1 <- subset(df1, select = c(pid, age.cat, tree.fall.during.interval, tree_fall_days_disabled_1))

df1 <- df1 %>%
  group_by(age.cat) %>%
  mutate(n = n_distinct(pid))

df1 <- df1 %>%
  filter(tree.fall.during.interval == 1)

df1 <- df1 %>%
  filter(!is.na(tree_fall_days_disabled_1)) # remove NA individuals

df1 <- df1 %>%
  group_by(age.cat, n) %>%
  summarise(days_disabled_risk = sum(tree_fall_days_disabled_1)) # get sum of days disabled

df1$risk <- "Tree Fall"


# Sickness ----

df2 <- read.csv("sickness_time_to_first_risk_short_interval.csv")

df2 <- df2 %>%
  mutate(days_disabled_sickness_1 =
           case_when(!is.na(days_disabled_sickness_2) ~
                       days_disabled_sickness_1 +
                       days_disabled_sickness_2,
                     T ~ days_disabled_sickness_1))

df2 <- df2 %>%
  mutate(days_disabled_sickness_1 =
           case_when(!is.na(days_disabled_sickness_3) ~
                       days_disabled_sickness_1 +
                       days_disabled_sickness_3,
                     T ~ days_disabled_sickness_1))

df2 <- subset(df2, select = c(pid, age.cat, sickness.during.interval, days_disabled_sickness_1))

df2 <- df2 %>%
  group_by(age.cat) %>%
  mutate(n = n_distinct(pid))

df2 <- df2 %>%
  filter(sickness.during.interval == 1)

df2 <- df2 %>%
  filter(!is.na(days_disabled_sickness_1))

df2 <- df2 %>%
  group_by(age.cat, n) %>%
  summarise(days_disabled_risk = sum(days_disabled_sickness_1))

df2$risk <- "Sickness"


# Animal Attack ----

df3 <- read.csv("Animal_Attack_time_to_first_risk_short_interval.csv")

df3 <- df3 %>%
  mutate(days_disabled_Animal_Attacked_1 =
           case_when(!is.na(days_disabled_Animal_Attacked_2) ~
                       days_disabled_Animal_Attacked_1 +
                       days_disabled_Animal_Attacked_2,
                     T ~ days_disabled_Animal_Attacked_1))

df3 <- df3 %>%
  mutate(days_disabled_Animal_Attacked_1 =
           case_when(!is.na(days_disabled_Animal_Attacked_3) ~
                       days_disabled_Animal_Attacked_1 +
                       days_disabled_Animal_Attacked_3,
                     T ~ days_disabled_Animal_Attacked_1))

df3 <- subset(df3, select = c(pid, age.cat, Animal_Attack.during.interval, days_disabled_Animal_Attacked_1))

df3 <- df3 %>%
  group_by(age.cat) %>%
  mutate(n = n_distinct(pid))

df3 <- df3 %>%
  filter(Animal_Attack.during.interval == 1)

df3 <- df3 %>%
  filter(!is.na(days_disabled_Animal_Attacked_1))

df3 <- df3 %>%
  group_by(age.cat, n) %>%
  summarise(days_disabled_risk = sum(days_disabled_Animal_Attacked_1))

df3$risk <- "Animal Attack"


# Cut Self ----

df4 <- read.csv("cut_self_time_to_first_risk_short_interval.csv")

df4 <- df4 %>%
  mutate(cut_self_days_disabled_1 =
           case_when(!is.na(cut_self_days_disabled_2) ~
                       cut_self_days_disabled_1 +
                       cut_self_days_disabled_2,
                     T ~ cut_self_days_disabled_1))

df4 <- df4 %>%
  mutate(cut_self_days_disabled_1 =
           case_when(!is.na(cut_self_days_disabled_3) ~
                       cut_self_days_disabled_1 +
                       cut_self_days_disabled_3,
                     T ~ cut_self_days_disabled_1))

df4 <- subset(df4, select = c(pid, age.cat, cut.self.during.interval, cut_self_days_disabled_1))

df4 <- df4 %>%
  group_by(age.cat) %>%
  mutate(n = n_distinct(pid))

df4 <- df4 %>%
  filter(cut.self.during.interval == 1)

df4 <- df4 %>%
  filter(!is.na(cut_self_days_disabled_1))

df4 <- df4 %>%
  group_by(age.cat, n) %>%
  summarise(days_disabled_risk = sum(cut_self_days_disabled_1))

df4$risk <- "Cut Self"


# Canoe Capsize ----

df5 <- read.csv("canoe_capsize_time_to_first_risk_short_interval.csv")

df5 <- df5 %>%
  mutate(canoe_capsize_days_disabled_1 =
           case_when(!is.na(canoe_capsize_days_disabled_2) ~
                       canoe_capsize_days_disabled_1 +
                       canoe_capsize_days_disabled_2,
                     T ~ canoe_capsize_days_disabled_1))

df5 <- subset(df5, select = c(pid, age.cat, canoe.capsize.during.interval, canoe_capsize_days_disabled_1))

df5 <- df5 %>%
  group_by(age.cat) %>%
  mutate(n = n_distinct(pid))

df5 <- df5 %>%
  filter(canoe.capsize.during.interval == 1)

df5 <- df5 %>%
  filter(!is.na(canoe_capsize_days_disabled_1))

df5 <- df5 %>%
  group_by(age.cat, n) %>%
  summarise(days_disabled_risk = sum(canoe_capsize_days_disabled_1))

df5$risk <- "Canoe Capsize"


# Fight ----

df6 <- read.csv("fought_time_to_first_risk_short_interval.csv")

df6 <- df6 %>%
  mutate(fought_days_injured_1 =
           case_when(!is.na(fought_days_injured_2) ~
                       fought_days_injured_1 +
                       fought_days_injured_2,
                     T ~ fought_days_injured_1))

df6 <- df6 %>%
  mutate(fought_days_injured_1 =
           case_when(!is.na(fought_days_injured_3) ~
                       fought_days_injured_1 +
                       fought_days_injured_3,
                     T ~ fought_days_injured_1))

df6 <- subset(df6, select = c(pid, age.cat, fought.during.interval, fought_days_injured_1))

df6 <- df6 %>%
  group_by(age.cat) %>%
  mutate(n = n_distinct(pid))

df6 <- df6 %>%
  filter(fought.during.interval == 1)

df6 <- df6 %>%
  filter(!is.na(fought_days_injured_1))

df6 <- df6 %>%
  group_by(age.cat, n) %>%
  summarise(days_disabled_risk = sum(fought_days_injured_1))

df6$risk <- "Fight"



# Combining ---------------------------------------------------------------

df <- bind_rows(df1, df2, df3, df4, df5, df6)

df$risk <- factor(df$risk, levels = c("Sickness", "Cut Self", "Animal Attack",
                                      "Tree Fall", "Fight", "Canoe Capsize"))

df$age.cat <- factor(df$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))

rm(df1, df2, df3, df4, df5, df6)

# 1. Stacked plot one year intervals --------------------------------------

df %>%
  ggplot(aes(x = age.cat, y = days_disabled_risk, group = risk, fill = risk)) +
  geom_area(position = 'stack', alpha = 0.8) +
  viridis::scale_fill_viridis(discrete = T) +
  theme_classic(base_size = 12) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(fill = "") +
  xlab("Age of Occurrence") +
  ylab("Days Disabled/Injured") +
  ggtitle("ALL RISKS") +
  theme(plot.title = element_text(size = 30, hjust = 0.5),
        legend.position = c(0.75, 0.7))

# 2. Stacked proportion plot one year intervals ---------------------------
df$n <- df$n * 365 # this is the denominator

df %>%
  ggplot(aes(x = age.cat, y = days_disabled_risk / n, group = risk, fill = risk)) +
  geom_area(position = 'stack', alpha = 0.8) +
  viridis::scale_fill_viridis(discrete = T) +
  theme_classic(base_size = 12) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(fill = "") +
  xlab("Age of Occurrence") +
  ylab("Proportion of Days Disabled/Injured") +
  ggtitle("ALL RISKS") +
  theme(plot.title = element_text(size = 30, hjust = 0.5),
        legend.position = c(0.6, 0.7))
