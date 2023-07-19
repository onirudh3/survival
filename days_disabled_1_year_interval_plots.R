
# Libraries and data import -----------------------------------------------

library(tidyverse)

# Tree Fall ----

df1 <- read.csv("tree_fall_time_to_first_risk_short_interval.csv")

df1$exit <- ceiling(df1$exit)

# df1 <- df1 %>%
# filter(!near(round(exit), exit)) # to see which exits are fractions

df1 <- df1 %>%
  mutate(tree_fall_days_disabled_1 =
           case_when(!is.na(tree_fall_days_disabled_2) ~
                       tree_fall_days_disabled_1 +
                       tree_fall_days_disabled_2,
                     T ~ tree_fall_days_disabled_1))

df1 <- subset(df1, select = c(pid, exit, tree.fall.during.interval, tree_fall_days_disabled_1))

df1 <- df1 %>%
  group_by(exit) %>%
  mutate(n = n())

df1 <- df1 %>%
  filter(tree.fall.during.interval == 1)

df1 <- df1 %>%
  filter(!is.na(tree_fall_days_disabled_1)) # remove NA individuals

df1 <- df1 %>%
  group_by(exit, n) %>%
  summarise(days_disabled_risk = sum(tree_fall_days_disabled_1)) # get sum of days disabled

df1$exit <- as.character(df1$exit)

df1 <- df1 %>%
  mutate(exit = case_when(exit %in% 40:45 ~ "40-45",
                         exit %in% 45:50 ~ "45-50",
                         exit %in% 50:55 ~ "50-55",
                         exit %in% 55:60 ~ "55-60",
                         exit %in% 60:100 ~ "60+",
                         T ~ exit)) %>%
  group_by(exit) %>%
  summarise(days_disabled_risk = sum(days_disabled_risk),
            n = sum(n))

df1$risk <- "Tree Fall"


# Sickness ----

df2 <- read.csv("sickness_time_to_first_risk_short_interval.csv")

df2$exit <- ceiling(df2$exit)

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

df2 <- subset(df2, select = c(pid, exit, sickness.during.interval, days_disabled_sickness_1))

df2 <- df2 %>%
  group_by(exit) %>%
  mutate(n = n())

df2 <- df2 %>%
  filter(sickness.during.interval == 1)

df2 <- df2 %>%
  filter(!is.na(days_disabled_sickness_1)) # remove NA individuals

df2 <- df2 %>%
  group_by(exit, n) %>%
  summarise(days_disabled_risk = sum(days_disabled_sickness_1))

df2$exit <- as.character(df2$exit)

df2 <- df2 %>%
  mutate(exit = case_when(exit %in% 40:45 ~ "40-45",
                          exit %in% 45:50 ~ "45-50",
                          exit %in% 50:55 ~ "50-55",
                          exit %in% 55:60 ~ "55-60",
                          exit %in% 60:100 ~ "60+",
                          T ~ exit)) %>%
  group_by(exit) %>%
  summarise(days_disabled_risk = sum(days_disabled_risk),
            n = sum(n))

df2$risk <- "Sickness"


# Animal Attack ----

df3 <- read.csv("Animal_Attack_time_to_first_risk_short_interval.csv")

df3$exit <- ceiling(df3$exit)

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

df3 <- subset(df3, select = c(pid, exit, Animal_Attack.during.interval, days_disabled_Animal_Attacked_1))

df3 <- df3 %>%
  group_by(exit) %>%
  mutate(n = n())

df3 <- df3 %>%
  filter(Animal_Attack.during.interval == 1)

df3 <- df3 %>%
  filter(!is.na(days_disabled_Animal_Attacked_1)) # remove NA individuals

df3 <- df3 %>%
  group_by(exit, n) %>%
  summarise(days_disabled_risk = sum(days_disabled_Animal_Attacked_1))

df3$exit <- as.character(df3$exit)

df3 <- df3 %>%
  mutate(exit = case_when(exit %in% 40:45 ~ "40-45",
                          exit %in% 45:50 ~ "45-50",
                          exit %in% 50:55 ~ "50-55",
                          exit %in% 55:60 ~ "55-60",
                          exit %in% 60:100 ~ "60+",
                          T ~ exit)) %>%
  group_by(exit) %>%
  summarise(days_disabled_risk = sum(days_disabled_risk),
            n = sum(n))

df3$risk <- "Animal Attack"



# Cut Self ----

df4 <- read.csv("cut_self_time_to_first_risk_short_interval.csv")

df4$exit <- ceiling(df4$exit)

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

df4 <- subset(df4, select = c(pid, exit, cut.self.during.interval, cut_self_days_disabled_1))

df4 <- df4 %>%
  group_by(exit) %>%
  mutate(n = n())

df4 <- df4 %>%
  filter(cut.self.during.interval == 1)

df4 <- df4 %>%
  filter(!is.na(cut_self_days_disabled_1)) # remove NA individuals

df4 <- df4 %>%
  group_by(exit, n) %>%
  summarise(days_disabled_risk = sum(cut_self_days_disabled_1))

df4$exit <- as.character(df4$exit)

df4 <- df4 %>%
  mutate(exit = case_when(exit %in% 40:45 ~ "40-45",
                          exit %in% 45:50 ~ "45-50",
                          exit %in% 50:55 ~ "50-55",
                          exit %in% 55:60 ~ "55-60",
                          exit %in% 60:100 ~ "60+",
                          T ~ exit)) %>%
  group_by(exit) %>%
  summarise(days_disabled_risk = sum(days_disabled_risk),
            n = sum(n))

df4$risk <- "Cut Self"


# Canoe Capsize ----

df5 <- read.csv("canoe_capsize_time_to_first_risk_short_interval.csv")

df5$exit <- ceiling(df5$exit)

df5 <- df5 %>%
  mutate(canoe_capsize_days_disabled_1 =
           case_when(!is.na(canoe_capsize_days_disabled_2) ~
                       canoe_capsize_days_disabled_1 +
                       canoe_capsize_days_disabled_2,
                     T ~ canoe_capsize_days_disabled_1))

df5 <- subset(df5, select = c(pid, exit, canoe.capsize.during.interval, canoe_capsize_days_disabled_1))

df5 <- df5 %>%
  group_by(exit) %>%
  mutate(n = n())

df5 <- df5 %>%
  filter(canoe.capsize.during.interval == 1)

df5 <- df5 %>%
  filter(!is.na(canoe_capsize_days_disabled_1)) # remove NA individuals

df5 <- df5 %>%
  group_by(exit, n) %>%
  summarise(days_disabled_risk = sum(canoe_capsize_days_disabled_1))

df5$exit <- as.character(df5$exit)

df5 <- df5 %>%
  mutate(exit = case_when(exit %in% 40:45 ~ "40-45",
                          exit %in% 45:50 ~ "45-50",
                          exit %in% 50:55 ~ "50-55",
                          exit %in% 55:60 ~ "55-60",
                          exit %in% 60:100 ~ "60+",
                          T ~ exit)) %>%
  group_by(exit) %>%
  summarise(days_disabled_risk = sum(days_disabled_risk),
            n = sum(n))

df5$risk <- "Canoe Capsize"


# Fight ----

df6 <- read.csv("fought_time_to_first_risk_short_interval.csv")

df6$exit <- ceiling(df6$exit)

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

df6 <- subset(df6, select = c(pid, exit, fought.during.interval, fought_days_injured_1))

df6 <- df6 %>%
  group_by(exit) %>%
  mutate(n = n())

df6 <- df6 %>%
  filter(fought.during.interval == 1)

df6 <- df6 %>%
  filter(!is.na(fought_days_injured_1)) # remove NA individuals

df6 <- df6 %>%
  group_by(exit, n) %>%
  summarise(days_disabled_risk = sum(fought_days_injured_1))

df6$exit <- as.character(df6$exit)

df6 <- df6 %>%
  mutate(exit = case_when(exit %in% 40:45 ~ "40-45",
                          exit %in% 45:50 ~ "45-50",
                          exit %in% 50:55 ~ "50-55",
                          exit %in% 55:60 ~ "55-60",
                          exit %in% 60:100 ~ "60+",
                          T ~ exit)) %>%
  group_by(exit) %>%
  summarise(days_disabled_risk = sum(days_disabled_risk),
            n = sum(n))

df6$risk <- "Fight"


# Combining dfs -----------------------------------------------------------

df <- bind_rows(df1, df2, df3, df4, df5, df6)

df$risk <- factor(df$risk, levels = c("Sickness", "Cut Self", "Animal Attack",
                                      "Tree Fall", "Fight", "Canoe Capsize"))

rm(df1, df2, df3, df4, df5, df6)

df <- df %>%
  group_by(exit, risk) %>%
  summarise(across(days_disabled_risk:n, sum))

df$exit <- factor(df$exit, levels = c("1", "2", "3", "4", "5", "6", "7", "8",
                                      "9", "10", "11", "12", "13", "14", "15",
                                      "16", "17", "18", "19", "20", "21", "22",
                                      "23", "24", "25", "26", "27", "28", "29",
                                      "30", "31", "32", "33", "34", "35", "36",
                                      "37", "38", "39", "40-45", "45-50", "50-55",
                                      "55-60", "60+"))

# 1. Stacked plot one year intervals --------------------------------------

df %>%
  ggplot(aes(x = exit, y = days_disabled_risk, group = risk, fill = risk)) +
  geom_area(position = 'stack', alpha = 0.8) +
  viridis::scale_fill_viridis(discrete = T) +
  theme_classic(base_size = 12) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(fill = "") +
  xlab("Age of Occurrence") +
  guides(x =  guide_axis(angle = 45)) +
  ylab("Days Disabled/Injured") +
  ggtitle("ALL RISKS") +
  theme(plot.title = element_text(size = 30, hjust = 0.5),
        legend.position = c(0.75, 0.7))


# 2. Stacked proportion plot one year intervals ---------------------------
df$n <- df$n * 365 # this is the denominator

df %>%
  ggplot(aes(x = exit, y = days_disabled_risk / n, group = risk, fill = risk)) +
  geom_area(position = 'stack', alpha = 0.8) +
  viridis::scale_fill_viridis(discrete = T) +
  theme_classic(base_size = 12) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(fill = "") +
  xlab("Age of Occurrence") +
  guides(x =  guide_axis(angle = 45)) +
  ylab("Proportion of Days Disabled/Injured") +
  ggtitle("ALL RISKS") +
  theme(plot.title = element_text(size = 30, hjust = 0.5),
        legend.position = c(0.75, 0.7))
