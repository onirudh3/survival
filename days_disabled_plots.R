
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

df1 <- df1 %>%
  filter(tree.fall.during.interval == 1)

df1 <- df1 %>%
  filter(!is.na(tree_fall_days_disabled_1)) # remove NA individuals

df1 <- subset(df1, select = c(pid, exit, tree_fall_days_disabled_1))

df1 <- df1 %>%
  group_by(exit) %>%
  summarise(days_disabled_risk = sum(tree_fall_days_disabled_1),
            n = n()) # get sum of days disabled

df1$exit <- as.character(df1$exit)

df <- df1 %>%
  mutate(exit = case_when(exit %in% 40:45 ~ "40-45",
                         exit %in% 45:50 ~ "45-50",
                         exit %in% 50:55 ~ "50-55",
                         exit %in% 55:60 ~ "55-60",
                         exit %in% 60:100 ~ "60+",
                         T ~ exit)) %>%
  group_by(exit) %>%
  summarise(days_disabled_risk = sum(days_disabled_risk),
            n = sum(n))

df1 <- df1 %>%
  mutate(exit = as.numeric(exit),
         age_range = paste0(floor(exit / 5) * 5, "-", ceiling(exit / 5) * 5)) %>%
  mutate(exit = case_when(between(exit, 40.00001, 60) ~ age_range, exit > 60 ~ "60+", T ~ as.character(exit))) %>%
  select(-age_range) %>%
  summarise(days_disabled_risk = sum(days_disabled_risk), n = sum(n), .by = "exit")

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

df2 <- df2 %>%
  filter(sickness.during.interval == 1)

df2 <- df2 %>%
  filter(!is.na(days_disabled_sickness_1)) # remove NA individuals

df2 <- subset(df2, select = c(pid, exit, days_disabled_sickness_1))

df2 <- df2 %>%
  group_by(exit) %>%
  summarise(days_disabled_risk = sum(days_disabled_sickness_1),
            n = n())

df2 <- df2 %>%
  mutate(exit = as.numeric(exit),
         age_range = paste0(floor(exit / 5) * 5, "-", ceiling(exit / 5) * 5)) %>%
  mutate(exit = case_when(between(exit, 40.00001, 60) ~ age_range, exit > 60 ~ "60+", T ~ as.character(exit))) %>%
  select(-age_range) %>%
  summarise(days_disabled_risk = sum(days_disabled_risk), n = sum(n), .by = "exit")

df2$risk <- "Sickness"



