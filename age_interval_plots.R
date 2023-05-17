# Libraries
library(tidyverse)
library(scales)
library(gtable)
library(grid)

# Import the tree fall final table
df_final <- read.csv("treefall_final_table.csv")

################################################################################
df_final$male <- ifelse(df_final$male == 1, "Male", "Female")
################################################################################

# Creating interval categories for occurrence of tree fall
df_final <- df_final %>%
  mutate(treefall.occurrence.interval = case_when(event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                  event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                  event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                  event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                  event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                  event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                  event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                  event == 1 & exit >= 70 & exit < 80 ~ "70-79"))

# Creating interval categories for co-occurrences of sickness
df_final <- df_final %>%
  mutate(sickness.co_occurrence.interval = case_when(sickness.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))
# df_final$sickness.co_occurrence.interval <- ifelse(is.na(df_final$sickness.co_occurrence.interval), "No Co-occurrence", df_final$sickness.co_occurrence.interval)

# Creating interval categories for co-occurrences of snake/ray bite
df_final <- df_final %>%
  mutate(bite.co_occurrence.interval = case_when(bite.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                 bite.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                 bite.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                 bite.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                 bite.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                 bite.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                 bite.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                 bite.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))
# df_final$bite.co_occurrence.interval <- ifelse(is.na(df_final$bite.co_occurrence.interval), "No Co-occurrence", df_final$sickness.co_occurrence.interval)


# Creating interval categories for co-occurrences of fought
df_final <- df_final %>%
  mutate(fought.co_occurrence.interval = case_when(fought.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                   fought.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                   fought.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                   fought.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                   fought.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                   fought.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                   fought.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                   fought.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))
# df_final$fought.co_occurrence.interval <- ifelse(is.na(df_final$fought.co_occurrence.interval), "No Co-occurrence", df_final$sickness.co_occurrence.interval)

# Creating interval categories for co-occurrences of animal.attack
df_final <- df_final %>%
  mutate(animal.attack.co_occurrence.interval = case_when(animal.attack.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))
# df_final$animal.attack.co_occurrence.interval <- ifelse(is.na(df_final$animal.attack.co_occurrence.interval), "No Co-occurrence", df_final$sickness.co_occurrence.interval)

# Creating interval categories for co-occurrences of canoe.capsize
df_final <- df_final %>%
  mutate(canoe.capsize.co_occurrence.interval = case_when(canoe.capsize.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))
# df_final$canoe.capsize.co_occurrence.interval <- ifelse(is.na(df_final$canoe.capsize.co_occurrence.interval), "No Co-occurrence", df_final$sickness.co_occurrence.interval)

# Creating interval categories for co-occurrences of cut.self
df_final <- df_final %>%
  mutate(cut.self.co_occurrence.interval = case_when(cut.self.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))
# df_final$cut.self.co_occurrence.interval <- ifelse(is.na(df_final$cut.self.co_occurrence.interval), "No Co-occurrence", df_final$sickness.co_occurrence.interval)



################################################################################
################################################################################
# Distribution of co-occurring sickness within tree fall age intervals
df_final %>%
  count(sickness.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(sickness.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = sickness.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.6) +
  geom_text(aes(x = sickness.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 6) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and SICKNESS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals")

# By gender
df_final %>%
  count(sickness.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(sickness.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = sickness.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.6) +
  geom_text(aes(x = sickness.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 6) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and SICKNESS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())

################################################################################
# Distribution of co-occurring snake/ray bite within tree fall age intervals
df_final %>%
  count(bite.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(bite.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = bite.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.6) +
  geom_text(aes(x = bite.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 6) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals")

# By gender
df_final %>%
  count(bite.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(bite.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = bite.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.6) +
  geom_text(aes(x = bite.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 6) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())

################################################################################
# Distribution of co-occurring fought within tree fall age intervals
df_final %>%
  count(fought.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(fought.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = fought.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.6) +
  geom_text(aes(x = fought.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 6) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and FOUGHT") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals")

# By gender
df_final %>%
  count(fought.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(fought.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = fought.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.6) +
  geom_text(aes(x = fought.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 6) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and FOUGHT") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())

################################################################################
# Distribution of co-occurring animal attack within tree fall age intervals
df_final %>%
  count(animal.attack.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(animal.attack.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = animal.attack.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.6) +
  geom_text(aes(x = animal.attack.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .0001, size = 6) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and ANIMAL ATTACK") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals")

# By gender
df_final %>%
  count(animal.attack.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(animal.attack.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = animal.attack.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.6) +
  geom_text(aes(x = animal.attack.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .0001, size = 6) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and ANIMAL ATTACK") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())

################################################################################
# Distribution of co-occurring canoe capsize within tree fall age intervals
df_final %>%
  count(canoe.capsize.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(canoe.capsize.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = canoe.capsize.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.6) +
  geom_text(aes(x = canoe.capsize.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 6) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals")

# By gender
df_final %>%
  count(canoe.capsize.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(canoe.capsize.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = canoe.capsize.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.6) +
  geom_text(aes(x = canoe.capsize.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .0001, size = 6) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())

################################################################################
# Distribution of co-occurring cut self within tree fall age intervals
df_final %>%
  count(cut.self.co_occurrence.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(cut.self.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = cut.self.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.6) +
  geom_text(aes(x = cut.self.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .001, size = 6) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and CUT SELF") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals")

# By gender
df_final %>%
  count(cut.self.co_occurrence.interval, male) %>%
  mutate(prop = prop.table(n)) %>%
  filter(!is.na(cut.self.co_occurrence.interval)) %>%
  ggplot() +
  geom_col(aes(x = cut.self.co_occurrence.interval, y = prop), fill = "lightblue", width = 0.6) +
  geom_text(aes(x = cut.self.co_occurrence.interval, y = prop, label = scales::percent(prop)), vjust = 0, nudge_y = .0005, size = 6) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and CUT SELF") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Co-occurrence") +
  ylab("Percentage of Intervals") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())

################################################################################
