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
