# Libraries
library(tidyverse)
library(scales)

# Import the tree fall final table
df_final <- read.csv("treefall_final_table.csv")


########################### PERCENTAGE PLOTS ###################################
################################################################################
# Changing the 0's and 1's of the variables for the plots to be more understandable
df_final$event <- ifelse(df_final$event == 1, "Treefall Occurred", "Treefall Did Not Occur")
df_final$male <- ifelse(df_final$male == 1, "Male", "Female")
df_final$sickness.during.interval <- ifelse(df_final$sickness.during.interval == 1, "Sickness Occured", "Sickness Did Not Occur")
df_final$bite.during.interval <- ifelse(df_final$bite.during.interval == 1, "Snake/Ray Bite Occured", "Snake/Ray Bite Did Not Occur")
df_final$fought.during.interval <- ifelse(df_final$fought.during.interval == 1, "Fight Occured", "Fight Did Not Occur")
df_final$animal.attack.during.interval <- ifelse(df_final$animal.attack.during.interval == 1, "Animal Attack Occured", "Animal Attack Did Not Occur")
df_final$canoe.capsize.during.interval <- ifelse(df_final$canoe.capsize.during.interval == 1, "Canoe Capsize Occured", "Canoe Capsize Did Not Occur")
df_final$cut.self.during.interval <- ifelse(df_final$cut.self.during.interval == 1, "Cut Self Occured", "Cut Self Not Occur")
################################################################################

# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which sickness occurs
df_final %>%
  count(sickness.during.interval, event) %>%
  group_by(sickness.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(sickness.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and SICKNESS") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "")

# For males and females
df_final %>%
  count(sickness.during.interval, event, male) %>%
  group_by(sickness.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(sickness.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 15) +
  ggtitle("TREE FALL and SICKNESS") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())

# By region
df_final %>%
  count(sickness.during.interval, event, region) %>%
  group_by(sickness.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(sickness.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 15) +
  ggtitle("TREE FALL and SICKNESS") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "") +
  facet_wrap(~region) +
  theme(strip.background = element_blank()) +
  guides(x =  guide_axis(angle = 90))

################################################################################
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which snake or ray bite occurs
df_final %>%
  count(bite.during.interval, event) %>%
  group_by(bite.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(bite.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "")

# For males and females
df_final %>%
  count(bite.during.interval, event, male) %>%
  group_by(bite.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(bite.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 15) +
  ggtitle("TREE FALL and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())

# By region
df_final %>%
  count(bite.during.interval, event, region) %>%
  group_by(bite.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(bite.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 15) +
  ggtitle("TREE FALL and SNAKE/RAY BITE") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "") +
  facet_wrap(~region) +
  theme(strip.background = element_blank()) +
  guides(x =  guide_axis(angle = 90))

################################################################################
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which fought
df_final %>%
  count(fought.during.interval, event) %>%
  group_by(fought.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(fought.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and FOUGHT") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "")

# For males and females
df_final %>%
  count(fought.during.interval, event, male) %>%
  group_by(fought.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(fought.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 15) +
  ggtitle("TREE FALL and FOUGHT") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())

# By region
df_final %>%
  count(fought.during.interval, event, region) %>%
  group_by(fought.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(fought.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 15) +
  ggtitle("TREE FALL and FOUGHT") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "") +
  facet_wrap(~region) +
  theme(strip.background = element_blank()) +
  guides(x =  guide_axis(angle = 90))

################################################################################
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which animal attack
df_final %>%
  count(animal.attack.during.interval, event) %>%
  group_by(animal.attack.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(animal.attack.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and ANIMAL ATTACK") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "")

# For males and females
df_final %>%
  count(animal.attack.during.interval, event, male) %>%
  group_by(animal.attack.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(animal.attack.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 15) +
  ggtitle("TREE FALL and ANIMAL ATTACK") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())

# By region
df_final %>%
  count(animal.attack.during.interval, event, region) %>%
  group_by(animal.attack.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(animal.attack.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 15) +
  ggtitle("TREE FALL and ANIMAL ATTACK") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "") +
  facet_wrap(~region) +
  theme(strip.background = element_blank()) +
  guides(x =  guide_axis(angle = 90))

# It appears that no tree fall occurred after animal attack upriver. Let us check
View(df_final[(df_final$region == "Upriver"),])

################################################################################
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which canoe capsize
df_final %>%
  count(canoe.capsize.during.interval, event) %>%
  group_by(canoe.capsize.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(canoe.capsize.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "")

# For males and females
df_final %>%
  count(canoe.capsize.during.interval, event, male) %>%
  group_by(canoe.capsize.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(canoe.capsize.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 15) +
  ggtitle("TREE FALL and CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())

# By region
df_final %>%
  count(canoe.capsize.during.interval, event, region) %>%
  group_by(canoe.capsize.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(canoe.capsize.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 15) +
  ggtitle("TREE FALL and CANOE CAPSIZE") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "") +
  facet_wrap(~region) +
  theme(strip.background = element_blank()) +
  guides(x =  guide_axis(angle = 90))

################################################################################
# Compare intervals where event = 1 vs. intervals where event = 0,
# what is the percentage in which cut self
df_final %>%
  count(cut.self.during.interval, event) %>%
  group_by(cut.self.during.interval) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(cut.self.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 20) +
  ggtitle("TREE FALL and CUT SELF") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "")

# For males and females
df_final %>%
  count(cut.self.during.interval, event, male) %>%
  group_by(cut.self.during.interval, male) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(cut.self.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 15) +
  ggtitle("TREE FALL and CUT SELF") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "") +
  facet_wrap(~male) +
  theme(strip.background = element_blank())

# By region
df_final %>%
  count(cut.self.during.interval, event, region) %>%
  group_by(cut.self.during.interval, region) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(cut.self.during.interval, pct, fill = event)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct),"%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 15) +
  ggtitle("TREE FALL and CUT SELF") +
  theme(plot.title = element_text(size = 50)) +
  xlab("") +
  ylab("Percentage of Intervals") +
  labs(fill = "") +
  facet_wrap(~region) +
  theme(strip.background = element_blank()) +
  guides(x =  guide_axis(angle = 90))

################################################################################
################################################################################
