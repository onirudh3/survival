############################ LIBRARIES #########################################
library(tidyverse)
library(eha)
library(survival)
library(readxl)
library(dplyr)

# Load data
df_final <- read.csv("treefall_final_table.csv")
raw_df <- read.csv("raw_data_no_duplicates.csv")
df_final <- df_final[c("pid", "age", "male", "region", "enter", "exit",
                       "event")]

#################### WORKING OUT RE-FORMATTING ISSUES ##########################

# # Reformat the data frame
# # ?survSplit()
# cut_vector <- c(1:80)
# df <- survSplit(df_final, cut = cut_vector, start = "enter", end = "exit",
#                 event = "event")
#
# # Did it work correctly?
# sum(raw_df$age)
# # Exact sum of ages is 13,254.9397672827
#
# raw_df$age_rounded_up <- ceiling(raw_df$age)
# sum(raw_df$age_rounded_up)
# # Shows 13451, ideally should match no. of rows in df which is showing 13463
#
# # What are these 12 rows where the problem is coming from?
# df_row <- df %>% count(pid)
# raw_df <- left_join(raw_df, df_row)
# View(raw_df[(raw_df$n != raw_df$age_rounded_up),])
# # So 12 individuals reported precise decimal tree fall times, so it is
# # creating extra interval
# id_list <- c("ALJ4", "TTWM", "3TUC", "VCV2", "DJB7", "D7FT", "VHKK", "VBYN",
#              "ISPN", "9XE5", "QBCD", "X9HY")
#
# # Let's fix this so only last interval (till age) for an individual is not a
# # year long


############################ RE-FORMATTING #####################################

# Rounding up the tree fall times so that we get the desired final output
raw_df$tf.age1 <- ceiling(raw_df$tf.age1)
raw_df$tf.age2 <-  ceiling(raw_df$tf.age2)

# Recycled code from tree fall cleaning script
# For splitting, we need ID, age, event, and the corresponding ages
db3 <- raw_df[c("pid", "tree.fall.ever", "age", "tf.age1", "tf.age2",
                "tf.age3")]

# Creating the row splits
db4 <- db3 %>%
  mutate(start = 0, end = age) %>%
  dplyr::select(-tree.fall.ever) %>%
  gather(tree.fall.ever, enter, -pid) %>%
  group_by(pid) %>%
  arrange(pid, enter) %>%
  filter(!is.na(enter)) %>%
  mutate(exit = lead(enter)) %>%
  filter(!is.na(exit), !grepl("time_to_risk_out_start", tree.fall.ever)) %>%
  mutate(event = lead(grepl("time_to_event", tree.fall.ever), default = 0)) %>%
  dplyr::select(pid, enter, exit, event) %>%
  ungroup()

# Cleaning up
db4 <- subset(db4, enter != exit)
db5 <- raw_df[c("pid", "age", "tf.age1",
                "tf.age2", "tf.age3")]
db6 <- left_join(db4, db5, by = "pid")
db6$event <- ifelse(db6$exit == db6$age, 0, 1)

# Adding region column
region_df <- read_xls("threat_wide___sumACEs_for anirudh.xls")
region_df <- region_df[c("pid", "region", "age")]
region_df <- region_df %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()
region_df <- region_df[c("pid", "region")]
db6 <- left_join(db6, region_df, by = "pid")

# Final table
db6 <- db6[c("pid", "age", "region", "enter", "exit", "event")]

# Re-formatted with one year intervals
cut_vector <- c(1:80)
df <- survSplit(db6, cut = cut_vector, start = "enter", end = "exit",
                event = "event")

# So we get 13454, the final pieces of the puzzle are beginning to reveal
# themselves. There seem to be three individuals reported tree fall age = age.
df_final <- df_final[c("pid", "age", "region", "enter", "exit", "event")]
anti <- anti_join(db6, df_final)
plyr::count(anti$pid)
# 3WPX, DYJA, F9DJ are the problem, where age = age of tree fall, hence three
# extra rows

# Let's fix this
# Approach is the following:
# If exit > age, delete current row and assign event = 1 to the preceding row
df <- df %>%
  mutate(event = case_when(lead(exit) > lead(age) ~ 1, TRUE ~ event))

# Remove the three rows where exit > age
df <- subset(df, exit <= age)
# We have got 13451 rows, so this is the correct data frame


#################### ADDING COLUMNS FOR DIFFERENT THREATS ######################
# Sickness ####
# Read sickness data
sick_df3 <- read.csv("sickness_cleaned.csv")

# Merging sickness and tree fall
df <- left_join(df, sick_df3, by = "pid")

# Have you ever been sick in the intervals that you experienced tree fall?
df <- df %>%
  mutate(sickness.during.interval = case_when(enter < sickness.age & sickness.age <= exit ~ 1,
                                              enter < sickness.age1 & sickness.age1 <= exit ~ 1,
                                              enter < sickness.age2 & sickness.age2 <= exit ~ 1,
                                              TRUE ~ 0))
# plyr::count(df$sickness.during.interval)

# Snake/Ray Bite ####
# Read snake/ray bite data
snake_df3 <- read.csv("snake_bite_cleaned.csv")

# Merging snake/ray bite and tree fall
df <- left_join(df, snake_df3, by = "pid")

# Have you ever been bit in the intervals that you experienced tree fall?
df <- df %>%
  mutate(bite.during.interval = case_when(enter < snake.or.ray.bite.age & snake.or.ray.bite.age <= exit ~ 1,
                                              enter < snake.or.ray.bite.age1 & snake.or.ray.bite.age1 <= exit ~ 1,
                                              enter < snake.or.ray.bite.age2 & snake.or.ray.bite.age2 <= exit ~ 1,
                                              TRUE ~ 0))


# Fought ####
# Read fought data
fought_df3 <- read.csv("fought_cleaned.csv")

# Merging fought and tree fall
df <- left_join(df, fought_df3, by = "pid")

# Have you ever fought in the intervals that you experienced tree fall?
df <- df %>%
  mutate(fought.during.interval = case_when(enter < fought.age & fought.age <= exit ~ 1,
                                              enter < fought.age1 & fought.age1 <= exit ~ 1,
                                              enter < fought.age2 & fought.age2 <= exit ~ 1,
                                              TRUE ~ 0))


# Animal Attack ####
# Read animal attack data
animal_attack_df3 <- read.csv("animal_attack_cleaned.csv")

# Merge animal attack and tree fall
df <- left_join(df, animal_attack_df3, by = "pid")

# Have you ever experienced animal attack in the same intervals that you
# experienced tree fall?
df <- df %>%
  mutate(animal.attack.during.interval = case_when(enter < animal.attack.age & animal.attack.age <= exit ~ 1,
                                            enter < animal.attack.age1 & animal.attack.age1 <= exit ~ 1,
                                            TRUE ~ 0))


# Canoe Capsize ####
# Read canoe capsize data
ds1 <- read.csv("canoe_cleaned.csv")

# Merge canoe capsize and tree fall
df <- left_join(df, ds1, by = "pid")

# Have you ever canoe capsize in the intervals that you experienced tree fall?
df <- df %>%
  mutate(canoe.capsize.during.interval = case_when(enter < cc.age1 & cc.age1 <= exit ~ 1,
                                            enter < cc.age2 & cc.age2 <= exit ~ 1,
                                            enter < cc.age3 & cc.age3 <= exit ~ 1,
                                            TRUE ~ 0))

# Cut Self ####
# Read cut self data
dc1 <- read.csv("cut_self_cleaned.csv")

# Merge cut self and tree fall
df <- left_join(df, dc1, by = "pid")

# Have you ever cut self in the intervals that you experienced tree fall?
df <- df %>%
  mutate(cut.self.during.interval = case_when(enter < cut.age1 & cut.age1 <= exit ~ 1,
                                              enter < cut.age2 & cut.age2 <= exit ~ 1,
                                              enter < cut.age3 & cut.age3 <= exit ~ 1,
                                              enter < cut.age4 & cut.age4 <= exit ~ 1,
                                              enter < cut.age5 & cut.age5 <= exit ~ 1,
                                              enter < cut.age6 & cut.age6 <= exit ~ 1,
                                                   TRUE ~ 0))



############################### FINAL STEPS ####################################
# Adding male column
male_df <- read_xls("threat_wide___sumACEs_for anirudh.xls")
male_df <- male_df %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()
male_df <- male_df[c("pid", "male")]
df <- left_join(df, male_df)

# Selecting only the necessary columns
df <- df[c("pid", "age", "male", "region", "enter", "exit", "event",
           "sickness.during.interval", "bite.during.interval",
           "fought.during.interval", "animal.attack.during.interval",
           "canoe.capsize.during.interval", "cut.self.during.interval")]

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

# Rename event column to tree.fall.during.interval
df <- df %>% dplyr::rename("tree.fall.during.interval" = "event")

# Export as csv
write.csv(df, "data_new_format.csv", row.names = F)

