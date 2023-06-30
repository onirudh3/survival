# Libraries and Data Import -----------------------------------------------


library(tidyverse)
library(eha)
library(survival)
library(readxl)
library(dplyr)

# Raw data
raw_data <- read_xls("threat_wide___sumACEs_for anirudh.xls")
raw_data <- raw_data %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()
write.csv(raw_data, "raw_data_no_duplicates.csv", row.names = F)

# Load data
df_final <- read.csv("tree_fall_final_table.csv")
raw_df <- read.csv("tree_fall_raw_data_no_duplicates.csv") # Raw data for tree falls
df_final <- df_final[c("pid", "age", "male", "region", "enter", "exit",
                       "event")]


# Working out reformatting issues -----------------------------------------


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



# Reformatting the data ---------------------------------------------------



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




# Adding columns for different threats ------------------------------------



## Sickness ----
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

## Snake/Ray Bite ----
# Read snake/ray bite data
snake_df3 <- read.csv("snake_ray_bite_cleaned.csv")

# Merging snake/ray bite and tree fall
df <- left_join(df, snake_df3, by = "pid")

# Have you ever been bit in the intervals that you experienced tree fall?
df <- df %>%
  mutate(bite.during.interval = case_when(enter < snake.or.ray.bite.age & snake.or.ray.bite.age <= exit ~ 1,
                                              enter < snake.or.ray.bite.age1 & snake.or.ray.bite.age1 <= exit ~ 1,
                                              enter < snake.or.ray.bite.age2 & snake.or.ray.bite.age2 <= exit ~ 1,
                                              TRUE ~ 0))


## Fought ----
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


## Animal Attack ----
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


## Canoe Capsize ----
# Read canoe capsize data
ds1 <- read.csv("canoe_capsize_cleaned.csv")

# Merge canoe capsize and tree fall
df <- left_join(df, ds1, by = "pid")

# Have you ever canoe capsize in the intervals that you experienced tree fall?
df <- df %>%
  mutate(canoe.capsize.during.interval = case_when(enter < cc.age1 & cc.age1 <= exit ~ 1,
                                            enter < cc.age2 & cc.age2 <= exit ~ 1,
                                            enter < cc.age3 & cc.age3 <= exit ~ 1,
                                            TRUE ~ 0))

## Cut Self ----
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




# Final Steps -------------------------------------------------------------


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

# Remove some dataframes
rm(anti, db3, db4, db5, db6, df_final, male_df, raw_df, region_df, cut_vector)




# Number of times risk experienced in given interval ----------------------

## Tree Fall ----

# Read tree fall cleaned
tree_fall_df <- read.csv("tree_fall_cleaned.csv")

# Attach tree fall ages to df
df <- left_join(df, tree_fall_df)
df$tf.age1 <- ceiling(df$tf.age1)
df$tf.age2 <- ceiling(df$tf.age2)
df$tf.age3 <- ceiling(df$tf.age3)

dx <- df[c("pid", "exit", "tree.fall.during.interval", "tf.age1", "tf.age2", "tf.age3")]

dy <- dx %>%
  filter(tree.fall.during.interval == 1)

# db$n.tree.fall <- rowSums(db == "b", na.rm = T)

dy <- dy %>%
  rowwise() %>%
  mutate(n.tree.fall = sum(c_across(tf.age1:tf.age3) == exit, na.rm = TRUE)) %>%
  ungroup()
dy$n.tree.fall <- ifelse(dy$n.tree.fall == 0, 1, dy$n.tree.fall)

dy <- dy[c("pid", "exit", "n.tree.fall")]
df <- left_join(df, dy)
df <- relocate(df, n.tree.fall, .after = tree.fall.during.interval)
df$n.tree.fall <- ifelse(is.na(df$n.tree.fall), 0, df$n.tree.fall)

df <- subset(df, select = -c(tf.age1, tf.age2, tf.age3))


## Sickness ----

df <- left_join(df, sick_df3)
df$sickness.age <- ceiling(df$sickness.age)
df$sickness.age1 <- ceiling(df$sickness.age1)
df$sickness.age2 <- ceiling(df$sickness.age2)

dx <- df[c("pid", "exit", "sickness.during.interval", "sickness.age", "sickness.age1", "sickness.age2")]

dy <- dx %>%
  filter(sickness.during.interval == 1)

dy <- dy %>%
  rowwise() %>%
  mutate(n.sickness = sum(c_across(sickness.age:sickness.age2) == exit, na.rm = TRUE)) %>%
  ungroup()
dy$n.sickness <- ifelse(dy$n.sickness == 0, 1, dy$n.sickness)

dy <- dy[c("pid", "exit", "n.sickness")]
df <- left_join(df, dy)
df <- relocate(df, n.sickness, .after = sickness.during.interval)
df$n.sickness <- ifelse(is.na(df$n.sickness), 0, df$n.sickness)

df <- subset(df, select = -c(sickness.age, sickness.age1, sickness.age2))

## Snake/Ray Bite ----

df <- left_join(df, snake_df3)
df$snake.or.ray.bite.age <- ceiling(df$snake.or.ray.bite.age)
df$snake.or.ray.bite.age1 <- ceiling(df$snake.or.ray.bite.age1)
df$snake.or.ray.bite.age2 <- ceiling(df$snake.or.ray.bite.age2)

dx <- df[c("pid", "exit", "bite.during.interval", "snake.or.ray.bite.age",
           "snake.or.ray.bite.age1", "snake.or.ray.bite.age2")]

dy <- dx %>%
  filter(bite.during.interval == 1)

dy <- dy %>%
  rowwise() %>%
  mutate(n.snake.ray.bite = sum(c_across(snake.or.ray.bite.age:snake.or.ray.bite.age2) == exit, na.rm = TRUE)) %>%
  ungroup()
dy$n.snake.ray.bite <- ifelse(dy$n.snake.ray.bite == 0, 1, dy$n.snake.ray.bite)

dy <- dy[c("pid", "exit", "n.snake.ray.bite")]
df <- left_join(df, dy)
df <- relocate(df, n.snake.ray.bite, .after = bite.during.interval)
df$n.snake.ray.bite <- ifelse(is.na(df$n.snake.ray.bite), 0, df$n.snake.ray.bite)

df <- subset(df, select = -c(snake.or.ray.bite.age, snake.or.ray.bite.age1, snake.or.ray.bite.age2))


## Fight ----

df <- left_join(df, fought_df3)
df$fought.age <- ceiling(df$fought.age)
df$fought.age1 <- ceiling(df$fought.age1)
df$fought.age2 <- ceiling(df$fought.age2)

dx <- df[c("pid", "exit", "fought.during.interval", "fought.age",
           "fought.age1", "fought.age2")]

dy <- dx %>%
  filter(fought.during.interval == 1)

dy <- dy %>%
  rowwise() %>%
  mutate(n.fought = sum(c_across(fought.age:fought.age2) == exit, na.rm = TRUE)) %>%
  ungroup()
dy$n.fought <- ifelse(dy$n.fought == 0, 1, dy$n.fought)

dy <- dy[c("pid", "exit", "n.fought")]
df <- left_join(df, dy)
df <- relocate(df, n.fought, .after = fought.during.interval)
df$n.fought <- ifelse(is.na(df$n.fought), 0, df$n.fought)

df <- subset(df, select = -c(fought.age, fought.age1, fought.age2))


## Animal Attack ----

df <- left_join(df, animal_attack_df3)
df$animal.attack.age <- ceiling(df$animal.attack.age)
df$animal.attack.age1 <- ceiling(df$animal.attack.age1)

dx <- df[c("pid", "exit", "animal.attack.during.interval", "animal.attack.age",
           "animal.attack.age1")]

dy <- dx %>%
  filter(animal.attack.during.interval == 1)

dy <- dy %>%
  rowwise() %>%
  mutate(n.animal.attack = sum(c_across(animal.attack.age:animal.attack.age1) == exit, na.rm = TRUE)) %>%
  ungroup()
dy$n.animal.attack <- ifelse(dy$n.animal.attack == 0, 1, dy$n.animal.attack)

dy <- dy[c("pid", "exit", "n.animal.attack")]
df <- left_join(df, dy)
df <- relocate(df, n.animal.attack, .after = animal.attack.during.interval)
df$n.animal.attack <- ifelse(is.na(df$n.animal.attack), 0, df$n.animal.attack)

df <- subset(df, select = -c(animal.attack.age, animal.attack.age1))


## Canoe Capsize ----

df <- left_join(df, ds1)
df$cc.age1 <- ceiling(df$cc.age1)
df$cc.age2 <- ceiling(df$cc.age2)
df$cc.age3 <- ceiling(df$cc.age3)

dx <- df[c("pid", "exit", "canoe.capsize.during.interval", "cc.age1",
           "cc.age2", "cc.age3")]

dy <- dx %>%
  filter(canoe.capsize.during.interval == 1)

dy <- dy %>%
  rowwise() %>%
  mutate(n.canoe.capsize = sum(c_across(cc.age1:cc.age3) == exit, na.rm = TRUE)) %>%
  ungroup()
dy$n.canoe.capsize <- ifelse(dy$n.canoe.capsize == 0, 1, dy$n.canoe.capsize)

dy <- dy[c("pid", "exit", "n.canoe.capsize")]
df <- left_join(df, dy)
df <- relocate(df, n.canoe.capsize, .after = canoe.capsize.during.interval)
df$n.canoe.capsize <- ifelse(is.na(df$n.canoe.capsize), 0, df$n.canoe.capsize)

df <- subset(df, select = -c(cc.age1, cc.age2, cc.age3))


## Cut Self ----

df <- left_join(df, dc1)
df$cut.age1 <- ceiling(df$cut.age1)
df$cut.age2 <- ceiling(df$cut.age2)
df$cut.age3 <- ceiling(df$cut.age3)
df$cut.age4 <- ceiling(df$cut.age4)
df$cut.age5 <- ceiling(df$cut.age5)
df$cut.age6 <- ceiling(df$cut.age6)

dx <- df[c("pid", "exit", "cut.self.during.interval", "cut.age1", "cut.age2",
           "cut.age3", "cut.age4", "cut.age5", "cut.age6")]

dy <- dx %>%
  filter(cut.self.during.interval == 1)

dy <- dy %>%
  rowwise() %>%
  mutate(n.cut.self = sum(c_across(cut.age1:cut.age6) == exit, na.rm = TRUE)) %>%
  ungroup()
dy$n.cut.self <- ifelse(dy$n.cut.self == 0, 1, dy$n.cut.self)

dy <- dy[c("pid", "exit", "n.cut.self")]
df <- left_join(df, dy)
df <- relocate(df, n.cut.self, .after = cut.self.during.interval)
df$n.cut.self <- ifelse(is.na(df$n.cut.self), 0, df$n.cut.self)

df <- subset(df, select = -c(cut.age1, cut.age2, cut.age3, cut.age4, cut.age5,
                             cut.age6))

# Add household ID column -------------------------------------------------

h_id <- read_xls("add household ids_a.xls")
df <- left_join(df, h_id)
df <- relocate(df, house.id, .after = pid)


rm(animal_attack_df3, dc1, ds1, dx, dy, fought_df3, h_id, raw_data, sick_df3,
   snake_df3, tree_fall_df)



# Adding columns for cause, etc. ------------------------------------------
# Note that this is all highly context-specific, meaning that each risk has a
# different number of reported ages, which means that the code to add these
# columns is slightly modified for every risk.





## Snake/Ray Bite ----

raw <- read.csv("raw_data_no_duplicates.csv")

raw <- select(raw, c(7, 16:35))
raw <- subset(raw, select = -c(snake.or.ray.bite.age, snake.or.ray.bite.age1))
dx <- left_join(df, raw)

dx <- dx %>%
  filter(bite.during.interval == 1)
dx <- dx %>%
  group_by(pid) %>%
  mutate(index = 1:n(),
         cum = cumsum(n.snake.ray.bite))
dx <- relocate(dx, index, .after = bite.during.interval)

## What bit you, snake or ray? ----

dx$what_bit_you_snake_ray_1 <- NA_character_
dx$what_bit_you_snake_ray_2 <- NA_character_
dx$what_bit_you_snake_ray_3 <- NA_character_

dx <- relocate(dx, c(what_bit_you_snake_ray_1, what_bit_you_snake_ray_2,
                     what_bit_you_snake_ray_3), .after = n.snake.ray.bite)

# index = 1
dx <- dx %>%
  mutate(what_bit_you_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 1 ~ what.bit.you, T ~ as.character(what_bit_you_snake_ray_1)),
         what_bit_you_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(what_bit_you_snake_ray_2)),
         what_bit_you_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(what_bit_you_snake_ray_3)))

dx <- dx %>%
  mutate(what_bit_you_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 2 ~ what.bit.you, T ~ as.character(what_bit_you_snake_ray_1)),
         what_bit_you_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 2 ~ what.bit.you1, T ~ as.character(what_bit_you_snake_ray_2)),
         what_bit_you_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 2 ~ NA_character_, T ~ as.character(what_bit_you_snake_ray_3)))

dx <- dx %>%
  mutate(what_bit_you_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 3 ~ what.bit.you, T ~ as.character(what_bit_you_snake_ray_1)),
         what_bit_you_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 3 ~ what.bit.you1, T ~ as.character(what_bit_you_snake_ray_2)),
         what_bit_you_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 3 ~ what.bit.you2, T ~ as.character(what_bit_you_snake_ray_3)))

# index = 2
dx <- dx %>%
  mutate(what_bit_you_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ what.bit.you1, T ~ as.character(what_bit_you_snake_ray_1)),
         what_bit_you_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ NA_character_, T ~ as.character(what_bit_you_snake_ray_2)),
         what_bit_you_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ NA_character_, T ~ as.character(what_bit_you_snake_ray_3)))

dx <- dx %>%
  mutate(what_bit_you_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ what.bit.you2, T ~ as.character(what_bit_you_snake_ray_1)),
         what_bit_you_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ NA_character_, T ~ as.character(what_bit_you_snake_ray_2)),
         what_bit_you_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ NA_character_, T ~ as.character(what_bit_you_snake_ray_3)))

dx <- dx %>%
  mutate(what_bit_you_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 2 ~ what.bit.you1, T ~ as.character(what_bit_you_snake_ray_1)),
         what_bit_you_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 2 ~ what.bit.you2, T ~ as.character(what_bit_you_snake_ray_2)),
         what_bit_you_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 2 ~ NA_character_, T ~ as.character(what_bit_you_snake_ray_3)))

# index = 3
dx <- dx %>%
  mutate(what_bit_you_snake_ray_1 = case_when(index == 3 & n.snake.ray.bite == 1 ~ what.bit.you2, T ~ as.character(what_bit_you_snake_ray_1)),
         what_bit_you_snake_ray_2 = case_when(index == 3 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(what_bit_you_snake_ray_2)),
         what_bit_you_snake_ray_3 = case_when(index == 3 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(what_bit_you_snake_ray_3)))

# Remove the old columns
dx <- subset(dx, select = -c(what.bit.you, what.bit.you1, what.bit.you2))


## Where bit body? ----

dx$where_bit_body_snake_ray_1 <- NA_character_
dx$where_bit_body_snake_ray_2 <- NA_character_
dx$where_bit_body_snake_ray_3 <- NA_character_

dx <- relocate(dx, c(where_bit_body_snake_ray_1, where_bit_body_snake_ray_2,
                     where_bit_body_snake_ray_3), .after = what_bit_you_snake_ray_3)

# index = 1
dx <- dx %>%
  mutate(where_bit_body_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 1 ~ where.bit.body, T ~ as.character(where_bit_body_snake_ray_1)),
         where_bit_body_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(where_bit_body_snake_ray_2)),
         where_bit_body_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(where_bit_body_snake_ray_3)))

dx <- dx %>%
  mutate(where_bit_body_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 2 ~ where.bit.body, T ~ as.character(where_bit_body_snake_ray_1)),
         where_bit_body_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 2 ~ where.bit.body1, T ~ as.character(where_bit_body_snake_ray_2)),
         where_bit_body_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 2 ~ NA_character_, T ~ as.character(where_bit_body_snake_ray_3)))

dx <- dx %>%
  mutate(where_bit_body_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 3 ~ where.bit.body, T ~ as.character(where_bit_body_snake_ray_1)),
         where_bit_body_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 3 ~ where.bit.body1, T ~ as.character(where_bit_body_snake_ray_2)),
         where_bit_body_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 3 ~ where.bit.body2, T ~ as.character(where_bit_body_snake_ray_3)))

# index = 2
dx <- dx %>%
  mutate(where_bit_body_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ where.bit.body1, T ~ as.character(where_bit_body_snake_ray_1)),
         where_bit_body_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ NA_character_, T ~ as.character(where_bit_body_snake_ray_2)),
         where_bit_body_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ NA_character_, T ~ as.character(where_bit_body_snake_ray_3)))

dx <- dx %>%
  mutate(where_bit_body_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ where.bit.body2, T ~ as.character(where_bit_body_snake_ray_1)),
         where_bit_body_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ NA_character_, T ~ as.character(where_bit_body_snake_ray_2)),
         where_bit_body_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ NA_character_, T ~ as.character(where_bit_body_snake_ray_3)))

dx <- dx %>%
  mutate(where_bit_body_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 2 ~ where.bit.body1, T ~ as.character(where_bit_body_snake_ray_1)),
         where_bit_body_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 2 ~ where.bit.body2, T ~ as.character(where_bit_body_snake_ray_2)),
         where_bit_body_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 2 ~ NA_character_, T ~ as.character(where_bit_body_snake_ray_3)))

# index = 3
dx <- dx %>%
  mutate(where_bit_body_snake_ray_1 = case_when(index == 3 & n.snake.ray.bite == 1 ~ where.bit.body2, T ~ as.character(where_bit_body_snake_ray_1)),
         where_bit_body_snake_ray_2 = case_when(index == 3 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(where_bit_body_snake_ray_2)),
         where_bit_body_snake_ray_3 = case_when(index == 3 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(where_bit_body_snake_ray_3)))

# Remove the old columns
dx <- subset(dx, select = -c(where.bit.body, where.bit.body1, where.bit.body2))


## Activity when bit? ----

dx$activity_when_bit_snake_ray_1 <- NA_character_
dx$activity_when_bit_snake_ray_2 <- NA_character_
dx$activity_when_bit_snake_ray_3 <- NA_character_

dx <- relocate(dx, c(activity_when_bit_snake_ray_1, activity_when_bit_snake_ray_2,
                     activity_when_bit_snake_ray_3), .after = where_bit_body_snake_ray_3)

# index = 1
dx <- dx %>%
  mutate(activity_when_bit_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 1 ~ activity.when.bit, T ~ as.character(activity_when_bit_snake_ray_1)),
         activity_when_bit_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(activity_when_bit_snake_ray_2)),
         activity_when_bit_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(activity_when_bit_snake_ray_3)))

dx <- dx %>%
  mutate(activity_when_bit_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 2 ~ activity.when.bit, T ~ as.character(activity_when_bit_snake_ray_1)),
         activity_when_bit_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 2 ~ activity.when.bit1...25, T ~ as.character(activity_when_bit_snake_ray_2)),
         activity_when_bit_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 2 ~ NA_character_, T ~ as.character(activity_when_bit_snake_ray_3)))

dx <- dx %>%
  mutate(activity_when_bit_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 3 ~ activity.when.bit, T ~ as.character(activity_when_bit_snake_ray_1)),
         activity_when_bit_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 3 ~ activity.when.bit1...25, T ~ as.character(activity_when_bit_snake_ray_2)),
         activity_when_bit_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 3 ~ activity.when.bit2, T ~ as.character(activity_when_bit_snake_ray_3)))

# index = 2
dx <- dx %>%
  mutate(activity_when_bit_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ activity.when.bit1...25, T ~ as.character(activity_when_bit_snake_ray_1)),
         activity_when_bit_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ NA_character_, T ~ as.character(activity_when_bit_snake_ray_2)),
         activity_when_bit_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ NA_character_, T ~ as.character(activity_when_bit_snake_ray_3)))

dx <- dx %>%
  mutate(activity_when_bit_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ activity.when.bit2, T ~ as.character(activity_when_bit_snake_ray_1)),
         activity_when_bit_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ NA_character_, T ~ as.character(activity_when_bit_snake_ray_2)),
         activity_when_bit_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ NA_character_, T ~ as.character(activity_when_bit_snake_ray_3)))

dx <- dx %>%
  mutate(activity_when_bit_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 2 ~ activity.when.bit1...25, T ~ as.character(activity_when_bit_snake_ray_1)),
         activity_when_bit_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 2 ~ activity.when.bit2, T ~ as.character(activity_when_bit_snake_ray_2)),
         activity_when_bit_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 2 ~ NA_character_, T ~ as.character(activity_when_bit_snake_ray_3)))

# index = 3
dx <- dx %>%
  mutate(activity_when_bit_snake_ray_1 = case_when(index == 3 & n.snake.ray.bite == 1 ~ activity.when.bit2, T ~ as.character(activity_when_bit_snake_ray_1)),
         activity_when_bit_snake_ray_2 = case_when(index == 3 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(activity_when_bit_snake_ray_2)),
         activity_when_bit_snake_ray_3 = case_when(index == 3 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(activity_when_bit_snake_ray_3)))

# Remove the old columns
dx <- subset(dx, select = -c(activity.when.bit, activity.when.bit1...25, activity.when.bit2))


## Days disabled ----

dx$days_disabled_snake_ray_1 <- NA_real_
dx$days_disabled_snake_ray_2 <- NA_real_
dx$days_disabled_snake_ray_3 <- NA_real_

dx <- relocate(dx, c(days_disabled_snake_ray_1, days_disabled_snake_ray_2,
                     days_disabled_snake_ray_3), .after = activity_when_bit_snake_ray_3)

# index = 1
dx <- dx %>%
  mutate(days_disabled_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 1 ~ days.disabled.bite, T ~ as.numeric(days_disabled_snake_ray_1)),
         days_disabled_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 1 ~ NA_real_, T ~ as.numeric(days_disabled_snake_ray_2)),
         days_disabled_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 1 ~ NA_real_, T ~ as.numeric(days_disabled_snake_ray_3)))

dx <- dx %>%
  mutate(days_disabled_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 2 ~ days.disabled.bite, T ~ as.numeric(days_disabled_snake_ray_1)),
         days_disabled_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 2 ~ days.disabled.bite1, T ~ as.numeric(days_disabled_snake_ray_2)),
         days_disabled_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 2 ~ NA_real_, T ~ as.numeric(days_disabled_snake_ray_3)))

dx <- dx %>%
  mutate(days_disabled_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 3 ~ days.disabled.bite, T ~ as.numeric(days_disabled_snake_ray_1)),
         days_disabled_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 3 ~ days.disabled.bite1, T ~ as.numeric(days_disabled_snake_ray_2)),
         days_disabled_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 3 ~ days.disabled.bite2, T ~ as.numeric(days_disabled_snake_ray_3)))

# index = 2
dx <- dx %>%
  mutate(days_disabled_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ days.disabled.bite1, T ~ as.numeric(days_disabled_snake_ray_1)),
         days_disabled_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ NA_real_, T ~ as.numeric(days_disabled_snake_ray_2)),
         days_disabled_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ NA_real_, T ~ as.numeric(days_disabled_snake_ray_3)))

dx <- dx %>%
  mutate(days_disabled_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ days.disabled.bite2, T ~ as.numeric(days_disabled_snake_ray_1)),
         days_disabled_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ NA_real_, T ~ as.numeric(days_disabled_snake_ray_2)),
         days_disabled_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ NA_real_, T ~ as.numeric(days_disabled_snake_ray_3)))

dx <- dx %>%
  mutate(days_disabled_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 2 ~ days.disabled.bite1, T ~ as.numeric(days_disabled_snake_ray_1)),
         days_disabled_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 2 ~ days.disabled.bite2, T ~ as.numeric(days_disabled_snake_ray_2)),
         days_disabled_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 2 ~ NA_real_, T ~ as.numeric(days_disabled_snake_ray_3)))

# index = 3
dx <- dx %>%
  mutate(days_disabled_snake_ray_1 = case_when(index == 3 & n.snake.ray.bite == 1 ~ days.disabled.bite2, T ~ as.numeric(days_disabled_snake_ray_1)),
         days_disabled_snake_ray_2 = case_when(index == 3 & n.snake.ray.bite == 1 ~ NA_real_, T ~ as.numeric(days_disabled_snake_ray_2)),
         days_disabled_snake_ray_3 = case_when(index == 3 & n.snake.ray.bite == 1 ~ NA_real_, T ~ as.numeric(days_disabled_snake_ray_3)))

# Remove the old columns
dx <- subset(dx, select = -c(days.disabled.bite, days.disabled.bite1, days.disabled.bite2))


## Days disabled ----

dx$almost_died_snake_ray_1 <- NA_integer_
dx$almost_died_snake_ray_2 <- NA_integer_
dx$almost_died_snake_ray_3 <- NA_integer_

dx <- relocate(dx, c(almost_died_snake_ray_1, almost_died_snake_ray_2,
                     almost_died_snake_ray_3), .after = days_disabled_snake_ray_3)

# index = 1
dx <- dx %>%
  mutate(almost_died_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 1 ~ almost.died.bite, T ~ as.integer(almost_died_snake_ray_1)),
         almost_died_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 1 ~ NA_integer_, T ~ as.integer(almost_died_snake_ray_2)),
         almost_died_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 1 ~ NA_integer_, T ~ as.integer(almost_died_snake_ray_3)))

dx <- dx %>%
  mutate(almost_died_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 2 ~ almost.died.bite, T ~ as.integer(almost_died_snake_ray_1)),
         almost_died_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 2 ~ almost.died.bite1, T ~ as.integer(almost_died_snake_ray_2)),
         almost_died_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 2 ~ NA_integer_, T ~ as.integer(almost_died_snake_ray_3)))

dx <- dx %>%
  mutate(almost_died_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 3 ~ almost.died.bite, T ~ as.integer(almost_died_snake_ray_1)),
         almost_died_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 3 ~ almost.died.bite1, T ~ as.integer(almost_died_snake_ray_2)),
         almost_died_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 3 ~ almost.died.bite2, T ~ as.integer(almost_died_snake_ray_3)))

# index = 2
dx <- dx %>%
  mutate(almost_died_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ almost.died.bite1, T ~ as.integer(almost_died_snake_ray_1)),
         almost_died_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(almost_died_snake_ray_2)),
         almost_died_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(almost_died_snake_ray_3)))

dx <- dx %>%
  mutate(almost_died_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ almost.died.bite2, T ~ as.integer(almost_died_snake_ray_1)),
         almost_died_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(almost_died_snake_ray_2)),
         almost_died_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(almost_died_snake_ray_3)))

dx <- dx %>%
  mutate(almost_died_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 2 ~ almost.died.bite1, T ~ as.integer(almost_died_snake_ray_1)),
         almost_died_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 2 ~ almost.died.bite2, T ~ as.integer(almost_died_snake_ray_2)),
         almost_died_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 2 ~ NA_integer_, T ~ as.integer(almost_died_snake_ray_3)))

# index = 3
dx <- dx %>%
  mutate(almost_died_snake_ray_1 = case_when(index == 3 & n.snake.ray.bite == 1 ~ almost.died.bite2, T ~ as.integer(almost_died_snake_ray_1)),
         almost_died_snake_ray_2 = case_when(index == 3 & n.snake.ray.bite == 1 ~ NA_integer_, T ~ as.integer(almost_died_snake_ray_2)),
         almost_died_snake_ray_3 = case_when(index == 3 & n.snake.ray.bite == 1 ~ NA_integer_, T ~ as.integer(almost_died_snake_ray_3)))

# Remove the old columns
dx <- subset(dx, select = -c(almost.died.bite, almost.died.bite1, almost.died.bite2))


## Still bothers ----

dx$still_bothers_snake_ray_1 <- NA_integer_
dx$still_bothers_snake_ray_2 <- NA_integer_
dx$still_bothers_snake_ray_3 <- NA_integer_

dx <- relocate(dx, c(still_bothers_snake_ray_1, still_bothers_snake_ray_2,
                     still_bothers_snake_ray_3), .after = almost_died_snake_ray_3)

# index = 1
dx <- dx %>%
  mutate(still_bothers_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 1 ~ still.bothers.bite, T ~ as.integer(still_bothers_snake_ray_1)),
         still_bothers_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 1 ~ NA_integer_, T ~ as.integer(still_bothers_snake_ray_2)),
         still_bothers_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 1 ~ NA_integer_, T ~ as.integer(still_bothers_snake_ray_3)))

dx <- dx %>%
  mutate(still_bothers_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 2 ~ still.bothers.bite, T ~ as.integer(still_bothers_snake_ray_1)),
         still_bothers_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 2 ~ still.bothers.bite1, T ~ as.integer(still_bothers_snake_ray_2)),
         still_bothers_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 2 ~ NA_integer_, T ~ as.integer(still_bothers_snake_ray_3)))

dx <- dx %>%
  mutate(still_bothers_snake_ray_1 = case_when(index == 1 & n.snake.ray.bite == 3 ~ still.bothers.bite, T ~ as.integer(still_bothers_snake_ray_1)),
         still_bothers_snake_ray_2 = case_when(index == 1 & n.snake.ray.bite == 3 ~ still.bothers.bite1, T ~ as.integer(still_bothers_snake_ray_2)),
         still_bothers_snake_ray_3 = case_when(index == 1 & n.snake.ray.bite == 3 ~ still.bothers.bite2, T ~ as.integer(still_bothers_snake_ray_3)))

# index = 2
dx <- dx %>%
  mutate(still_bothers_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ still.bothers.bite1, T ~ as.integer(still_bothers_snake_ray_1)),
         still_bothers_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(still_bothers_snake_ray_2)),
         still_bothers_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(still_bothers_snake_ray_3)))

dx <- dx %>%
  mutate(still_bothers_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ still.bothers.bite2, T ~ as.integer(still_bothers_snake_ray_1)),
         still_bothers_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(still_bothers_snake_ray_2)),
         still_bothers_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(still_bothers_snake_ray_3)))

dx <- dx %>%
  mutate(still_bothers_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 2 ~ still.bothers.bite1, T ~ as.integer(still_bothers_snake_ray_1)),
         still_bothers_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 2 ~ still.bothers.bite2, T ~ as.integer(still_bothers_snake_ray_2)),
         still_bothers_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 2 ~ NA_integer_, T ~ as.integer(still_bothers_snake_ray_3)))

# index = 3
dx <- dx %>%
  mutate(still_bothers_snake_ray_1 = case_when(index == 3 & n.snake.ray.bite == 1 ~ still.bothers.bite2, T ~ as.integer(still_bothers_snake_ray_1)),
         still_bothers_snake_ray_2 = case_when(index == 3 & n.snake.ray.bite == 1 ~ NA_integer_, T ~ as.integer(still_bothers_snake_ray_2)),
         still_bothers_snake_ray_3 = case_when(index == 3 & n.snake.ray.bite == 1 ~ NA_integer_, T ~ as.integer(still_bothers_snake_ray_3)))

# Remove the old columns
dx <- subset(dx, select = -c(still.bothers.bite, still.bothers.bite1, still.bothers.bite2))


## Get back to original dataframe
df <- left_join(df, dx)
df <- relocate(df, c(what_bit_you_snake_ray_1:still_bothers_snake_ray_3), .after = n.snake.ray.bite)
df <- subset(df, select = -c(index, cum))






## Animal Attack ----
raw <- read.csv("raw_data_no_duplicates.csv")

raw <- select(raw, c(7, 39:50))
raw <- subset(raw, select = -c(animal.attack.age))
dx <- left_join(df, raw)

dx <- dx %>%
  filter(animal.attack.during.interval == 1)
dx <- dx %>%
  group_by(pid) %>%
  mutate(index = 1:n())
dx <- relocate(dx, index, .after = animal.attack.during.interval)

# It can be observed that multiple attacks never occur in any given interval

### What attacked you? ----
dx$what_attacked_you <- NA_character_
dx <- relocate(dx, c(what_attacked_you), .after = n.animal.attack)
dx <- dx %>%
  mutate(what_attacked_you = case_when(index == 1 & n.animal.attack == 1 ~ what.attacked.you, T ~ as.character(what_attacked_you)),
         what_attacked_you = case_when(index == 2 & n.animal.attack == 1 ~ what.attacked.you1, T ~ as.character(what_attacked_you)))
dx <- subset(dx, select = -c(what.attacked.you, what.attacked.you1))

### Where attacked body? ----
dx$where_attacked_body <- NA_character_
dx <- relocate(dx, c(where_attacked_body), .after = what_attacked_you)
dx <- dx %>%
  mutate(where_attacked_body = case_when(index == 1 & n.animal.attack == 1 ~ where.attacked.body, T ~ as.character(where_attacked_body)),
         where_attacked_body = case_when(index == 2 & n.animal.attack == 1 ~ where.attacked.body1, T ~ as.character(where_attacked_body)))
dx <- subset(dx, select = -c(where.attacked.body, where.attacked.body1))

### Activity when attacked ----
# Note that there is no column in the raw data for activity when it is experienced a second time
dx$activity_when_attacked <- NA_character_
dx <- relocate(dx, c(activity_when_attacked), .after = where_attacked_body)
dx <- dx %>%
  mutate(activity_when_attacked = case_when(index == 1 & n.animal.attack == 1 ~ activity.when.bit1...41, T ~ as.character(activity_when_attacked)),
         activity_when_attacked = case_when(index == 2 & n.animal.attack == 1 ~ NA_character_, T ~ as.character(activity_when_attacked)))
dx <- subset(dx, select = -c(activity.when.bit1...41))

### Days disabled attack ----
dx$days_disabled_attack <- NA_integer_
dx <- relocate(dx, c(days_disabled_attack), .after = activity_when_attacked)
dx <- dx %>%
  mutate(days_disabled_attack = case_when(index == 1 & n.animal.attack == 1 ~ days.disabled.attack, T ~ as.integer(days_disabled_attack)),
         days_disabled_attack = case_when(index == 2 & n.animal.attack == 1 ~ days.disabled.attack1, T ~ as.integer(days_disabled_attack)))
dx <- subset(dx, select = -c(days.disabled.attack, days.disabled.attack1))

### Almost died attack ----
dx$almost_died_attack <- NA_integer_
dx <- relocate(dx, c(almost_died_attack), .after = days_disabled_attack)
dx <- dx %>%
  mutate(almost_died_attack = case_when(index == 1 & n.animal.attack == 1 ~ almost.died.attack, T ~ as.integer(almost_died_attack)),
         almost_died_attack = case_when(index == 2 & n.animal.attack == 1 ~ almost.died.attack1, T ~ as.integer(almost_died_attack)))
dx <- subset(dx, select = -c(almost.died.attack, almost.died.attack1))

### Still bothers attack ----
dx$still_bothers_attack <- NA_integer_
dx <- relocate(dx, c(still_bothers_attack), .after = almost_died_attack)
dx <- dx %>%
  mutate(still_bothers_attack = case_when(index == 1 & n.animal.attack == 1 ~ still.bothers.attack, T ~ as.integer(still_bothers_attack)),
         still_bothers_attack = case_when(index == 2 & n.animal.attack == 1 ~ still.bothers.attack1, T ~ as.integer(still_bothers_attack)))
dx <- subset(dx, select = -c(still.bothers.attack, still.bothers.attack1))

## Get back to original data frame
df <- left_join(df, dx)
df <- relocate(df, c(what_attacked_you:still_bothers_attack), .after = n.animal.attack)
df <- subset(df, select = -c(index))







## Sickness ----
raw <- read.csv("raw_data_no_duplicates.csv")

raw <- select(raw, c(7, sickness.what:sickness.cause2))

# Arrange the instances in ascending order
raw <- transform(raw,
                 sickness.what = ifelse(sickness.age > sickness.age1, sickness.what1, sickness.what),
                 sickness.what1 = ifelse(sickness.age > sickness.age1, sickness.what, sickness.what1),
                 days.disabled.sickness = ifelse(sickness.age > sickness.age1, days.disabled.sickness1, days.disabled.sickness),
                 days.disabled.sickness1 = ifelse(sickness.age > sickness.age1, days.disabled.sickness, days.disabled.sickness1),
                 almost.died.sickness = ifelse(sickness.age > sickness.age1, almost.died.sickness1, almost.died.sickness),
                 almost.died.sickness1 = ifelse(sickness.age > sickness.age1, almost.died.sickness, almost.died.sickness1),
                 how.cured.sickness = ifelse(sickness.age > sickness.age1, how.cured.sickness1, how.cured.sickness),
                 how.cured.sickness1 = ifelse(sickness.age > sickness.age1, how.cured.sickness, how.cured.sickness1),
                 who.helped.sickness = ifelse(sickness.age > sickness.age1, who.helped.sickness1, who.helped.sickness),
                 who.helped.sickness1 = ifelse(sickness.age > sickness.age1, who.helped.sickness, who.helped.sickness1),
                 sickness.cause = ifelse(sickness.age > sickness.age1, sickness.cause1, sickness.cause),
                 sickness.cause1 = ifelse(sickness.age > sickness.age1, sickness.cause, sickness.cause1))

raw <- subset(raw, select = -c(sickness.age, sickness.age1, sickness.age2))
dx <- left_join(df, raw)

dx <- dx %>%
  filter(sickness.during.interval == 1)
dx <- dx %>%
  group_by(pid) %>%
  mutate(index = 1:n(),
         cum = cumsum(n.sickness))
dx <- relocate(dx, index, .after = sickness.during.interval)

### Sickness what ----
dx$sickness_what_1 <- NA_character_
dx$sickness_what_2 <- NA_character_
dx$sickness_what_3 <- NA_character_

dx <- relocate(dx, c(sickness_what_1, sickness_what_2,
                     sickness_what_3), .after = n.sickness)

# index = 1
dx <- dx %>%
  mutate(sickness_what_1 = case_when(index == 1 & n.sickness == 1 ~ sickness.what, T ~ as.character(sickness_what_1)),
         sickness_what_2 = case_when(index == 1 & n.sickness == 1 ~ NA_character_, T ~ as.character(sickness_what_2)),
         sickness_what_3 = case_when(index == 1 & n.sickness == 1 ~ NA_character_, T ~ as.character(sickness_what_3)))

dx <- dx %>%
  mutate(sickness_what_1 = case_when(index == 1 & n.sickness == 2 ~ sickness.what, T ~ as.character(sickness_what_1)),
         sickness_what_2 = case_when(index == 1 & n.sickness == 2 ~ sickness.what1, T ~ as.character(sickness_what_2)),
         sickness_what_3 = case_when(index == 1 & n.sickness == 2 ~ NA_character_, T ~ as.character(sickness_what_3)))

dx <- dx %>%
  mutate(sickness_what_1 = case_when(index == 1 & n.sickness == 3 ~ sickness.what, T ~ as.character(sickness_what_1)),
         sickness_what_2 = case_when(index == 1 & n.sickness == 3 ~ sickness.what1, T ~ as.character(sickness_what_2)),
         sickness_what_3 = case_when(index == 1 & n.sickness == 3 ~ sickness.what2, T ~ as.character(sickness_what_3)))

# index = 2
dx <- dx %>%
  mutate(sickness_what_1 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ sickness.what1, T ~ as.character(sickness_what_1)),
         sickness_what_2 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ NA_character_, T ~ as.character(sickness_what_2)),
         sickness_what_3 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ NA_character_, T ~ as.character(sickness_what_3)))

dx <- dx %>%
  mutate(sickness_what_1 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ sickness.what2, T ~ as.character(sickness_what_1)),
         sickness_what_2 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ NA_character_, T ~ as.character(sickness_what_2)),
         sickness_what_3 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ NA_character_, T ~ as.character(sickness_what_3)))

dx <- dx %>%
  mutate(sickness_what_1 = case_when(index == 2 & n.sickness == 2 ~ sickness.what1, T ~ as.character(sickness_what_1)),
         sickness_what_2 = case_when(index == 2 & n.sickness == 2 ~ sickness.what2, T ~ as.character(sickness_what_2)),
         sickness_what_3 = case_when(index == 2 & n.sickness == 2 ~ NA_character_, T ~ as.character(sickness_what_3)))

# index = 3
dx <- dx %>%
  mutate(sickness_what_1 = case_when(index == 3 & n.sickness == 1 ~ sickness.what2, T ~ as.character(sickness_what_1)),
         sickness_what_2 = case_when(index == 3 & n.sickness == 1 ~ NA_character_, T ~ as.character(sickness_what_2)),
         sickness_what_3 = case_when(index == 3 & n.sickness == 1 ~ NA_character_, T ~ as.character(sickness_what_3)))

# Remove the old columns
dx <- subset(dx, select = -c(sickness.what, sickness.what1, sickness.what2))

### Days disabled ----
dx$days_disabled_sickness_1 <- NA_integer_
dx$days_disabled_sickness_2 <- NA_integer_
dx$days_disabled_sickness_3 <- NA_integer_

dx <- relocate(dx, c(days_disabled_sickness_1, days_disabled_sickness_2,
                     days_disabled_sickness_3), .after = sickness_what_3)

# index = 1
dx <- dx %>%
  mutate(days_disabled_sickness_1 = case_when(index == 1 & n.sickness == 1 ~ days.disabled.sickness, T ~ as.integer(days_disabled_sickness_1)),
         days_disabled_sickness_2 = case_when(index == 1 & n.sickness == 1 ~ NA_integer_, T ~ as.integer(days_disabled_sickness_2)),
         days_disabled_sickness_3 = case_when(index == 1 & n.sickness == 1 ~ NA_integer_, T ~ as.integer(days_disabled_sickness_3)))

dx <- dx %>%
  mutate(days_disabled_sickness_1 = case_when(index == 1 & n.sickness == 2 ~ days.disabled.sickness, T ~ as.integer(days_disabled_sickness_1)),
         days_disabled_sickness_2 = case_when(index == 1 & n.sickness == 2 ~ days.disabled.sickness1, T ~ as.integer(days_disabled_sickness_2)),
         days_disabled_sickness_3 = case_when(index == 1 & n.sickness == 2 ~ NA_integer_, T ~ as.integer(days_disabled_sickness_3)))

dx <- dx %>%
  mutate(days_disabled_sickness_1 = case_when(index == 1 & n.sickness == 3 ~ days.disabled.sickness, T ~ as.integer(days_disabled_sickness_1)),
         days_disabled_sickness_2 = case_when(index == 1 & n.sickness == 3 ~ days.disabled.sickness1, T ~ as.integer(days_disabled_sickness_2)),
         days_disabled_sickness_3 = case_when(index == 1 & n.sickness == 3 ~ days.disabled.sickness2, T ~ as.integer(days_disabled_sickness_3)))

# index = 2
dx <- dx %>%
  mutate(days_disabled_sickness_1 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ days.disabled.sickness1, T ~ as.integer(days_disabled_sickness_1)),
         days_disabled_sickness_2 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(days_disabled_sickness_2)),
         days_disabled_sickness_3 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(days_disabled_sickness_3)))

dx <- dx %>%
  mutate(days_disabled_sickness_1 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ days.disabled.sickness2, T ~ as.integer(days_disabled_sickness_1)),
         days_disabled_sickness_2 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(days_disabled_sickness_2)),
         days_disabled_sickness_3 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(days_disabled_sickness_3)))

dx <- dx %>%
  mutate(days_disabled_sickness_1 = case_when(index == 2 & n.sickness == 2 ~ days.disabled.sickness1, T ~ as.integer(days_disabled_sickness_1)),
         days_disabled_sickness_2 = case_when(index == 2 & n.sickness == 2 ~ days.disabled.sickness2, T ~ as.integer(days_disabled_sickness_2)),
         days_disabled_sickness_3 = case_when(index == 2 & n.sickness == 2 ~ NA_integer_, T ~ as.integer(days_disabled_sickness_3)))

# index = 3
dx <- dx %>%
  mutate(days_disabled_sickness_1 = case_when(index == 3 & n.sickness == 1 ~ days.disabled.sickness2, T ~ as.integer(days_disabled_sickness_1)),
         days_disabled_sickness_2 = case_when(index == 3 & n.sickness == 1 ~ NA_integer_, T ~ as.integer(days_disabled_sickness_2)),
         days_disabled_sickness_3 = case_when(index == 3 & n.sickness == 1 ~ NA_integer_, T ~ as.integer(days_disabled_sickness_3)))

# Remove the old columns
dx <- subset(dx, select = -c(days.disabled.sickness, days.disabled.sickness1, days.disabled.sickness2))
### Almost died ----
dx$almost_died_sickness_1 <- NA_integer_
dx$almost_died_sickness_2 <- NA_integer_
dx$almost_died_sickness_3 <- NA_integer_

dx <- relocate(dx, c(almost_died_sickness_1, almost_died_sickness_2,
                     almost_died_sickness_3), .after = days_disabled_sickness_3)

# index = 1
dx <- dx %>%
  mutate(almost_died_sickness_1 = case_when(index == 1 & n.sickness == 1 ~ almost.died.sickness, T ~ as.integer(almost_died_sickness_1)),
         almost_died_sickness_2 = case_when(index == 1 & n.sickness == 1 ~ NA_integer_, T ~ as.integer(almost_died_sickness_2)),
         almost_died_sickness_3 = case_when(index == 1 & n.sickness == 1 ~ NA_integer_, T ~ as.integer(almost_died_sickness_3)))

dx <- dx %>%
  mutate(almost_died_sickness_1 = case_when(index == 1 & n.sickness == 2 ~ almost.died.sickness, T ~ as.integer(almost_died_sickness_1)),
         almost_died_sickness_2 = case_when(index == 1 & n.sickness == 2 ~ almost.died.sickness1, T ~ as.integer(almost_died_sickness_2)),
         almost_died_sickness_3 = case_when(index == 1 & n.sickness == 2 ~ NA_integer_, T ~ as.integer(almost_died_sickness_3)))

dx <- dx %>%
  mutate(almost_died_sickness_1 = case_when(index == 1 & n.sickness == 3 ~ almost.died.sickness, T ~ as.integer(almost_died_sickness_1)),
         almost_died_sickness_2 = case_when(index == 1 & n.sickness == 3 ~ almost.died.sickness1, T ~ as.integer(almost_died_sickness_2)),
         almost_died_sickness_3 = case_when(index == 1 & n.sickness == 3 ~ almost.died.sickness2, T ~ as.integer(almost_died_sickness_3)))

# index = 2
dx <- dx %>%
  mutate(almost_died_sickness_1 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ almost.died.sickness1, T ~ as.integer(almost_died_sickness_1)),
         almost_died_sickness_2 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(almost_died_sickness_2)),
         almost_died_sickness_3 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(almost_died_sickness_3)))

dx <- dx %>%
  mutate(almost_died_sickness_1 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ almost.died.sickness2, T ~ as.integer(almost_died_sickness_1)),
         almost_died_sickness_2 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(almost_died_sickness_2)),
         almost_died_sickness_3 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(almost_died_sickness_3)))

dx <- dx %>%
  mutate(almost_died_sickness_1 = case_when(index == 2 & n.sickness == 2 ~ almost.died.sickness1, T ~ as.integer(almost_died_sickness_1)),
         almost_died_sickness_2 = case_when(index == 2 & n.sickness == 2 ~ almost.died.sickness2, T ~ as.integer(almost_died_sickness_2)),
         almost_died_sickness_3 = case_when(index == 2 & n.sickness == 2 ~ NA_integer_, T ~ as.integer(almost_died_sickness_3)))

# index = 3
dx <- dx %>%
  mutate(almost_died_sickness_1 = case_when(index == 3 & n.sickness == 1 ~ almost.died.sickness2, T ~ as.integer(almost_died_sickness_1)),
         almost_died_sickness_2 = case_when(index == 3 & n.sickness == 1 ~ NA_integer_, T ~ as.integer(almost_died_sickness_2)),
         almost_died_sickness_3 = case_when(index == 3 & n.sickness == 1 ~ NA_integer_, T ~ as.integer(almost_died_sickness_3)))

# Remove the old columns
dx <- subset(dx, select = -c(almost.died.sickness, almost.died.sickness1, almost.died.sickness2))
### How cured sickness ----
dx$how_cured_sickness_1 <- NA_character_
dx$how_cured_sickness_2 <- NA_character_
dx$how_cured_sickness_3 <- NA_character_

dx <- relocate(dx, c(how_cured_sickness_1, how_cured_sickness_2,
                     how_cured_sickness_3), .after = almost_died_sickness_3)

# index = 1
dx <- dx %>%
  mutate(how_cured_sickness_1 = case_when(index == 1 & n.sickness == 1 ~ how.cured.sickness, T ~ as.character(how_cured_sickness_1)),
         how_cured_sickness_2 = case_when(index == 1 & n.sickness == 1 ~ NA_character_, T ~ as.character(how_cured_sickness_2)),
         how_cured_sickness_3 = case_when(index == 1 & n.sickness == 1 ~ NA_character_, T ~ as.character(how_cured_sickness_3)))

dx <- dx %>%
  mutate(how_cured_sickness_1 = case_when(index == 1 & n.sickness == 2 ~ how.cured.sickness, T ~ as.character(how_cured_sickness_1)),
         how_cured_sickness_2 = case_when(index == 1 & n.sickness == 2 ~ how.cured.sickness1, T ~ as.character(how_cured_sickness_2)),
         how_cured_sickness_3 = case_when(index == 1 & n.sickness == 2 ~ NA_character_, T ~ as.character(how_cured_sickness_3)))

dx <- dx %>%
  mutate(how_cured_sickness_1 = case_when(index == 1 & n.sickness == 3 ~ how.cured.sickness, T ~ as.character(how_cured_sickness_1)),
         how_cured_sickness_2 = case_when(index == 1 & n.sickness == 3 ~ how.cured.sickness1, T ~ as.character(how_cured_sickness_2)),
         how_cured_sickness_3 = case_when(index == 1 & n.sickness == 3 ~ how.cured.sickness2, T ~ as.character(how_cured_sickness_3)))

# index = 2
dx <- dx %>%
  mutate(how_cured_sickness_1 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ how.cured.sickness1, T ~ as.character(how_cured_sickness_1)),
         how_cured_sickness_2 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ NA_character_, T ~ as.character(how_cured_sickness_2)),
         how_cured_sickness_3 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ NA_character_, T ~ as.character(how_cured_sickness_3)))

dx <- dx %>%
  mutate(how_cured_sickness_1 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ how.cured.sickness2, T ~ as.character(how_cured_sickness_1)),
         how_cured_sickness_2 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ NA_character_, T ~ as.character(how_cured_sickness_2)),
         how_cured_sickness_3 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ NA_character_, T ~ as.character(how_cured_sickness_3)))

dx <- dx %>%
  mutate(how_cured_sickness_1 = case_when(index == 2 & n.sickness == 2 ~ how.cured.sickness1, T ~ as.character(how_cured_sickness_1)),
         how_cured_sickness_2 = case_when(index == 2 & n.sickness == 2 ~ how.cured.sickness2, T ~ as.character(how_cured_sickness_2)),
         how_cured_sickness_3 = case_when(index == 2 & n.sickness == 2 ~ NA_character_, T ~ as.character(how_cured_sickness_3)))

# index = 3
dx <- dx %>%
  mutate(how_cured_sickness_1 = case_when(index == 3 & n.sickness == 1 ~ how.cured.sickness2, T ~ as.character(how_cured_sickness_1)),
         how_cured_sickness_2 = case_when(index == 3 & n.sickness == 1 ~ NA_character_, T ~ as.character(how_cured_sickness_2)),
         how_cured_sickness_3 = case_when(index == 3 & n.sickness == 1 ~ NA_character_, T ~ as.character(how_cured_sickness_3)))

# Remove the old columns
dx <- subset(dx, select = -c(how.cured.sickness, how.cured.sickness1, how.cured.sickness2))
### Who helped sickness ----
dx$who_helped_sickness_1 <- NA_character_
dx$who_helped_sickness_2 <- NA_character_
dx$who_helped_sickness_3 <- NA_character_

dx <- relocate(dx, c(who_helped_sickness_1, who_helped_sickness_2,
                     who_helped_sickness_3), .after = how_cured_sickness_3)

# index = 1
dx <- dx %>%
  mutate(who_helped_sickness_1 = case_when(index == 1 & n.sickness == 1 ~ who.helped.sickness, T ~ as.character(who_helped_sickness_1)),
         who_helped_sickness_2 = case_when(index == 1 & n.sickness == 1 ~ NA_character_, T ~ as.character(who_helped_sickness_2)),
         who_helped_sickness_3 = case_when(index == 1 & n.sickness == 1 ~ NA_character_, T ~ as.character(who_helped_sickness_3)))

dx <- dx %>%
  mutate(who_helped_sickness_1 = case_when(index == 1 & n.sickness == 2 ~ who.helped.sickness, T ~ as.character(who_helped_sickness_1)),
         who_helped_sickness_2 = case_when(index == 1 & n.sickness == 2 ~ who.helped.sickness1, T ~ as.character(who_helped_sickness_2)),
         who_helped_sickness_3 = case_when(index == 1 & n.sickness == 2 ~ NA_character_, T ~ as.character(who_helped_sickness_3)))

dx <- dx %>%
  mutate(who_helped_sickness_1 = case_when(index == 1 & n.sickness == 3 ~ who.helped.sickness, T ~ as.character(who_helped_sickness_1)),
         who_helped_sickness_2 = case_when(index == 1 & n.sickness == 3 ~ who.helped.sickness1, T ~ as.character(who_helped_sickness_2)),
         who_helped_sickness_3 = case_when(index == 1 & n.sickness == 3 ~ who.helped.sickness2, T ~ as.character(who_helped_sickness_3)))

# index = 2
dx <- dx %>%
  mutate(who_helped_sickness_1 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ who.helped.sickness1, T ~ as.character(who_helped_sickness_1)),
         who_helped_sickness_2 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ NA_character_, T ~ as.character(who_helped_sickness_2)),
         who_helped_sickness_3 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ NA_character_, T ~ as.character(who_helped_sickness_3)))

dx <- dx %>%
  mutate(who_helped_sickness_1 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ who.helped.sickness2, T ~ as.character(who_helped_sickness_1)),
         who_helped_sickness_2 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ NA_character_, T ~ as.character(who_helped_sickness_2)),
         who_helped_sickness_3 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ NA_character_, T ~ as.character(who_helped_sickness_3)))

dx <- dx %>%
  mutate(who_helped_sickness_1 = case_when(index == 2 & n.sickness == 2 ~ who.helped.sickness1, T ~ as.character(who_helped_sickness_1)),
         who_helped_sickness_2 = case_when(index == 2 & n.sickness == 2 ~ who.helped.sickness2, T ~ as.character(who_helped_sickness_2)),
         who_helped_sickness_3 = case_when(index == 2 & n.sickness == 2 ~ NA_character_, T ~ as.character(who_helped_sickness_3)))

# index = 3
dx <- dx %>%
  mutate(who_helped_sickness_1 = case_when(index == 3 & n.sickness == 1 ~ who.helped.sickness2, T ~ as.character(who_helped_sickness_1)),
         who_helped_sickness_2 = case_when(index == 3 & n.sickness == 1 ~ NA_character_, T ~ as.character(who_helped_sickness_2)),
         who_helped_sickness_3 = case_when(index == 3 & n.sickness == 1 ~ NA_character_, T ~ as.character(who_helped_sickness_3)))

# Remove the old columns
dx <- subset(dx, select = -c(who.helped.sickness, who.helped.sickness1, who.helped.sickness2))
### Sickness cause ----
dx$sickness_cause_1 <- NA_character_
dx$sickness_cause_2 <- NA_character_
dx$sickness_cause_3 <- NA_character_

dx <- relocate(dx, c(sickness_cause_1, sickness_cause_2,
                     sickness_cause_3), .after = who_helped_sickness_3)

# index = 1
dx <- dx %>%
  mutate(sickness_cause_1 = case_when(index == 1 & n.sickness == 1 ~ sickness.cause, T ~ as.character(sickness_cause_1)),
         sickness_cause_2 = case_when(index == 1 & n.sickness == 1 ~ NA_character_, T ~ as.character(sickness_cause_2)),
         sickness_cause_3 = case_when(index == 1 & n.sickness == 1 ~ NA_character_, T ~ as.character(sickness_cause_3)))

dx <- dx %>%
  mutate(sickness_cause_1 = case_when(index == 1 & n.sickness == 2 ~ sickness.cause, T ~ as.character(sickness_cause_1)),
         sickness_cause_2 = case_when(index == 1 & n.sickness == 2 ~ sickness.cause1, T ~ as.character(sickness_cause_2)),
         sickness_cause_3 = case_when(index == 1 & n.sickness == 2 ~ NA_character_, T ~ as.character(sickness_cause_3)))

dx <- dx %>%
  mutate(sickness_cause_1 = case_when(index == 1 & n.sickness == 3 ~ sickness.cause, T ~ as.character(sickness_cause_1)),
         sickness_cause_2 = case_when(index == 1 & n.sickness == 3 ~ sickness.cause1, T ~ as.character(sickness_cause_2)),
         sickness_cause_3 = case_when(index == 1 & n.sickness == 3 ~ sickness.cause2, T ~ as.character(sickness_cause_3)))

# index = 2
dx <- dx %>%
  mutate(sickness_cause_1 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ sickness.cause1, T ~ as.character(sickness_cause_1)),
         sickness_cause_2 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ NA_character_, T ~ as.character(sickness_cause_2)),
         sickness_cause_3 = case_when(index == 2 & n.sickness == 1 & cum == 2 ~ NA_character_, T ~ as.character(sickness_cause_3)))

dx <- dx %>%
  mutate(sickness_cause_1 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ sickness.cause2, T ~ as.character(sickness_cause_1)),
         sickness_cause_2 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ NA_character_, T ~ as.character(sickness_cause_2)),
         sickness_cause_3 = case_when(index == 2 & n.sickness == 1 & cum == 3 ~ NA_character_, T ~ as.character(sickness_cause_3)))

dx <- dx %>%
  mutate(sickness_cause_1 = case_when(index == 2 & n.sickness == 2 ~ sickness.cause1, T ~ as.character(sickness_cause_1)),
         sickness_cause_2 = case_when(index == 2 & n.sickness == 2 ~ sickness.cause2, T ~ as.character(sickness_cause_2)),
         sickness_cause_3 = case_when(index == 2 & n.sickness == 2 ~ NA_character_, T ~ as.character(sickness_cause_3)))

# index = 3
dx <- dx %>%
  mutate(sickness_cause_1 = case_when(index == 3 & n.sickness == 1 ~ sickness.cause2, T ~ as.character(sickness_cause_1)),
         sickness_cause_2 = case_when(index == 3 & n.sickness == 1 ~ NA_character_, T ~ as.character(sickness_cause_2)),
         sickness_cause_3 = case_when(index == 3 & n.sickness == 1 ~ NA_character_, T ~ as.character(sickness_cause_3)))

# Remove the old columns
dx <- subset(dx, select = -c(sickness.cause, sickness.cause1, sickness.cause2))

## Get back to original data frame
df <- left_join(df, dx)
df <- relocate(df, c(sickness_what_1:sickness_cause_3), .after = n.sickness)
df <- subset(df, select = -c(index, cum))

## Fight ----
raw <- read.csv("raw_data_no_duplicates.csv")

raw <- select(raw, c(7, Fought.whom:Fought.either.drunk2))
raw <- subset(raw, select = -c(fought.age, fought.age1, fought.age2))
dx <- left_join(df, raw)

dx <- dx %>%
  filter(fought.during.interval == 1)
dx <- dx %>%
  group_by(pid) %>%
  mutate(index = 1:n(),
         cum = cumsum(n.fought))
dx <- relocate(dx, index, .after = fought.during.interval)

### Fought whom ----
dx$fought_whom_1 <- NA_character_
dx$fought_whom_2 <- NA_character_
dx$fought_whom_3 <- NA_character_

dx <- relocate(dx, c(fought_whom_1, fought_whom_2,
                     fought_whom_3), .after = n.fought)

# index = 1
dx <- dx %>%
  mutate(fought_whom_1 = case_when(index == 1 & n.fought == 1 ~ Fought.whom, T ~ as.character(fought_whom_1)),
         fought_whom_2 = case_when(index == 1 & n.fought == 1 ~ NA_character_, T ~ as.character(fought_whom_2)),
         fought_whom_3 = case_when(index == 1 & n.fought == 1 ~ NA_character_, T ~ as.character(fought_whom_3)))

dx <- dx %>%
  mutate(fought_whom_1 = case_when(index == 1 & n.fought == 2 ~ Fought.whom, T ~ as.character(fought_whom_1)),
         fought_whom_2 = case_when(index == 1 & n.fought == 2 ~ Fought.whom1, T ~ as.character(fought_whom_2)),
         fought_whom_3 = case_when(index == 1 & n.fought == 2 ~ NA_character_, T ~ as.character(fought_whom_3)))

dx <- dx %>%
  mutate(fought_whom_1 = case_when(index == 1 & n.fought == 3 ~ Fought.whom, T ~ as.character(fought_whom_1)),
         fought_whom_2 = case_when(index == 1 & n.fought == 3 ~ Fought.whom1, T ~ as.character(fought_whom_2)),
         fought_whom_3 = case_when(index == 1 & n.fought == 3 ~ Fought.whom2, T ~ as.character(fought_whom_3)))

# index = 2
dx <- dx %>%
  mutate(fought_whom_1 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ Fought.whom1, T ~ as.character(fought_whom_1)),
         fought_whom_2 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ NA_character_, T ~ as.character(fought_whom_2)),
         fought_whom_3 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ NA_character_, T ~ as.character(fought_whom_3)))

dx <- dx %>%
  mutate(fought_whom_1 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ Fought.whom2, T ~ as.character(fought_whom_1)),
         fought_whom_2 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ NA_character_, T ~ as.character(fought_whom_2)),
         fought_whom_3 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ NA_character_, T ~ as.character(fought_whom_3)))

dx <- dx %>%
  mutate(fought_whom_1 = case_when(index == 2 & n.fought == 2 ~ Fought.whom1, T ~ as.character(fought_whom_1)),
         fought_whom_2 = case_when(index == 2 & n.fought == 2 ~ Fought.whom2, T ~ as.character(fought_whom_2)),
         fought_whom_3 = case_when(index == 2 & n.fought == 2 ~ NA_character_, T ~ as.character(fought_whom_3)))

# index = 3
dx <- dx %>%
  mutate(fought_whom_1 = case_when(index == 3 & n.fought == 1 ~ Fought.whom2, T ~ as.character(fought_whom_1)),
         fought_whom_2 = case_when(index == 3 & n.fought == 1 ~ NA_character_, T ~ as.character(fought_whom_2)),
         fought_whom_3 = case_when(index == 3 & n.fought == 1 ~ NA_character_, T ~ as.character(fought_whom_3)))

# Remove the old columns
dx <- subset(dx, select = -c(Fought.whom, Fought.whom1, Fought.whom2))
### Cause ----
dx$fought_cause_1 <- NA_character_
dx$fought_cause_2 <- NA_character_
dx$fought_cause_3 <- NA_character_

dx <- relocate(dx, c(fought_cause_1, fought_cause_2,
                     fought_cause_3), .after = fought_whom_3)

# index = 1
dx <- dx %>%
  mutate(fought_cause_1 = case_when(index == 1 & n.fought == 1 ~ Fought.cause, T ~ as.character(fought_cause_1)),
         fought_cause_2 = case_when(index == 1 & n.fought == 1 ~ NA_character_, T ~ as.character(fought_cause_2)),
         fought_cause_3 = case_when(index == 1 & n.fought == 1 ~ NA_character_, T ~ as.character(fought_cause_3)))

dx <- dx %>%
  mutate(fought_cause_1 = case_when(index == 1 & n.fought == 2 ~ Fought.cause, T ~ as.character(fought_cause_1)),
         fought_cause_2 = case_when(index == 1 & n.fought == 2 ~ Fought.cause1, T ~ as.character(fought_cause_2)),
         fought_cause_3 = case_when(index == 1 & n.fought == 2 ~ NA_character_, T ~ as.character(fought_cause_3)))

dx <- dx %>%
  mutate(fought_cause_1 = case_when(index == 1 & n.fought == 3 ~ Fought.cause, T ~ as.character(fought_cause_1)),
         fought_cause_2 = case_when(index == 1 & n.fought == 3 ~ Fought.cause1, T ~ as.character(fought_cause_2)),
         fought_cause_3 = case_when(index == 1 & n.fought == 3 ~ Fought.cause2, T ~ as.character(fought_cause_3)))

# index = 2
dx <- dx %>%
  mutate(fought_cause_1 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ Fought.cause1, T ~ as.character(fought_cause_1)),
         fought_cause_2 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ NA_character_, T ~ as.character(fought_cause_2)),
         fought_cause_3 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ NA_character_, T ~ as.character(fought_cause_3)))

dx <- dx %>%
  mutate(fought_cause_1 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ Fought.cause2, T ~ as.character(fought_cause_1)),
         fought_cause_2 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ NA_character_, T ~ as.character(fought_cause_2)),
         fought_cause_3 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ NA_character_, T ~ as.character(fought_cause_3)))

dx <- dx %>%
  mutate(fought_cause_1 = case_when(index == 2 & n.fought == 2 ~ Fought.cause1, T ~ as.character(fought_cause_1)),
         fought_cause_2 = case_when(index == 2 & n.fought == 2 ~ Fought.cause2, T ~ as.character(fought_cause_2)),
         fought_cause_3 = case_when(index == 2 & n.fought == 2 ~ NA_character_, T ~ as.character(fought_cause_3)))

# index = 3
dx <- dx %>%
  mutate(fought_cause_1 = case_when(index == 3 & n.fought == 1 ~ Fought.cause2, T ~ as.character(fought_cause_1)),
         fought_cause_2 = case_when(index == 3 & n.fought == 1 ~ NA_character_, T ~ as.character(fought_cause_2)),
         fought_cause_3 = case_when(index == 3 & n.fought == 1 ~ NA_character_, T ~ as.character(fought_cause_3)))

# Remove the old columns
dx <- subset(dx, select = -c(Fought.cause, Fought.cause1, Fought.cause2))
### Injured ----
dx$fought_injured_1 <- NA_integer_
dx$fought_injured_2 <- NA_integer_
dx$fought_injured_3 <- NA_integer_

dx <- relocate(dx, c(fought_injured_1, fought_injured_2,
                     fought_injured_3), .after = fought_cause_3)

# index = 1
dx <- dx %>%
  mutate(fought_injured_1 = case_when(index == 1 & n.fought == 1 ~ Fought.injured, T ~ as.integer(fought_injured_1)),
         fought_injured_2 = case_when(index == 1 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_injured_2)),
         fought_injured_3 = case_when(index == 1 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_injured_3)))

dx <- dx %>%
  mutate(fought_injured_1 = case_when(index == 1 & n.fought == 2 ~ Fought.injured, T ~ as.integer(fought_injured_1)),
         fought_injured_2 = case_when(index == 1 & n.fought == 2 ~ Fought.injured1, T ~ as.integer(fought_injured_2)),
         fought_injured_3 = case_when(index == 1 & n.fought == 2 ~ NA_integer_, T ~ as.integer(fought_injured_3)))

dx <- dx %>%
  mutate(fought_injured_1 = case_when(index == 1 & n.fought == 3 ~ Fought.injured, T ~ as.integer(fought_injured_1)),
         fought_injured_2 = case_when(index == 1 & n.fought == 3 ~ Fought.injured1, T ~ as.integer(fought_injured_2)),
         fought_injured_3 = case_when(index == 1 & n.fought == 3 ~ Fought.injured2, T ~ as.integer(fought_injured_3)))

# index = 2
dx <- dx %>%
  mutate(fought_injured_1 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ Fought.injured1, T ~ as.integer(fought_injured_1)),
         fought_injured_2 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(fought_injured_2)),
         fought_injured_3 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(fought_injured_3)))

dx <- dx %>%
  mutate(fought_injured_1 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ Fought.injured2, T ~ as.integer(fought_injured_1)),
         fought_injured_2 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(fought_injured_2)),
         fought_injured_3 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(fought_injured_3)))

dx <- dx %>%
  mutate(fought_injured_1 = case_when(index == 2 & n.fought == 2 ~ Fought.injured1, T ~ as.integer(fought_injured_1)),
         fought_injured_2 = case_when(index == 2 & n.fought == 2 ~ Fought.injured2, T ~ as.integer(fought_injured_2)),
         fought_injured_3 = case_when(index == 2 & n.fought == 2 ~ NA_integer_, T ~ as.integer(fought_injured_3)))

# index = 3
dx <- dx %>%
  mutate(fought_injured_1 = case_when(index == 3 & n.fought == 1 ~ Fought.injured2, T ~ as.integer(fought_injured_1)),
         fought_injured_2 = case_when(index == 3 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_injured_2)),
         fought_injured_3 = case_when(index == 3 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_injured_3)))

# Remove the old columns
dx <- subset(dx, select = -c(Fought.injured, Fought.injured1, Fought.injured2))
### Days injured ----
dx$fought_days_injured_1 <- NA_integer_
dx$fought_days_injured_2 <- NA_integer_
dx$fought_days_injured_3 <- NA_integer_

dx <- relocate(dx, c(fought_days_injured_1, fought_days_injured_2,
                     fought_days_injured_3), .after = fought_injured_3)

# index = 1
dx <- dx %>%
  mutate(fought_days_injured_1 = case_when(index == 1 & n.fought == 1 ~ Fought.days.injured, T ~ as.integer(fought_days_injured_1)),
         fought_days_injured_2 = case_when(index == 1 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_days_injured_2)),
         fought_days_injured_3 = case_when(index == 1 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_days_injured_3)))

dx <- dx %>%
  mutate(fought_days_injured_1 = case_when(index == 1 & n.fought == 2 ~ Fought.days.injured, T ~ as.integer(fought_days_injured_1)),
         fought_days_injured_2 = case_when(index == 1 & n.fought == 2 ~ Fought.days.injured1, T ~ as.integer(fought_days_injured_2)),
         fought_days_injured_3 = case_when(index == 1 & n.fought == 2 ~ NA_integer_, T ~ as.integer(fought_days_injured_3)))

dx <- dx %>%
  mutate(fought_days_injured_1 = case_when(index == 1 & n.fought == 3 ~ Fought.days.injured, T ~ as.integer(fought_days_injured_1)),
         fought_days_injured_2 = case_when(index == 1 & n.fought == 3 ~ Fought.days.injured1, T ~ as.integer(fought_days_injured_2)),
         fought_days_injured_3 = case_when(index == 1 & n.fought == 3 ~ Fought.days.injured2, T ~ as.integer(fought_days_injured_3)))

# index = 2
dx <- dx %>%
  mutate(fought_days_injured_1 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ Fought.days.injured1, T ~ as.integer(fought_days_injured_1)),
         fought_days_injured_2 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(fought_days_injured_2)),
         fought_days_injured_3 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(fought_days_injured_3)))

dx <- dx %>%
  mutate(fought_days_injured_1 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ Fought.days.injured2, T ~ as.integer(fought_days_injured_1)),
         fought_days_injured_2 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(fought_days_injured_2)),
         fought_days_injured_3 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(fought_days_injured_3)))

dx <- dx %>%
  mutate(fought_days_injured_1 = case_when(index == 2 & n.fought == 2 ~ Fought.days.injured1, T ~ as.integer(fought_days_injured_1)),
         fought_days_injured_2 = case_when(index == 2 & n.fought == 2 ~ Fought.days.injured2, T ~ as.integer(fought_days_injured_2)),
         fought_days_injured_3 = case_when(index == 2 & n.fought == 2 ~ NA_integer_, T ~ as.integer(fought_days_injured_3)))

# index = 3
dx <- dx %>%
  mutate(fought_days_injured_1 = case_when(index == 3 & n.fought == 1 ~ Fought.days.injured2, T ~ as.integer(fought_days_injured_1)),
         fought_days_injured_2 = case_when(index == 3 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_days_injured_2)),
         fought_days_injured_3 = case_when(index == 3 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_days_injured_3)))

# Remove the old columns
dx <- subset(dx, select = -c(Fought.days.injured, Fought.days.injured1, Fought.days.injured2))
### Almost died ----
dx$fought_almost_died_1 <- NA_integer_
dx$fought_almost_died_2 <- NA_integer_
dx$fought_almost_died_3 <- NA_integer_

dx <- relocate(dx, c(fought_almost_died_1, fought_almost_died_2,
                     fought_almost_died_3), .after = fought_days_injured_3)

# index = 1
dx <- dx %>%
  mutate(fought_almost_died_1 = case_when(index == 1 & n.fought == 1 ~ Fought.almost.died, T ~ as.integer(fought_almost_died_1)),
         fought_almost_died_2 = case_when(index == 1 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_almost_died_2)),
         fought_almost_died_3 = case_when(index == 1 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_almost_died_3)))

dx <- dx %>%
  mutate(fought_almost_died_1 = case_when(index == 1 & n.fought == 2 ~ Fought.almost.died, T ~ as.integer(fought_almost_died_1)),
         fought_almost_died_2 = case_when(index == 1 & n.fought == 2 ~ Fought.almost.died1, T ~ as.integer(fought_almost_died_2)),
         fought_almost_died_3 = case_when(index == 1 & n.fought == 2 ~ NA_integer_, T ~ as.integer(fought_almost_died_3)))

dx <- dx %>%
  mutate(fought_almost_died_1 = case_when(index == 1 & n.fought == 3 ~ Fought.almost.died, T ~ as.integer(fought_almost_died_1)),
         fought_almost_died_2 = case_when(index == 1 & n.fought == 3 ~ Fought.almost.died1, T ~ as.integer(fought_almost_died_2)),
         fought_almost_died_3 = case_when(index == 1 & n.fought == 3 ~ Fought.almost.died2, T ~ as.integer(fought_almost_died_3)))

# index = 2
dx <- dx %>%
  mutate(fought_almost_died_1 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ Fought.almost.died1, T ~ as.integer(fought_almost_died_1)),
         fought_almost_died_2 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(fought_almost_died_2)),
         fought_almost_died_3 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(fought_almost_died_3)))

dx <- dx %>%
  mutate(fought_almost_died_1 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ Fought.almost.died2, T ~ as.integer(fought_almost_died_1)),
         fought_almost_died_2 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(fought_almost_died_2)),
         fought_almost_died_3 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(fought_almost_died_3)))

dx <- dx %>%
  mutate(fought_almost_died_1 = case_when(index == 2 & n.fought == 2 ~ Fought.almost.died1, T ~ as.integer(fought_almost_died_1)),
         fought_almost_died_2 = case_when(index == 2 & n.fought == 2 ~ Fought.almost.died2, T ~ as.integer(fought_almost_died_2)),
         fought_almost_died_3 = case_when(index == 2 & n.fought == 2 ~ NA_integer_, T ~ as.integer(fought_almost_died_3)))

# index = 3
dx <- dx %>%
  mutate(fought_almost_died_1 = case_when(index == 3 & n.fought == 1 ~ Fought.almost.died2, T ~ as.integer(fought_almost_died_1)),
         fought_almost_died_2 = case_when(index == 3 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_almost_died_2)),
         fought_almost_died_3 = case_when(index == 3 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_almost_died_3)))

# Remove the old columns
dx <- subset(dx, select = -c(Fought.almost.died, Fought.almost.died1, Fought.almost.died2))
### Still bothers ----
dx$fought_still_bother_1 <- NA_integer_
dx$fought_still_bother_2 <- NA_integer_
dx$fought_still_bother_3 <- NA_integer_

dx <- relocate(dx, c(fought_still_bother_1, fought_still_bother_2,
                     fought_still_bother_3), .after = fought_almost_died_3)

# index = 1
dx <- dx %>%
  mutate(fought_still_bother_1 = case_when(index == 1 & n.fought == 1 ~ Fought.still.bother, T ~ as.integer(fought_still_bother_1)),
         fought_still_bother_2 = case_when(index == 1 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_still_bother_2)),
         fought_still_bother_3 = case_when(index == 1 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_still_bother_3)))

dx <- dx %>%
  mutate(fought_still_bother_1 = case_when(index == 1 & n.fought == 2 ~ Fought.still.bother, T ~ as.integer(fought_still_bother_1)),
         fought_still_bother_2 = case_when(index == 1 & n.fought == 2 ~ Fought.still.bother1, T ~ as.integer(fought_still_bother_2)),
         fought_still_bother_3 = case_when(index == 1 & n.fought == 2 ~ NA_integer_, T ~ as.integer(fought_still_bother_3)))

dx <- dx %>%
  mutate(fought_still_bother_1 = case_when(index == 1 & n.fought == 3 ~ Fought.still.bother, T ~ as.integer(fought_still_bother_1)),
         fought_still_bother_2 = case_when(index == 1 & n.fought == 3 ~ Fought.still.bother1, T ~ as.integer(fought_still_bother_2)),
         fought_still_bother_3 = case_when(index == 1 & n.fought == 3 ~ Fought.still.bother2, T ~ as.integer(fought_still_bother_3)))

# index = 2
dx <- dx %>%
  mutate(fought_still_bother_1 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ Fought.still.bother1, T ~ as.integer(fought_still_bother_1)),
         fought_still_bother_2 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(fought_still_bother_2)),
         fought_still_bother_3 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(fought_still_bother_3)))

dx <- dx %>%
  mutate(fought_still_bother_1 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ Fought.still.bother2, T ~ as.integer(fought_still_bother_1)),
         fought_still_bother_2 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(fought_still_bother_2)),
         fought_still_bother_3 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(fought_still_bother_3)))

dx <- dx %>%
  mutate(fought_still_bother_1 = case_when(index == 2 & n.fought == 2 ~ Fought.still.bother1, T ~ as.integer(fought_still_bother_1)),
         fought_still_bother_2 = case_when(index == 2 & n.fought == 2 ~ Fought.still.bother2, T ~ as.integer(fought_still_bother_2)),
         fought_still_bother_3 = case_when(index == 2 & n.fought == 2 ~ NA_integer_, T ~ as.integer(fought_still_bother_3)))

# index = 3
dx <- dx %>%
  mutate(fought_still_bother_1 = case_when(index == 3 & n.fought == 1 ~ Fought.still.bother2, T ~ as.integer(fought_still_bother_1)),
         fought_still_bother_2 = case_when(index == 3 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_still_bother_2)),
         fought_still_bother_3 = case_when(index == 3 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_still_bother_3)))

# Remove the old columns
dx <- subset(dx, select = -c(Fought.still.bother, Fought.still.bother1, Fought.still.bother2))
### Fought either drunk ----
dx$fought_either_drunk_1 <- NA_integer_
dx$fought_either_drunk_2 <- NA_integer_
dx$fought_either_drunk_3 <- NA_integer_

dx <- relocate(dx, c(fought_either_drunk_1, fought_either_drunk_2,
                     fought_either_drunk_3), .after = fought_still_bother_3)

# index = 1
dx <- dx %>%
  mutate(fought_either_drunk_1 = case_when(index == 1 & n.fought == 1 ~ Fought.either.drunk, T ~ as.integer(fought_either_drunk_1)),
         fought_either_drunk_2 = case_when(index == 1 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_either_drunk_2)),
         fought_either_drunk_3 = case_when(index == 1 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_either_drunk_3)))

dx <- dx %>%
  mutate(fought_either_drunk_1 = case_when(index == 1 & n.fought == 2 ~ Fought.either.drunk, T ~ as.integer(fought_either_drunk_1)),
         fought_either_drunk_2 = case_when(index == 1 & n.fought == 2 ~ Fought.either.drunk1, T ~ as.integer(fought_either_drunk_2)),
         fought_either_drunk_3 = case_when(index == 1 & n.fought == 2 ~ NA_integer_, T ~ as.integer(fought_either_drunk_3)))

dx <- dx %>%
  mutate(fought_either_drunk_1 = case_when(index == 1 & n.fought == 3 ~ Fought.either.drunk, T ~ as.integer(fought_either_drunk_1)),
         fought_either_drunk_2 = case_when(index == 1 & n.fought == 3 ~ Fought.either.drunk1, T ~ as.integer(fought_either_drunk_2)),
         fought_either_drunk_3 = case_when(index == 1 & n.fought == 3 ~ Fought.either.drunk2, T ~ as.integer(fought_either_drunk_3)))

# index = 2
dx <- dx %>%
  mutate(fought_either_drunk_1 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ Fought.either.drunk1, T ~ as.integer(fought_either_drunk_1)),
         fought_either_drunk_2 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(fought_either_drunk_2)),
         fought_either_drunk_3 = case_when(index == 2 & n.fought == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(fought_either_drunk_3)))

dx <- dx %>%
  mutate(fought_either_drunk_1 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ Fought.either.drunk2, T ~ as.integer(fought_either_drunk_1)),
         fought_either_drunk_2 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(fought_either_drunk_2)),
         fought_either_drunk_3 = case_when(index == 2 & n.fought == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(fought_either_drunk_3)))

dx <- dx %>%
  mutate(fought_either_drunk_1 = case_when(index == 2 & n.fought == 2 ~ Fought.either.drunk1, T ~ as.integer(fought_either_drunk_1)),
         fought_either_drunk_2 = case_when(index == 2 & n.fought == 2 ~ Fought.either.drunk2, T ~ as.integer(fought_either_drunk_2)),
         fought_either_drunk_3 = case_when(index == 2 & n.fought == 2 ~ NA_integer_, T ~ as.integer(fought_either_drunk_3)))

# index = 3
dx <- dx %>%
  mutate(fought_either_drunk_1 = case_when(index == 3 & n.fought == 1 ~ Fought.either.drunk2, T ~ as.integer(fought_either_drunk_1)),
         fought_either_drunk_2 = case_when(index == 3 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_either_drunk_2)),
         fought_either_drunk_3 = case_when(index == 3 & n.fought == 1 ~ NA_integer_, T ~ as.integer(fought_either_drunk_3)))

# Remove the old columns
dx <- subset(dx, select = -c(Fought.either.drunk, Fought.either.drunk1, Fought.either.drunk2))

## Get back to original data frame
df <- left_join(df, dx)
df <- relocate(df, c(fought_whom_1:fought_either_drunk_3), .after = n.fought)
df <- subset(df, select = -c(index, cum))




## Tree Fall ----
raw <- read.csv("raw_data_no_duplicates.csv")
raw <- select(raw, c(7, TIPO1:other.serious.accident.age5))
### Arrange the instances in ascending order, implementing bubble sort algorithm ----
# Very tedious but could not find a better way

raw$other.serious.accident.activity3 <- NA_character_
raw$other.serious.accident.activity4 <- NA_character_
raw$other.serious.accident.activity5 <- NA_character_
raw$other.serious.accident.days.disabled3 <- as.numeric(raw$other.serious.accident.days.disabled3)

# Iteration 1/5
raw <- transform(raw, TIPO1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 2/5
raw <- transform(raw, TIPO1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 3/5
raw <- transform(raw, TIPO1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 4/5
raw <- transform(raw, TIPO1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 5/5
raw <- transform(raw, TIPO1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))


## Applying same algorithm to get ascending order of tree fall ages ----
# Iteration 1/5
raw <- transform(raw, TIPO1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 2/5
raw <- transform(raw, TIPO1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 3/5
raw <- transform(raw, TIPO1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 4/5
raw <- transform(raw, TIPO1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 5/5
raw <- transform(raw, TIPO1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO1 %in% "b") & (TIPO2 %in% "b") ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO2 %in% "b") & (TIPO3 %in% "b") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO3 %in% "b") & (TIPO4 %in% "b") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO4 %in% "b") & (TIPO5 %in% "b") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(!(TIPO5 %in% "b") & (TIPO6 %in% "b") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))


### Some additional steps ----
raw <- raw %>% dplyr::rename("other.serious.accident.where.hurt2" = "other.serious.accidents.where.hurt2")
raw <- raw %>% dplyr::rename("other.serious.accident.activity2" = "other.serious.accidents.activity2")
raw <- raw %>% dplyr::rename("other.serious.accident.injured1" = "other.serious.accident.injured.yesno1")
raw <- raw %>% dplyr::rename("other.serious.accident.injured2" = "other.serious.accidents.injured.yesno2")

raw <- subset(raw, select = -c(other.serious.accident.age, other.serious.accident.age1,
                               other.serious.accident.age2, other.serious.accident.age3,
                               other.serious.accident.age4, other.serious.accident.age5))

### Merging with df ----
dx <- left_join(df, raw)

dx <- dx %>%
  filter(tree.fall.during.interval == 1)
dx <- dx %>%
  group_by(pid) %>%
  mutate(index = 1:n())
dx <- relocate(dx, index, .after = tree.fall.during.interval)
dx <- relocate(dx, other.serious.accident.activity3, .after = other.serious.accident.where.hurt3)
dx <- relocate(dx, other.serious.accident.activity4, .after = other.serious.accident.where.hurt4)
dx <- relocate(dx, other.serious.accident.activity5, .after = other.serious.accident.where.hurt5)
dx <- subset(dx, select = c(pid, exit, index, n.tree.fall, TIPO1:other.serious.accident.still.bothers5))

### Where hurt? ----
dx$tree_fall_where_hurt_1 <- NA_character_
dx$tree_fall_where_hurt_2 <- NA_character_

dx <- relocate(dx, c(tree_fall_where_hurt_1, tree_fall_where_hurt_2),
               .after = n.tree.fall)

# index = 1
dx <- dx %>%
  mutate(tree_fall_where_hurt_1 = case_when(index == 1 & n.tree.fall == 1 ~ other.serious.accident.where.hurt, T ~ as.character(tree_fall_where_hurt_1)),
         tree_fall_where_hurt_2 = case_when(index == 1 & n.tree.fall == 1 ~ NA_character_, T ~ as.character(tree_fall_where_hurt_2)))

dx <- dx %>%
  mutate(tree_fall_where_hurt_1 = case_when(index == 1 & n.tree.fall == 2 ~ other.serious.accident.where.hurt, T ~ as.character(tree_fall_where_hurt_1)),
         tree_fall_where_hurt_2 = case_when(index == 1 & n.tree.fall == 2 ~ other.serious.accident.where.hurt1, T ~ as.character(tree_fall_where_hurt_2)))

# index = 2
dx <- dx %>%
  mutate(tree_fall_where_hurt_1 = case_when(index == 2 & n.tree.fall == 1 ~ other.serious.accident.where.hurt1, T ~ as.character(tree_fall_where_hurt_1)),
         tree_fall_where_hurt_2 = case_when(index == 2 & n.tree.fall == 1 ~ NA_character_, T ~ as.character(tree_fall_where_hurt_2)))

# index = 3
dx <- dx %>%
  mutate(tree_fall_where_hurt_1 = case_when(index == 3 & n.tree.fall == 1 ~ other.serious.accident.where.hurt2, T ~ as.character(tree_fall_where_hurt_1)),
         tree_fall_where_hurt_2 = case_when(index == 3 & n.tree.fall == 1 ~ NA_character_, T ~ as.character(tree_fall_where_hurt_2)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.where.hurt,
                             other.serious.accident.where.hurt1,
                             other.serious.accident.where.hurt2,
                             other.serious.accident.where.hurt3,
                             other.serious.accident.where.hurt4,
                             other.serious.accident.where.hurt5))

### Activity ----
dx$tree_fall_activity_1 <- NA_character_
dx$tree_fall_activity_2 <- NA_character_

dx <- relocate(dx, c(tree_fall_activity_1, tree_fall_activity_2),
               .after = tree_fall_where_hurt_2)

# index = 1
dx <- dx %>%
  mutate(tree_fall_activity_1 = case_when(index == 1 & n.tree.fall == 1 ~ other.serious.accident.activity, T ~ as.character(tree_fall_activity_1)),
         tree_fall_activity_2 = case_when(index == 1 & n.tree.fall == 1 ~ NA_character_, T ~ as.character(tree_fall_activity_2)))

dx <- dx %>%
  mutate(tree_fall_activity_1 = case_when(index == 1 & n.tree.fall == 2 ~ other.serious.accident.activity, T ~ as.character(tree_fall_activity_1)),
         tree_fall_activity_2 = case_when(index == 1 & n.tree.fall == 2 ~ other.serious.accident.activity1, T ~ as.character(tree_fall_activity_2)))

# index = 2
dx <- dx %>%
  mutate(tree_fall_activity_1 = case_when(index == 2 & n.tree.fall == 1 ~ other.serious.accident.activity1, T ~ as.character(tree_fall_activity_1)),
         tree_fall_activity_2 = case_when(index == 2 & n.tree.fall == 1 ~ NA_character_, T ~ as.character(tree_fall_activity_2)))

# index = 3
dx <- dx %>%
  mutate(tree_fall_activity_1 = case_when(index == 3 & n.tree.fall == 1 ~ other.serious.accident.activity2, T ~ as.character(tree_fall_activity_1)),
         tree_fall_activity_2 = case_when(index == 3 & n.tree.fall == 1 ~ NA_character_, T ~ as.character(tree_fall_activity_2)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.activity,
                             other.serious.accident.activity1,
                             other.serious.accident.activity2,
                             other.serious.accident.activity3,
                             other.serious.accident.activity4,
                             other.serious.accident.activity5))

### Injured ----
dx$tree_fall_injured_1 <- NA_integer_
dx$tree_fall_injured_2 <- NA_integer_

dx <- relocate(dx, c(tree_fall_injured_1, tree_fall_injured_2),
               .after = tree_fall_activity_2)

# index = 1
dx <- dx %>%
  mutate(tree_fall_injured_1 = case_when(index == 1 & n.tree.fall == 1 ~ other.serious.accident.injured, T ~ as.integer(tree_fall_injured_1)),
         tree_fall_injured_2 = case_when(index == 1 & n.tree.fall == 1 ~ NA_integer_, T ~ as.integer(tree_fall_injured_2)))

dx <- dx %>%
  mutate(tree_fall_injured_1 = case_when(index == 1 & n.tree.fall == 2 ~ other.serious.accident.injured, T ~ as.integer(tree_fall_injured_1)),
         tree_fall_injured_2 = case_when(index == 1 & n.tree.fall == 2 ~ other.serious.accident.injured1, T ~ as.integer(tree_fall_injured_2)))

# index = 2
dx <- dx %>%
  mutate(tree_fall_injured_1 = case_when(index == 2 & n.tree.fall == 1 ~ other.serious.accident.injured1, T ~ as.integer(tree_fall_injured_1)),
         tree_fall_injured_2 = case_when(index == 2 & n.tree.fall == 1 ~ NA_integer_, T ~ as.integer(tree_fall_injured_2)))

# index = 3
dx <- dx %>%
  mutate(tree_fall_injured_1 = case_when(index == 3 & n.tree.fall == 1 ~ other.serious.accident.injured2, T ~ as.integer(tree_fall_injured_1)),
         tree_fall_injured_2 = case_when(index == 3 & n.tree.fall == 1 ~ NA_integer_, T ~ as.integer(tree_fall_injured_2)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.injured,
                             other.serious.accident.injured1,
                             other.serious.accident.injured2,
                             other.serious.accident.injured3,
                             other.serious.accident.injured4,
                             other.serious.accident.injured5))

### Days disabled ----
dx$tree_fall_days_disabled_1 <- NA_real_
dx$tree_fall_days_disabled_2 <- NA_real_

dx <- relocate(dx, c(tree_fall_days_disabled_1, tree_fall_days_disabled_2),
               .after = tree_fall_injured_2)

# index = 1
dx <- dx %>%
  mutate(tree_fall_days_disabled_1 = case_when(index == 1 & n.tree.fall == 1 ~ other.serious.accident.days.disabled, T ~ as.numeric(tree_fall_days_disabled_1)),
         tree_fall_days_disabled_2 = case_when(index == 1 & n.tree.fall == 1 ~ NA_real_, T ~ as.numeric(tree_fall_days_disabled_2)))

dx <- dx %>%
  mutate(tree_fall_days_disabled_1 = case_when(index == 1 & n.tree.fall == 2 ~ other.serious.accident.days.disabled, T ~ as.numeric(tree_fall_days_disabled_1)),
         tree_fall_days_disabled_2 = case_when(index == 1 & n.tree.fall == 2 ~ other.serious.accident.days.disabled1, T ~ as.numeric(tree_fall_days_disabled_2)))

# index = 2
dx <- dx %>%
  mutate(tree_fall_days_disabled_1 = case_when(index == 2 & n.tree.fall == 1 ~ other.serious.accident.days.disabled1, T ~ as.numeric(tree_fall_days_disabled_1)),
         tree_fall_days_disabled_2 = case_when(index == 2 & n.tree.fall == 1 ~ NA_real_, T ~ as.numeric(tree_fall_days_disabled_2)))

# index = 3
dx <- dx %>%
  mutate(tree_fall_days_disabled_1 = case_when(index == 3 & n.tree.fall == 1 ~ other.serious.accident.days.disabled2, T ~ as.numeric(tree_fall_days_disabled_1)),
         tree_fall_days_disabled_2 = case_when(index == 3 & n.tree.fall == 1 ~ NA_real_, T ~ as.numeric(tree_fall_days_disabled_2)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.days.disabled,
                             other.serious.accident.days.disabled1,
                             other.serious.accident.days.disabled2,
                             other.serious.accident.days.disabled3,
                             other.serious.accident.days.disabled4,
                             other.serious.accident.days.disabled5))

### Almost died ----
dx$tree_fall_almost_died_1 <- NA_integer_
dx$tree_fall_almost_died_2 <- NA_integer_

dx <- relocate(dx, c(tree_fall_almost_died_1, tree_fall_almost_died_2),
               .after = tree_fall_days_disabled_2)

# index = 1
dx <- dx %>%
  mutate(tree_fall_almost_died_1 = case_when(index == 1 & n.tree.fall == 1 ~ other.serious.accident.almost.died, T ~ as.integer(tree_fall_almost_died_1)),
         tree_fall_almost_died_2 = case_when(index == 1 & n.tree.fall == 1 ~ NA_integer_, T ~ as.integer(tree_fall_almost_died_2)))

dx <- dx %>%
  mutate(tree_fall_almost_died_1 = case_when(index == 1 & n.tree.fall == 2 ~ other.serious.accident.almost.died, T ~ as.integer(tree_fall_almost_died_1)),
         tree_fall_almost_died_2 = case_when(index == 1 & n.tree.fall == 2 ~ other.serious.accident.almost.died1, T ~ as.integer(tree_fall_almost_died_2)))

# index = 2
dx <- dx %>%
  mutate(tree_fall_almost_died_1 = case_when(index == 2 & n.tree.fall == 1 ~ other.serious.accident.almost.died1, T ~ as.integer(tree_fall_almost_died_1)),
         tree_fall_almost_died_2 = case_when(index == 2 & n.tree.fall == 1 ~ NA_integer_, T ~ as.integer(tree_fall_almost_died_2)))

# index = 3
dx <- dx %>%
  mutate(tree_fall_almost_died_1 = case_when(index == 3 & n.tree.fall == 1 ~ other.serious.accident.almost.died2, T ~ as.integer(tree_fall_almost_died_1)),
         tree_fall_almost_died_2 = case_when(index == 3 & n.tree.fall == 1 ~ NA_integer_, T ~ as.integer(tree_fall_almost_died_2)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.almost.died,
                             other.serious.accident.almost.died1,
                             other.serious.accident.almost.died2,
                             other.serious.accident.almost.died3,
                             other.serious.accident.almost.died4,
                             other.serious.accident.almost.died5))

### Still bothers ----
dx$tree_fall_still_bothers_1 <- NA_integer_
dx$tree_fall_still_bothers_2 <- NA_integer_

dx <- relocate(dx, c(tree_fall_still_bothers_1, tree_fall_still_bothers_2),
               .after = tree_fall_almost_died_2)

# index = 1
dx <- dx %>%
  mutate(tree_fall_still_bothers_1 = case_when(index == 1 & n.tree.fall == 1 ~ other.serious.accident.still.bothers, T ~ as.integer(tree_fall_still_bothers_1)),
         tree_fall_still_bothers_2 = case_when(index == 1 & n.tree.fall == 1 ~ NA_integer_, T ~ as.integer(tree_fall_still_bothers_2)))

dx <- dx %>%
  mutate(tree_fall_still_bothers_1 = case_when(index == 1 & n.tree.fall == 2 ~ other.serious.accident.still.bothers, T ~ as.integer(tree_fall_still_bothers_1)),
         tree_fall_still_bothers_2 = case_when(index == 1 & n.tree.fall == 2 ~ other.serious.accident.still.bothers1, T ~ as.integer(tree_fall_still_bothers_2)))

# index = 2
dx <- dx %>%
  mutate(tree_fall_still_bothers_1 = case_when(index == 2 & n.tree.fall == 1 ~ other.serious.accident.still.bothers1, T ~ as.integer(tree_fall_still_bothers_1)),
         tree_fall_still_bothers_2 = case_when(index == 2 & n.tree.fall == 1 ~ NA_integer_, T ~ as.integer(tree_fall_still_bothers_2)))

# index = 3
dx <- dx %>%
  mutate(tree_fall_still_bothers_1 = case_when(index == 3 & n.tree.fall == 1 ~ other.serious.accident.still.bothers2, T ~ as.integer(tree_fall_still_bothers_1)),
         tree_fall_still_bothers_2 = case_when(index == 3 & n.tree.fall == 1 ~ NA_integer_, T ~ as.integer(tree_fall_still_bothers_2)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.still.bothers,
                             other.serious.accident.still.bothers1,
                             other.serious.accident.still.bothers2,
                             other.serious.accident.still.bothers3,
                             other.serious.accident.still.bothers4,
                             other.serious.accident.still.bothers5))

dx <- subset(dx, select = -c(TIPO1, TIPO2, TIPO3, TIPO4, TIPO5, TIPO6))

## Get back to original data frame
df <- left_join(df, dx)
df <- relocate(df, c(tree_fall_where_hurt_1:tree_fall_still_bothers_2), .after = n.tree.fall)
df <- subset(df, select = -c(index))




## Canoe Capsize ----
raw <- read.csv("raw_data_no_duplicates.csv")
raw <- select(raw, c(7, TIPO1:other.serious.accident.age5))

## Arrange the instances in ascending order, implementing bubble sort algorithm ----
# Very tedious but could not find a better way

raw$other.serious.accident.activity3 <- NA_character_
raw$other.serious.accident.activity4 <- NA_character_
raw$other.serious.accident.activity5 <- NA_character_
raw$other.serious.accident.days.disabled3 <- as.numeric(raw$other.serious.accident.days.disabled3)

# Iteration 1/5
raw <- transform(raw, TIPO1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 2/5
raw <- transform(raw, TIPO1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 3/5
raw <- transform(raw, TIPO1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 4/5
raw <- transform(raw, TIPO1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 5/5
raw <- transform(raw, TIPO1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))














## Applying same algorithm to get ascending order of canoe capsize ages ----
# Iteration 1/5
raw <- transform(raw, TIPO1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 2/5
raw <- transform(raw, TIPO1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 3/5
raw <- transform(raw, TIPO1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 4/5
raw <- transform(raw, TIPO1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 5/5
raw <- transform(raw, TIPO1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO1 %in% "a") & (TIPO2 %in% "a") ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO2 %in% "a") & (TIPO3 %in% "a") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO3 %in% "a") & (TIPO4 %in% "a") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO4 %in% "a") & (TIPO5 %in% "a") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(!(TIPO5 %in% "a") & (TIPO6 %in% "a") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))


## Some additional steps ----
raw <- raw %>% dplyr::rename("other.serious.accident.where.hurt2" = "other.serious.accidents.where.hurt2")
raw <- raw %>% dplyr::rename("other.serious.accident.activity2" = "other.serious.accidents.activity2")
raw <- raw %>% dplyr::rename("other.serious.accident.injured1" = "other.serious.accident.injured.yesno1")
raw <- raw %>% dplyr::rename("other.serious.accident.injured2" = "other.serious.accidents.injured.yesno2")

raw <- subset(raw, select = -c(other.serious.accident.age, other.serious.accident.age1,
                               other.serious.accident.age2, other.serious.accident.age3,
                               other.serious.accident.age4, other.serious.accident.age5))

## Merging with df ----
dx <- left_join(df, raw)

dx <- dx %>%
  filter(canoe.capsize.during.interval == 1)
dx <- dx %>%
  group_by(pid) %>%
  mutate(index = 1:n())
dx <- relocate(dx, index, .after = canoe.capsize.during.interval)
dx <- relocate(dx, other.serious.accident.activity3, .after = other.serious.accident.where.hurt3)
dx <- relocate(dx, other.serious.accident.activity4, .after = other.serious.accident.where.hurt4)
dx <- relocate(dx, other.serious.accident.activity5, .after = other.serious.accident.where.hurt5)
dx <- subset(dx, select = c(pid, exit, index, n.canoe.capsize, TIPO1:other.serious.accident.still.bothers5))

plyr::count(dx$TIPO1)
plyr::count(dx$TIPO2)
plyr::count(dx$TIPO3)
plyr::count(dx$TIPO4)
plyr::count(dx$TIPO5)
plyr::count(dx$TIPO6)

### Where hurt? ----
dx$canoe_capsize_where_hurt_1 <- NA_character_
dx$canoe_capsize_where_hurt_2 <- NA_character_

dx <- relocate(dx, c(canoe_capsize_where_hurt_1, canoe_capsize_where_hurt_2),
               .after = n.canoe.capsize)

# index = 1
dx <- dx %>%
  mutate(canoe_capsize_where_hurt_1 = case_when(index == 1 & n.canoe.capsize == 1 ~ other.serious.accident.where.hurt, T ~ as.character(canoe_capsize_where_hurt_1)),
         canoe_capsize_where_hurt_2 = case_when(index == 1 & n.canoe.capsize == 1 ~ NA_character_, T ~ as.character(canoe_capsize_where_hurt_2)))

dx <- dx %>%
  mutate(canoe_capsize_where_hurt_1 = case_when(index == 1 & n.canoe.capsize == 2 ~ other.serious.accident.where.hurt, T ~ as.character(canoe_capsize_where_hurt_1)),
         canoe_capsize_where_hurt_2 = case_when(index == 1 & n.canoe.capsize == 2 ~ other.serious.accident.where.hurt1, T ~ as.character(canoe_capsize_where_hurt_2)))

# index = 2
dx <- dx %>%
  mutate(canoe_capsize_where_hurt_1 = case_when(index == 2 & n.canoe.capsize == 1 ~ other.serious.accident.where.hurt1, T ~ as.character(canoe_capsize_where_hurt_1)),
         canoe_capsize_where_hurt_2 = case_when(index == 2 & n.canoe.capsize == 1 ~ NA_character_, T ~ as.character(canoe_capsize_where_hurt_2)))

# index = 3
dx <- dx %>%
  mutate(canoe_capsize_where_hurt_1 = case_when(index == 3 & n.canoe.capsize == 1 ~ other.serious.accident.where.hurt2, T ~ as.character(canoe_capsize_where_hurt_1)),
         canoe_capsize_where_hurt_2 = case_when(index == 3 & n.canoe.capsize == 1 ~ NA_character_, T ~ as.character(canoe_capsize_where_hurt_2)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.where.hurt,
                             other.serious.accident.where.hurt1,
                             other.serious.accident.where.hurt2,
                             other.serious.accident.where.hurt3,
                             other.serious.accident.where.hurt4,
                             other.serious.accident.where.hurt5))

### Activity ----
dx$canoe_capsize_activity_1 <- NA_character_
dx$canoe_capsize_activity_2 <- NA_character_

dx <- relocate(dx, c(canoe_capsize_activity_1, canoe_capsize_activity_2),
               .after = canoe_capsize_where_hurt_2)

# index = 1
dx <- dx %>%
  mutate(canoe_capsize_activity_1 = case_when(index == 1 & n.canoe.capsize == 1 ~ other.serious.accident.activity, T ~ as.character(canoe_capsize_activity_1)),
         canoe_capsize_activity_2 = case_when(index == 1 & n.canoe.capsize == 1 ~ NA_character_, T ~ as.character(canoe_capsize_activity_2)))

dx <- dx %>%
  mutate(canoe_capsize_activity_1 = case_when(index == 1 & n.canoe.capsize == 2 ~ other.serious.accident.activity, T ~ as.character(canoe_capsize_activity_1)),
         canoe_capsize_activity_2 = case_when(index == 1 & n.canoe.capsize == 2 ~ other.serious.accident.activity1, T ~ as.character(canoe_capsize_activity_2)))

# index = 2
dx <- dx %>%
  mutate(canoe_capsize_activity_1 = case_when(index == 2 & n.canoe.capsize == 1 ~ other.serious.accident.activity1, T ~ as.character(canoe_capsize_activity_1)),
         canoe_capsize_activity_2 = case_when(index == 2 & n.canoe.capsize == 1 ~ NA_character_, T ~ as.character(canoe_capsize_activity_2)))

# index = 3
dx <- dx %>%
  mutate(canoe_capsize_activity_1 = case_when(index == 3 & n.canoe.capsize == 1 ~ other.serious.accident.activity2, T ~ as.character(canoe_capsize_activity_1)),
         canoe_capsize_activity_2 = case_when(index == 3 & n.canoe.capsize == 1 ~ NA_character_, T ~ as.character(canoe_capsize_activity_2)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.activity,
                             other.serious.accident.activity1,
                             other.serious.accident.activity2,
                             other.serious.accident.activity3,
                             other.serious.accident.activity4,
                             other.serious.accident.activity5))

### Injured ----
dx$canoe_capsize_injured_1 <- NA_integer_
dx$canoe_capsize_injured_2 <- NA_integer_

dx <- relocate(dx, c(canoe_capsize_injured_1, canoe_capsize_injured_2),
               .after = canoe_capsize_activity_2)

# index = 1
dx <- dx %>%
  mutate(canoe_capsize_injured_1 = case_when(index == 1 & n.canoe.capsize == 1 ~ other.serious.accident.injured, T ~ as.integer(canoe_capsize_injured_1)),
         canoe_capsize_injured_2 = case_when(index == 1 & n.canoe.capsize == 1 ~ NA_integer_, T ~ as.integer(canoe_capsize_injured_2)))

dx <- dx %>%
  mutate(canoe_capsize_injured_1 = case_when(index == 1 & n.canoe.capsize == 2 ~ other.serious.accident.injured, T ~ as.integer(canoe_capsize_injured_1)),
         canoe_capsize_injured_2 = case_when(index == 1 & n.canoe.capsize == 2 ~ other.serious.accident.injured1, T ~ as.integer(canoe_capsize_injured_2)))

# index = 2
dx <- dx %>%
  mutate(canoe_capsize_injured_1 = case_when(index == 2 & n.canoe.capsize == 1 ~ other.serious.accident.injured1, T ~ as.integer(canoe_capsize_injured_1)),
         canoe_capsize_injured_2 = case_when(index == 2 & n.canoe.capsize == 1 ~ NA_integer_, T ~ as.integer(canoe_capsize_injured_2)))

# index = 3
dx <- dx %>%
  mutate(canoe_capsize_injured_1 = case_when(index == 3 & n.canoe.capsize == 1 ~ other.serious.accident.injured2, T ~ as.integer(canoe_capsize_injured_1)),
         canoe_capsize_injured_2 = case_when(index == 3 & n.canoe.capsize == 1 ~ NA_integer_, T ~ as.integer(canoe_capsize_injured_2)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.injured,
                             other.serious.accident.injured1,
                             other.serious.accident.injured2,
                             other.serious.accident.injured3,
                             other.serious.accident.injured4,
                             other.serious.accident.injured5))

### Days disabled ----
dx$canoe_capsize_days_disabled_1 <- NA_real_
dx$canoe_capsize_days_disabled_2 <- NA_real_

dx <- relocate(dx, c(canoe_capsize_days_disabled_1, canoe_capsize_days_disabled_2),
               .after = canoe_capsize_injured_2)

# index = 1
dx <- dx %>%
  mutate(canoe_capsize_days_disabled_1 = case_when(index == 1 & n.canoe.capsize == 1 ~ other.serious.accident.days.disabled, T ~ as.numeric(canoe_capsize_days_disabled_1)),
         canoe_capsize_days_disabled_2 = case_when(index == 1 & n.canoe.capsize == 1 ~ NA_real_, T ~ as.numeric(canoe_capsize_days_disabled_2)))

dx <- dx %>%
  mutate(canoe_capsize_days_disabled_1 = case_when(index == 1 & n.canoe.capsize == 2 ~ other.serious.accident.days.disabled, T ~ as.numeric(canoe_capsize_days_disabled_1)),
         canoe_capsize_days_disabled_2 = case_when(index == 1 & n.canoe.capsize == 2 ~ other.serious.accident.days.disabled1, T ~ as.numeric(canoe_capsize_days_disabled_2)))

# index = 2
dx <- dx %>%
  mutate(canoe_capsize_days_disabled_1 = case_when(index == 2 & n.canoe.capsize == 1 ~ other.serious.accident.days.disabled1, T ~ as.numeric(canoe_capsize_days_disabled_1)),
         canoe_capsize_days_disabled_2 = case_when(index == 2 & n.canoe.capsize == 1 ~ NA_real_, T ~ as.numeric(canoe_capsize_days_disabled_2)))

# index = 3
dx <- dx %>%
  mutate(canoe_capsize_days_disabled_1 = case_when(index == 3 & n.canoe.capsize == 1 ~ other.serious.accident.days.disabled2, T ~ as.numeric(canoe_capsize_days_disabled_1)),
         canoe_capsize_days_disabled_2 = case_when(index == 3 & n.canoe.capsize == 1 ~ NA_real_, T ~ as.numeric(canoe_capsize_days_disabled_2)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.days.disabled,
                             other.serious.accident.days.disabled1,
                             other.serious.accident.days.disabled2,
                             other.serious.accident.days.disabled3,
                             other.serious.accident.days.disabled4,
                             other.serious.accident.days.disabled5))

### Almost died ----
dx$canoe_capsize_almost_died_1 <- NA_integer_
dx$canoe_capsize_almost_died_2 <- NA_integer_

dx <- relocate(dx, c(canoe_capsize_almost_died_1, canoe_capsize_almost_died_2),
               .after = canoe_capsize_days_disabled_2)

# index = 1
dx <- dx %>%
  mutate(canoe_capsize_almost_died_1 = case_when(index == 1 & n.canoe.capsize == 1 ~ other.serious.accident.almost.died, T ~ as.integer(canoe_capsize_almost_died_1)),
         canoe_capsize_almost_died_2 = case_when(index == 1 & n.canoe.capsize == 1 ~ NA_integer_, T ~ as.integer(canoe_capsize_almost_died_2)))

dx <- dx %>%
  mutate(canoe_capsize_almost_died_1 = case_when(index == 1 & n.canoe.capsize == 2 ~ other.serious.accident.almost.died, T ~ as.integer(canoe_capsize_almost_died_1)),
         canoe_capsize_almost_died_2 = case_when(index == 1 & n.canoe.capsize == 2 ~ other.serious.accident.almost.died1, T ~ as.integer(canoe_capsize_almost_died_2)))

# index = 2
dx <- dx %>%
  mutate(canoe_capsize_almost_died_1 = case_when(index == 2 & n.canoe.capsize == 1 ~ other.serious.accident.almost.died1, T ~ as.integer(canoe_capsize_almost_died_1)),
         canoe_capsize_almost_died_2 = case_when(index == 2 & n.canoe.capsize == 1 ~ NA_integer_, T ~ as.integer(canoe_capsize_almost_died_2)))

# index = 3
dx <- dx %>%
  mutate(canoe_capsize_almost_died_1 = case_when(index == 3 & n.canoe.capsize == 1 ~ other.serious.accident.almost.died2, T ~ as.integer(canoe_capsize_almost_died_1)),
         canoe_capsize_almost_died_2 = case_when(index == 3 & n.canoe.capsize == 1 ~ NA_integer_, T ~ as.integer(canoe_capsize_almost_died_2)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.almost.died,
                             other.serious.accident.almost.died1,
                             other.serious.accident.almost.died2,
                             other.serious.accident.almost.died3,
                             other.serious.accident.almost.died4,
                             other.serious.accident.almost.died5))

### Still bothers ----
dx$canoe_capsize_still_bothers_1 <- NA_integer_
dx$canoe_capsize_still_bothers_2 <- NA_integer_

dx <- relocate(dx, c(canoe_capsize_still_bothers_1, canoe_capsize_still_bothers_2),
               .after = canoe_capsize_almost_died_2)

# index = 1
dx <- dx %>%
  mutate(canoe_capsize_still_bothers_1 = case_when(index == 1 & n.canoe.capsize == 1 ~ other.serious.accident.still.bothers, T ~ as.integer(canoe_capsize_still_bothers_1)),
         canoe_capsize_still_bothers_2 = case_when(index == 1 & n.canoe.capsize == 1 ~ NA_integer_, T ~ as.integer(canoe_capsize_still_bothers_2)))

dx <- dx %>%
  mutate(canoe_capsize_still_bothers_1 = case_when(index == 1 & n.canoe.capsize == 2 ~ other.serious.accident.still.bothers, T ~ as.integer(canoe_capsize_still_bothers_1)),
         canoe_capsize_still_bothers_2 = case_when(index == 1 & n.canoe.capsize == 2 ~ other.serious.accident.still.bothers1, T ~ as.integer(canoe_capsize_still_bothers_2)))

# index = 2
dx <- dx %>%
  mutate(canoe_capsize_still_bothers_1 = case_when(index == 2 & n.canoe.capsize == 1 ~ other.serious.accident.still.bothers1, T ~ as.integer(canoe_capsize_still_bothers_1)),
         canoe_capsize_still_bothers_2 = case_when(index == 2 & n.canoe.capsize == 1 ~ NA_integer_, T ~ as.integer(canoe_capsize_still_bothers_2)))

# index = 3
dx <- dx %>%
  mutate(canoe_capsize_still_bothers_1 = case_when(index == 3 & n.canoe.capsize == 1 ~ other.serious.accident.still.bothers2, T ~ as.integer(canoe_capsize_still_bothers_1)),
         canoe_capsize_still_bothers_2 = case_when(index == 3 & n.canoe.capsize == 1 ~ NA_integer_, T ~ as.integer(canoe_capsize_still_bothers_2)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.still.bothers,
                             other.serious.accident.still.bothers1,
                             other.serious.accident.still.bothers2,
                             other.serious.accident.still.bothers3,
                             other.serious.accident.still.bothers4,
                             other.serious.accident.still.bothers5))

dx <- subset(dx, select = -c(TIPO1, TIPO2, TIPO3, TIPO4, TIPO5, TIPO6))

## Get back to original data frame
df <- left_join(df, dx)
df <- relocate(df, c(canoe_capsize_where_hurt_1:canoe_capsize_still_bothers_2), .after = n.canoe.capsize)
df <- subset(df, select = -c(index))




## Cut Self ----
raw <- read.csv("raw_data_no_duplicates.csv")
raw <- select(raw, c(7, TIPO1:other.serious.accident.age5))

## Arrange the instances in ascending order, implementing bubble sort algorithm ----
# Very tedious but could not find a better way

raw$other.serious.accident.activity3 <- NA_character_
raw$other.serious.accident.activity4 <- NA_character_
raw$other.serious.accident.activity5 <- NA_character_
raw$other.serious.accident.days.disabled3 <- as.numeric(raw$other.serious.accident.days.disabled3)

# Iteration 1/5
raw <- transform(raw, TIPO1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 2/5
raw <- transform(raw, TIPO1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 3/5
raw <- transform(raw, TIPO1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 4/5
raw <- transform(raw, TIPO1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 5/5
raw <- transform(raw, TIPO1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age > other.serious.accident.age1 ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age1 > other.serious.accident.age2 ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age2 > other.serious.accident.age3 ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age3 > other.serious.accident.age4 ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(other.serious.accident.age4 > other.serious.accident.age5 ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))





## Applying same algorithm to get ascending order of cut self ages ----
# Iteration 1/5
raw <- transform(raw, TIPO1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 2/5
raw <- transform(raw, TIPO1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 3/5
raw <- transform(raw, TIPO1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 4/5
raw <- transform(raw, TIPO1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))

# Iteration 5/5
raw <- transform(raw, TIPO1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ TIPO2, T ~ TIPO1),
                 TIPO2 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ TIPO1, T ~ TIPO2),
                 other.serious.accident.age = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.age1, T ~ other.serious.accident.age),
                 other.serious.accident.age1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.age, T ~ other.serious.accident.age1),
                 other.serious.accident.where.hurt = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.where.hurt1, T ~ other.serious.accident.where.hurt),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.where.hurt, T ~ other.serious.accident.where.hurt1),
                 other.serious.accident.activity = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.activity1, T ~ other.serious.accident.activity),
                 other.serious.accident.activity1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.activity, T ~ other.serious.accident.activity1),
                 other.serious.accident.injured = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accident.injured),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.injured, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accident.days.disabled = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.days.disabled, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.almost.died = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died),
                 other.serious.accident.almost.died1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.almost.died, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.still.bothers = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO1 %in% "c") & (TIPO2 %in% "c") ~ other.serious.accident.still.bothers, T ~ other.serious.accident.still.bothers1))

raw <- transform(raw, TIPO2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ TIPO3, T ~ TIPO2),
                 TIPO3 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ TIPO2, T ~ TIPO3),
                 other.serious.accident.age1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.age2, T ~ other.serious.accident.age1),
                 other.serious.accident.age2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.age1, T ~ other.serious.accident.age2),
                 other.serious.accident.where.hurt1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt1),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.where.hurt1, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.activity1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity1),
                 other.serious.accidents.activity2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.activity1, T ~ other.serious.accidents.activity2),
                 other.serious.accident.injured.yesno1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured.yesno1),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.injured.yesno1, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.days.disabled1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled1),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.days.disabled1, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.almost.died1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died1),
                 other.serious.accident.almost.died2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.almost.died1, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.still.bothers1 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers1),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO2 %in% "c") & (TIPO3 %in% "c") ~ other.serious.accident.still.bothers1, T ~ other.serious.accident.still.bothers2))

raw <- transform(raw, TIPO3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ TIPO4, T ~ TIPO3),
                 TIPO4 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ TIPO3, T ~ TIPO4),
                 other.serious.accident.age2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.age3, T ~ other.serious.accident.age2),
                 other.serious.accident.age3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.age2, T ~ other.serious.accident.age3),
                 other.serious.accidents.where.hurt2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.where.hurt3, T ~ other.serious.accidents.where.hurt2),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accidents.where.hurt2, T ~ other.serious.accident.where.hurt3),
                 other.serious.accidents.activity2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.activity3, T ~ other.serious.accidents.activity2),
                 other.serious.accident.activity3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accidents.activity2, T ~ other.serious.accident.activity3),
                 other.serious.accidents.injured.yesno2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.injured3, T ~ other.serious.accidents.injured.yesno2),
                 other.serious.accident.injured3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accidents.injured.yesno2, T ~ other.serious.accident.injured3),
                 other.serious.accident.days.disabled2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled2),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.days.disabled2, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.almost.died2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died2),
                 other.serious.accident.almost.died3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.almost.died2, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.still.bothers2 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers2),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO3 %in% "c") & (TIPO4 %in% "c") ~ other.serious.accident.still.bothers2, T ~ other.serious.accident.still.bothers3))

raw <- transform(raw, TIPO4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ TIPO5, T ~ TIPO4),
                 TIPO5 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ TIPO4, T ~ TIPO5),
                 other.serious.accident.age3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.age4, T ~ other.serious.accident.age3),
                 other.serious.accident.age4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.age3, T ~ other.serious.accident.age4),
                 other.serious.accident.where.hurt3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt3),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.where.hurt3, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.activity3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity3),
                 other.serious.accident.activity4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.activity3, T ~ other.serious.accident.activity4),
                 other.serious.accident.injured3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured3),
                 other.serious.accident.injured4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.injured3, T ~ other.serious.accident.injured4),
                 other.serious.accident.days.disabled3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled3),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.days.disabled3, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.almost.died3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died3),
                 other.serious.accident.almost.died4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.almost.died3, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.still.bothers3 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers3),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO4 %in% "c") & (TIPO5 %in% "c") ~ other.serious.accident.still.bothers3, T ~ other.serious.accident.still.bothers4))

raw <- transform(raw, TIPO5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ TIPO6, T ~ TIPO5),
                 TIPO6 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ TIPO5, T ~ TIPO6),
                 other.serious.accident.age4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.age5, T ~ other.serious.accident.age4),
                 other.serious.accident.age5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.age4, T ~ other.serious.accident.age5),
                 other.serious.accident.where.hurt4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.where.hurt5, T ~ other.serious.accident.where.hurt4),
                 other.serious.accident.where.hurt5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.where.hurt4, T ~ other.serious.accident.where.hurt5),
                 other.serious.accident.activity4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.activity5, T ~ other.serious.accident.activity4),
                 other.serious.accident.activity5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.activity4, T ~ other.serious.accident.activity5),
                 other.serious.accident.injured4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.injured5, T ~ other.serious.accident.injured4),
                 other.serious.accident.injured5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.injured4, T ~ other.serious.accident.injured5),
                 other.serious.accident.days.disabled4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.days.disabled5, T ~ other.serious.accident.days.disabled4),
                 other.serious.accident.days.disabled5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.days.disabled4, T ~ other.serious.accident.days.disabled5),
                 other.serious.accident.almost.died4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.almost.died5, T ~ other.serious.accident.almost.died4),
                 other.serious.accident.almost.died5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.almost.died4, T ~ other.serious.accident.almost.died5),
                 other.serious.accident.still.bothers4 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.still.bothers5, T ~ other.serious.accident.still.bothers4),
                 other.serious.accident.still.bothers5 = case_when(!(TIPO5 %in% "c") & (TIPO6 %in% "c") ~ other.serious.accident.still.bothers4, T ~ other.serious.accident.still.bothers5))


## Some additional steps ----
raw <- raw %>% dplyr::rename("other.serious.accident.where.hurt2" = "other.serious.accidents.where.hurt2")
raw <- raw %>% dplyr::rename("other.serious.accident.activity2" = "other.serious.accidents.activity2")
raw <- raw %>% dplyr::rename("other.serious.accident.injured1" = "other.serious.accident.injured.yesno1")
raw <- raw %>% dplyr::rename("other.serious.accident.injured2" = "other.serious.accidents.injured.yesno2")

raw <- subset(raw, select = -c(other.serious.accident.age, other.serious.accident.age1,
                               other.serious.accident.age2, other.serious.accident.age3,
                               other.serious.accident.age4, other.serious.accident.age5))


## Merging with df ----
dx <- left_join(df, raw)

dx <- dx %>%
  filter(cut.self.during.interval == 1)
dx <- dx %>%
  group_by(pid) %>%
  mutate(index = 1:n())
dx <- relocate(dx, index, .after = cut.self.during.interval)
dx <- relocate(dx, other.serious.accident.activity3, .after = other.serious.accident.where.hurt3)
dx <- relocate(dx, other.serious.accident.activity4, .after = other.serious.accident.where.hurt4)
dx <- relocate(dx, other.serious.accident.activity5, .after = other.serious.accident.where.hurt5)
dx <- subset(dx, select = c(pid, exit, index, n.cut.self, TIPO1:other.serious.accident.still.bothers5))


plyr::count(dx$TIPO1)
plyr::count(dx$TIPO2)
plyr::count(dx$TIPO3)
plyr::count(dx$TIPO4)
plyr::count(dx$TIPO5)
plyr::count(dx$TIPO6)

### Where hurt? ----
dx$cut_self_where_hurt_1 <- NA_character_
dx$cut_self_where_hurt_2 <- NA_character_
dx$cut_self_where_hurt_3 <- NA_character_

dx <- relocate(dx, c(cut_self_where_hurt_1, cut_self_where_hurt_2,
                     cut_self_where_hurt_3), .after = n.cut.self)

# index = 1
dx <- dx %>%
  mutate(cut_self_where_hurt_1 = case_when(index == 1 & n.cut.self == 1 ~ other.serious.accident.where.hurt, T ~ as.character(cut_self_where_hurt_1)),
         cut_self_where_hurt_2 = case_when(index == 1 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_where_hurt_2)),
         cut_self_where_hurt_3 = case_when(index == 1 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_where_hurt_3)))

dx <- dx %>%
  mutate(cut_self_where_hurt_1 = case_when(index == 1 & n.cut.self == 2 ~ other.serious.accident.where.hurt, T ~ as.character(cut_self_where_hurt_1)),
         cut_self_where_hurt_2 = case_when(index == 1 & n.cut.self == 2 ~ other.serious.accident.where.hurt1, T ~ as.character(cut_self_where_hurt_2)),
         cut_self_where_hurt_3 = case_when(index == 1 & n.cut.self == 2 ~ NA_character_, T ~ as.character(cut_self_where_hurt_3)))

dx <- dx %>%
  mutate(cut_self_where_hurt_1 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.where.hurt, T ~ as.character(cut_self_where_hurt_1)),
         cut_self_where_hurt_2 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.where.hurt1, T ~ as.character(cut_self_where_hurt_2)),
         cut_self_where_hurt_3 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.where.hurt2, T ~ as.character(cut_self_where_hurt_3)))

# index = 2
dx <- dx %>%
  mutate(cut_self_where_hurt_1 = case_when(index == 2 & n.cut.self == 1 ~ other.serious.accident.where.hurt1, T ~ as.character(cut_self_where_hurt_1)),
         cut_self_where_hurt_2 = case_when(index == 2 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_where_hurt_2)),
         cut_self_where_hurt_3 = case_when(index == 2 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_where_hurt_3)))

dx <- dx %>%
  mutate(cut_self_where_hurt_1 = case_when(index == 2 & n.cut.self == 2 ~ other.serious.accident.where.hurt1, T ~ as.character(cut_self_where_hurt_1)),
         cut_self_where_hurt_2 = case_when(index == 2 & n.cut.self == 2 ~ other.serious.accident.where.hurt2, T ~ as.character(cut_self_where_hurt_2)),
         cut_self_where_hurt_3 = case_when(index == 2 & n.cut.self == 2 ~ NA_character_, T ~ as.character(cut_self_where_hurt_3)))

dx <- dx %>%
  mutate(cut_self_where_hurt_1 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.where.hurt1, T ~ as.character(cut_self_where_hurt_1)),
         cut_self_where_hurt_2 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.where.hurt2, T ~ as.character(cut_self_where_hurt_2)),
         cut_self_where_hurt_3 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.where.hurt3, T ~ as.character(cut_self_where_hurt_3)))

# index = 3
dx <- dx %>%
  mutate(cut_self_where_hurt_1 = case_when(index == 3 & n.cut.self == 1 ~ other.serious.accident.where.hurt2, T ~ as.character(cut_self_where_hurt_1)),
         cut_self_where_hurt_2 = case_when(index == 3 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_where_hurt_2)),
         cut_self_where_hurt_3 = case_when(index == 3 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_where_hurt_3)))

dx <- dx %>%
  mutate(cut_self_where_hurt_1 = case_when(index == 3 & n.cut.self == 2 ~ other.serious.accident.where.hurt2, T ~ as.character(cut_self_where_hurt_1)),
         cut_self_where_hurt_2 = case_when(index == 3 & n.cut.self == 2 ~ other.serious.accident.where.hurt3, T ~ as.character(cut_self_where_hurt_2)),
         cut_self_where_hurt_3 = case_when(index == 3 & n.cut.self == 2 ~ NA_character_, T ~ as.character(cut_self_where_hurt_3)))

dx <- dx %>%
  mutate(cut_self_where_hurt_1 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.where.hurt2, T ~ as.character(cut_self_where_hurt_1)),
         cut_self_where_hurt_2 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.where.hurt3, T ~ as.character(cut_self_where_hurt_2)),
         cut_self_where_hurt_3 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.where.hurt4, T ~ as.character(cut_self_where_hurt_3)))

# index = 4
dx <- dx %>%
  mutate(cut_self_where_hurt_1 = case_when(index == 4 & n.cut.self == 1 ~ other.serious.accident.where.hurt3, T ~ as.character(cut_self_where_hurt_1)),
         cut_self_where_hurt_2 = case_when(index == 4 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_where_hurt_2)),
         cut_self_where_hurt_3 = case_when(index == 4 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_where_hurt_3)))

dx <- dx %>%
  mutate(cut_self_where_hurt_1 = case_when(index == 4 & n.cut.self == 2 ~ other.serious.accident.where.hurt3, T ~ as.character(cut_self_where_hurt_1)),
         cut_self_where_hurt_2 = case_when(index == 4 & n.cut.self == 2 ~ other.serious.accident.where.hurt4, T ~ as.character(cut_self_where_hurt_2)),
         cut_self_where_hurt_3 = case_when(index == 4 & n.cut.self == 2 ~ NA_character_, T ~ as.character(cut_self_where_hurt_3)))

dx <- dx %>%
  mutate(cut_self_where_hurt_1 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.where.hurt3, T ~ as.character(cut_self_where_hurt_1)),
         cut_self_where_hurt_2 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.where.hurt4, T ~ as.character(cut_self_where_hurt_2)),
         cut_self_where_hurt_3 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.where.hurt5, T ~ as.character(cut_self_where_hurt_3)))

# index = 5
dx <- dx %>%
  mutate(cut_self_where_hurt_1 = case_when(index == 5 & n.cut.self == 1 ~ other.serious.accident.where.hurt4, T ~ as.character(cut_self_where_hurt_1)),
         cut_self_where_hurt_2 = case_when(index == 5 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_where_hurt_2)),
         cut_self_where_hurt_3 = case_when(index == 5 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_where_hurt_3)))

# index = 6
dx <- dx %>%
  mutate(cut_self_where_hurt_1 = case_when(index == 6 & n.cut.self == 1 ~ other.serious.accident.where.hurt5, T ~ as.character(cut_self_where_hurt_1)),
         cut_self_where_hurt_2 = case_when(index == 6 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_where_hurt_2)),
         cut_self_where_hurt_3 = case_when(index == 6 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_where_hurt_3)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.where.hurt,
                             other.serious.accident.where.hurt1,
                             other.serious.accident.where.hurt2,
                             other.serious.accident.where.hurt3,
                             other.serious.accident.where.hurt4,
                             other.serious.accident.where.hurt5))

### Activity ----
dx$cut_self_activity_1 <- NA_character_
dx$cut_self_activity_2 <- NA_character_
dx$cut_self_activity_3 <- NA_character_

dx <- relocate(dx, c(cut_self_activity_1, cut_self_activity_2,
                     cut_self_activity_3), .after = cut_self_where_hurt_3)

# index = 1
dx <- dx %>%
  mutate(cut_self_activity_1 = case_when(index == 1 & n.cut.self == 1 ~ other.serious.accident.activity, T ~ as.character(cut_self_activity_1)),
         cut_self_activity_2 = case_when(index == 1 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_activity_2)),
         cut_self_activity_3 = case_when(index == 1 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_activity_3)))

dx <- dx %>%
  mutate(cut_self_activity_1 = case_when(index == 1 & n.cut.self == 2 ~ other.serious.accident.activity, T ~ as.character(cut_self_activity_1)),
         cut_self_activity_2 = case_when(index == 1 & n.cut.self == 2 ~ other.serious.accident.activity1, T ~ as.character(cut_self_activity_2)),
         cut_self_activity_3 = case_when(index == 1 & n.cut.self == 2 ~ NA_character_, T ~ as.character(cut_self_activity_3)))

dx <- dx %>%
  mutate(cut_self_activity_1 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.activity, T ~ as.character(cut_self_activity_1)),
         cut_self_activity_2 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.activity1, T ~ as.character(cut_self_activity_2)),
         cut_self_activity_3 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.activity2, T ~ as.character(cut_self_activity_3)))

# index = 2
dx <- dx %>%
  mutate(cut_self_activity_1 = case_when(index == 2 & n.cut.self == 1 ~ other.serious.accident.activity1, T ~ as.character(cut_self_activity_1)),
         cut_self_activity_2 = case_when(index == 2 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_activity_2)),
         cut_self_activity_3 = case_when(index == 2 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_activity_3)))

dx <- dx %>%
  mutate(cut_self_activity_1 = case_when(index == 2 & n.cut.self == 2 ~ other.serious.accident.activity1, T ~ as.character(cut_self_activity_1)),
         cut_self_activity_2 = case_when(index == 2 & n.cut.self == 2 ~ other.serious.accident.activity2, T ~ as.character(cut_self_activity_2)),
         cut_self_activity_3 = case_when(index == 2 & n.cut.self == 2 ~ NA_character_, T ~ as.character(cut_self_activity_3)))

dx <- dx %>%
  mutate(cut_self_activity_1 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.activity1, T ~ as.character(cut_self_activity_1)),
         cut_self_activity_2 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.activity2, T ~ as.character(cut_self_activity_2)),
         cut_self_activity_3 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.activity3, T ~ as.character(cut_self_activity_3)))

# index = 3
dx <- dx %>%
  mutate(cut_self_activity_1 = case_when(index == 3 & n.cut.self == 1 ~ other.serious.accident.activity2, T ~ as.character(cut_self_activity_1)),
         cut_self_activity_2 = case_when(index == 3 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_activity_2)),
         cut_self_activity_3 = case_when(index == 3 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_activity_3)))

dx <- dx %>%
  mutate(cut_self_activity_1 = case_when(index == 3 & n.cut.self == 2 ~ other.serious.accident.activity2, T ~ as.character(cut_self_activity_1)),
         cut_self_activity_2 = case_when(index == 3 & n.cut.self == 2 ~ other.serious.accident.activity3, T ~ as.character(cut_self_activity_2)),
         cut_self_activity_3 = case_when(index == 3 & n.cut.self == 2 ~ NA_character_, T ~ as.character(cut_self_activity_3)))

dx <- dx %>%
  mutate(cut_self_activity_1 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.activity2, T ~ as.character(cut_self_activity_1)),
         cut_self_activity_2 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.activity3, T ~ as.character(cut_self_activity_2)),
         cut_self_activity_3 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.activity4, T ~ as.character(cut_self_activity_3)))

# index = 4
dx <- dx %>%
  mutate(cut_self_activity_1 = case_when(index == 4 & n.cut.self == 1 ~ other.serious.accident.activity3, T ~ as.character(cut_self_activity_1)),
         cut_self_activity_2 = case_when(index == 4 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_activity_2)),
         cut_self_activity_3 = case_when(index == 4 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_activity_3)))

dx <- dx %>%
  mutate(cut_self_activity_1 = case_when(index == 4 & n.cut.self == 2 ~ other.serious.accident.activity3, T ~ as.character(cut_self_activity_1)),
         cut_self_activity_2 = case_when(index == 4 & n.cut.self == 2 ~ other.serious.accident.activity4, T ~ as.character(cut_self_activity_2)),
         cut_self_activity_3 = case_when(index == 4 & n.cut.self == 2 ~ NA_character_, T ~ as.character(cut_self_activity_3)))

dx <- dx %>%
  mutate(cut_self_activity_1 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.activity3, T ~ as.character(cut_self_activity_1)),
         cut_self_activity_2 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.activity4, T ~ as.character(cut_self_activity_2)),
         cut_self_activity_3 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.activity5, T ~ as.character(cut_self_activity_3)))

# index = 5
dx <- dx %>%
  mutate(cut_self_activity_1 = case_when(index == 5 & n.cut.self == 1 ~ other.serious.accident.activity4, T ~ as.character(cut_self_activity_1)),
         cut_self_activity_2 = case_when(index == 5 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_activity_2)),
         cut_self_activity_3 = case_when(index == 5 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_activity_3)))

# index = 6
dx <- dx %>%
  mutate(cut_self_activity_1 = case_when(index == 6 & n.cut.self == 1 ~ other.serious.accident.activity5, T ~ as.character(cut_self_activity_1)),
         cut_self_activity_2 = case_when(index == 6 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_activity_2)),
         cut_self_activity_3 = case_when(index == 6 & n.cut.self == 1 ~ NA_character_, T ~ as.character(cut_self_activity_3)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.activity,
                             other.serious.accident.activity1,
                             other.serious.accident.activity2,
                             other.serious.accident.activity3,
                             other.serious.accident.activity4,
                             other.serious.accident.activity5))

### Injured? ----
dx$cut_self_injured_1 <- NA_integer_
dx$cut_self_injured_2 <- NA_integer_
dx$cut_self_injured_3 <- NA_integer_

dx <- relocate(dx, c(cut_self_injured_1, cut_self_injured_2,
                     cut_self_injured_3), .after = cut_self_activity_3)

# index = 1
dx <- dx %>%
  mutate(cut_self_injured_1 = case_when(index == 1 & n.cut.self == 1 ~ other.serious.accident.injured, T ~ as.integer(cut_self_injured_1)),
         cut_self_injured_2 = case_when(index == 1 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_injured_2)),
         cut_self_injured_3 = case_when(index == 1 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_injured_3)))

dx <- dx %>%
  mutate(cut_self_injured_1 = case_when(index == 1 & n.cut.self == 2 ~ other.serious.accident.injured, T ~ as.integer(cut_self_injured_1)),
         cut_self_injured_2 = case_when(index == 1 & n.cut.self == 2 ~ other.serious.accident.injured1, T ~ as.integer(cut_self_injured_2)),
         cut_self_injured_3 = case_when(index == 1 & n.cut.self == 2 ~ NA_integer_, T ~ as.integer(cut_self_injured_3)))

dx <- dx %>%
  mutate(cut_self_injured_1 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.injured, T ~ as.integer(cut_self_injured_1)),
         cut_self_injured_2 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.injured1, T ~ as.integer(cut_self_injured_2)),
         cut_self_injured_3 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.injured2, T ~ as.integer(cut_self_injured_3)))

# index = 2
dx <- dx %>%
  mutate(cut_self_injured_1 = case_when(index == 2 & n.cut.self == 1 ~ other.serious.accident.injured1, T ~ as.integer(cut_self_injured_1)),
         cut_self_injured_2 = case_when(index == 2 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_injured_2)),
         cut_self_injured_3 = case_when(index == 2 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_injured_3)))

dx <- dx %>%
  mutate(cut_self_injured_1 = case_when(index == 2 & n.cut.self == 2 ~ other.serious.accident.injured1, T ~ as.integer(cut_self_injured_1)),
         cut_self_injured_2 = case_when(index == 2 & n.cut.self == 2 ~ other.serious.accident.injured2, T ~ as.integer(cut_self_injured_2)),
         cut_self_injured_3 = case_when(index == 2 & n.cut.self == 2 ~ NA_integer_, T ~ as.integer(cut_self_injured_3)))

dx <- dx %>%
  mutate(cut_self_injured_1 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.injured1, T ~ as.integer(cut_self_injured_1)),
         cut_self_injured_2 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.injured2, T ~ as.integer(cut_self_injured_2)),
         cut_self_injured_3 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.injured3, T ~ as.integer(cut_self_injured_3)))

# index = 3
dx <- dx %>%
  mutate(cut_self_injured_1 = case_when(index == 3 & n.cut.self == 1 ~ other.serious.accident.injured2, T ~ as.integer(cut_self_injured_1)),
         cut_self_injured_2 = case_when(index == 3 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_injured_2)),
         cut_self_injured_3 = case_when(index == 3 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_injured_3)))

dx <- dx %>%
  mutate(cut_self_injured_1 = case_when(index == 3 & n.cut.self == 2 ~ other.serious.accident.injured2, T ~ as.integer(cut_self_injured_1)),
         cut_self_injured_2 = case_when(index == 3 & n.cut.self == 2 ~ other.serious.accident.injured3, T ~ as.integer(cut_self_injured_2)),
         cut_self_injured_3 = case_when(index == 3 & n.cut.self == 2 ~ NA_integer_, T ~ as.integer(cut_self_injured_3)))

dx <- dx %>%
  mutate(cut_self_injured_1 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.injured2, T ~ as.integer(cut_self_injured_1)),
         cut_self_injured_2 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.injured3, T ~ as.integer(cut_self_injured_2)),
         cut_self_injured_3 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.injured4, T ~ as.integer(cut_self_injured_3)))

# index = 4
dx <- dx %>%
  mutate(cut_self_injured_1 = case_when(index == 4 & n.cut.self == 1 ~ other.serious.accident.injured3, T ~ as.integer(cut_self_injured_1)),
         cut_self_injured_2 = case_when(index == 4 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_injured_2)),
         cut_self_injured_3 = case_when(index == 4 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_injured_3)))

dx <- dx %>%
  mutate(cut_self_injured_1 = case_when(index == 4 & n.cut.self == 2 ~ other.serious.accident.injured3, T ~ as.integer(cut_self_injured_1)),
         cut_self_injured_2 = case_when(index == 4 & n.cut.self == 2 ~ other.serious.accident.injured4, T ~ as.integer(cut_self_injured_2)),
         cut_self_injured_3 = case_when(index == 4 & n.cut.self == 2 ~ NA_integer_, T ~ as.integer(cut_self_injured_3)))

dx <- dx %>%
  mutate(cut_self_injured_1 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.injured3, T ~ as.integer(cut_self_injured_1)),
         cut_self_injured_2 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.injured4, T ~ as.integer(cut_self_injured_2)),
         cut_self_injured_3 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.injured5, T ~ as.integer(cut_self_injured_3)))

# index = 5
dx <- dx %>%
  mutate(cut_self_injured_1 = case_when(index == 5 & n.cut.self == 1 ~ other.serious.accident.injured4, T ~ as.integer(cut_self_injured_1)),
         cut_self_injured_2 = case_when(index == 5 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_injured_2)),
         cut_self_injured_3 = case_when(index == 5 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_injured_3)))

# index = 6
dx <- dx %>%
  mutate(cut_self_injured_1 = case_when(index == 6 & n.cut.self == 1 ~ other.serious.accident.injured5, T ~ as.integer(cut_self_injured_1)),
         cut_self_injured_2 = case_when(index == 6 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_injured_2)),
         cut_self_injured_3 = case_when(index == 6 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_injured_3)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.injured,
                             other.serious.accident.injured1,
                             other.serious.accident.injured2,
                             other.serious.accident.injured3,
                             other.serious.accident.injured4,
                             other.serious.accident.injured5))

### Days disabled? ----
dx$cut_self_days_disabled_1 <- NA_real_
dx$cut_self_days_disabled_2 <- NA_real_
dx$cut_self_days_disabled_3 <- NA_real_

dx <- relocate(dx, c(cut_self_days_disabled_1, cut_self_days_disabled_2,
                     cut_self_days_disabled_3), .after = cut_self_injured_3)

# index = 1
dx <- dx %>%
  mutate(cut_self_days_disabled_1 = case_when(index == 1 & n.cut.self == 1 ~ other.serious.accident.days.disabled, T ~ as.numeric(cut_self_days_disabled_1)),
         cut_self_days_disabled_2 = case_when(index == 1 & n.cut.self == 1 ~ NA_real_, T ~ as.numeric(cut_self_days_disabled_2)),
         cut_self_days_disabled_3 = case_when(index == 1 & n.cut.self == 1 ~ NA_real_, T ~ as.numeric(cut_self_days_disabled_3)))

dx <- dx %>%
  mutate(cut_self_days_disabled_1 = case_when(index == 1 & n.cut.self == 2 ~ other.serious.accident.days.disabled, T ~ as.numeric(cut_self_days_disabled_1)),
         cut_self_days_disabled_2 = case_when(index == 1 & n.cut.self == 2 ~ other.serious.accident.days.disabled1, T ~ as.numeric(cut_self_days_disabled_2)),
         cut_self_days_disabled_3 = case_when(index == 1 & n.cut.self == 2 ~ NA_real_, T ~ as.numeric(cut_self_days_disabled_3)))

dx <- dx %>%
  mutate(cut_self_days_disabled_1 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.days.disabled, T ~ as.numeric(cut_self_days_disabled_1)),
         cut_self_days_disabled_2 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.days.disabled1, T ~ as.numeric(cut_self_days_disabled_2)),
         cut_self_days_disabled_3 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.days.disabled2, T ~ as.numeric(cut_self_days_disabled_3)))

# index = 2
dx <- dx %>%
  mutate(cut_self_days_disabled_1 = case_when(index == 2 & n.cut.self == 1 ~ other.serious.accident.days.disabled1, T ~ as.numeric(cut_self_days_disabled_1)),
         cut_self_days_disabled_2 = case_when(index == 2 & n.cut.self == 1 ~ NA_real_, T ~ as.numeric(cut_self_days_disabled_2)),
         cut_self_days_disabled_3 = case_when(index == 2 & n.cut.self == 1 ~ NA_real_, T ~ as.numeric(cut_self_days_disabled_3)))

dx <- dx %>%
  mutate(cut_self_days_disabled_1 = case_when(index == 2 & n.cut.self == 2 ~ other.serious.accident.days.disabled1, T ~ as.numeric(cut_self_days_disabled_1)),
         cut_self_days_disabled_2 = case_when(index == 2 & n.cut.self == 2 ~ other.serious.accident.days.disabled2, T ~ as.numeric(cut_self_days_disabled_2)),
         cut_self_days_disabled_3 = case_when(index == 2 & n.cut.self == 2 ~ NA_real_, T ~ as.numeric(cut_self_days_disabled_3)))

dx <- dx %>%
  mutate(cut_self_days_disabled_1 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.days.disabled1, T ~ as.numeric(cut_self_days_disabled_1)),
         cut_self_days_disabled_2 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.days.disabled2, T ~ as.numeric(cut_self_days_disabled_2)),
         cut_self_days_disabled_3 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.days.disabled3, T ~ as.numeric(cut_self_days_disabled_3)))

# index = 3
dx <- dx %>%
  mutate(cut_self_days_disabled_1 = case_when(index == 3 & n.cut.self == 1 ~ other.serious.accident.days.disabled2, T ~ as.numeric(cut_self_days_disabled_1)),
         cut_self_days_disabled_2 = case_when(index == 3 & n.cut.self == 1 ~ NA_real_, T ~ as.numeric(cut_self_days_disabled_2)),
         cut_self_days_disabled_3 = case_when(index == 3 & n.cut.self == 1 ~ NA_real_, T ~ as.numeric(cut_self_days_disabled_3)))

dx <- dx %>%
  mutate(cut_self_days_disabled_1 = case_when(index == 3 & n.cut.self == 2 ~ other.serious.accident.days.disabled2, T ~ as.numeric(cut_self_days_disabled_1)),
         cut_self_days_disabled_2 = case_when(index == 3 & n.cut.self == 2 ~ other.serious.accident.days.disabled3, T ~ as.numeric(cut_self_days_disabled_2)),
         cut_self_days_disabled_3 = case_when(index == 3 & n.cut.self == 2 ~ NA_real_, T ~ as.numeric(cut_self_days_disabled_3)))

dx <- dx %>%
  mutate(cut_self_days_disabled_1 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.days.disabled2, T ~ as.numeric(cut_self_days_disabled_1)),
         cut_self_days_disabled_2 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.days.disabled3, T ~ as.numeric(cut_self_days_disabled_2)),
         cut_self_days_disabled_3 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.days.disabled4, T ~ as.numeric(cut_self_days_disabled_3)))

# index = 4
dx <- dx %>%
  mutate(cut_self_days_disabled_1 = case_when(index == 4 & n.cut.self == 1 ~ other.serious.accident.days.disabled3, T ~ as.numeric(cut_self_days_disabled_1)),
         cut_self_days_disabled_2 = case_when(index == 4 & n.cut.self == 1 ~ NA_real_, T ~ as.numeric(cut_self_days_disabled_2)),
         cut_self_days_disabled_3 = case_when(index == 4 & n.cut.self == 1 ~ NA_real_, T ~ as.numeric(cut_self_days_disabled_3)))

dx <- dx %>%
  mutate(cut_self_days_disabled_1 = case_when(index == 4 & n.cut.self == 2 ~ other.serious.accident.days.disabled3, T ~ as.numeric(cut_self_days_disabled_1)),
         cut_self_days_disabled_2 = case_when(index == 4 & n.cut.self == 2 ~ other.serious.accident.days.disabled4, T ~ as.numeric(cut_self_days_disabled_2)),
         cut_self_days_disabled_3 = case_when(index == 4 & n.cut.self == 2 ~ NA_real_, T ~ as.numeric(cut_self_days_disabled_3)))

dx <- dx %>%
  mutate(cut_self_days_disabled_1 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.days.disabled3, T ~ as.numeric(cut_self_days_disabled_1)),
         cut_self_days_disabled_2 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.days.disabled4, T ~ as.numeric(cut_self_days_disabled_2)),
         cut_self_days_disabled_3 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.days.disabled5, T ~ as.numeric(cut_self_days_disabled_3)))

# index = 5
dx <- dx %>%
  mutate(cut_self_days_disabled_1 = case_when(index == 5 & n.cut.self == 1 ~ other.serious.accident.days.disabled4, T ~ as.numeric(cut_self_days_disabled_1)),
         cut_self_days_disabled_2 = case_when(index == 5 & n.cut.self == 1 ~ NA_real_, T ~ as.numeric(cut_self_days_disabled_2)),
         cut_self_days_disabled_3 = case_when(index == 5 & n.cut.self == 1 ~ NA_real_, T ~ as.numeric(cut_self_days_disabled_3)))

# index = 6
dx <- dx %>%
  mutate(cut_self_days_disabled_1 = case_when(index == 6 & n.cut.self == 1 ~ other.serious.accident.days.disabled5, T ~ as.numeric(cut_self_days_disabled_1)),
         cut_self_days_disabled_2 = case_when(index == 6 & n.cut.self == 1 ~ NA_real_, T ~ as.numeric(cut_self_days_disabled_2)),
         cut_self_days_disabled_3 = case_when(index == 6 & n.cut.self == 1 ~ NA_real_, T ~ as.numeric(cut_self_days_disabled_3)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.days.disabled,
                             other.serious.accident.days.disabled1,
                             other.serious.accident.days.disabled2,
                             other.serious.accident.days.disabled3,
                             other.serious.accident.days.disabled4,
                             other.serious.accident.days.disabled5))

### Almost died? ----
dx$cut_self_almost_died_1 <- NA_integer_
dx$cut_self_almost_died_2 <- NA_integer_
dx$cut_self_almost_died_3 <- NA_integer_

dx <- relocate(dx, c(cut_self_almost_died_1, cut_self_almost_died_2,
                     cut_self_almost_died_3), .after = cut_self_days_disabled_3)

# index = 1
dx <- dx %>%
  mutate(cut_self_almost_died_1 = case_when(index == 1 & n.cut.self == 1 ~ other.serious.accident.almost.died, T ~ as.integer(cut_self_almost_died_1)),
         cut_self_almost_died_2 = case_when(index == 1 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_almost_died_2)),
         cut_self_almost_died_3 = case_when(index == 1 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_almost_died_3)))

dx <- dx %>%
  mutate(cut_self_almost_died_1 = case_when(index == 1 & n.cut.self == 2 ~ other.serious.accident.almost.died, T ~ as.integer(cut_self_almost_died_1)),
         cut_self_almost_died_2 = case_when(index == 1 & n.cut.self == 2 ~ other.serious.accident.almost.died1, T ~ as.integer(cut_self_almost_died_2)),
         cut_self_almost_died_3 = case_when(index == 1 & n.cut.self == 2 ~ NA_integer_, T ~ as.integer(cut_self_almost_died_3)))

dx <- dx %>%
  mutate(cut_self_almost_died_1 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.almost.died, T ~ as.integer(cut_self_almost_died_1)),
         cut_self_almost_died_2 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.almost.died1, T ~ as.integer(cut_self_almost_died_2)),
         cut_self_almost_died_3 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.almost.died2, T ~ as.integer(cut_self_almost_died_3)))

# index = 2
dx <- dx %>%
  mutate(cut_self_almost_died_1 = case_when(index == 2 & n.cut.self == 1 ~ other.serious.accident.almost.died1, T ~ as.integer(cut_self_almost_died_1)),
         cut_self_almost_died_2 = case_when(index == 2 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_almost_died_2)),
         cut_self_almost_died_3 = case_when(index == 2 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_almost_died_3)))

dx <- dx %>%
  mutate(cut_self_almost_died_1 = case_when(index == 2 & n.cut.self == 2 ~ other.serious.accident.almost.died1, T ~ as.integer(cut_self_almost_died_1)),
         cut_self_almost_died_2 = case_when(index == 2 & n.cut.self == 2 ~ other.serious.accident.almost.died2, T ~ as.integer(cut_self_almost_died_2)),
         cut_self_almost_died_3 = case_when(index == 2 & n.cut.self == 2 ~ NA_integer_, T ~ as.integer(cut_self_almost_died_3)))

dx <- dx %>%
  mutate(cut_self_almost_died_1 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.almost.died1, T ~ as.integer(cut_self_almost_died_1)),
         cut_self_almost_died_2 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.almost.died2, T ~ as.integer(cut_self_almost_died_2)),
         cut_self_almost_died_3 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.almost.died3, T ~ as.integer(cut_self_almost_died_3)))

# index = 3
dx <- dx %>%
  mutate(cut_self_almost_died_1 = case_when(index == 3 & n.cut.self == 1 ~ other.serious.accident.almost.died2, T ~ as.integer(cut_self_almost_died_1)),
         cut_self_almost_died_2 = case_when(index == 3 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_almost_died_2)),
         cut_self_almost_died_3 = case_when(index == 3 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_almost_died_3)))

dx <- dx %>%
  mutate(cut_self_almost_died_1 = case_when(index == 3 & n.cut.self == 2 ~ other.serious.accident.almost.died2, T ~ as.integer(cut_self_almost_died_1)),
         cut_self_almost_died_2 = case_when(index == 3 & n.cut.self == 2 ~ other.serious.accident.almost.died3, T ~ as.integer(cut_self_almost_died_2)),
         cut_self_almost_died_3 = case_when(index == 3 & n.cut.self == 2 ~ NA_integer_, T ~ as.integer(cut_self_almost_died_3)))

dx <- dx %>%
  mutate(cut_self_almost_died_1 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.almost.died2, T ~ as.integer(cut_self_almost_died_1)),
         cut_self_almost_died_2 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.almost.died3, T ~ as.integer(cut_self_almost_died_2)),
         cut_self_almost_died_3 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.almost.died4, T ~ as.integer(cut_self_almost_died_3)))

# index = 4
dx <- dx %>%
  mutate(cut_self_almost_died_1 = case_when(index == 4 & n.cut.self == 1 ~ other.serious.accident.almost.died3, T ~ as.integer(cut_self_almost_died_1)),
         cut_self_almost_died_2 = case_when(index == 4 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_almost_died_2)),
         cut_self_almost_died_3 = case_when(index == 4 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_almost_died_3)))

dx <- dx %>%
  mutate(cut_self_almost_died_1 = case_when(index == 4 & n.cut.self == 2 ~ other.serious.accident.almost.died3, T ~ as.integer(cut_self_almost_died_1)),
         cut_self_almost_died_2 = case_when(index == 4 & n.cut.self == 2 ~ other.serious.accident.almost.died4, T ~ as.integer(cut_self_almost_died_2)),
         cut_self_almost_died_3 = case_when(index == 4 & n.cut.self == 2 ~ NA_integer_, T ~ as.integer(cut_self_almost_died_3)))

dx <- dx %>%
  mutate(cut_self_almost_died_1 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.almost.died3, T ~ as.integer(cut_self_almost_died_1)),
         cut_self_almost_died_2 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.almost.died4, T ~ as.integer(cut_self_almost_died_2)),
         cut_self_almost_died_3 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.almost.died5, T ~ as.integer(cut_self_almost_died_3)))

# index = 5
dx <- dx %>%
  mutate(cut_self_almost_died_1 = case_when(index == 5 & n.cut.self == 1 ~ other.serious.accident.almost.died4, T ~ as.integer(cut_self_almost_died_1)),
         cut_self_almost_died_2 = case_when(index == 5 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_almost_died_2)),
         cut_self_almost_died_3 = case_when(index == 5 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_almost_died_3)))

# index = 6
dx <- dx %>%
  mutate(cut_self_almost_died_1 = case_when(index == 6 & n.cut.self == 1 ~ other.serious.accident.almost.died5, T ~ as.integer(cut_self_almost_died_1)),
         cut_self_almost_died_2 = case_when(index == 6 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_almost_died_2)),
         cut_self_almost_died_3 = case_when(index == 6 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_almost_died_3)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.almost.died,
                             other.serious.accident.almost.died1,
                             other.serious.accident.almost.died2,
                             other.serious.accident.almost.died3,
                             other.serious.accident.almost.died4,
                             other.serious.accident.almost.died5))

### Still bothers ----
dx$cut_self_still_bothers_1 <- NA_integer_
dx$cut_self_still_bothers_2 <- NA_integer_
dx$cut_self_still_bothers_3 <- NA_integer_

dx <- relocate(dx, c(cut_self_still_bothers_1, cut_self_still_bothers_2,
                     cut_self_still_bothers_3), .after = cut_self_almost_died_3)

# index = 1
dx <- dx %>%
  mutate(cut_self_still_bothers_1 = case_when(index == 1 & n.cut.self == 1 ~ other.serious.accident.still.bothers, T ~ as.integer(cut_self_still_bothers_1)),
         cut_self_still_bothers_2 = case_when(index == 1 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_still_bothers_2)),
         cut_self_still_bothers_3 = case_when(index == 1 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_still_bothers_3)))

dx <- dx %>%
  mutate(cut_self_still_bothers_1 = case_when(index == 1 & n.cut.self == 2 ~ other.serious.accident.still.bothers, T ~ as.integer(cut_self_still_bothers_1)),
         cut_self_still_bothers_2 = case_when(index == 1 & n.cut.self == 2 ~ other.serious.accident.still.bothers1, T ~ as.integer(cut_self_still_bothers_2)),
         cut_self_still_bothers_3 = case_when(index == 1 & n.cut.self == 2 ~ NA_integer_, T ~ as.integer(cut_self_still_bothers_3)))

dx <- dx %>%
  mutate(cut_self_still_bothers_1 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.still.bothers, T ~ as.integer(cut_self_still_bothers_1)),
         cut_self_still_bothers_2 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.still.bothers1, T ~ as.integer(cut_self_still_bothers_2)),
         cut_self_still_bothers_3 = case_when(index == 1 & n.cut.self == 3 ~ other.serious.accident.still.bothers2, T ~ as.integer(cut_self_still_bothers_3)))

# index = 2
dx <- dx %>%
  mutate(cut_self_still_bothers_1 = case_when(index == 2 & n.cut.self == 1 ~ other.serious.accident.still.bothers1, T ~ as.integer(cut_self_still_bothers_1)),
         cut_self_still_bothers_2 = case_when(index == 2 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_still_bothers_2)),
         cut_self_still_bothers_3 = case_when(index == 2 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_still_bothers_3)))

dx <- dx %>%
  mutate(cut_self_still_bothers_1 = case_when(index == 2 & n.cut.self == 2 ~ other.serious.accident.still.bothers1, T ~ as.integer(cut_self_still_bothers_1)),
         cut_self_still_bothers_2 = case_when(index == 2 & n.cut.self == 2 ~ other.serious.accident.still.bothers2, T ~ as.integer(cut_self_still_bothers_2)),
         cut_self_still_bothers_3 = case_when(index == 2 & n.cut.self == 2 ~ NA_integer_, T ~ as.integer(cut_self_still_bothers_3)))

dx <- dx %>%
  mutate(cut_self_still_bothers_1 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.still.bothers1, T ~ as.integer(cut_self_still_bothers_1)),
         cut_self_still_bothers_2 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.still.bothers2, T ~ as.integer(cut_self_still_bothers_2)),
         cut_self_still_bothers_3 = case_when(index == 2 & n.cut.self == 3 ~ other.serious.accident.still.bothers3, T ~ as.integer(cut_self_still_bothers_3)))

# index = 3
dx <- dx %>%
  mutate(cut_self_still_bothers_1 = case_when(index == 3 & n.cut.self == 1 ~ other.serious.accident.still.bothers2, T ~ as.integer(cut_self_still_bothers_1)),
         cut_self_still_bothers_2 = case_when(index == 3 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_still_bothers_2)),
         cut_self_still_bothers_3 = case_when(index == 3 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_still_bothers_3)))

dx <- dx %>%
  mutate(cut_self_still_bothers_1 = case_when(index == 3 & n.cut.self == 2 ~ other.serious.accident.still.bothers2, T ~ as.integer(cut_self_still_bothers_1)),
         cut_self_still_bothers_2 = case_when(index == 3 & n.cut.self == 2 ~ other.serious.accident.still.bothers3, T ~ as.integer(cut_self_still_bothers_2)),
         cut_self_still_bothers_3 = case_when(index == 3 & n.cut.self == 2 ~ NA_integer_, T ~ as.integer(cut_self_still_bothers_3)))

dx <- dx %>%
  mutate(cut_self_still_bothers_1 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.still.bothers2, T ~ as.integer(cut_self_still_bothers_1)),
         cut_self_still_bothers_2 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.still.bothers3, T ~ as.integer(cut_self_still_bothers_2)),
         cut_self_still_bothers_3 = case_when(index == 3 & n.cut.self == 3 ~ other.serious.accident.still.bothers4, T ~ as.integer(cut_self_still_bothers_3)))

# index = 4
dx <- dx %>%
  mutate(cut_self_still_bothers_1 = case_when(index == 4 & n.cut.self == 1 ~ other.serious.accident.still.bothers3, T ~ as.integer(cut_self_still_bothers_1)),
         cut_self_still_bothers_2 = case_when(index == 4 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_still_bothers_2)),
         cut_self_still_bothers_3 = case_when(index == 4 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_still_bothers_3)))

dx <- dx %>%
  mutate(cut_self_still_bothers_1 = case_when(index == 4 & n.cut.self == 2 ~ other.serious.accident.still.bothers3, T ~ as.integer(cut_self_still_bothers_1)),
         cut_self_still_bothers_2 = case_when(index == 4 & n.cut.self == 2 ~ other.serious.accident.still.bothers4, T ~ as.integer(cut_self_still_bothers_2)),
         cut_self_still_bothers_3 = case_when(index == 4 & n.cut.self == 2 ~ NA_integer_, T ~ as.integer(cut_self_still_bothers_3)))

dx <- dx %>%
  mutate(cut_self_still_bothers_1 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.still.bothers3, T ~ as.integer(cut_self_still_bothers_1)),
         cut_self_still_bothers_2 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.still.bothers4, T ~ as.integer(cut_self_still_bothers_2)),
         cut_self_still_bothers_3 = case_when(index == 4 & n.cut.self == 3 ~ other.serious.accident.still.bothers5, T ~ as.integer(cut_self_still_bothers_3)))

# index = 5
dx <- dx %>%
  mutate(cut_self_still_bothers_1 = case_when(index == 5 & n.cut.self == 1 ~ other.serious.accident.still.bothers4, T ~ as.integer(cut_self_still_bothers_1)),
         cut_self_still_bothers_2 = case_when(index == 5 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_still_bothers_2)),
         cut_self_still_bothers_3 = case_when(index == 5 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_still_bothers_3)))

# index = 6
dx <- dx %>%
  mutate(cut_self_still_bothers_1 = case_when(index == 6 & n.cut.self == 1 ~ other.serious.accident.still.bothers5, T ~ as.integer(cut_self_still_bothers_1)),
         cut_self_still_bothers_2 = case_when(index == 6 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_still_bothers_2)),
         cut_self_still_bothers_3 = case_when(index == 6 & n.cut.self == 1 ~ NA_integer_, T ~ as.integer(cut_self_still_bothers_3)))

# Remove the old columns
dx <- subset(dx, select = -c(other.serious.accident.still.bothers,
                             other.serious.accident.still.bothers1,
                             other.serious.accident.still.bothers2,
                             other.serious.accident.still.bothers3,
                             other.serious.accident.still.bothers4,
                             other.serious.accident.still.bothers5))

dx <- subset(dx, select = -c(TIPO1, TIPO2, TIPO3, TIPO4, TIPO5, TIPO6))

## Get back to original data frame
df <- left_join(df, dx)
df <- relocate(df, c(cut_self_where_hurt_1:cut_self_still_bothers_3), .after = n.cut.self)
df <- subset(df, select = -c(index))




# Creating "Animal_Attack", by merging snake/ray bite and animal attack ----

# Any intervals where both snake/ray bite and animal attack occur?
subset(df, bite.during.interval == 1 & animal.attack.during.interval == 1)
# The two risks never occur in the same interval

# Creating columns for Animal_Attack which is merged snake/ray and animal attack
df$Animal_Attack.during.interval <- df$bite.during.interval
df <- df %>% mutate(Animal_Attack.during.interval =
                      case_when(Animal_Attack.during.interval == 0 ~
                                  animal.attack.during.interval, T ~
                                  Animal_Attack.during.interval))


df$n.Animal_Attack <- df$n.snake.ray.bite
df <- df %>% mutate(n.Animal_Attack =
                      case_when(n.Animal_Attack == 0 ~
                                  n.animal.attack, T ~
                                  n.Animal_Attack))


df$what_Animal_Attacked_you_1 <- df$what_bit_you_snake_ray_1
df <- df %>% mutate(what_Animal_Attacked_you_1 =
                      case_when(what_Animal_Attacked_you_1 == 0 ~
                                  what_attacked_you, T ~
                                  what_Animal_Attacked_you_1))
df$what_Animal_Attacked_you_2 <- df$what_bit_you_snake_ray_2
df$what_Animal_Attacked_you_3 <- df$what_bit_you_snake_ray_3
# Since animal attack never occurred more than once in an interval,
# the second and third column are just snake/ray bite data


df$where_Animal_Attacked_body_1 <- df$where_bit_body_snake_ray_1
df <- df %>% mutate(where_Animal_Attacked_body_1 =
                      case_when(where_Animal_Attacked_body_1 == 0 ~
                                  where_attacked_body, T ~
                                  where_Animal_Attacked_body_1))
df$where_Animal_Attacked_body_2 <- df$where_bit_body_snake_ray_2
df$where_Animal_Attacked_body_3 <- df$where_bit_body_snake_ray_3


df$activity_when_Animal_Attacked_1 <- df$activity_when_bit_snake_ray_1
df <- df %>% mutate(activity_when_Animal_Attacked_1 =
                      case_when(activity_when_Animal_Attacked_1 == 0 ~
                                  activity_when_attacked, T ~
                                  activity_when_Animal_Attacked_1))
df$activity_when_Animal_Attacked_2 <- df$activity_when_bit_snake_ray_2
df$activity_when_Animal_Attacked_3 <- df$activity_when_bit_snake_ray_3


df$days_disabled_Animal_Attacked_1 <- df$days_disabled_snake_ray_1
df <- df %>% mutate(days_disabled_Animal_Attacked_1 =
                      case_when(days_disabled_Animal_Attacked_1 == 0 ~
                                  as.numeric(days_disabled_attack), T ~
                                  days_disabled_Animal_Attacked_1))
df$days_disabled_Animal_Attacked_2 <- df$days_disabled_snake_ray_2
df$days_disabled_Animal_Attacked_3 <- df$days_disabled_snake_ray_3


df$almost_died_Animal_Attacked_1 <- df$almost_died_snake_ray_1
df <- df %>% mutate(almost_died_Animal_Attacked_1 =
                      case_when(almost_died_Animal_Attacked_1 == 0 ~
                                  almost_died_attack, T ~
                                  almost_died_Animal_Attacked_1))
df$almost_died_Animal_Attacked_2 <- df$almost_died_snake_ray_2
df$almost_died_Animal_Attacked_3 <- df$almost_died_snake_ray_3


df$still_bothers_Animal_Attacked_1 <- df$still_bothers_snake_ray_1
df <- df %>% mutate(still_bothers_Animal_Attacked_1 =
                      case_when(still_bothers_Animal_Attacked_1 == 0 ~
                                  still_bothers_attack, T ~
                                  still_bothers_Animal_Attacked_1))
df$still_bothers_Animal_Attacked_2 <- df$still_bothers_snake_ray_2
df$still_bothers_Animal_Attacked_3 <- df$still_bothers_snake_ray_3

# Export as csv -----------------------------------------------------------

write.csv(df, "data_new_format.csv", row.names = F)








