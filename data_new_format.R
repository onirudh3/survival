
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

# Export as csv -----------------------------------------------------------

write.csv(df, "data_new_format.csv", row.names = F)



# Adding columns for cause, etc. ------------------------------------------
# Note that this is all highly context-specific, meaning that each risk has a
# different number of recorded ages, which means that the code to add these
# columns is slightly modified for every risk

## Snake/Ray Bite ----

raw <- read.csv("raw_data_no_duplicates.csv")

raw <- select(raw, c(7, 16:35))
raw <- subset(raw, select = -c(snake.or.ray.bite.age, snake.or.ray.bite.age1))
dx <- left_join(df, raw)

dx <- dx %>%
  filter(bite.during.interval == 1)
dx <- dx %>%
  group_by(pid) %>%
  mutate(index = 1:n())
dx <- relocate(dx, index, .after = bite.during.interval)


# What bit you, snake or ray? ----

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
  mutate(what_bit_you_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 ~ what.bit.you1, T ~ as.character(what_bit_you_snake_ray_1)),
         what_bit_you_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(what_bit_you_snake_ray_2)),
         what_bit_you_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(what_bit_you_snake_ray_3)))

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


# Where bit body? ----

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
  mutate(where_bit_body_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 ~ where.bit.body1, T ~ as.character(where_bit_body_snake_ray_1)),
         where_bit_body_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(where_bit_body_snake_ray_2)),
         where_bit_body_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(where_bit_body_snake_ray_3)))

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


# Activity when bit? ----

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
  mutate(activity_when_bit_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 ~ activity.when.bit1...25, T ~ as.character(activity_when_bit_snake_ray_1)),
         activity_when_bit_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(activity_when_bit_snake_ray_2)),
         activity_when_bit_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 ~ NA_character_, T ~ as.character(activity_when_bit_snake_ray_3)))

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


# Days disabled ----

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
  mutate(days_disabled_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 ~ days.disabled.bite1, T ~ as.numeric(days_disabled_snake_ray_1)),
         days_disabled_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 ~ NA_real_, T ~ as.numeric(days_disabled_snake_ray_2)),
         days_disabled_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 ~ NA_real_, T ~ as.numeric(days_disabled_snake_ray_3)))

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


# Days disabled ----

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
  mutate(almost_died_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 ~ almost.died.bite1, T ~ as.integer(almost_died_snake_ray_1)),
         almost_died_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 ~ NA_integer_, T ~ as.integer(almost_died_snake_ray_2)),
         almost_died_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 ~ NA_integer_, T ~ as.integer(almost_died_snake_ray_3)))

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


# Still bothers ----

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
  mutate(still_bothers_snake_ray_1 = case_when(index == 2 & n.snake.ray.bite == 1 ~ still.bothers.bite1, T ~ as.integer(still_bothers_snake_ray_1)),
         still_bothers_snake_ray_2 = case_when(index == 2 & n.snake.ray.bite == 1 ~ NA_integer_, T ~ as.integer(still_bothers_snake_ray_2)),
         still_bothers_snake_ray_3 = case_when(index == 2 & n.snake.ray.bite == 1 ~ NA_integer_, T ~ as.integer(still_bothers_snake_ray_3)))

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


# Get back to original dataframe
df <- left_join(df, dx)
df <- relocate(df, c(what_bit_you_snake_ray_1:still_bothers_snake_ray_3), .after = n.snake.ray.bite)
