library(tidyverse)
library(readxl)

# Import raw data, this will import sheet "db"
df <- read_xls("threat_wide___sumACEs_for anirudh.xls")

# Create data frame with only the columns we need
df <- df[c("pid", "age", "male", "region", "snake.or.ray.bite.ever",
           "snake.or.ray.bite.age", "snake.or.ray.bite.age1",
           "snake.or.ray.bite.age2")]

# View any duplicate rows
# View(df[duplicated(df$pid), ])
df[duplicated(df$pid), ]
# View(df[duplicated(df$pid) | duplicated(df$pid, fromLast = TRUE), ])
df[duplicated(df$pid) | duplicated(df$pid, fromLast = TRUE), ]

# Delete duplicate rows, keeping the latest observation for a person
# db1 <- db1[!duplicated(db$pid), ]
df <- df %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()

# View(subset(df, snake.or.ray.bite.age > snake.or.ray.bite.age1))
# View(subset(df, snake.or.ray.bite.age1 > snake.or.ray.bite.age2))

# Making sure that the ages are chronological for each observation
df1 <- df[c("snake.or.ray.bite.age", "snake.or.ray.bite.age1",
            "snake.or.ray.bite.age2")]
df1[] <- t(apply(df1, 1, function(x) x[order(x)]))
df2 <- df[c("pid")]
df3 <- cbind(df2, df1)

# Export as csv
write.csv(df3, "snake_ray_bite_cleaned.csv", row.names = F)

# Have you been bit more than once in the same interval?
# View(df3[(df3$snake.or.ray.bite.age == df3$snake.or.ray.bite.age1),])
df3[(df3$snake.or.ray.bite.age == df3$snake.or.ray.bite.age1),] # 9 people have

# Is age = snake/ray bite age ever?
age_df <- df[c("pid", "age")]

df4 <- left_join(df3, age_df)

# View(df4[(df4$age == df4$snake.or.ray.bite.age |
#                   df4$age == df4$snake.or.ray.bite.age1 |
#                   df4$age == df4$snake.or.ray.bite.age2),])
# Yes, for 6 individuals RPWM, YWP3, U92T, V9Q4, FVQR, 8FLL


# Counting process format -------------------------------------------------

# For splitting, we need ID, age, event, and the corresponding ages
df5 <- df[c("pid", "snake.or.ray.bite.ever", "age", "snake.or.ray.bite.age",
            "snake.or.ray.bite.age1", "snake.or.ray.bite.age2")]

# Creating the row splits
df6 <- df5 %>%
  mutate(start = 0, end = age) %>%
  dplyr::select(-snake.or.ray.bite.ever) %>%
  gather(snake.or.ray.bite.ever, enter, -pid) %>%
  group_by(pid) %>%
  arrange(pid, enter) %>%
  filter(!is.na(enter)) %>%
  mutate(exit = lead(enter)) %>%
  filter(!is.na(exit), !grepl("time_to_risk_out_start", snake.or.ray.bite.ever)) %>%
  mutate(event = lead(grepl("time_to_event", snake.or.ray.bite.ever), default = 0)) %>%
  dplyr::select(pid, enter, exit, event) %>%
  ungroup()

# Cleaning up
df6 <- subset(df6, enter != exit)

# Adding age, male, and region columns
df7 <- df[c("pid", "age", "male", "region", "snake.or.ray.bite.age",
            "snake.or.ray.bite.age1", "snake.or.ray.bite.age2")]
df6 <- left_join(df6, df7)

# If exit > age, delete current row and assign event = 1 to the preceding row
df6 <- df6 %>%
  mutate(event = case_when(lead(exit) > lead(age) ~ 1, TRUE ~ event))

# Remove the rows where exit > age
df6 <- subset(df6, exit <= age)

# If sickness age matches exit, event = 1
df6$event <- ifelse(df6$exit == df6$snake.or.ray.bite.age |
                      df6$exit == df6$snake.or.ray.bite.age1 |
                      df6$exit == df6$snake.or.ray.bite.age2, 1, 0)

# Remove NAs
df6$event <- ifelse(is.na(df6$event), 0, df6$event)

# Final Table
df6 <- df6[c("pid", "age", "male", "region", "enter", "exit", "event")]

rm(df1, df2, df4, df5, df7, age_df)

# Two more columns --------------------------------------------------------

# Creating column for time since last snake/ray bite
df6$time.since.last.bite <- ifelse(df6$enter == 0 & df6$exit <= df6$age,
                                    NA_real_, df6$exit - df6$enter)

# Create column for length of prior snake/ray bite interval
df6$length.of.last.bite <- lag(df6$exit) - lag(df6$enter)
df6$length.of.last.bite <- ifelse(df6$enter == 0, NA_real_,
                                   df6$length.of.last.bite)

# Add columns for other risks ---------------------------------------------

db6 <- read.csv("tree_fall_cleaned.csv")
fought_df3 <- read.csv("fought_cleaned.csv")
sick_df3 <- read.csv("sickness_cleaned.csv")
animal_attack_df3 <- read.csv("animal_attack_cleaned.csv")
ds1 <- read.csv("canoe_capsize_cleaned.csv")
dc1 <- read.csv("cut_self_cleaned.csv")

## Tree Fall ----
df6 <- left_join(df6, db6)
df6$tree.fall.during.interval <- ifelse((df6$enter < df6$tf.age1 & df6$tf.age1 <= df6$exit) |
                                          (df6$enter < df6$tf.age2 & df6$tf.age2 <= df6$exit) |
                                          (df6$enter < df6$tf.age3 & df6$tf.age3 <= df6$exit), 1, 0)
df6$tree.fall.during.interval <- ifelse(is.na(df6$tree.fall.during.interval), 0, df6$tree.fall.during.interval)

## Fight ----
df6 <- left_join(df6, fought_df3)
df6$fought.during.interval <- ifelse((df6$enter < df6$fought.age & df6$fought.age <= df6$exit) |
                                       (df6$enter < df6$fought.age1 & df6$fought.age1 <= df6$exit) |
                                       (df6$enter < df6$fought.age2 & df6$fought.age2 <= df6$exit), 1, 0)
df6$fought.during.interval <- ifelse(is.na(df6$fought.during.interval), 0, df6$fought.during.interval)

## Sickness ----
df6 <- left_join(df6, sick_df3)
df6$sickness.during.interval <- ifelse((df6$enter < df6$sickness.age & df6$sickness.age <= df6$exit) |
                                         (df6$enter < df6$sickness.age1 & df6$sickness.age1 <= df6$exit) |
                                         (df6$enter < df6$sickness.age2 & df6$sickness.age2 <= df6$exit), 1, 0)
df6$sickness.during.interval <- ifelse(is.na(df6$sickness.during.interval), 0, df6$sickness.during.interval)

## Animal Attack ----
df6 <- left_join(df6, animal_attack_df3)
df6$animal.attack.during.interval <- ifelse((df6$enter < df6$animal.attack.age & df6$animal.attack.age <= df6$exit) |
                                              (df6$enter < df6$animal.attack.age1 & df6$animal.attack.age1 <= df6$exit), 1, 0)
df6$animal.attack.during.interval <- ifelse(is.na(df6$animal.attack.during.interval), 0, df6$animal.attack.during.interval)

## Canoe Capsize ----
df6 <- left_join(df6, ds1)
df6$canoe.capsize.during.interval <- ifelse((df6$enter < df6$cc.age1 & df6$cc.age1 <= df6$exit) |
                                              (df6$enter < df6$cc.age2 & df6$cc.age2 <= df6$exit) |
                                              (df6$enter < df6$cc.age3 & df6$cc.age3 <= df6$exit), 1, 0)
df6$canoe.capsize.during.interval <- ifelse(is.na(df6$canoe.capsize.during.interval), 0, df6$canoe.capsize.during.interval)

## Cut Self ----
df6 <- left_join(df6, dc1)
df6$cut.self.during.interval <- ifelse((df6$enter < df6$cut.age1 & df6$cut.age1 <= df6$exit) |
                                         (df6$enter < df6$cut.age2 & df6$cut.age2 <= df6$exit) |
                                         (df6$enter < df6$cut.age3 & df6$cut.age3 <= df6$exit) |
                                         (df6$enter < df6$cut.age4 & df6$cut.age4 <= df6$exit) |
                                         (df6$enter < df6$cut.age5 & df6$cut.age5 <= df6$exit) |
                                         (df6$enter < df6$cut.age6 & df6$cut.age6 <= df6$exit), 1, 0)
df6$cut.self.during.interval <- ifelse(is.na(df6$cut.self.during.interval), 0, df6$cut.self.during.interval)

# Final Table
df6 <- df6[c("pid", "age", "male", "region", "enter", "exit", "event",
             "length.of.last.bite", "time.since.last.bite",
             "tree.fall.during.interval", "fought.during.interval",
             "sickness.during.interval", "animal.attack.during.interval",
             "canoe.capsize.during.interval", "cut.self.during.interval")]

rm(db6, fought_df3, sick_df3, animal_attack_df3, ds1, dc1)

# Adding columns for risk co-occurrences ----------------------------------

## Tree Fall ----
df6$tree.fall.co_occurrence <- ifelse(df6$event == 1 & df6$tree.fall.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of tree fall
df6 <- df6 %>%
  mutate(tree.fall.co_occurrence.interval = case_when(tree.fall.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                      tree.fall.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                      tree.fall.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                      tree.fall.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                      tree.fall.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                      tree.fall.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                      tree.fall.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                      tree.fall.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))

## Fight ----

df6$fought.co_occurrence <- ifelse(df6$event == 1 & df6$fought.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of fight
df6 <- df6 %>%
  mutate(fought.co_occurrence.interval = case_when(fought.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                   fought.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                   fought.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                   fought.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                   fought.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                   fought.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                   fought.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                   fought.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))

## Sickness ----

df6$sickness.co_occurrence <- ifelse(df6$event == 1 & df6$sickness.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of fight
df6 <- df6 %>%
  mutate(sickness.co_occurrence.interval = case_when(sickness.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))

## Animal Attack ----

df6$animal.attack.co_occurrence <- ifelse(df6$event == 1 & df6$animal.attack.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of animal.attack
df6 <- df6 %>%
  mutate(animal.attack.co_occurrence.interval = case_when(animal.attack.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))

## Canoe Capsize ----

df6$canoe.capsize.co_occurrence <- ifelse(df6$event == 1 & df6$canoe.capsize.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of canoe.capsize
df6 <- df6 %>%
  mutate(canoe.capsize.co_occurrence.interval = case_when(canoe.capsize.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))

## Cut Self ----

df6$cut.self.co_occurrence <- ifelse(df6$event == 1 & df6$cut.self.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of cut.self
df6 <- df6 %>%
  mutate(cut.self.co_occurrence.interval = case_when(cut.self.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))



# Add column for number of occurrences per interval -----------------------
df <- df6
df <- left_join(df, df3)
df$snake.or.ray.bite.age <- ceiling(df$snake.or.ray.bite.age)
df$snake.or.ray.bite.age1 <- ceiling(df$snake.or.ray.bite.age1)
df$snake.or.ray.bite.age2 <- ceiling(df$snake.or.ray.bite.age2)

dx <- df[c("pid", "exit", "event", "snake.or.ray.bite.age",
           "snake.or.ray.bite.age1", "snake.or.ray.bite.age2")]

dy <- dx %>%
  filter(event == 1)

dy <- dy %>%
  rowwise() %>%
  mutate(n.snake.ray.bite = sum(c_across(snake.or.ray.bite.age:snake.or.ray.bite.age2) == exit, na.rm = TRUE)) %>%
  ungroup()
dy$n.snake.ray.bite <- ifelse(dy$n.snake.ray.bite == 0, 1, dy$n.snake.ray.bite)

dy <- dy[c("pid", "exit", "n.snake.ray.bite")]
df <- left_join(df, dy)
df <- relocate(df, n.snake.ray.bite, .after = event)
df$n.snake.ray.bite <- ifelse(is.na(df$n.snake.ray.bite), 0, df$n.snake.ray.bite)

df <- subset(df, select = -c(snake.or.ray.bite.age, snake.or.ray.bite.age1, snake.or.ray.bite.age2))



# Add household ID column -------------------------------------------------

h_id <- read_xls("add household ids_a.xls")
df <- left_join(df, h_id)
df <- relocate(df, house.id, .after = pid)


rm(df3, df6, dx, dy, h_id)

# Adding columns for cause, etc. ------------------------------------------
# Note that this is all highly context-specific, meaning that each risk has a
# different number of recorded ages, which means that the code to add these
# columns is slightly modified for every risk

raw <- read.csv("raw_data_no_duplicates.csv")

raw <- select(raw, c(7, 16:35))
raw <- subset(raw, select = -c(snake.or.ray.bite.age, snake.or.ray.bite.age1))
dx <- left_join(df, raw)
dx <- subset(dx, select = -c(house.id, age, male, region, length.of.last.bite:cut.self.co_occurrence.interval))

dx <- dx %>%
  filter(event == 1)
dx <- dx %>%
  group_by(pid) %>%
  mutate(index = 1:n())
dx <- relocate(dx, index, .after = event)



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


## Get back to original dataframe
df <- left_join(df, dx)
df <- relocate(df, c(what_bit_you_snake_ray_1:still_bothers_snake_ray_3), .after = time.since.last.bite)
df <- subset(df, select = -c(index))

# Export final table to csv -----------------------------------------------

write.csv(df, "snake_ray_bite_final_table.csv", row.names = F)

