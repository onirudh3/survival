library(tidyverse)
library(readxl)

# Import raw data, this will import sheet "db"
df <- read_xls("threat_wide___sumACEs_for anirudh.xls")

# Create data frame with only the columns we need
df <- df[c("pid", "age", "male", "region", "animal.attack.ever",
           "animal.attack.age", "animal.attack.age1")]

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

# View(subset(df, animal.attack.age > animal.attack.age1))

# Making sure that the ages are chronological for each observation
df1 <- df[c("animal.attack.age", "animal.attack.age1")]
df1[] <- t(apply(df1, 1, function(x) x[order(x)]))
df2 <- df[c("pid")]
df3 <- cbind(df2, df1)

# Export as csv
write.csv(df3, "animal_attack_cleaned.csv", row.names = F)

# Is age = animal attack age ever?
age_df <- df[c("pid", "age")]

df4 <- left_join(df3, age_df)

# View(df4[(df4$age == df4$animal.attack.age | df4$age == df4$animal.attack.age1),
#          ])
# Yes, for 5 individuals 89RD, RXEC, C9NU, 7QKV, DPC7

# Have you been attacked more than once in the same interval?
# View(df3[(df3$animal.attack.age == df3$animal.attack.age1),])
df3[(df3$animal.attack.age == df3$animal.attack.age1),] # 0 people have

# Counting process format -------------------------------------------------

# For splitting, we need ID, age, event, and the corresponding ages
df5 <- df[c("pid", "animal.attack.ever", "age", "animal.attack.age", "animal.attack.age1")]

# Creating the row splits
df6 <- df5 %>%
  mutate(start = 0, end = age) %>%
  dplyr::select(-animal.attack.ever) %>%
  gather(animal.attack.ever, enter, -pid) %>%
  group_by(pid) %>%
  arrange(pid, enter) %>%
  filter(!is.na(enter)) %>%
  mutate(exit = lead(enter)) %>%
  filter(!is.na(exit), !grepl("time_to_risk_out_start", animal.attack.ever)) %>%
  mutate(event = lead(grepl("time_to_event", animal.attack.ever), default = 0)) %>%
  dplyr::select(pid, enter, exit, event) %>%
  ungroup()

# Cleaning up
df6 <- subset(df6, enter != exit)

# Adding age, male, and region columns
df7 <- df[c("pid", "age", "male", "region", "animal.attack.age",
            "animal.attack.age1")]
df6 <- left_join(df6, df7)

# If exit > age, delete current row and assign event = 1 to the preceding row
df6 <- df6 %>%
  mutate(event = case_when(lead(exit) > lead(age) ~ 1, TRUE ~ event))

# Remove the rows where exit > age
df6 <- subset(df6, exit <= age)

# If sickness age matches exit, event = 1
df6$event <- ifelse(df6$exit == df6$animal.attack.age |
                      df6$exit == df6$animal.attack.age1, 1, 0)

# Remove NAs
df6$event <- ifelse(is.na(df6$event), 0, df6$event)

# Final Table
df6 <- df6[c("pid", "age", "male", "region", "enter", "exit", "event")]

rm(df1, df2, df4, df5, df7, age_df)

# Two more columns --------------------------------------------------------

# Creating column for time since last fight
df6$time.since.last.animal.attack <- ifelse(df6$enter == 0 & df6$exit <= df6$age,
                                    NA_real_, df6$exit - df6$enter)

# Create column for length of prior fight interval
df6$length.of.last.animal.attack <- lag(df6$exit) - lag(df6$enter)
df6$length.of.last.animal.attack <- ifelse(df6$enter == 0, NA_real_,
                                   df6$length.of.last.animal.attack)

# Add columns for other risks ---------------------------------------------

db6 <- read.csv("tree_fall_cleaned.csv")
snake_df3 <- read.csv("snake_ray_bite_cleaned.csv")
sick_df3 <- read.csv("sickness_cleaned.csv")
fought_df3 <- read.csv("fought_cleaned.csv")
ds1 <- read.csv("canoe_capsize_cleaned.csv")
dc1 <- read.csv("cut_self_cleaned.csv")

## Tree Fall ----
df6 <- left_join(df6, db6)
df6$tree.fall.during.interval <- ifelse((df6$enter < df6$tf.age1 & df6$tf.age1 <= df6$exit) |
                                          (df6$enter < df6$tf.age2 & df6$tf.age2 <= df6$exit) |
                                          (df6$enter < df6$tf.age3 & df6$tf.age3 <= df6$exit), 1, 0)
df6$tree.fall.during.interval <- ifelse(is.na(df6$tree.fall.during.interval), 0, df6$tree.fall.during.interval)

## Snake/Ray Bite ----
df6 <- left_join(df6, snake_df3)
df6$bite.during.interval <- ifelse((df6$enter < df6$snake.or.ray.bite.age & df6$snake.or.ray.bite.age <= df6$exit) |
                                     (df6$enter < df6$snake.or.ray.bite.age1 & df6$snake.or.ray.bite.age1 <= df6$exit) |
                                     (df6$enter < df6$snake.or.ray.bite.age2 & df6$snake.or.ray.bite.age2 <= df6$exit), 1, 0)
df6$bite.during.interval <- ifelse(is.na(df6$bite.during.interval), 0, df6$bite.during.interval)

## Sickness ----
df6 <- left_join(df6, sick_df3)
df6$sickness.during.interval <- ifelse((df6$enter < df6$sickness.age & df6$sickness.age <= df6$exit) |
                                         (df6$enter < df6$sickness.age1 & df6$sickness.age1 <= df6$exit) |
                                         (df6$enter < df6$sickness.age2 & df6$sickness.age2 <= df6$exit), 1, 0)
df6$sickness.during.interval <- ifelse(is.na(df6$sickness.during.interval), 0, df6$sickness.during.interval)

## Fight ----
df6 <- left_join(df6, fought_df3)
df6$fought.during.interval <- ifelse((df6$enter < df6$fought.age & df6$fought.age <= df6$exit) |
                                       (df6$enter < df6$fought.age1 & df6$fought.age1 <= df6$exit) |
                                       (df6$enter < df6$fought.age2 & df6$fought.age2 <= df6$exit), 1, 0)
df6$fought.during.interval <- ifelse(is.na(df6$fought.during.interval), 0, df6$fought.during.interval)

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
             "length.of.last.animal.attack", "time.since.last.animal.attack",
             "tree.fall.during.interval", "bite.during.interval",
             "sickness.during.interval", "fought.during.interval",
             "canoe.capsize.during.interval", "cut.self.during.interval")]

rm(db6, snake_df3, sick_df3, fought_df3, ds1, dc1)

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

## Snake/Ray Bite ----
df6$bite.co_occurrence <- ifelse(df6$event == 1 & df6$bite.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of snake/ray bite
df6 <- df6 %>%
  mutate(bite.co_occurrence.interval = case_when(bite.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                 bite.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                 bite.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                 bite.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                 bite.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                 bite.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                 bite.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                 bite.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))

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
df$animal.attack.age <- ceiling(df$animal.attack.age)
df$animal.attack.age1 <- ceiling(df$animal.attack.age1)

dx <- df[c("pid", "exit", "event", "animal.attack.age",
           "animal.attack.age1")]

dy <- dx %>%
  filter(event == 1)

dy <- dy %>%
  rowwise() %>%
  mutate(n.animal.attack = sum(c_across(animal.attack.age:animal.attack.age1) == exit, na.rm = TRUE)) %>%
  ungroup()
dy$n.animal.attack <- ifelse(dy$n.animal.attack == 0, 1, dy$n.animal.attack)

dy <- dy[c("pid", "exit", "n.animal.attack")]
df <- left_join(df, dy)
df <- relocate(df, n.animal.attack, .after = event)
df$n.animal.attack <- ifelse(is.na(df$n.animal.attack), 0, df$n.animal.attack)

df <- subset(df, select = -c(animal.attack.age, animal.attack.age1))


# Add household ID column -------------------------------------------------

h_id <- read_xls("add household ids_a.xls")
df <- left_join(df, h_id)
df <- relocate(df, house.id, .after = pid)


rm(df3, df6, dx, dy, h_id)


# Adding columns for cause, etc. ------------------------------------------
# Note that this is all highly context-specific, meaning that each risk has a
# different number of reported ages, which means that the code to add these
# columns is slightly modified for every risk.

## Animal Attack ----

raw <- read.csv("raw_data_no_duplicates.csv")

raw <- select(raw, c(7, 39:50))
raw <- subset(raw, select = -c(animal.attack.age))
dx <- left_join(df, raw)

dx <- dx %>%
  filter(event == 1)
dx <- dx %>%
  group_by(pid) %>%
  mutate(index = 1:n())
dx <- relocate(dx, index, .after = event)

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
df <- relocate(df, c(what_attacked_you:still_bothers_attack), .after = time.since.last.animal.attack)
df <- subset(df, select = -c(index))

# Export final table to csv -----------------------------------------------
write.csv(df, "animal_attack_final_table.csv", row.names = F)
