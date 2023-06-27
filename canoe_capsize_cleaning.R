# Libraries
library(tidyverse)
library(readxl)

# Import raw data, this will import sheet "df"
df <- read_xls("threat_wide___sumACEs_for anirudh.xls")

# Create data frame with only the columns we need
df <- df[c("pid", "age", "male", "region", "canoe.capsize.ever", "TIPO1",
           "other.serious.accident.age", "TIPO2",
           "other.serious.accident.age1", "TIPO3",
           "other.serious.accident.age2", "TIPO4",
           "other.serious.accident.age3", "TIPO5",
           "other.serious.accident.age5", "TIPO6",
           "other.serious.accident.age5")]

# Delete rows where no canoe capsize ever occurred
# df <- df[(!df$canoe.capsize.ever == 0), ]

# Creating column counting number of canoe capsizes per person
df$n.canoe.capsize <- rowSums(df == "a", na.rm = T)

# Counting stuff
# plyr::count(df, c("TIPO4")) # There are no canoe capsizes in TIPO4, TIPO5, TIPO6

# Get capsize age 1
df$cc.age1 <- if_else(df$TIPO1 == "a", df$other.serious.accident.age, NA_real_)

# Get capsize age 2
df$cc.age2 <- if_else(df$TIPO2 == "a", df$other.serious.accident.age1, NA_real_)

# Get capsize age 3
df$cc.age3 <- if_else(df$TIPO3 == "a", df$other.serious.accident.age2, NA_real_)

# Create a dataframe with the columns we want
df1 <- df[c("pid", "age", "male", "region", "canoe.capsize.ever", "cc.age1",
            "cc.age2", "cc.age3", "n.canoe.capsize")]

# View any duplicate rows
# View(df1[duplicated(df1$pid), ])
df1[duplicated(df1$pid), ]
# View(df1[duplicated(df1$pid) | duplicated(df1$pid, fromLast = TRUE), ])
df1[duplicated(df1$pid) | duplicated(df1$pid, fromLast = TRUE), ]

# Delete duplicate rows, keeping the latest observation for a person
# df1 <- df1[!duplicated(df$pid), ]
df <- df1 %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()

df1 <- df1 %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()

# View(subset(df, cc.age1 > cc.age2))
# View(subset(df, cc.age2 > cc.age3))
# View(subset(df, cc.age1 > cc.age3))

# Moving values so there is no NA in cc.age1
df1 <- dedupewider::na_move(df1, cols = names(df1)[grepl("^cc.age\\d$",
                                                         names(df1))])
# Trying to check if more than one canoe capsize occurred in one interval
# View(df1[(df1$cc.age1 == df1$cc.age2), ])
df1[(df1$cc.age1 == df1$cc.age2), ] # There is one row where this happened

# Sorting the ages horizontally
df2 <- df1[c("cc.age1", "cc.age2", "cc.age3")]
df2[] <- t(apply(df2, 1, function(x) x[order(x)]))
df1 <- df1[c("pid", "age", "male", "canoe.capsize.ever", "n.canoe.capsize")]
df1 <- cbind(df1, df2)

# Is age = canoe capsize age ever?
age_df <- df1[c("pid", "age")]

df3 <- left_join(df1, age_df)

# View(df3[(df3$age == df3$cc.age1 | df3$age == df3$cc.age2 | df3$age == df3$cc.age3),])
# Just one guy, TTWM

# Final step
df1 <- df1[c("pid", "cc.age1", "cc.age2", "cc.age3")]

# Export as csv
write.csv(df1, "canoe_capsize_cleaned.csv", row.names = F)

# Counting process format -------------------------------------------------

# For splitting, we need ID, age, event, and the corresponding ages
df5 <- df[c("pid", "canoe.capsize.ever", "age", "cc.age1", "cc.age2",
            "cc.age3")]

# Creating the row splits
df6 <- df5 %>%
  mutate(start = 0, end = age) %>%
  dplyr::select(-canoe.capsize.ever) %>%
  gather(canoe.capsize.ever, enter, -pid) %>%
  group_by(pid) %>%
  arrange(pid, enter) %>%
  filter(!is.na(enter)) %>%
  mutate(exit = lead(enter)) %>%
  filter(!is.na(exit), !grepl("time_to_risk_out_start", canoe.capsize.ever)) %>%
  mutate(event = lead(grepl("time_to_event", canoe.capsize.ever), default = 0)) %>%
  dplyr::select(pid, enter, exit, event) %>%
  ungroup()

# Cleaning up
df6 <- subset(df6, enter != exit)

# Adding age, male, and region columns
df7 <- df[c("pid", "age", "male", "region", "cc.age1", "cc.age2", "cc.age3")]
df6 <- left_join(df6, df7)

# If exit > age, delete current row and assign event = 1 to the preceding row
df6 <- df6 %>%
  mutate(event = case_when(lead(exit) > lead(age) ~ 1, TRUE ~ event))

# Remove the rows where exit > age
df6 <- subset(df6, exit <= age)

# If sickness age matches exit, event = 1
df6$event <- ifelse(df6$exit == df6$cc.age1 |
                      df6$exit == df6$cc.age2 |
                      df6$exit == df6$cc.age3, 1, 0)

# Remove NAs
df6$event <- ifelse(is.na(df6$event), 0, df6$event)

# Final Table
df6 <- df6[c("pid", "age", "male", "region", "enter", "exit", "event")]

rm(df2, df3, df5, df7, age_df)

# Two more columns --------------------------------------------------------

# Creating column for time since last fight
df6$time.since.last.canoe.capsize <- ifelse(df6$enter == 0 & df6$exit <= df6$age,
                                            NA_real_, df6$exit - df6$enter)

# Create column for length of prior fight interval
df6$length.of.last.canoe.capsize <- lag(df6$exit) - lag(df6$enter)
df6$length.of.last.canoe.capsize <- ifelse(df6$enter == 0, NA_real_,
                                           df6$length.of.last.canoe.capsize)

# Add columns for other risks ---------------------------------------------

db6 <- read.csv("tree_fall_cleaned.csv")
snake_df3 <- read.csv("snake_ray_bite_cleaned.csv")
sick_df3 <- read.csv("sickness_cleaned.csv")
animal_attack_df3 <- read.csv("animal_attack_cleaned.csv")
fought_df3 <- read.csv("fought_cleaned.csv")
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

## Animal Attack ----
df6 <- left_join(df6, animal_attack_df3)
df6$animal.attack.during.interval <- ifelse((df6$enter < df6$animal.attack.age & df6$animal.attack.age <= df6$exit) |
                                              (df6$enter < df6$animal.attack.age1 & df6$animal.attack.age1 <= df6$exit), 1, 0)
df6$animal.attack.during.interval <- ifelse(is.na(df6$animal.attack.during.interval), 0, df6$animal.attack.during.interval)

## Fight ----
df6 <- left_join(df6, fought_df3)
df6$fought.during.interval <- ifelse((df6$enter < df6$fought.age & df6$fought.age <= df6$exit) |
                                       (df6$enter < df6$fought.age1 & df6$fought.age1 <= df6$exit) |
                                       (df6$enter < df6$fought.age2 & df6$fought.age2 <= df6$exit), 1, 0)
df6$fought.during.interval <- ifelse(is.na(df6$fought.during.interval), 0, df6$fought.during.interval)

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
             "length.of.last.canoe.capsize", "time.since.last.canoe.capsize",
             "tree.fall.during.interval", "bite.during.interval",
             "sickness.during.interval", "animal.attack.during.interval",
             "fought.during.interval", "cut.self.during.interval")]

rm(db6, snake_df3, sick_df3, animal_attack_df3, fought_df3, dc1)


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
df <- left_join(df, df1)
# df$cc.age1 <- ceiling(df$cc.age1)
# df$cc.age2 <- ceiling(df$cc.age2)
# df$cc.age3 <- ceiling(df$cc.age3)

dx <- df[c("pid", "exit", "event", "cc.age1",
           "cc.age2", "cc.age3")]

dy <- dx %>%
  filter(event == 1)

dy <- dy %>%
  rowwise() %>%
  mutate(n.canoe.capsize = sum(c_across(cc.age1:cc.age3) == exit, na.rm = TRUE)) %>%
  ungroup()
dy$n.canoe.capsize <- ifelse(dy$n.canoe.capsize == 0, 1, dy$n.canoe.capsize)

dy <- dy[c("pid", "exit", "n.canoe.capsize")]
df <- left_join(df, dy)
df <- relocate(df, n.canoe.capsize, .after = event)
df$n.canoe.capsize <- ifelse(is.na(df$n.canoe.capsize), 0, df$n.canoe.capsize)

df <- subset(df, select = -c(cc.age1, cc.age2, cc.age3))

# Add household ID column -------------------------------------------------

h_id <- read_xls("add household ids_a.xls")
df <- left_join(df, h_id)
df <- relocate(df, house.id, .after = pid)

rm(df1, df6, dx, dy, h_id)

# Who reported experiencing the risk but did not report any ages? ---------
#
# df %>%
#   filter(canoe.capsize.ever == 1) %>%
#   summarise(count = n_distinct(pid)) # Apparently 99 out of 388 people experienced it
# # But there is a story here...
# trial_df <- df %>%
#   filter(canoe.capsize.ever == 1)
# trial_df1 <- plyr::count(unique(trial_df$pid))
#
# trial_df2 <- df6 %>%
#   filter(event == 1)
# trial_df3 <- plyr::count(unique(trial_df2$pid))
#
# anti_join(trial_df1, trial_df3) # 6 people said they experienced it but did not report any ages.




# Adding columns for cause, etc. ------------------------------------------
# Note that this is all highly context-specific, meaning that each risk has a
# different number of reported ages, which means that the code to add these
# columns is slightly modified for every risk.

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
  filter(event == 1)
dx <- dx %>%
  group_by(pid) %>%
  mutate(index = 1:n())
dx <- relocate(dx, index, .after = event)
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
df <- relocate(df, c(canoe_capsize_where_hurt_1:canoe_capsize_still_bothers_2), .after = time.since.last.canoe.capsize)
df <- subset(df, select = -c(index))


# Export final table to csv -----------------------------------------------

write.csv(df, "canoe_capsize_final_table.csv", row.names = F)




