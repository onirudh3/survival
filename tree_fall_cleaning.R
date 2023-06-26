# Libraries
library(tidyverse)
library(readxl)

# Import raw data, this will import sheet "df"
df <- read_xls("threat_wide___sumACEs_for anirudh.xls")

# Create data frame with only the columns we need
df <- df[c("pid", "age", "male", "region", "tree.fall.ever", "TIPO1",
           "other.serious.accident.age", "TIPO2",
           "other.serious.accident.age1", "TIPO3",
           "other.serious.accident.age2", "TIPO4",
           "other.serious.accident.age3", "TIPO5",
           "other.serious.accident.age5", "TIPO6",
           "other.serious.accident.age5")]

# Delete rows where no tree fall ever occurred
# df <- df[(!df$tree.fall.ever == 0), ]

# Counting stuff
# plyr::count(df, c("TIPO4")) # There are no tree falls in TIPO4, TIPO5, TIPO6

# Get capsize age 1
df$tf.age1 <- if_else(df$TIPO1 == "b", df$other.serious.accident.age, NA_real_)

# Get capsize age 2
df$tf.age2 <- if_else(df$TIPO2 == "b", df$other.serious.accident.age1, NA_real_)

# Get capsize age 3
df$tf.age3 <- if_else(df$TIPO3 == "b", df$other.serious.accident.age2, NA_real_)

# Create a dataframe with the columns we want
df1 <- df[c("pid", "age", "male", "region", "tree.fall.ever", "tf.age1",
            "tf.age2", "tf.age3")]

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

# View(subset(df1, tf.age2 > tf.age3))

# Moving values so there is no NA in tf.age1
df1 <- dedupewider::na_move(df1, cols = names(df1)[grepl("^tf.age\\d$",
                                                         names(df1))])
# Trying to check if more than one tree fall occurred in one interval
# View(df1[(df1$tf.age1 == df1$tf.age2), ])
df1[(df1$tf.age1 == df1$tf.age2), ] # There is one row where this happened

# Sorting the ages horizontally
df2 <- df1[c("tf.age1", "tf.age2", "tf.age3")]
df2[] <- t(apply(df2, 1, function(x) x[order(x)]))
df1 <- df1[c("pid", "age", "male", "tree.fall.ever")]
df1 <- cbind(df1, df2)

# Is age = tree fall age ever?
age_df <- df1[c("pid", "age")]

df3 <- left_join(df1, age_df)

# View(df3[(df3$age == df3$tf.age1 | df3$age == df3$tf.age2 | df3$age == df3$tf.age3),])
# Just one guy, TTWM

# Final step
df1 <- df1[c("pid", "tf.age1", "tf.age2", "tf.age3")]

# Export as csv
write.csv(df1, "tree_fall_cleaned.csv", row.names = F)
write.csv(df, "tree_fall_raw_data_no_duplicates.csv", row.names = F)

# Counting process format -------------------------------------------------

# For splitting, we need ID, age, event, and the corresponding ages
df5 <- df[c("pid", "tree.fall.ever", "age", "tf.age1", "tf.age2",
            "tf.age3")]

# Creating the row splits
df6 <- df5 %>%
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
df6 <- subset(df6, enter != exit)

# Adding age, male, and region columns
df7 <- df[c("pid", "age", "male", "region", "tf.age1", "tf.age2", "tf.age3")]
df6 <- left_join(df6, df7)

# If exit > age, delete current row and assign event = 1 to the preceding row
df6 <- df6 %>%
  mutate(event = case_when(lead(exit) > lead(age) ~ 1, TRUE ~ event))

# Remove the rows where exit > age
df6 <- subset(df6, exit <= age)

# If tree fall age matches exit, event = 1
df6$event <- ifelse(df6$exit == df6$tf.age1 |
                      df6$exit == df6$tf.age2 |
                      df6$exit == df6$tf.age3, 1, 0)

# Remove NAs
df6$event <- ifelse(is.na(df6$event), 0, df6$event)

# Final Table
df6 <- df6[c("pid", "age", "male", "region", "enter", "exit", "event")]

rm(df2, df3, df5, df7, age_df)

# Two more columns --------------------------------------------------------

# Creating column for time since last tree fall
df6$time.since.last.tree.fall <- ifelse(df6$enter == 0 & df6$exit <= df6$age,
                                            NA_real_, df6$exit - df6$enter)

# Create column for length of prior fight interval
df6$length.of.last.tree.fall <- lag(df6$exit) - lag(df6$enter)
df6$length.of.last.tree.fall <- ifelse(df6$enter == 0, NA_real_,
                                           df6$length.of.last.tree.fall)

# Add columns for other risks ---------------------------------------------

ds1 <- read.csv("canoe_capsize_cleaned.csv")
snake_df3 <- read.csv("snake_ray_bite_cleaned.csv")
sick_df3 <- read.csv("sickness_cleaned.csv")
animal_attack_df3 <- read.csv("animal_attack_cleaned.csv")
fought_df3 <- read.csv("fought_cleaned.csv")
dc1 <- read.csv("cut_self_cleaned.csv")

## Canoe Capsize ----
df6 <- left_join(df6, ds1)
df6$canoe.capsize.during.interval <- ifelse((df6$enter < df6$cc.age1 & df6$cc.age1 <= df6$exit) |
                                              (df6$enter < df6$cc.age2 & df6$cc.age2 <= df6$exit) |
                                              (df6$enter < df6$cc.age3 & df6$cc.age3 <= df6$exit), 1, 0)
df6$canoe.capsize.during.interval <- ifelse(is.na(df6$canoe.capsize.during.interval), 0, df6$canoe.capsize.during.interval)

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
             "length.of.last.tree.fall", "time.since.last.tree.fall",
             "canoe.capsize.during.interval", "bite.during.interval",
             "sickness.during.interval", "animal.attack.during.interval",
             "fought.during.interval", "cut.self.during.interval")]

rm(ds1, snake_df3, sick_df3, animal_attack_df3, fought_df3, dc1)


# Adding columns for risk co-occurrences ----------------------------------

## Canoe Capsize ----
df6$canoe.capsize.co_occurrence <- ifelse(df6$event == 1 & df6$canoe.capsize.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of Canoe capsize
df6 <- df6 %>%
  mutate(canoe.capsize.co_occurrence.interval = case_when(canoe.capsize.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))

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
df$tf.age1 <- ceiling(df$tf.age1)
df$tf.age2 <- ceiling(df$tf.age2)
df$tf.age3 <- ceiling(df$tf.age3)

dx <- df[c("pid", "exit", "event", "tf.age1", "tf.age2", "tf.age3")]

dy <- dx %>%
  filter(event == 1)

dy <- dy %>%
  rowwise() %>%
  mutate(n.tree.fall = sum(c_across(tf.age1:tf.age3) == exit, na.rm = TRUE)) %>%
  ungroup()
dy$n.tree.fall <- ifelse(dy$n.tree.fall == 0, 1, dy$n.tree.fall)

dy <- dy[c("pid", "exit", "n.tree.fall")]
df <- left_join(df, dy)
df <- relocate(df, n.tree.fall, .after = event)
df$n.tree.fall <- ifelse(is.na(df$n.tree.fall), 0, df$n.tree.fall)

df <- subset(df, select = -c(tf.age1, tf.age2, tf.age3))


# Add household ID column -------------------------------------------------

h_id <- read_xls("add household ids_a.xls")
df <- left_join(df, h_id)
df <- relocate(df, house.id, .after = pid)


# Export final table to csv -----------------------------------------------
write.csv(df, "tree_fall_final_table.csv", row.names = F)

# Who reported experiencing the risk but did not report any ages? ---------

# df %>%
#   filter(tree.fall.ever == 1) %>%
#   summarise(count = n_distinct(pid)) # Apparently 99 out of 388 people experienced it
# # But there is a story here...
# trial_df <- df %>%
#   filter(tree.fall.ever == 1)
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
raw <- select(raw, c(7, TIPO1:other.serious.accident.still.bothers5))
raw <- subset(raw, select = -c(other.serious.accident.age, other.serious.accident.age1,
                               other.serious.accident.age2, other.serious.accident.age3,
                               other.serious.accident.age4))
dx <- left_join(df, raw)

dx <- dx %>%
  filter(event == 1)
dx <- dx %>%
  group_by(pid) %>%
  mutate(index = 1:n())
dx <- relocate(dx, index, .after = event)

# Tree fall occurs maximum thrice in an interval
# plyr::count(raw$TIPO1)
# plyr::count(raw$TIPO2)
# plyr::count(raw$TIPO3)
# plyr::count(raw$TIPO4)
# plyr::count(raw$TIPO5)
# plyr::count(raw$TIPO6)

### Where hurt? ----

