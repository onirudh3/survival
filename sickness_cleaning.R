
# Cleaning Sickness -------------------------------------------------------

library(tidyverse)
library(readxl)

# Import raw data, this will import sheet "db"
df <- read_xls("threat_wide___sumACEs_for anirudh.xls")

# Create data frame with only the columns we need
df <- df[c("pid", "age", "male", "region", "sickness", "sickness.age",
           "sickness.age1", "sickness.age2")]

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

# ref_df <- df %>%
#   group_by(pid) %>%
#   filter(age == max(age)) %>%
#   ungroup()
# ref_df <- subset(ref_df, select = -c(sickness))

# View(subset(df, sickness.age > sickness.age1))

# Making sure that the ages are chronological for each observation
df1 <- df[c("sickness.age", "sickness.age1", "sickness.age2")]
df1[] <- t(apply(df1, 1, function(x) x[order(x)]))
df2 <- df[c("pid")]
df3 <- cbind(df2, df1)

# Export as csv
write.csv(df3, "sickness_cleaned.csv", row.names = F)

# Have you been sick more than once in the same interval?
# View(df3[(df3$sickness.age == df3$sickness.age1),])
df3[(df3$sickness.age == df3$sickness.age1),] # 17 people have

# Is age = sickness age ever?
age_df <- df[c("pid", "age")]
df4 <- left_join(df3, age_df)
# View(df4[(df4$age == df4$sickness.age |
#                           df4$age == df4$sickness.age1 |
#                           df4$age == df4$sickness.age2),])
# Yes, for 31 individuals!


# Counting Process Format for Sickness ------------------------------------

# For splitting, we need ID, age, event, and the corresponding ages
df5 <- df[c("pid", "sickness", "age", "sickness.age", "sickness.age1",
                     "sickness.age2")]


# Creating the row splits
df6 <- df5 %>%
  mutate(start = 0, end = age) %>%
  dplyr::select(-sickness) %>%
  gather(sickness, enter, -pid) %>%
  group_by(pid) %>%
  arrange(pid, enter) %>%
  filter(!is.na(enter)) %>%
  mutate(exit = lead(enter)) %>%
  filter(!is.na(exit), !grepl("time_to_risk_out_start", sickness)) %>%
  mutate(event = lead(grepl("time_to_event", sickness), default = 0)) %>%
  dplyr::select(pid, enter, exit, event) %>%
  ungroup()

# Cleaning up
df6 <- subset(df6, enter != exit)

# Adding age, male, and region columns
df7 <- df[c("pid", "age", "male", "region", "sickness.age", "sickness.age1",
            "sickness.age2")]
df6 <- left_join(df6, df7)

# If exit > age, delete current row and assign event = 1 to the preceding row
df6 <- df6 %>%
  mutate(event = case_when(lead(exit) > lead(age) ~ 1, TRUE ~ event))

# Remove the rows where exit > age
df6 <- subset(df6, exit <= age)

# If sickness age matches exit, event = 1
df6$event <- ifelse(df6$exit == df6$sickness.age |
                      df6$exit == df6$sickness.age1 |
                      df6$exit == df6$sickness.age2, 1, 0)

# Remove NAs
df6$event <- ifelse(is.na(df6$event), 0, df6$event)

# Final Table
df6 <- df6[c("pid", "age", "male", "region", "enter", "exit", "event")]

rm(df1, df2, df4, df5, df7, age_df)

# Two more columns --------------------------------------------------------

# Creating column for time since last sickness
df6$time.since.last.sickness <- ifelse(df6$enter == 0 & df6$exit <= df6$age,
                                       NA_real_, df6$exit - df6$enter)

# Create column for length of prior sickness interval
df6$length.of.last.sickness <- lag(df6$exit) - lag(df6$enter)
df6$length.of.last.sickness <- ifelse(df6$enter == 0, NA_real_,
                                      df6$length.of.last.sickness)


# Add columns for other risks ---------------------------------------------

db6 <- read.csv("tree_fall_cleaned.csv")
snake_df3 <- read.csv("snake_ray_bite_cleaned.csv")
fought_df3 <- read.csv("fought_cleaned.csv")
animal_attack_df3 <- read.csv("animal_attack_cleaned.csv")
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

## Fight ----
df6 <- left_join(df6, fought_df3)
df6$fought.during.interval <- ifelse((df6$enter < df6$fought.age & df6$fought.age <= df6$exit) |
                                       (df6$enter < df6$fought.age1 & df6$fought.age1 <= df6$exit) |
                                       (df6$enter < df6$fought.age2 & df6$fought.age2 <= df6$exit), 1, 0)
df6$fought.during.interval <- ifelse(is.na(df6$fought.during.interval), 0, df6$fought.during.interval)

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
             "length.of.last.sickness", "time.since.last.sickness",
             "tree.fall.during.interval", "bite.during.interval",
             "fought.during.interval", "animal.attack.during.interval",
             "canoe.capsize.during.interval", "cut.self.during.interval")]

rm(db6, snake_df3, fought_df3, animal_attack_df3, ds1, dc1)



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
# df$sickness.age <- ceiling(df$sickness.age)
# df$sickness.age1 <- ceiling(df$sickness.age1)
# df$sickness.age2 <- ceiling(df$sickness.age2)

dx <- df[c("pid", "exit", "event", "sickness.age", "sickness.age1", "sickness.age2")]

dy <- dx %>%
  filter(event == 1)

dy <- dy %>%
  rowwise() %>%
  mutate(n.sickness = sum(c_across(sickness.age:sickness.age2) == exit, na.rm = TRUE)) %>%
  ungroup()
dy$n.sickness <- ifelse(dy$n.sickness == 0, 1, dy$n.sickness)

dy <- dy[c("pid", "exit", "n.sickness")]
df <- left_join(df, dy)
df <- relocate(df, n.sickness, .after = event)
df$n.sickness <- ifelse(is.na(df$n.sickness), 0, df$n.sickness)

df <- subset(df, select = -c(sickness.age, sickness.age1, sickness.age2))


# Add household ID column -------------------------------------------------

h_id <- read_xls("add household ids_a.xls")
df <- left_join(df, h_id)
df <- relocate(df, house.id, .after = pid)

rm(df3, df6, dx, dy, h_id)

# # Who reported experiencing the risk but did not report any ages? ---------
#
# df %>%
#   filter(sickness == 1) %>%
#   summarise(count = n_distinct(pid)) # Apparently 349 out of 388 people experienced it
# # But there is a story here...
# trial_df <- df %>%
#   filter(sickness == 1)
# trial_df1 <- plyr::count(unique(trial_df$pid))
#
# trial_df2 <- df6 %>%
#   filter(event == 1)
# trial_df3 <- plyr::count(unique(trial_df2$pid))
#
# anti_join(trial_df1, trial_df3) # 8 people said they experienced it but did not report any ages.


# Adding columns for cause, etc. ------------------------------------------
# Note that this is all highly context-specific, meaning that each risk has a
# different number of recorded ages, which means that the code to add these
# columns is slightly modified for every risk

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
  filter(event == 1)
dx <- dx %>%
  group_by(pid) %>%
  mutate(index = 1:n())
dx <- relocate(dx, index, .after = event)

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
  mutate(sickness_what_1 = case_when(index == 2 & n.sickness == 1 ~ sickness.what1, T ~ as.character(sickness_what_1)),
         sickness_what_2 = case_when(index == 2 & n.sickness == 1 ~ NA_character_, T ~ as.character(sickness_what_2)),
         sickness_what_3 = case_when(index == 2 & n.sickness == 1 ~ NA_character_, T ~ as.character(sickness_what_3)))

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
  mutate(days_disabled_sickness_1 = case_when(index == 2 & n.sickness == 1 ~ days.disabled.sickness1, T ~ as.integer(days_disabled_sickness_1)),
         days_disabled_sickness_2 = case_when(index == 2 & n.sickness == 1 ~ NA_integer_, T ~ as.integer(days_disabled_sickness_2)),
         days_disabled_sickness_3 = case_when(index == 2 & n.sickness == 1 ~ NA_integer_, T ~ as.integer(days_disabled_sickness_3)))

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
  mutate(almost_died_sickness_1 = case_when(index == 2 & n.sickness == 1 ~ almost.died.sickness1, T ~ as.integer(almost_died_sickness_1)),
         almost_died_sickness_2 = case_when(index == 2 & n.sickness == 1 ~ NA_integer_, T ~ as.integer(almost_died_sickness_2)),
         almost_died_sickness_3 = case_when(index == 2 & n.sickness == 1 ~ NA_integer_, T ~ as.integer(almost_died_sickness_3)))

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
  mutate(how_cured_sickness_1 = case_when(index == 2 & n.sickness == 1 ~ how.cured.sickness1, T ~ as.character(how_cured_sickness_1)),
         how_cured_sickness_2 = case_when(index == 2 & n.sickness == 1 ~ NA_character_, T ~ as.character(how_cured_sickness_2)),
         how_cured_sickness_3 = case_when(index == 2 & n.sickness == 1 ~ NA_character_, T ~ as.character(how_cured_sickness_3)))

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
  mutate(who_helped_sickness_1 = case_when(index == 2 & n.sickness == 1 ~ who.helped.sickness1, T ~ as.character(who_helped_sickness_1)),
         who_helped_sickness_2 = case_when(index == 2 & n.sickness == 1 ~ NA_character_, T ~ as.character(who_helped_sickness_2)),
         who_helped_sickness_3 = case_when(index == 2 & n.sickness == 1 ~ NA_character_, T ~ as.character(who_helped_sickness_3)))

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
  mutate(sickness_cause_1 = case_when(index == 2 & n.sickness == 1 ~ sickness.cause1, T ~ as.character(sickness_cause_1)),
         sickness_cause_2 = case_when(index == 2 & n.sickness == 1 ~ NA_character_, T ~ as.character(sickness_cause_2)),
         sickness_cause_3 = case_when(index == 2 & n.sickness == 1 ~ NA_character_, T ~ as.character(sickness_cause_3)))

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
df <- relocate(df, c(sickness_what_1:sickness_cause_3), .after = time.since.last.sickness)
df <- subset(df, select = -c(index))

# Export final table to csv -----------------------------------------------

write.csv(df, "sickness_final_table.csv", row.names = F)
