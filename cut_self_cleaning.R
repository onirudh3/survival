# Libraries
library(tidyverse)
library(readxl)

# Import raw data, this will import sheet "df"
df <- read_xls("threat_wide___sumACEs_for anirudh.xls")

# Create data frame with only the columns we need
df <- df[c("pid", "age", "male", "region", "cut.self.ever", "TIPO1",
           "other.serious.accident.age", "TIPO2",
           "other.serious.accident.age1", "TIPO3",
           "other.serious.accident.age2", "TIPO4",
           "other.serious.accident.age3", "TIPO5",
           "other.serious.accident.age4", "TIPO6",
           "other.serious.accident.age5")]

# Delete rows where no cut self ever occurred
# df <- df[(!df$cut.self.ever == 0), ]

# Creating column counting number of cut selfs per person
df$n.cut.self <- rowSums(df == "c", na.rm = T)

# Counting stuff
plyr::count(df, c("TIPO6")) # There are no cut selfs in TIPO4, TIPO5, TIPO6

# Get cut self age 1
df$cut.age1 <- if_else(df$TIPO1 == "c", df$other.serious.accident.age, NA_real_)

# Get cut self age 2
df$cut.age2 <- if_else(df$TIPO2 == "c", df$other.serious.accident.age1, NA_real_)

# Get cut self age 3
df$cut.age3 <- if_else(df$TIPO3 == "c", df$other.serious.accident.age2, NA_real_)

# Get cut self age 4
df$cut.age4 <- if_else(df$TIPO4 == "c", df$other.serious.accident.age3, NA_real_)

# Get cut self age 5
df$cut.age5 <- if_else(df$TIPO5 == "c", df$other.serious.accident.age4, NA_real_)

# Get cut self age 6
df$cut.age6 <- if_else(df$TIPO6 == "c", df$other.serious.accident.age5, NA_real_)

# Create a dataframe with the columns we want
df1 <- df[c("pid", "age", "male", "cut.self.ever", "cut.age1", "cut.age2", "cut.age3",
            "cut.age4", "cut.age5", "cut.age6", "n.cut.self")]

# View any duplicate rows
# View(df1[duplicated(df1$pid), ])
df1[duplicated(df1$pid), ]
# View(df1[duplicated(df1$pid) | duplicated(df1$pid, fromLast = TRUE), ])
df1[duplicated(df1$pid) | duplicated(df1$pid, fromLast = TRUE), ]

# Delete duplicate rows, keeping the latest observation for a person
# df1 <- df1[!duplicated(df$pid), ]
df <- df %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()

df1 <- df1 %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()

View(subset(df1, cut.age1 > cut.age2))
View(subset(df1, cut.age2 > cut.age3))
View(subset(df1, cut.age3 > cut.age4))
View(subset(df1, cut.age4 > cut.age5))
View(subset(df1, cut.age5 > cut.age6))
# God this one is a mess

# Region df
region_df <- df[c("pid", "region")]

# Is age = cut self age ever?
age_df <- df1[c("pid", "age")]

df3 <- left_join(df1, age_df)

# View(df3[(df3$age == df3$cut.age1 | df3$age == df3$cut.age2 |
#             df3$age == df3$cut.age3 | df3$age == df3$cut.age4 |
#             df3$age == df3$cut.age5 | df3$age == df3$cut.age6),])
# Yes, for 44 people! Bizarre!

# Moving values so there is no NA in cut.age1
df1 <- dedupewider::na_move(df1, cols = names(df1)[grepl("^cut.age\\d$",
                                                         names(df1))])
# Trying to check if more than one cut self occurred in one interval
# View(df1[(df1$cut.age1 == df1$cut.age2), ])
df1[(df1$cut.age1 == df1$cut.age2), ] # There is one row where this happened

# Sorting the ages horizontally
df2 <- df1[c("cut.age1", "cut.age2", "cut.age3", "cut.age4", "cut.age5", "cut.age6")]
df2[] <- t(apply(df2, 1, function(x) x[order(x)]))
df1 <- df1[c("pid", "age", "male", "cut.self.ever", "n.cut.self")]
df1 <- cbind(df1, df2)

df1 <- df1[c("pid", "cut.age1", "cut.age2", "cut.age3", "cut.age4", "cut.age5", "cut.age6")]

# Export as csv
write.csv(df1, "cut_self_cleaned.csv", row.names = F)

# Counting process format -------------------------------------------------

# For splitting, we need ID, age, event, and the corresponding ages
df5 <- df[c("pid", "cut.self.ever", "age", "cut.age1", "cut.age2", "cut.age3",
            "cut.age4", "cut.age5", "cut.age6")]

# Creating the row splits
df6 <- df5 %>%
  mutate(start = 0, end = age) %>%
  dplyr::select(-cut.self.ever) %>%
  gather(cut.self.ever, enter, -pid) %>%
  group_by(pid) %>%
  arrange(pid, enter) %>%
  filter(!is.na(enter)) %>%
  mutate(exit = lead(enter)) %>%
  filter(!is.na(exit), !grepl("time_to_risk_out_start", cut.self.ever)) %>%
  mutate(event = lead(grepl("time_to_event", cut.self.ever), default = 0)) %>%
  dplyr::select(pid, enter, exit, event) %>%
  ungroup()

# Cleaning up
df6 <- subset(df6, enter != exit)

# Adding age, male, and region columns
df7 <- df[c("pid", "age", "male", "cut.age1", "cut.age2", "cut.age3",
            "cut.age4", "cut.age5", "cut.age6")]
df7 <- left_join(df7, region_df)
df6 <- left_join(df6, df7)

# If exit > age, delete current row and assign event = 1 to the preceding row
df6 <- df6 %>%
  mutate(event = case_when(lead(exit) > lead(age) ~ 1, TRUE ~ event))

# Remove the rows where exit > age
anti_join(df6, subset(df6, exit <= age)) # 2 such rows
df6 <- subset(df6, exit <= age)

# If sickness age matches exit, event = 1
df6$event <- ifelse(df6$exit == df6$cut.age1 |
                      df6$exit == df6$cut.age2 |
                      df6$exit == df6$cut.age3 |
                      df6$exit == df6$cut.age4 |
                      df6$exit == df6$cut.age5 |
                      df6$exit == df6$cut.age6, 1, 0)

# Remove NAs
df6$event <- ifelse(is.na(df6$event), 0, df6$event)

# Final Table
df6 <- df6[c("pid", "age", "male", "region", "enter", "exit", "event")]

rm(df, df2, df5, df7, age_df, region_df)

# Two more columns --------------------------------------------------------

# Creating column for time since last cut self
df6$time.since.last.cut.self <- ifelse(df6$enter == 0 & df6$exit <= df6$age,
                                    NA_real_, df6$exit - df6$enter)

# Create column for length of prior cut self interval
df6$length.of.last.cut.self <- lag(df6$exit) - lag(df6$enter)
df6$length.of.last.cut.self <- ifelse(df6$enter == 0, NA_real_, df6$length.of.last.cut.self)


# Add columns for other risks ---------------------------------------------

db6 <- read.csv("tree_fall_cleaned.csv")
snake_df3 <- read.csv("snake_ray_bite_cleaned.csv")
sick_df3 <- read.csv("sickness_cleaned.csv")
animal_attack_df3 <- read.csv("animal_attack_cleaned.csv")
ds1 <- read.csv("canoe_capsize_cleaned.csv")
fought_df3 <- read.csv("fought_cleaned.csv")

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

## Canoe Capsize ----
df6 <- left_join(df6, ds1)
df6$canoe.capsize.during.interval <- ifelse((df6$enter < df6$cc.age1 & df6$cc.age1 <= df6$exit) |
                                              (df6$enter < df6$cc.age2 & df6$cc.age2 <= df6$exit) |
                                              (df6$enter < df6$cc.age3 & df6$cc.age3 <= df6$exit), 1, 0)
df6$canoe.capsize.during.interval <- ifelse(is.na(df6$canoe.capsize.during.interval), 0, df6$canoe.capsize.during.interval)

## Fight ----
df6 <- left_join(df6, fought_df3)
df6$fought.during.interval <- ifelse((df6$enter < df6$fought.age & df6$fought.age <= df6$exit) |
                                       (df6$enter < df6$fought.age1 & df6$fought.age1 <= df6$exit) |
                                       (df6$enter < df6$fought.age2 & df6$fought.age2 <= df6$exit), 1, 0)
df6$fought.during.interval <- ifelse(is.na(df6$fought.during.interval), 0, df6$fought.during.interval)

# Final Table
df6 <- df6[c("pid", "age", "male", "region", "enter", "exit", "event",
             "length.of.last.cut.self", "time.since.last.cut.self",
             "tree.fall.during.interval", "bite.during.interval",
             "sickness.during.interval", "animal.attack.during.interval",
             "canoe.capsize.during.interval", "fought.during.interval")]

rm(db6, snake_df3, sick_df3, animal_attack_df3, ds1, fought_df3)


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


# Add column for number of occurrences per interval -----------------------
df <- df6
df <- left_join(df, df1)
# df$cut.age1 <- ceiling(df$cut.age1)
# df$cut.age2 <- ceiling(df$cut.age2)
# df$cut.age3 <- ceiling(df$cut.age3)
# df$cut.age4 <- ceiling(df$cut.age4)
# df$cut.age5 <- ceiling(df$cut.age5)
# df$cut.age6 <- ceiling(df$cut.age6)

dx <- df[c("pid", "exit", "event", "cut.age1", "cut.age2",
           "cut.age3", "cut.age4", "cut.age5", "cut.age6")]

dy <- dx %>%
  filter(event == 1)

dy <- dy %>%
  rowwise() %>%
  mutate(n.cut.self = sum(c_across(cut.age1:cut.age6) == exit, na.rm = TRUE)) %>%
  ungroup()
dy$n.cut.self <- ifelse(dy$n.cut.self == 0, 1, dy$n.cut.self)

dy <- dy[c("pid", "exit", "n.cut.self")]
df <- left_join(df, dy)
df <- relocate(df, n.cut.self, .after = event)
df$n.cut.self <- ifelse(is.na(df$n.cut.self), 0, df$n.cut.self)

df <- subset(df, select = -c(cut.age1, cut.age2, cut.age3, cut.age4, cut.age5,
                             cut.age6))


# Add household ID column -------------------------------------------------

h_id <- read_xls("add household ids_a.xls")
df <- left_join(df, h_id)
df <- relocate(df, house.id, .after = pid)

# Export final table to csv -----------------------------------------------

write.csv(df6, "cut_self_final_table.csv", row.names = F)

