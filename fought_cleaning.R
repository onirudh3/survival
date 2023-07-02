library(tidyverse)
library(readxl)

# Import raw data, this will import sheet "db"
df <- read_xls("threat_wide___sumACEs_for anirudh.xls")

# Create data frame with only the columns we need
df <- df[c("pid", "age", "male", "region", "Fought.other", "fought.age",
                         "fought.age1", "fought.age2")]

# View any duplicate rows
# View(df[duplicated(df$pid), ])
df[duplicated(df$pid), ]
# View(df[duplicated(df$pid) | duplicated(df$pid,
# fromLast = TRUE), ])
df[duplicated(df$pid) | duplicated(df$pid,
                                                 fromLast = TRUE), ]

# Delete duplicate rows, keeping the latest observation for a person
# db1 <- db1[!duplicated(db$pid), ]
df <- df %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()

# View(subset(df, fought.age > fought.age1))
# View(subset(df, fought.age1 > fought.age2))

# Making sure that the ages are chronological for each observation
df1 <- df[c("fought.age", "fought.age1", "fought.age2")]
df1[] <- t(apply(df1, 1, function(x) x[order(x)]))
df2 <- df[c("pid")]
df3 <- cbind(df2, df1)

# Export as csv
write.csv(df3, "fought_cleaned.csv", row.names = F)

# Have you fought more than once in the same interval?
# View(df3[(df3$fought.age == df3$fought.age1),])
df3[(df3$fought.age == df3$fought.age1),] # 7 people have

# Is age = fought age ever?
age_df <- df[c("pid", "age")]

df4 <- left_join(df3, age_df)

# View(df4[(df4$age == df4$fought.age |
#                    df4$age == df4$fought.age1 |
#                    df4$age == df4$fought.age2),])
# Yes, for 8 individuals


# Counting process format -------------------------------------------------

# For splitting, we need ID, age, event, and the corresponding ages
df5 <- df[c("pid", "Fought.other", "age", "fought.age", "fought.age1",
            "fought.age2")]

# Creating the row splits
df6 <- df5 %>%
  mutate(start = 0, end = age) %>%
  dplyr::select(-Fought.other) %>%
  gather(Fought.other, enter, -pid) %>%
  group_by(pid) %>%
  arrange(pid, enter) %>%
  filter(!is.na(enter)) %>%
  mutate(exit = lead(enter)) %>%
  filter(!is.na(exit), !grepl("time_to_risk_out_start", Fought.other)) %>%
  mutate(event = lead(grepl("time_to_event", Fought.other), default = 0)) %>%
  dplyr::select(pid, enter, exit, event) %>%
  ungroup()

# Cleaning up
df6 <- subset(df6, enter != exit)

# Adding age, male, and region columns
df7 <- df[c("pid", "age", "male", "region", "fought.age", "fought.age1",
            "fought.age2")]
df6 <- left_join(df6, df7)

# If exit > age, delete current row and assign event = 1 to the preceding row
df6 <- df6 %>%
  mutate(event = case_when(lead(exit) > lead(age) ~ 1, TRUE ~ event))

# Remove the rows where exit > age
df6 <- subset(df6, exit <= age)

# If sickness age matches exit, event = 1
df6$event <- ifelse(df6$exit == df6$fought.age |
                      df6$exit == df6$fought.age1 |
                      df6$exit == df6$fought.age2, 1, 0)

# Remove NAs
df6$event <- ifelse(is.na(df6$event), 0, df6$event)

# Final Table
df6 <- df6[c("pid", "age", "male", "region", "enter", "exit", "event")]

rm(df1, df2, df4, df5, df7, age_df)

# Two more columns --------------------------------------------------------

# Creating column for time since last fight
df6$time.since.last.fight <- ifelse(df6$enter == 0 & df6$exit <= df6$age,
                                       NA_real_, df6$exit - df6$enter)

# Create column for length of prior fight interval
df6$length.of.last.fight <- lag(df6$exit) - lag(df6$enter)
df6$length.of.last.fight <- ifelse(df6$enter == 0, NA_real_, df6$length.of.last.fight)


# Add columns for other risks ---------------------------------------------

db6 <- read.csv("tree_fall_cleaned.csv")
snake_df3 <- read.csv("snake_ray_bite_cleaned.csv")
sick_df3 <- read.csv("sickness_cleaned.csv")
animal_attack_df3 <- read.csv("animal_attack_cleaned.csv")
ds1 <- read.csv("canoe_capsize_cleaned.csv")
dc1 <- read.csv("cut_self_cleaned.csv")
dg <- read.csv("Animal_Attack_combined_cleaned.csv")


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

## Cut Self ----
df6 <- left_join(df6, dc1)
df6$cut.self.during.interval <- ifelse((df6$enter < df6$cut.age1 & df6$cut.age1 <= df6$exit) |
                                         (df6$enter < df6$cut.age2 & df6$cut.age2 <= df6$exit) |
                                         (df6$enter < df6$cut.age3 & df6$cut.age3 <= df6$exit) |
                                         (df6$enter < df6$cut.age4 & df6$cut.age4 <= df6$exit) |
                                         (df6$enter < df6$cut.age5 & df6$cut.age5 <= df6$exit) |
                                         (df6$enter < df6$cut.age6 & df6$cut.age6 <= df6$exit), 1, 0)
df6$cut.self.during.interval <- ifelse(is.na(df6$cut.self.during.interval), 0, df6$cut.self.during.interval)

## Animal Attack (c) ----
df6 <- left_join(df6, dg)
df6$Animal_Attack.during.interval <- ifelse((df6$enter < df6$Animal_Attack_age & df6$Animal_Attack_age <= df6$exit) |
                                              (df6$enter < df6$Animal_Attack_age1 & df6$Animal_Attack_age1 <= df6$exit), 1, 0)
df6$Animal_Attack.during.interval <- ifelse(is.na(df6$Animal_Attack.during.interval), 0, df6$Animal_Attack.during.interval)


# Final Table
df6 <- df6[c("pid", "age", "male", "region", "enter", "exit", "event",
             "length.of.last.fight", "time.since.last.fight",
             "tree.fall.during.interval", "bite.during.interval",
             "sickness.during.interval", "animal.attack.during.interval",
             "canoe.capsize.during.interval", "cut.self.during.interval", "Animal_Attack.during.interval")]

rm(db6, snake_df3, sick_df3, animal_attack_df3, ds1, dc1, dg)

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

## Animal Attack (c) ----

df6$Animal_Attack.co_occurrence <- ifelse(df6$event == 1 & df6$Animal_Attack.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of Animal_Attack
df6 <- df6 %>%
  mutate(Animal_Attack.co_occurrence.interval = case_when(Animal_Attack.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                          Animal_Attack.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                          Animal_Attack.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                          Animal_Attack.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                          Animal_Attack.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                          Animal_Attack.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                          Animal_Attack.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                          Animal_Attack.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))


# Add column for number of occurrences per interval -----------------------
df <- df6
df <- left_join(df, df3)
# df$fought.age <- ceiling(df$fought.age)
# df$fought.age1 <- ceiling(df$fought.age1)
# df$fought.age2 <- ceiling(df$fought.age2)

dx <- df[c("pid", "exit", "event", "fought.age",
           "fought.age1", "fought.age2")]

dy <- dx %>%
  filter(event == 1)

dy <- dy %>%
  rowwise() %>%
  mutate(n.fought = sum(c_across(fought.age:fought.age2) == exit, na.rm = TRUE)) %>%
  ungroup()
dy$n.fought <- ifelse(dy$n.fought == 0, 1, dy$n.fought)

dy <- dy[c("pid", "exit", "n.fought")]
df <- left_join(df, dy)
df <- relocate(df, n.fought, .after = event)
df$n.fought <- ifelse(is.na(df$n.fought), 0, df$n.fought)

df <- subset(df, select = -c(fought.age, fought.age1, fought.age2))


# Add household ID column -------------------------------------------------

h_id <- read_xls("add household ids_a.xls")
df <- left_join(df, h_id)
df <- relocate(df, house.id, .after = pid)

rm(df3, df6, dx, dy, h_id)


# Adding columns for cause, etc. ------------------------------------------
# Note that this is all highly context-specific, meaning that each risk has a
# different number of reported ages, which means that the code to add these
# columns is slightly modified for every risk.

raw <- read.csv("raw_data_no_duplicates.csv")

raw <- dplyr::select(raw, c(7, Fought.whom:Fought.either.drunk2))
raw <- subset(raw, select = -c(fought.age, fought.age1, fought.age2))
dx <- left_join(df, raw)

dx <- dx %>%
  filter(event == 1)
dx <- dx %>%
  group_by(pid) %>%
  mutate(index = 1:n(),
         cum = cumsum(n.fought))
dx <- relocate(dx, index, .after = event)

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
df <- relocate(df, c(fought_whom_1:fought_either_drunk_3), .after = time.since.last.fight)
df <- subset(df, select = -c(index, cum))




# Export final table to csv -----------------------------------------------

write.csv(df, "fought_final_table.csv", row.names = F)

