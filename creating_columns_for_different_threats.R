###################### LIBRARIES AND IMPORT DATA ###############################
library(tidyverse)
library(readxl)

# Import data
db6 <- read.csv("treefall_cleaned.csv")
sick_df3 <- read.csv("sickness_cleaned.csv")
snake_df3 <- read.csv("snake_bite_cleaned.csv")
fought_df3 <- read.csv("fought_cleaned.csv")
animal_attack_df3 <- read.csv("animal_attack_cleaned.csv")
ds1 <- read.csv("canoe_cleaned.csv")
dc1 <- read.csv("cut_self_cleaned.csv")


################################ SICKNESS ######################################

# Merging sickness and tree fall
df <- left_join(db6, sick_df3, by = "pid")

# Have you been sick between the ages you fell from tree?
df$sickness.during.interval <- ifelse((df$enter < df$sickness.age & df$sickness.age <= df$exit) |
                                      (df$enter < df$sickness.age1 & df$sickness.age1 <= df$exit) |
                                      (df$enter < df$sickness.age2 & df$sickness.age2 <= df$exit), 1, 0)
# For some reason the code above is assigning NA instead of 0 in the false case
df$sickness.during.interval <- ifelse(is.na(df$sickness.during.interval), 0, df$sickness.during.interval)

# Removing unnecessary columns
df_final <- df[c("pid", "age", "male", "region", "enter", "exit", "event", "n.tree.fall",
           "sickness.during.interval")]

######################## SNAKE/RAY BITE ########################################

# Merging snake/ray bite and tree fall
df <- left_join(db6, snake_df3, by = "pid")

# Have you been bit between the ages you fell from tree?
df$bite.during.interval <- ifelse((df$enter < df$snake.or.ray.bite.age & df$snake.or.ray.bite.age <= df$exit) |
                                    (df$enter < df$snake.or.ray.bite.age1 & df$snake.or.ray.bite.age1 <= df$exit) |
                                    (df$enter < df$snake.or.ray.bite.age2 & df$snake.or.ray.bite.age2 <= df$exit), 1, 0)
df$bite.during.interval <- ifelse(is.na(df$bite.during.interval), 0, df$bite.during.interval)

df2 <- df[c("bite.during.interval")]

# Getting back to final table
# df_final <- left_join(df_final, df2) # Don't know why this isn't working
df_final <- cbind(df_final, df2)

######################## FOUGHT ################################################

# Merging fought and tree fall
df <- left_join(db6, fought_df3, by = "pid")

# Have you fought between the ages you fell from tree?
df$fought.during.interval <- ifelse((df$enter < df$fought.age & df$fought.age <= df$exit) |
                                      (df$enter < df$fought.age1 & df$fought.age1 <= df$exit) |
                                      (df$enter < df$fought.age2 & df$fought.age2 <= df$exit), 1, 0)
df$fought.during.interval <- ifelse(is.na(df$fought.during.interval), 0, df$fought.during.interval)

df2 <- df[c("fought.during.interval")]

# Getting back to final table
df_final <- cbind(df_final, df2)

######################## ANIMAL ATTACK #########################################

# Merging animal attack and tree fall
df <- left_join(db6, animal_attack_df3, by = "pid")

# Have you been attacked between the ages you fell from tree?
df$animal.attack.during.interval <- ifelse((df$enter < df$animal.attack.age & df$animal.attack.age <= df$exit) |
                                             (df$enter < df$animal.attack.age1 & df$animal.attack.age1 <= df$exit), 1, 0)
df$animal.attack.during.interval <- ifelse(is.na(df$animal.attack.during.interval), 0, df$animal.attack.during.interval)

df2 <- df[c("animal.attack.during.interval")]

# Getting back to final table
df_final <- cbind(df_final, df2)


######################## CANOE CAPSIZE #########################################

# Merging canoe capsize and tree fall
df <- left_join(db6, ds1, by = "pid")

# Have your canoe capsized between the ages you fell from tree?
df$canoe.capsize.during.interval <- ifelse((df$enter < df$cc.age1 & df$cc.age1 <= df$exit) |
                                             (df$enter < df$cc.age2 & df$cc.age2 <= df$exit) |
                                             (df$enter < df$cc.age3 & df$cc.age3 <= df$exit), 1, 0)
df$canoe.capsize.during.interval <- ifelse(is.na(df$canoe.capsize.during.interval), 0, df$canoe.capsize.during.interval)

df2 <- df[c("canoe.capsize.during.interval")]

# Getting back to final table
df_final <- cbind(df_final, df2)

######################## CUT SELF ##############################################

# Merging cut self and tree fall
df <- left_join(db6, dc1, by = "pid")

# Have you been cut between the ages you fell from tree?
df$cut.self.during.interval <- ifelse((df$enter < df$cut.age1 & df$cut.age1 <= df$exit) |
                                        (df$enter < df$cut.age2 & df$cut.age2 <= df$exit) |
                                        (df$enter < df$cut.age3 & df$cut.age3 <= df$exit) |
                                        (df$enter < df$cut.age4 & df$cut.age4 <= df$exit) |
                                        (df$enter < df$cut.age5 & df$cut.age5 <= df$exit) |
                                        (df$enter < df$cut.age6 & df$cut.age6 <= df$exit), 1, 0)
df$cut.self.during.interval <- ifelse(is.na(df$cut.self.during.interval), 0, df$cut.self.during.interval)

df2 <- df[c("cut.self.during.interval")]

# Getting back to final table
df_final <- cbind(df_final, df2)

################################################################################
############# SOME ADDITIONAL STUFF ############################################

# Creating column for time since last fall
df_final$time.since.last.fall <- ifelse(df_final$enter == 0 & df_final$exit <= df_final$age,
                                        NA_real_, df_final$exit - df_final$enter)

# Moving the column to where I want it, after the n.tree.fall column
df_final <- df_final %>%
  relocate(time.since.last.fall, .after = n.tree.fall)

# Create column for length of prior tree fall interval
df_final$length.of.last.fall <- lag(df_final$exit) - lag(df_final$enter)
df_final$length.of.last.fall <- ifelse(df_final$enter == 0, NA_real_, df_final$length.of.last.fall)

df_final <- df_final %>%
  relocate(length.of.last.fall, .after = time.since.last.fall)

# Creating age categories for age at interview
# df_final <- df_final %>%
#   mutate(age.category = case_when(age >= 10 & age < 20 ~ "10-19",
#                                   age >= 20 & age < 30 ~ "20-29",
#                                   age >= 30 & age < 40 ~ "30-39",
#                                   age >= 40 & age < 50 ~ "40-49",
#                                   age >= 50 & age < 60 ~ "50-59",
#                                   age >= 60 & age < 70 ~ "60-69",
#                                   age >= 70 & age < 80 ~ "70-79"))

################################################################################
# Co-occurrence of sickness
df_final$sickness.co_occurrence <- ifelse(df_final$event == 1 & df_final$sickness.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of sickness
df_final <- df_final %>%
  mutate(sickness.co_occurrence.interval = case_when(sickness.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))

# Co-occurrence of snake/ray bite
df_final$bite.co_occurrence <- ifelse(df_final$event == 1 & df_final$bite.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of snake/ray bite
df_final <- df_final %>%
  mutate(bite.co_occurrence.interval = case_when(bite.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                 bite.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                 bite.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                 bite.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                 bite.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                 bite.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                 bite.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                 bite.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))

# Co-occurrence of fight
df_final$fought.co_occurrence <- ifelse(df_final$event == 1 & df_final$fought.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of fight
df_final <- df_final %>%
  mutate(fought.co_occurrence.interval = case_when(fought.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                   fought.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                   fought.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                   fought.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                   fought.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                   fought.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                   fought.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                   fought.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))

# Co-occurrence of animal attack
df_final$animal.attack.co_occurrence <- ifelse(df_final$event == 1 & df_final$animal.attack.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of animal.attack
df_final <- df_final %>%
  mutate(animal.attack.co_occurrence.interval = case_when(animal.attack.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                          animal.attack.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))

# Co-occurrence of canoe capsize
df_final$canoe.capsize.co_occurrence <- ifelse(df_final$event == 1 & df_final$canoe.capsize.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of canoe.capsize
df_final <- df_final %>%
  mutate(canoe.capsize.co_occurrence.interval = case_when(canoe.capsize.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))

# Co-occurrence of cut self
df_final$cut.self.co_occurrence <- ifelse(df_final$event == 1 & df_final$cut.self.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of cut.self
df_final <- df_final %>%
  mutate(cut.self.co_occurrence.interval = case_when(cut.self.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))



# Export final table to csv -----------------------------------------------

write.csv(df_final, "treefall_final_table.csv", row.names = F)
