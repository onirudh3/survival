######################## SICKNESS ##############################################

# Merging sickness and tree fall
df <- left_join(db6, sick_df3, by = "pid")

# Have you been sick between the ages you fell from tree?
df$sickness.during.interval <- ifelse((df$enter < df$sickness.age & df$sickness.age <= df$exit) |
                                      (df$enter < df$sickness.age1 & df$sickness.age1 <= df$exit) | 
                                      (df$enter < df$sickness.age2 & df$sickness.age2 <= df$exit), 1, 0) 
# For some reason the code above is assigning NA instead of 0 in the false case
df$sickness.during.interval <- ifelse(is.na(df$sickness.during.interval), 0, df$sickness.during.interval)

# Removing unnecessary columns
df_final <- df[c("pid", "age", "male", "enter", "exit", "event", "n.tree.fall", 
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

# Adding region column
region_df <- read_xls("threat_wide___sumACEs_for anirudh.xls")
region_df <- region_df[c("pid", "region", "age")]
region_df <- region_df %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()
df_final <- left_join(df_final, region_df, by = "pid")

################################################################################

# Export final table to csv
write.csv(df_final, "treefall_final_table.csv", row.names = F)