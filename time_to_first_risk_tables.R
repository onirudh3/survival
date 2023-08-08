
# Libraries ---------------------------------------------------------------

library(tidyverse)

# Read raw data for adding calendar year ----
raw_df <- subset(read.csv("raw_data_no_duplicates.csv"), select = c(pid, YearBorn))



# Time to first risk ------------------------------------------------------


## Tree Fall ----
# Short intervals
df_short <- read.csv("data_new_format.csv")

dg <- df_short %>%
  group_by(pid) %>%
  mutate(y = cumall(lag(!tree.fall.during.interval))) %>%
  filter(is.na(y)) %>%
  dplyr::select(-y)

dg <- left_join(dg, raw_df)
dg <- dg %>%
  group_by(pid) %>%
  mutate(year = seq(first(YearBorn), length.out = n()), .after = age)
dg <- subset(dg, select = -c(YearBorn))

# Calendar year median split variable
dg$pre_median <- ifelse(dg$year < median(dg$year), sprintf("Pre %d", median(dg$year)), sprintf("Post %d", median(dg$year)))
dg <- dg %>% relocate(pre_median, .after = year)

# Calendar year tercile split variable
dg$tercile <- fabricatr::split_quantile(dg$year, 3)
dg <- dg %>% relocate(tercile, .after = pre_median)
dg <- dg %>%
  group_by(tercile) %>%
  mutate(tercile = sprintf("Period %d-%d", min(year), max(year)), .after = pre_median)

write.csv(dg, "tree_fall_time_to_first_risk_short_interval.csv", row.names = F)

# Long intervals
df_long <- read.csv("tree_fall_final_table.csv")

df <- subset(df_long, enter == 0)

df <- df %>%
  mutate(exit = case_when(age != exit ~ ceiling(exit), T ~ exit))

dg <- subset(dg, select = c(pid, exit, pre_median, tercile))
df <- left_join(df, dg)
df <- df %>% relocate(pre_median, tercile, .after = exit)

write.csv(df, "tree_fall_time_to_first_risk_long_interval.csv", row.names = F)


## Snake/Ray Bite ----
# Short intervals
df_short <- read.csv("data_new_format.csv")

dg <- df_short %>%
  group_by(pid) %>%
  mutate(y = cumall(lag(!bite.during.interval))) %>%
  filter(is.na(y)) %>%
  dplyr::select(-y)

dg <- left_join(dg, raw_df)
dg <- dg %>%
  group_by(pid) %>%
  mutate(year = seq(first(YearBorn), length.out = n()), .after = age)
dg <- subset(dg, select = -c(YearBorn))

# Calendar year median split variable
dg$pre_median <- ifelse(dg$year < median(dg$year), sprintf("Pre %d", median(dg$year)), sprintf("Post %d", median(dg$year)))
dg <- dg %>% relocate(pre_median, .after = year)

# Calendar year tercile split variable
dg$tercile <- fabricatr::split_quantile(dg$year, 3)
dg <- dg %>% relocate(tercile, .after = pre_median)
dg <- dg %>%
  group_by(tercile) %>%
  mutate(tercile = sprintf("Period %d-%d", min(year), max(year)), .after = pre_median)

write.csv(dg, "snake_ray_bite_time_to_first_risk_short_interval.csv", row.names = F)

# Long intervals
df_long <- read.csv("snake_ray_bite_final_table.csv")

df <- subset(df_long, enter == 0)

df <- df %>%
  mutate(exit = case_when(age != exit ~ ceiling(exit), T ~ exit))

dg <- subset(dg, select = c(pid, exit, pre_median, tercile))
df <- left_join(df, dg)
df <- df %>% relocate(pre_median, tercile, .after = exit)

write.csv(df, "snake_ray_bite_time_to_first_risk_long_interval.csv",
          row.names = F)


## Sickness ----
# Short intervals
df_short <- read.csv("data_new_format.csv")

dg <- df_short %>%
  group_by(pid) %>%
  mutate(y = cumall(lag(!sickness.during.interval))) %>%
  filter(is.na(y)) %>%
  dplyr::select(-y)

dg <- left_join(dg, raw_df)
dg <- dg %>%
  group_by(pid) %>%
  mutate(year = seq(first(YearBorn), length.out = n()), .after = age)
dg <- subset(dg, select = -c(YearBorn))

# Calendar year median split variable
dg$pre_median <- ifelse(dg$year < median(dg$year), sprintf("Pre %d", median(dg$year)), sprintf("Post %d", median(dg$year)))
dg <- dg %>% relocate(pre_median, .after = year)

# Calendar year tercile split variable
dg$tercile <- fabricatr::split_quantile(dg$year, 3)
dg <- dg %>% relocate(tercile, .after = pre_median)
dg <- dg %>%
  group_by(tercile) %>%
  mutate(tercile = sprintf("Period %d-%d", min(year), max(year)), .after = pre_median)

write.csv(dg, "sickness_time_to_first_risk_short_interval.csv", row.names = F)

# Long intervals
df_long <- read.csv("sickness_final_table.csv")

df <- subset(df_long, enter == 0)

df <- df %>%
  mutate(exit = case_when(age != exit ~ ceiling(exit), T ~ exit))

dg <- subset(dg, select = c(pid, exit, pre_median, tercile))
df <- left_join(df, dg)
df <- df %>% relocate(pre_median, tercile, .after = exit)

write.csv(df, "sickness_time_to_first_risk_long_interval.csv", row.names = F)


## Fight ----
# Short intervals
df_short <- read.csv("data_new_format.csv")

dg <- df_short %>%
  group_by(pid) %>%
  mutate(y = cumall(lag(!fought.during.interval))) %>%
  filter(is.na(y)) %>%
  dplyr::select(-y)

dg <- left_join(dg, raw_df)
dg <- dg %>%
  group_by(pid) %>%
  mutate(year = seq(first(YearBorn), length.out = n()), .after = age)
dg <- subset(dg, select = -c(YearBorn))

# Calendar year median split variable
dg$pre_median <- ifelse(dg$year < median(dg$year), sprintf("Pre %d", median(dg$year)), sprintf("Post %d", median(dg$year)))
dg <- dg %>% relocate(pre_median, .after = year)

# Calendar year tercile split variable
dg$tercile <- fabricatr::split_quantile(dg$year, 3)
dg <- dg %>% relocate(tercile, .after = pre_median)
dg <- dg %>%
  group_by(tercile) %>%
  mutate(tercile = sprintf("Period %d-%d", min(year), max(year)), .after = pre_median)

write.csv(dg, "fought_time_to_first_risk_short_interval.csv", row.names = F)

# Long intervals
df_long <- read.csv("fought_final_table.csv")

df <- subset(df_long, enter == 0)

df <- df %>%
  mutate(exit = case_when(age != exit ~ ceiling(exit), T ~ exit))

dg <- subset(dg, select = c(pid, exit, pre_median, tercile))
df <- left_join(df, dg)
df <- df %>% relocate(pre_median, tercile, .after = exit)

write.csv(df, "fought_time_to_first_risk_long_interval.csv", row.names = F)


## Animal Attack ----
# Short intervals
df_short <- read.csv("data_new_format.csv")

dg <- df_short %>%
  group_by(pid) %>%
  mutate(y = cumall(lag(!animal.attack.during.interval))) %>%
  filter(is.na(y)) %>%
  dplyr::select(-y)

dg <- left_join(dg, raw_df)
dg <- dg %>%
  group_by(pid) %>%
  mutate(year = seq(first(YearBorn), length.out = n()), .after = age)
dg <- subset(dg, select = -c(YearBorn))

# Calendar year median split variable
dg$pre_median <- ifelse(dg$year < median(dg$year), sprintf("Pre %d", median(dg$year)), sprintf("Post %d", median(dg$year)))
dg <- dg %>% relocate(pre_median, .after = year)

# Calendar year tercile split variable
dg$tercile <- fabricatr::split_quantile(dg$year, 3)
dg <- dg %>% relocate(tercile, .after = pre_median)
dg <- dg %>%
  group_by(tercile) %>%
  mutate(tercile = sprintf("Period %d-%d", min(year), max(year)), .after = pre_median)

write.csv(dg, "animal_attack_time_to_first_risk_short_interval.csv", row.names = F)

# Long intervals
df_long <- read.csv("animal_attack_final_table.csv")

df <- subset(df_long, enter == 0)

df <- df %>%
  mutate(exit = case_when(age != exit ~ ceiling(exit), T ~ exit))

dg <- subset(dg, select = c(pid, exit, pre_median, tercile))
df <- left_join(df, dg)
df <- df %>% relocate(pre_median, tercile, .after = exit)

write.csv(df, "animal_attack_time_to_first_risk_long_interval.csv", row.names = F)


## Canoe Capsize ----
# Short intervals
df_short <- read.csv("data_new_format.csv")

dg <- df_short %>%
  group_by(pid) %>%
  mutate(y = cumall(lag(!canoe.capsize.during.interval))) %>%
  filter(is.na(y)) %>%
  dplyr::select(-y)

dg <- left_join(dg, raw_df)
dg <- dg %>%
  group_by(pid) %>%
  mutate(year = seq(first(YearBorn), length.out = n()), .after = age)
dg <- subset(dg, select = -c(YearBorn))

# Calendar year median split variable
dg$pre_median <- ifelse(dg$year < median(dg$year), sprintf("Pre %d", median(dg$year)), sprintf("Post %d", median(dg$year)))
dg <- dg %>% relocate(pre_median, .after = year)

# Calendar year tercile split variable
dg$tercile <- fabricatr::split_quantile(dg$year, 3)
dg <- dg %>% relocate(tercile, .after = pre_median)
dg <- dg %>%
  group_by(tercile) %>%
  mutate(tercile = sprintf("Period %d-%d", min(year), max(year)), .after = pre_median)

write.csv(dg, "canoe_capsize_time_to_first_risk_short_interval.csv", row.names = F)

# Long intervals
df_long <- read.csv("canoe_capsize_final_table.csv")

df <- subset(df_long, enter == 0)

df <- df %>%
  mutate(exit = case_when(age != exit ~ ceiling(exit), T ~ exit))

dg <- subset(dg, select = c(pid, exit, pre_median, tercile))
df <- left_join(df, dg)
df <- df %>% relocate(pre_median, tercile, .after = exit)

write.csv(df, "canoe_capsize_time_to_first_risk_long_interval.csv", row.names = F)


## Cut Self ----
# Short intervals
df_short <- read.csv("data_new_format.csv")

dg <- df_short %>%
  group_by(pid) %>%
  mutate(y = cumall(lag(!cut.self.during.interval))) %>%
  filter(is.na(y)) %>%
  dplyr::select(-y)

dg <- left_join(dg, raw_df)
dg <- dg %>%
  group_by(pid) %>%
  mutate(year = seq(first(YearBorn), length.out = n()), .after = age)
dg <- subset(dg, select = -c(YearBorn))

# Calendar year median split variable
dg$pre_median <- ifelse(dg$year < median(dg$year), sprintf("Pre %d", median(dg$year)), sprintf("Post %d", median(dg$year)))
dg <- dg %>% relocate(pre_median, .after = year)

# Calendar year tercile split variable
dg$tercile <- fabricatr::split_quantile(dg$year, 3)
dg <- dg %>% relocate(tercile, .after = pre_median)
dg <- dg %>%
  group_by(tercile) %>%
  mutate(tercile = sprintf("Period %d-%d", min(year), max(year)), .after = pre_median)

write.csv(dg, "cut_self_time_to_first_risk_short_interval.csv", row.names = F)

# Long intervals
df_long <- read.csv("cut_self_final_table.csv")

df <- subset(df_long, enter == 0)

df <- df %>%
  mutate(exit = case_when(age != exit ~ ceiling(exit), T ~ exit))

dg <- subset(dg, select = c(pid, exit, pre_median, tercile))
df <- left_join(df, dg)
df <- df %>% relocate(pre_median, tercile, .after = exit)

write.csv(df, "cut_self_time_to_first_risk_long_interval.csv", row.names = F)


## Animal Attack (c) ----
# Short intervals
df_short <- read.csv("data_new_format.csv")

dg <- df_short %>%
  group_by(pid) %>%
  mutate(y = cumall(lag(!Animal_Attack.during.interval))) %>%
  filter(is.na(y)) %>%
  dplyr::select(-y)

dg <- left_join(dg, raw_df)
dg <- dg %>%
  group_by(pid) %>%
  mutate(year = seq(first(YearBorn), length.out = n()), .after = age)
dg <- subset(dg, select = -c(YearBorn))

# Calendar year median split variable
dg$pre_median <- ifelse(dg$year < median(dg$year), sprintf("Pre %d", median(dg$year)), sprintf("Post %d", median(dg$year)))
dg <- dg %>% relocate(pre_median, .after = year)

# Calendar year tercile split variable
dg$tercile <- fabricatr::split_quantile(dg$year, 3)
dg <- dg %>% relocate(tercile, .after = pre_median)
dg <- dg %>%
  group_by(tercile) %>%
  mutate(tercile = sprintf("Period %d-%d", min(year), max(year)), .after = pre_median)

write.csv(dg, "Animal_Attack_combined_time_to_first_risk_short_interval.csv", row.names = F)

# Long intervals
df_long <- read.csv("Animal_Attack_combined_final_table.csv")

df <- subset(df_long, enter == 0)

df <- df %>%
  mutate(exit = case_when(age != exit ~ ceiling(exit), T ~ exit))

dg <- subset(dg, select = c(pid, exit, pre_median, tercile))
df <- left_join(df, dg)
df <- df %>% relocate(pre_median, tercile, .after = exit)

write.csv(df, "Animal_Attack_combined_time_to_first_risk_long_interval.csv", row.names = F)

