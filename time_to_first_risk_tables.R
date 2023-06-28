
# Libraries ---------------------------------------------------------------

library(tidyverse)


# Time to first risk ------------------------------------------------------


## Tree Fall ----
# Long intervals
df_long <- read.csv("tree_fall_final_table.csv")

df <- subset(df_long, enter == 0)

write.csv(df, "tree_fall_time_to_first_risk_long_interval.csv", row.names = F)

# Short intervals
df_short <- read.csv("data_new_format.csv")

dg <- df_short %>%
  group_by(pid) %>%
  mutate(y = cumall(lag(!tree.fall.during.interval))) %>%
  filter(is.na(y)) %>%
  select(-y)

write.csv(dg, "tree_fall_time_to_first_risk_short_interval.csv", row.names = F)


## Snake/Ray Bite ----
# Long intervals
df_long <- read.csv("snake_ray_bite_final_table.csv")

df <- subset(df_long, enter == 0)

write.csv(df, "snake_ray_bite_time_to_first_risk_long_interval.csv",
          row.names = F)

# Short intervals
df_short <- read.csv("data_new_format.csv")

dg <- df_short %>%
  group_by(pid) %>%
  mutate(y = cumall(lag(!bite.during.interval))) %>%
  filter(is.na(y)) %>%
  select(-y)

write.csv(dg, "snake_ray_bite_time_to_first_risk_short_interval.csv", row.names = F)


## Sickness ----
# Long intervals
df_long <- read.csv("sickness_final_table.csv")

df <- subset(df_long, enter == 0)

write.csv(df, "sickness_time_to_first_risk_long_interval.csv", row.names = F)

# Short intervals
df_short <- read.csv("data_new_format.csv")

dg <- df_short %>%
  group_by(pid) %>%
  mutate(y = cumall(lag(!sickness.during.interval))) %>%
  filter(is.na(y)) %>%
  select(-y)

write.csv(dg, "sickness_time_to_first_risk_short_interval.csv", row.names = F)


## Fight ----
# Long intervals
df_long <- read.csv("fought_final_table.csv")

df <- subset(df_long, enter == 0)

write.csv(df, "fought_time_to_first_risk_long_interval.csv", row.names = F)

# Short intervals
df_short <- read.csv("data_new_format.csv")

dg <- df_short %>%
  group_by(pid) %>%
  mutate(y = cumall(lag(!fought.during.interval))) %>%
  filter(is.na(y)) %>%
  select(-y)

write.csv(dg, "fought_time_to_first_risk_short_interval.csv", row.names = F)


## Animal Attack ----
# Long intervals
df_long <- read.csv("animal_attack_final_table.csv")

df <- subset(df_long, enter == 0)

write.csv(df, "animal_attack_time_to_first_risk_long_interval.csv", row.names = F)

# Short intervals
df_short <- read.csv("data_new_format.csv")

dg <- df_short %>%
  group_by(pid) %>%
  mutate(y = cumall(lag(!animal.attack.during.interval))) %>%
  filter(is.na(y)) %>%
  select(-y)

write.csv(dg, "animal_attack_time_to_first_risk_short_interval.csv", row.names = F)


## Canoe Capsize ----
# Long intervals
df_long <- read.csv("canoe_capsize_final_table.csv")

df <- subset(df_long, enter == 0)

write.csv(df, "canoe_capsize_time_to_first_risk_long_interval.csv", row.names = F)

# Short intervals
df_short <- read.csv("data_new_format.csv")

dg <- df_short %>%
  group_by(pid) %>%
  mutate(y = cumall(lag(!canoe.capsize.during.interval))) %>%
  filter(is.na(y)) %>%
  select(-y)

write.csv(dg, "canoe_capsize_time_to_first_risk_short_interval.csv", row.names = F)


## Cut Self ----
# Long intervals
df_long <- read.csv("cut_self_final_table.csv")

df <- subset(df_long, enter == 0)

write.csv(df, "cut_self_time_to_first_risk_long_interval.csv", row.names = F)

# Short intervals
df_short <- read.csv("data_new_format.csv")

dg <- df_short %>%
  group_by(pid) %>%
  mutate(y = cumall(lag(!cut.self.during.interval))) %>%
  filter(is.na(y)) %>%
  select(-y)

write.csv(dg, "cut_self_time_to_first_risk_short_interval.csv", row.names = F)



