
# Libraries and data import -----------------------------------------------

library(tidyverse)

# Read df data
df <- read.csv("raw_data_no_duplicates.csv")

# Cleaning ----------------------------------------------------------------

# Create data frame with only the columns we need
df <- subset(df, select = c(pid, age, male, region,
                            snake.or.ray.bite.ever:animal.attack.age1))
df <- subset(df, select = -c(n.times.bit.snake.or.ray, n.times.animal.attack))

# Consolidate snake.or.ray.bite.ever and animal.attack.ever
df <- df %>%
  mutate(Animal_Attack_ever = case_when(snake.or.ray.bite.ever == 1 |
                                          animal.attack.ever == 1 ~ 1, T ~ 0))
df <- subset(df, select = -c(snake.or.ray.bite.ever, animal.attack.ever))
df <- relocate(df, Animal_Attack_ever, .after = region)

# Rename columns
df <- df %>% rename("what.bit.you3" = "what.attacked.you",
             "what.bit.you4" = "what.attacked.you1",
             "where.bit.body3" = "where.attacked.body",
             "where.bit.body4" = "where.attacked.body1",
             "activity.when.bit1" = "activity.when.bit1...25",
             "activity.when.bit3" = "activity.when.bit1...41",
             "days.disabled.bite3" = "days.disabled.attack",
             "days.disabled.bite4" = "days.disabled.attack1",
             "almost.died.bite3" = "almost.died.attack",
             "almost.died.bite4" = "almost.died.attack1",
             "still.bothers.bite3" = "still.bothers.attack",
             "still.bothers.bite4" = "still.bothers.attack1",
             "Animal_Attack_age" = "snake.or.ray.bite.age",
             "Animal_Attack_age1" = "snake.or.ray.bite.age1",
             "Animal_Attack_age2" = "snake.or.ray.bite.age2",
             "Animal_Attack_age3" = "animal.attack.age",
             "Animal_Attack_age4" = "animal.attack.age1")

df$activity.when.bit4 <- NA_character_
df <- relocate(df, activity.when.bit4, .after = where.bit.body4)
df$days.disabled.bite3 <- as.numeric(df$days.disabled.bite3)
df$days.disabled.bite4 <- as.numeric(df$days.disabled.bite4)
df$Animal_Attack_age2 <- as.numeric(df$Animal_Attack_age2)

# Arranging ages ----------------------------------------------------------
# Iteration 1/4
df <- transform(df,
                Animal_Attack_age = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ Animal_Attack_age1, T ~ Animal_Attack_age),
                Animal_Attack_age1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ Animal_Attack_age, T ~ Animal_Attack_age1),
                what.bit.you = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ what.bit.you1, T ~ what.bit.you),
                what.bit.you1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ what.bit.you, T ~ what.bit.you1),
                where.bit.body = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ where.bit.body1, T ~ where.bit.body),
                where.bit.body1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ where.bit.body, T ~ where.bit.body1),
                activity.when.bit = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ activity.when.bit1, T ~ activity.when.bit),
                activity.when.bit1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ activity.when.bit, T ~ activity.when.bit1),
                days.disabled.bite = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ days.disabled.bite1, T ~ days.disabled.bite),
                days.disabled.bite1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ days.disabled.bite, T ~ days.disabled.bite1),
                almost.died.bite = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ almost.died.bite1, T ~ almost.died.bite),
                almost.died.bite1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ almost.died.bite, T ~ almost.died.bite1),
                still.bothers.bite = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ still.bothers.bite1, T ~ still.bothers.bite),
                still.bothers.bite1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ still.bothers.bite, T ~ still.bothers.bite1))

df <- transform(df,
                Animal_Attack_age1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ Animal_Attack_age2, T ~ Animal_Attack_age1),
                Animal_Attack_age2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ Animal_Attack_age1, T ~ Animal_Attack_age2),
                what.bit.you1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ what.bit.you2, T ~ what.bit.you1),
                what.bit.you2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ what.bit.you1, T ~ what.bit.you2),
                where.bit.body1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ where.bit.body2, T ~ where.bit.body1),
                where.bit.body2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ where.bit.body1, T ~ where.bit.body2),
                activity.when.bit1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ activity.when.bit2, T ~ activity.when.bit1),
                activity.when.bit2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ activity.when.bit1, T ~ activity.when.bit2),
                days.disabled.bite1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ days.disabled.bite2, T ~ days.disabled.bite1),
                days.disabled.bite2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ days.disabled.bite1, T ~ days.disabled.bite2),
                almost.died.bite1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ almost.died.bite2, T ~ almost.died.bite1),
                almost.died.bite2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ almost.died.bite1, T ~ almost.died.bite2),
                still.bothers.bite1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ still.bothers.bite2, T ~ still.bothers.bite1),
                still.bothers.bite2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ still.bothers.bite1, T ~ still.bothers.bite2))

df <- transform(df,
                Animal_Attack_age2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ Animal_Attack_age3, T ~ Animal_Attack_age2),
                Animal_Attack_age3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ Animal_Attack_age2, T ~ Animal_Attack_age3),
                what.bit.you2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ what.bit.you3, T ~ what.bit.you2),
                what.bit.you3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ what.bit.you2, T ~ what.bit.you3),
                where.bit.body2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ where.bit.body3, T ~ where.bit.body2),
                where.bit.body3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ where.bit.body2, T ~ where.bit.body3),
                activity.when.bit2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ activity.when.bit3, T ~ activity.when.bit2),
                activity.when.bit3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ activity.when.bit2, T ~ activity.when.bit3),
                days.disabled.bite2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ days.disabled.bite3, T ~ days.disabled.bite2),
                days.disabled.bite3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ days.disabled.bite2, T ~ days.disabled.bite3),
                almost.died.bite2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ almost.died.bite3, T ~ almost.died.bite2),
                almost.died.bite3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ almost.died.bite2, T ~ almost.died.bite3),
                still.bothers.bite2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ still.bothers.bite3, T ~ still.bothers.bite2),
                still.bothers.bite3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ still.bothers.bite2, T ~ still.bothers.bite3))

df <- transform(df,
                Animal_Attack_age3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ Animal_Attack_age4, T ~ Animal_Attack_age3),
                Animal_Attack_age4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ Animal_Attack_age3, T ~ Animal_Attack_age4),
                what.bit.you3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ what.bit.you4, T ~ what.bit.you3),
                what.bit.you4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ what.bit.you3, T ~ what.bit.you4),
                where.bit.body3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ where.bit.body4, T ~ where.bit.body3),
                where.bit.body4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ where.bit.body3, T ~ where.bit.body4),
                activity.when.bit3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ activity.when.bit4, T ~ activity.when.bit3),
                activity.when.bit4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ activity.when.bit3, T ~ activity.when.bit4),
                days.disabled.bite3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ days.disabled.bite4, T ~ days.disabled.bite3),
                days.disabled.bite4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ days.disabled.bite3, T ~ days.disabled.bite4),
                almost.died.bite3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ almost.died.bite4, T ~ almost.died.bite3),
                almost.died.bite4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ almost.died.bite3, T ~ almost.died.bite4),
                still.bothers.bite3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ still.bothers.bite4, T ~ still.bothers.bite3),
                still.bothers.bite4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ still.bothers.bite3, T ~ still.bothers.bite4))

# Iteration 2/4
df <- transform(df,
                Animal_Attack_age = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ Animal_Attack_age1, T ~ Animal_Attack_age),
                Animal_Attack_age1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ Animal_Attack_age, T ~ Animal_Attack_age1),
                what.bit.you = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ what.bit.you1, T ~ what.bit.you),
                what.bit.you1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ what.bit.you, T ~ what.bit.you1),
                where.bit.body = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ where.bit.body1, T ~ where.bit.body),
                where.bit.body1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ where.bit.body, T ~ where.bit.body1),
                activity.when.bit = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ activity.when.bit1, T ~ activity.when.bit),
                activity.when.bit1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ activity.when.bit, T ~ activity.when.bit1),
                days.disabled.bite = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ days.disabled.bite1, T ~ days.disabled.bite),
                days.disabled.bite1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ days.disabled.bite, T ~ days.disabled.bite1),
                almost.died.bite = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ almost.died.bite1, T ~ almost.died.bite),
                almost.died.bite1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ almost.died.bite, T ~ almost.died.bite1),
                still.bothers.bite = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ still.bothers.bite1, T ~ still.bothers.bite),
                still.bothers.bite1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ still.bothers.bite, T ~ still.bothers.bite1))

df <- transform(df,
                Animal_Attack_age1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ Animal_Attack_age2, T ~ Animal_Attack_age1),
                Animal_Attack_age2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ Animal_Attack_age1, T ~ Animal_Attack_age2),
                what.bit.you1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ what.bit.you2, T ~ what.bit.you1),
                what.bit.you2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ what.bit.you1, T ~ what.bit.you2),
                where.bit.body1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ where.bit.body2, T ~ where.bit.body1),
                where.bit.body2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ where.bit.body1, T ~ where.bit.body2),
                activity.when.bit1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ activity.when.bit2, T ~ activity.when.bit1),
                activity.when.bit2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ activity.when.bit1, T ~ activity.when.bit2),
                days.disabled.bite1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ days.disabled.bite2, T ~ days.disabled.bite1),
                days.disabled.bite2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ days.disabled.bite1, T ~ days.disabled.bite2),
                almost.died.bite1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ almost.died.bite2, T ~ almost.died.bite1),
                almost.died.bite2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ almost.died.bite1, T ~ almost.died.bite2),
                still.bothers.bite1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ still.bothers.bite2, T ~ still.bothers.bite1),
                still.bothers.bite2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ still.bothers.bite1, T ~ still.bothers.bite2))

df <- transform(df,
                Animal_Attack_age2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ Animal_Attack_age3, T ~ Animal_Attack_age2),
                Animal_Attack_age3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ Animal_Attack_age2, T ~ Animal_Attack_age3),
                what.bit.you2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ what.bit.you3, T ~ what.bit.you2),
                what.bit.you3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ what.bit.you2, T ~ what.bit.you3),
                where.bit.body2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ where.bit.body3, T ~ where.bit.body2),
                where.bit.body3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ where.bit.body2, T ~ where.bit.body3),
                activity.when.bit2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ activity.when.bit3, T ~ activity.when.bit2),
                activity.when.bit3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ activity.when.bit2, T ~ activity.when.bit3),
                days.disabled.bite2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ days.disabled.bite3, T ~ days.disabled.bite2),
                days.disabled.bite3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ days.disabled.bite2, T ~ days.disabled.bite3),
                almost.died.bite2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ almost.died.bite3, T ~ almost.died.bite2),
                almost.died.bite3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ almost.died.bite2, T ~ almost.died.bite3),
                still.bothers.bite2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ still.bothers.bite3, T ~ still.bothers.bite2),
                still.bothers.bite3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ still.bothers.bite2, T ~ still.bothers.bite3))

df <- transform(df,
                Animal_Attack_age3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ Animal_Attack_age4, T ~ Animal_Attack_age3),
                Animal_Attack_age4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ Animal_Attack_age3, T ~ Animal_Attack_age4),
                what.bit.you3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ what.bit.you4, T ~ what.bit.you3),
                what.bit.you4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ what.bit.you3, T ~ what.bit.you4),
                where.bit.body3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ where.bit.body4, T ~ where.bit.body3),
                where.bit.body4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ where.bit.body3, T ~ where.bit.body4),
                activity.when.bit3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ activity.when.bit4, T ~ activity.when.bit3),
                activity.when.bit4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ activity.when.bit3, T ~ activity.when.bit4),
                days.disabled.bite3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ days.disabled.bite4, T ~ days.disabled.bite3),
                days.disabled.bite4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ days.disabled.bite3, T ~ days.disabled.bite4),
                almost.died.bite3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ almost.died.bite4, T ~ almost.died.bite3),
                almost.died.bite4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ almost.died.bite3, T ~ almost.died.bite4),
                still.bothers.bite3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ still.bothers.bite4, T ~ still.bothers.bite3),
                still.bothers.bite4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ still.bothers.bite3, T ~ still.bothers.bite4))

# Iteration 3/4
df <- transform(df,
                Animal_Attack_age = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ Animal_Attack_age1, T ~ Animal_Attack_age),
                Animal_Attack_age1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ Animal_Attack_age, T ~ Animal_Attack_age1),
                what.bit.you = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ what.bit.you1, T ~ what.bit.you),
                what.bit.you1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ what.bit.you, T ~ what.bit.you1),
                where.bit.body = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ where.bit.body1, T ~ where.bit.body),
                where.bit.body1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ where.bit.body, T ~ where.bit.body1),
                activity.when.bit = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ activity.when.bit1, T ~ activity.when.bit),
                activity.when.bit1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ activity.when.bit, T ~ activity.when.bit1),
                days.disabled.bite = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ days.disabled.bite1, T ~ days.disabled.bite),
                days.disabled.bite1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ days.disabled.bite, T ~ days.disabled.bite1),
                almost.died.bite = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ almost.died.bite1, T ~ almost.died.bite),
                almost.died.bite1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ almost.died.bite, T ~ almost.died.bite1),
                still.bothers.bite = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ still.bothers.bite1, T ~ still.bothers.bite),
                still.bothers.bite1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ still.bothers.bite, T ~ still.bothers.bite1))

df <- transform(df,
                Animal_Attack_age1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ Animal_Attack_age2, T ~ Animal_Attack_age1),
                Animal_Attack_age2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ Animal_Attack_age1, T ~ Animal_Attack_age2),
                what.bit.you1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ what.bit.you2, T ~ what.bit.you1),
                what.bit.you2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ what.bit.you1, T ~ what.bit.you2),
                where.bit.body1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ where.bit.body2, T ~ where.bit.body1),
                where.bit.body2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ where.bit.body1, T ~ where.bit.body2),
                activity.when.bit1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ activity.when.bit2, T ~ activity.when.bit1),
                activity.when.bit2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ activity.when.bit1, T ~ activity.when.bit2),
                days.disabled.bite1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ days.disabled.bite2, T ~ days.disabled.bite1),
                days.disabled.bite2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ days.disabled.bite1, T ~ days.disabled.bite2),
                almost.died.bite1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ almost.died.bite2, T ~ almost.died.bite1),
                almost.died.bite2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ almost.died.bite1, T ~ almost.died.bite2),
                still.bothers.bite1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ still.bothers.bite2, T ~ still.bothers.bite1),
                still.bothers.bite2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ still.bothers.bite1, T ~ still.bothers.bite2))

df <- transform(df,
                Animal_Attack_age2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ Animal_Attack_age3, T ~ Animal_Attack_age2),
                Animal_Attack_age3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ Animal_Attack_age2, T ~ Animal_Attack_age3),
                what.bit.you2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ what.bit.you3, T ~ what.bit.you2),
                what.bit.you3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ what.bit.you2, T ~ what.bit.you3),
                where.bit.body2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ where.bit.body3, T ~ where.bit.body2),
                where.bit.body3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ where.bit.body2, T ~ where.bit.body3),
                activity.when.bit2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ activity.when.bit3, T ~ activity.when.bit2),
                activity.when.bit3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ activity.when.bit2, T ~ activity.when.bit3),
                days.disabled.bite2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ days.disabled.bite3, T ~ days.disabled.bite2),
                days.disabled.bite3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ days.disabled.bite2, T ~ days.disabled.bite3),
                almost.died.bite2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ almost.died.bite3, T ~ almost.died.bite2),
                almost.died.bite3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ almost.died.bite2, T ~ almost.died.bite3),
                still.bothers.bite2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ still.bothers.bite3, T ~ still.bothers.bite2),
                still.bothers.bite3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ still.bothers.bite2, T ~ still.bothers.bite3))

df <- transform(df,
                Animal_Attack_age3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ Animal_Attack_age4, T ~ Animal_Attack_age3),
                Animal_Attack_age4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ Animal_Attack_age3, T ~ Animal_Attack_age4),
                what.bit.you3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ what.bit.you4, T ~ what.bit.you3),
                what.bit.you4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ what.bit.you3, T ~ what.bit.you4),
                where.bit.body3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ where.bit.body4, T ~ where.bit.body3),
                where.bit.body4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ where.bit.body3, T ~ where.bit.body4),
                activity.when.bit3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ activity.when.bit4, T ~ activity.when.bit3),
                activity.when.bit4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ activity.when.bit3, T ~ activity.when.bit4),
                days.disabled.bite3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ days.disabled.bite4, T ~ days.disabled.bite3),
                days.disabled.bite4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ days.disabled.bite3, T ~ days.disabled.bite4),
                almost.died.bite3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ almost.died.bite4, T ~ almost.died.bite3),
                almost.died.bite4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ almost.died.bite3, T ~ almost.died.bite4),
                still.bothers.bite3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ still.bothers.bite4, T ~ still.bothers.bite3),
                still.bothers.bite4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ still.bothers.bite3, T ~ still.bothers.bite4))

# Iteration 4/4
df <- transform(df,
                Animal_Attack_age = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ Animal_Attack_age1, T ~ Animal_Attack_age),
                Animal_Attack_age1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ Animal_Attack_age, T ~ Animal_Attack_age1),
                what.bit.you = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ what.bit.you1, T ~ what.bit.you),
                what.bit.you1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ what.bit.you, T ~ what.bit.you1),
                where.bit.body = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ where.bit.body1, T ~ where.bit.body),
                where.bit.body1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ where.bit.body, T ~ where.bit.body1),
                activity.when.bit = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ activity.when.bit1, T ~ activity.when.bit),
                activity.when.bit1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ activity.when.bit, T ~ activity.when.bit1),
                days.disabled.bite = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ days.disabled.bite1, T ~ days.disabled.bite),
                days.disabled.bite1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ days.disabled.bite, T ~ days.disabled.bite1),
                almost.died.bite = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ almost.died.bite1, T ~ almost.died.bite),
                almost.died.bite1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ almost.died.bite, T ~ almost.died.bite1),
                still.bothers.bite = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ still.bothers.bite1, T ~ still.bothers.bite),
                still.bothers.bite1 = case_when(Animal_Attack_age > Animal_Attack_age1 | is.na(Animal_Attack_age) ~ still.bothers.bite, T ~ still.bothers.bite1))

df <- transform(df,
                Animal_Attack_age1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ Animal_Attack_age2, T ~ Animal_Attack_age1),
                Animal_Attack_age2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ Animal_Attack_age1, T ~ Animal_Attack_age2),
                what.bit.you1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ what.bit.you2, T ~ what.bit.you1),
                what.bit.you2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ what.bit.you1, T ~ what.bit.you2),
                where.bit.body1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ where.bit.body2, T ~ where.bit.body1),
                where.bit.body2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ where.bit.body1, T ~ where.bit.body2),
                activity.when.bit1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ activity.when.bit2, T ~ activity.when.bit1),
                activity.when.bit2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ activity.when.bit1, T ~ activity.when.bit2),
                days.disabled.bite1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ days.disabled.bite2, T ~ days.disabled.bite1),
                days.disabled.bite2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ days.disabled.bite1, T ~ days.disabled.bite2),
                almost.died.bite1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ almost.died.bite2, T ~ almost.died.bite1),
                almost.died.bite2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ almost.died.bite1, T ~ almost.died.bite2),
                still.bothers.bite1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ still.bothers.bite2, T ~ still.bothers.bite1),
                still.bothers.bite2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 | is.na(Animal_Attack_age1) ~ still.bothers.bite1, T ~ still.bothers.bite2))

df <- transform(df,
                Animal_Attack_age2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ Animal_Attack_age3, T ~ Animal_Attack_age2),
                Animal_Attack_age3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ Animal_Attack_age2, T ~ Animal_Attack_age3),
                what.bit.you2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ what.bit.you3, T ~ what.bit.you2),
                what.bit.you3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ what.bit.you2, T ~ what.bit.you3),
                where.bit.body2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ where.bit.body3, T ~ where.bit.body2),
                where.bit.body3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ where.bit.body2, T ~ where.bit.body3),
                activity.when.bit2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ activity.when.bit3, T ~ activity.when.bit2),
                activity.when.bit3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ activity.when.bit2, T ~ activity.when.bit3),
                days.disabled.bite2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ days.disabled.bite3, T ~ days.disabled.bite2),
                days.disabled.bite3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ days.disabled.bite2, T ~ days.disabled.bite3),
                almost.died.bite2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ almost.died.bite3, T ~ almost.died.bite2),
                almost.died.bite3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ almost.died.bite2, T ~ almost.died.bite3),
                still.bothers.bite2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ still.bothers.bite3, T ~ still.bothers.bite2),
                still.bothers.bite3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 | is.na(Animal_Attack_age2) ~ still.bothers.bite2, T ~ still.bothers.bite3))

df <- transform(df,
                Animal_Attack_age3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ Animal_Attack_age4, T ~ Animal_Attack_age3),
                Animal_Attack_age4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ Animal_Attack_age3, T ~ Animal_Attack_age4),
                what.bit.you3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ what.bit.you4, T ~ what.bit.you3),
                what.bit.you4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ what.bit.you3, T ~ what.bit.you4),
                where.bit.body3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ where.bit.body4, T ~ where.bit.body3),
                where.bit.body4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ where.bit.body3, T ~ where.bit.body4),
                activity.when.bit3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ activity.when.bit4, T ~ activity.when.bit3),
                activity.when.bit4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ activity.when.bit3, T ~ activity.when.bit4),
                days.disabled.bite3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ days.disabled.bite4, T ~ days.disabled.bite3),
                days.disabled.bite4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ days.disabled.bite3, T ~ days.disabled.bite4),
                almost.died.bite3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ almost.died.bite4, T ~ almost.died.bite3),
                almost.died.bite4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ almost.died.bite3, T ~ almost.died.bite4),
                still.bothers.bite3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ still.bothers.bite4, T ~ still.bothers.bite3),
                still.bothers.bite4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 | is.na(Animal_Attack_age3) ~ still.bothers.bite3, T ~ still.bothers.bite4))

# No one has ever had a fifth occurrence, or has not reported a fifth age
df <- subset(df, select = -c(what.bit.you4, where.bit.body4, activity.when.bit4, days.disabled.bite4,
                             almost.died.bite4, still.bothers.bite4, Animal_Attack_age4))

# Export as csv
write.csv(df[c("pid", "Animal_Attack_age", "Animal_Attack_age1", "Animal_Attack_age2", "Animal_Attack_age3")],
          "Animal_Attack_combined_cleaned.csv", row.names = F)

# Counting process format -------------------------------------------------

# For splitting, we need ID, age, event, and the corresponding ages
df1 <- df[c("pid", "Animal_Attack_ever", "age", "Animal_Attack_age", "Animal_Attack_age1",
            "Animal_Attack_age2", "Animal_Attack_age3")]

# Creating the row splits
df1 <- df1 %>%
  mutate(start = 0, end = age) %>%
  dplyr::select(-Animal_Attack_ever) %>%
  gather(Animal_Attack_ever, enter, -pid) %>%
  group_by(pid) %>%
  arrange(pid, enter) %>%
  filter(!is.na(enter)) %>%
  mutate(exit = lead(enter)) %>%
  filter(!is.na(exit), !grepl("time_to_risk_out_start", Animal_Attack_ever)) %>%
  mutate(event = lead(grepl("time_to_event", Animal_Attack_ever), default = 0)) %>%
  dplyr::select(pid, enter, exit, event) %>%
  ungroup()

# Cleaning up
df1 <- subset(df1, enter != exit)

# Adding back age, male, and region columns
df2 <- df[c("pid", "age", "male", "region", "Animal_Attack_age", "Animal_Attack_age1",
            "Animal_Attack_age2", "Animal_Attack_age3")]
df1 <- left_join(df1, df2)

# If exit > age, delete current row and assign event = 1 to the preceding row
df1 <- df1 %>%
  mutate(event = case_when(lead(exit) > lead(age) ~ 1, TRUE ~ event))

# Remove the rows where exit > age
df1 <- subset(df1, exit <= age)

# If Animal Attack age matches exit, event = 1
df1$event <- ifelse(df1$exit == df1$Animal_Attack_age |
                      df1$exit == df1$Animal_Attack_age1 |
                      df1$exit == df1$Animal_Attack_age2 |
                      df1$exit == df1$Animal_Attack_age3, 1, 0)

# Remove NAs
df1$event <- ifelse(is.na(df1$event), 0, df1$event)

# Final Table
df1 <- df1[c("pid", "age", "male", "region", "enter", "exit", "event")]

rm(df2)

# Two more columns --------------------------------------------------------

# Creating column for time since last risk
df1$time.since.last.Animal_Attack <- ifelse(df1$enter == 0 & df1$exit <= df1$age,
                                        NA_real_, df1$exit - df1$enter)

# Create column for length of prior risk interval
df1$length.of.last.Animal_Attack <- lag(df1$exit) - lag(df1$enter)
df1$length.of.last.Animal_Attack <- ifelse(df1$enter == 0, NA_real_,
                                       df1$length.of.last.Animal_Attack)

# Add columns for other risks ---------------------------------------------

db6 <- read.csv("tree_fall_cleaned.csv")
ds1 <- read.csv("canoe_capsize_cleaned.csv")
sick_df3 <- read.csv("sickness_cleaned.csv")
fought_df3 <- read.csv("fought_cleaned.csv")
dc1 <- read.csv("cut_self_cleaned.csv")

## Tree Fall ----
df1 <- left_join(df1, db6)
df1$tree.fall.during.interval <- ifelse((df1$enter < df1$tf.age1 & df1$tf.age1 <= df1$exit) |
                                          (df1$enter < df1$tf.age2 & df1$tf.age2 <= df1$exit) |
                                          (df1$enter < df1$tf.age3 & df1$tf.age3 <= df1$exit), 1, 0)
df1$tree.fall.during.interval <- ifelse(is.na(df1$tree.fall.during.interval), 0, df1$tree.fall.during.interval)

## Sickness ----
df1 <- left_join(df1, sick_df3)
df1$sickness.during.interval <- ifelse((df1$enter < df1$sickness.age & df1$sickness.age <= df1$exit) |
                                         (df1$enter < df1$sickness.age1 & df1$sickness.age1 <= df1$exit) |
                                         (df1$enter < df1$sickness.age2 & df1$sickness.age2 <= df1$exit), 1, 0)
df1$sickness.during.interval <- ifelse(is.na(df1$sickness.during.interval), 0, df1$sickness.during.interval)

## Fight ----
df1 <- left_join(df1, fought_df3)
df1$fought.during.interval <- ifelse((df1$enter < df1$fought.age & df1$fought.age <= df1$exit) |
                                       (df1$enter < df1$fought.age1 & df1$fought.age1 <= df1$exit) |
                                       (df1$enter < df1$fought.age2 & df1$fought.age2 <= df1$exit), 1, 0)
df1$fought.during.interval <- ifelse(is.na(df1$fought.during.interval), 0, df1$fought.during.interval)

## Canoe Capsize ----
df1 <- left_join(df1, ds1)
df1$canoe.capsize.during.interval <- ifelse((df1$enter < df1$cc.age1 & df1$cc.age1 <= df1$exit) |
                                              (df1$enter < df1$cc.age2 & df1$cc.age2 <= df1$exit) |
                                              (df1$enter < df1$cc.age3 & df1$cc.age3 <= df1$exit), 1, 0)
df1$canoe.capsize.during.interval <- ifelse(is.na(df1$canoe.capsize.during.interval), 0, df1$canoe.capsize.during.interval)

## Cut Self ----
df1 <- left_join(df1, dc1)
df1$cut.self.during.interval <- ifelse((df1$enter < df1$cut.age1 & df1$cut.age1 <= df1$exit) |
                                         (df1$enter < df1$cut.age2 & df1$cut.age2 <= df1$exit) |
                                         (df1$enter < df1$cut.age3 & df1$cut.age3 <= df1$exit) |
                                         (df1$enter < df1$cut.age4 & df1$cut.age4 <= df1$exit) |
                                         (df1$enter < df1$cut.age5 & df1$cut.age5 <= df1$exit) |
                                         (df1$enter < df1$cut.age6 & df1$cut.age6 <= df1$exit), 1, 0)
df1$cut.self.during.interval <- ifelse(is.na(df1$cut.self.during.interval), 0, df1$cut.self.during.interval)

# Final Table
df1 <- df1[c("pid", "age", "male", "region", "enter", "exit", "event",
             "length.of.last.Animal_Attack", "time.since.last.Animal_Attack",
             "tree.fall.during.interval", "sickness.during.interval",
             "fought.during.interval", "canoe.capsize.during.interval",
             "cut.self.during.interval")]

rm(db6, sick_df3, fought_df3, ds1, dc1)

# Adding columns for risk co-occurrences ----------------------------------

## Tree Fall ----
df1$tree.fall.co_occurrence <- ifelse(df1$event == 1 & df1$tree.fall.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of tree fall
df1 <- df1 %>%
  mutate(tree.fall.co_occurrence.interval = case_when(tree.fall.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                      tree.fall.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                      tree.fall.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                      tree.fall.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                      tree.fall.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                      tree.fall.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                      tree.fall.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                      tree.fall.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))

## Fight ----

df1$fought.co_occurrence <- ifelse(df1$event == 1 & df1$fought.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of fight
df1 <- df1 %>%
  mutate(fought.co_occurrence.interval = case_when(fought.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                   fought.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                   fought.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                   fought.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                   fought.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                   fought.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                   fought.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                   fought.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))

## Sickness ----

df1$sickness.co_occurrence <- ifelse(df1$event == 1 & df1$sickness.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of fight
df1 <- df1 %>%
  mutate(sickness.co_occurrence.interval = case_when(sickness.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                     sickness.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))

## Canoe Capsize ----

df1$canoe.capsize.co_occurrence <- ifelse(df1$event == 1 & df1$canoe.capsize.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of canoe.capsize
df1 <- df1 %>%
  mutate(canoe.capsize.co_occurrence.interval = case_when(canoe.capsize.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                          canoe.capsize.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))

## Cut Self ----

df1$cut.self.co_occurrence <- ifelse(df1$event == 1 & df1$cut.self.during.interval == 1, 1, 0)

# Creating interval categories for co-occurrences of cut.self
df1 <- df1 %>%
  mutate(cut.self.co_occurrence.interval = case_when(cut.self.during.interval == 1 & event == 1 & exit >= 0 & exit < 10 ~ "0-9",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 10 & exit < 20 ~ "10-19",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 20 & exit < 30 ~ "20-29",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 30 & exit < 40 ~ "30-39",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 40 & exit < 50 ~ "40-49",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 50 & exit < 60 ~ "50-59",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 60 & exit < 70 ~ "60-69",
                                                     cut.self.during.interval == 1 & event == 1 & exit >= 70 & exit < 80 ~ "70-79"))


# Add column for number of occurrences per interval -----------------------
df1 <- left_join(df1, df[c("pid", "Animal_Attack_age", "Animal_Attack_age1",
                           "Animal_Attack_age2", "Animal_Attack_age3")])
df2 <- df1[c("pid", "exit", "event", "Animal_Attack_age", "Animal_Attack_age1",
             "Animal_Attack_age2", "Animal_Attack_age3")]

df3 <- df2 %>%
  filter(event == 1)

df3 <- df3 %>%
  rowwise() %>%
  mutate(n.Animal_Attack = sum(c_across(Animal_Attack_age:Animal_Attack_age3) == exit, na.rm = TRUE)) %>%
  ungroup()
df3$n.Animal_Attack <- ifelse(df3$n.Animal_Attack == 0, 1, df3$n.Animal_Attack)

df3 <- df3[c("pid", "exit", "n.Animal_Attack")]
df1 <- left_join(df1, df3)
df1 <- relocate(df1, n.Animal_Attack, .after = event)
df1$n.Animal_Attack <- ifelse(is.na(df1$n.Animal_Attack), 0, df1$n.Animal_Attack)

df1 <- subset(df1, select = -c(Animal_Attack_age, Animal_Attack_age1, Animal_Attack_age2, Animal_Attack_age3))

# Add household ID column -------------------------------------------------

h_id <- readxl::read_xls("add household ids_a.xls")
df1 <- left_join(df1, h_id)
df1 <- relocate(df1, house.id, .after = pid)


rm(df2, df3, h_id)



# Columns for cause, days disabled, etc. ----------------------------------
raw <- dplyr::select(df, c(pid, what.bit.you:still.bothers.bite, what.bit.you1:still.bothers.bite1,
                   what.bit.you2:still.bothers.bite2, what.bit.you3:still.bothers.bite3))
dx <- left_join(df1, raw)
rm(raw)
dx <- dx %>%
  filter(event == 1)
dx <- dx %>%
  group_by(pid) %>%
  mutate(index = 1:n(),
         cum = cumsum(n.Animal_Attack))
dx <- relocate(dx, index, cum, .after = event)

## What attacked you? ----

dx$what_Animal_Attacked_you_1 <- NA_character_
dx$what_Animal_Attacked_you_2 <- NA_character_
dx$what_Animal_Attacked_you_3 <- NA_character_

dx <- relocate(dx, c(what_Animal_Attacked_you_1, what_Animal_Attacked_you_2,
                     what_Animal_Attacked_you_3), .after = n.Animal_Attack)

# index = 1
dx <- dx %>%
  mutate(what_Animal_Attacked_you_1 = case_when(index == 1 & n.Animal_Attack == 1 ~ what.bit.you, T ~ as.character(what_Animal_Attacked_you_1)),
         what_Animal_Attacked_you_2 = case_when(index == 1 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(what_Animal_Attacked_you_2)),
         what_Animal_Attacked_you_3 = case_when(index == 1 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(what_Animal_Attacked_you_3)))

dx <- dx %>%
  mutate(what_Animal_Attacked_you_1 = case_when(index == 1 & n.Animal_Attack == 2 ~ what.bit.you, T ~ as.character(what_Animal_Attacked_you_1)),
         what_Animal_Attacked_you_2 = case_when(index == 1 & n.Animal_Attack == 2 ~ what.bit.you1, T ~ as.character(what_Animal_Attacked_you_2)),
         what_Animal_Attacked_you_3 = case_when(index == 1 & n.Animal_Attack == 2 ~ NA_character_, T ~ as.character(what_Animal_Attacked_you_3)))

dx <- dx %>%
  mutate(what_Animal_Attacked_you_1 = case_when(index == 1 & n.Animal_Attack == 3 ~ what.bit.you, T ~ as.character(what_Animal_Attacked_you_1)),
         what_Animal_Attacked_you_2 = case_when(index == 1 & n.Animal_Attack == 3 ~ what.bit.you1, T ~ as.character(what_Animal_Attacked_you_2)),
         what_Animal_Attacked_you_3 = case_when(index == 1 & n.Animal_Attack == 3 ~ what.bit.you2, T ~ as.character(what_Animal_Attacked_you_3)))

# index = 2
dx <- dx %>%
  mutate(what_Animal_Attacked_you_1 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ what.bit.you1, T ~ as.character(what_Animal_Attacked_you_1)),
         what_Animal_Attacked_you_2 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ NA_character_, T ~ as.character(what_Animal_Attacked_you_2)),
         what_Animal_Attacked_you_3 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ NA_character_, T ~ as.character(what_Animal_Attacked_you_3)))

dx <- dx %>%
  mutate(what_Animal_Attacked_you_1 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ what.bit.you2, T ~ as.character(what_Animal_Attacked_you_1)),
         what_Animal_Attacked_you_2 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ NA_character_, T ~ as.character(what_Animal_Attacked_you_2)),
         what_Animal_Attacked_you_3 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ NA_character_, T ~ as.character(what_Animal_Attacked_you_3)))

dx <- dx %>%
  mutate(what_Animal_Attacked_you_1 = case_when(index == 2 & n.Animal_Attack == 2 ~ what.bit.you1, T ~ as.character(what_Animal_Attacked_you_1)),
         what_Animal_Attacked_you_2 = case_when(index == 2 & n.Animal_Attack == 2 ~ what.bit.you2, T ~ as.character(what_Animal_Attacked_you_2)),
         what_Animal_Attacked_you_3 = case_when(index == 2 & n.Animal_Attack == 2 ~ NA_character_, T ~ as.character(what_Animal_Attacked_you_3)))

# index = 3
dx <- dx %>%
  mutate(what_Animal_Attacked_you_1 = case_when(index == 3 & n.Animal_Attack == 1 ~ what.bit.you2, T ~ as.character(what_Animal_Attacked_you_1)),
         what_Animal_Attacked_you_2 = case_when(index == 3 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(what_Animal_Attacked_you_2)),
         what_Animal_Attacked_you_3 = case_when(index == 3 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(what_Animal_Attacked_you_3)))

# index = 4
dx <- dx %>%
  mutate(what_Animal_Attacked_you_1 = case_when(index == 4 & n.Animal_Attack == 1 ~ what.bit.you3, T ~ as.character(what_Animal_Attacked_you_1)),
         what_Animal_Attacked_you_2 = case_when(index == 4 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(what_Animal_Attacked_you_2)),
         what_Animal_Attacked_you_3 = case_when(index == 4 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(what_Animal_Attacked_you_3)))

# Remove the old columns
dx <- subset(dx, select = -c(what.bit.you, what.bit.you1, what.bit.you2, what.bit.you3))


## Where attacked body? ----

dx$where_body_Animal_Attacked_1 <- NA_character_
dx$where_body_Animal_Attacked_2 <- NA_character_
dx$where_body_Animal_Attacked_3 <- NA_character_

dx <- relocate(dx, c(where_body_Animal_Attacked_1, where_body_Animal_Attacked_2,
                     where_body_Animal_Attacked_3), .after = what_Animal_Attacked_you_3)

# index = 1
dx <- dx %>%
  mutate(where_body_Animal_Attacked_1 = case_when(index == 1 & n.Animal_Attack == 1 ~ where.bit.body, T ~ as.character(where_body_Animal_Attacked_1)),
         where_body_Animal_Attacked_2 = case_when(index == 1 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(where_body_Animal_Attacked_2)),
         where_body_Animal_Attacked_3 = case_when(index == 1 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(where_body_Animal_Attacked_3)))

dx <- dx %>%
  mutate(where_body_Animal_Attacked_1 = case_when(index == 1 & n.Animal_Attack == 2 ~ where.bit.body, T ~ as.character(where_body_Animal_Attacked_1)),
         where_body_Animal_Attacked_2 = case_when(index == 1 & n.Animal_Attack == 2 ~ where.bit.body1, T ~ as.character(where_body_Animal_Attacked_2)),
         where_body_Animal_Attacked_3 = case_when(index == 1 & n.Animal_Attack == 2 ~ NA_character_, T ~ as.character(where_body_Animal_Attacked_3)))

dx <- dx %>%
  mutate(where_body_Animal_Attacked_1 = case_when(index == 1 & n.Animal_Attack == 3 ~ where.bit.body, T ~ as.character(where_body_Animal_Attacked_1)),
         where_body_Animal_Attacked_2 = case_when(index == 1 & n.Animal_Attack == 3 ~ where.bit.body1, T ~ as.character(where_body_Animal_Attacked_2)),
         where_body_Animal_Attacked_3 = case_when(index == 1 & n.Animal_Attack == 3 ~ where.bit.body2, T ~ as.character(where_body_Animal_Attacked_3)))

# index = 2
dx <- dx %>%
  mutate(where_body_Animal_Attacked_1 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ where.bit.body1, T ~ as.character(where_body_Animal_Attacked_1)),
         where_body_Animal_Attacked_2 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ NA_character_, T ~ as.character(where_body_Animal_Attacked_2)),
         where_body_Animal_Attacked_3 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ NA_character_, T ~ as.character(where_body_Animal_Attacked_3)))

dx <- dx %>%
  mutate(where_body_Animal_Attacked_1 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ where.bit.body2, T ~ as.character(where_body_Animal_Attacked_1)),
         where_body_Animal_Attacked_2 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ NA_character_, T ~ as.character(where_body_Animal_Attacked_2)),
         where_body_Animal_Attacked_3 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ NA_character_, T ~ as.character(where_body_Animal_Attacked_3)))

dx <- dx %>%
  mutate(where_body_Animal_Attacked_1 = case_when(index == 2 & n.Animal_Attack == 2 ~ where.bit.body1, T ~ as.character(where_body_Animal_Attacked_1)),
         where_body_Animal_Attacked_2 = case_when(index == 2 & n.Animal_Attack == 2 ~ where.bit.body2, T ~ as.character(where_body_Animal_Attacked_2)),
         where_body_Animal_Attacked_3 = case_when(index == 2 & n.Animal_Attack == 2 ~ NA_character_, T ~ as.character(where_body_Animal_Attacked_3)))

# index = 3
dx <- dx %>%
  mutate(where_body_Animal_Attacked_1 = case_when(index == 3 & n.Animal_Attack == 1 ~ where.bit.body2, T ~ as.character(where_body_Animal_Attacked_1)),
         where_body_Animal_Attacked_2 = case_when(index == 3 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(where_body_Animal_Attacked_2)),
         where_body_Animal_Attacked_3 = case_when(index == 3 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(where_body_Animal_Attacked_3)))

# index = 4
dx <- dx %>%
  mutate(where_body_Animal_Attacked_1 = case_when(index == 4 & n.Animal_Attack == 1 ~ where.bit.body3, T ~ as.character(where_body_Animal_Attacked_1)),
         where_body_Animal_Attacked_2 = case_when(index == 4 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(where_body_Animal_Attacked_2)),
         where_body_Animal_Attacked_3 = case_when(index == 4 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(where_body_Animal_Attacked_3)))

# Remove the old columns
dx <- subset(dx, select = -c(where.bit.body, where.bit.body1, where.bit.body2, where.bit.body3))


## Activity when attacked? ----
dx$activity_when_Animal_Attacked_1 <- NA_character_
dx$activity_when_Animal_Attacked_2 <- NA_character_
dx$activity_when_Animal_Attacked_3 <- NA_character_

dx <- relocate(dx, c(activity_when_Animal_Attacked_1, activity_when_Animal_Attacked_2,
                     activity_when_Animal_Attacked_3), .after = where_body_Animal_Attacked_3)

# index = 1
dx <- dx %>%
  mutate(activity_when_Animal_Attacked_1 = case_when(index == 1 & n.Animal_Attack == 1 ~ activity.when.bit, T ~ as.character(activity_when_Animal_Attacked_1)),
         activity_when_Animal_Attacked_2 = case_when(index == 1 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(activity_when_Animal_Attacked_2)),
         activity_when_Animal_Attacked_3 = case_when(index == 1 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(activity_when_Animal_Attacked_3)))

dx <- dx %>%
  mutate(activity_when_Animal_Attacked_1 = case_when(index == 1 & n.Animal_Attack == 2 ~ activity.when.bit, T ~ as.character(activity_when_Animal_Attacked_1)),
         activity_when_Animal_Attacked_2 = case_when(index == 1 & n.Animal_Attack == 2 ~ activity.when.bit1, T ~ as.character(activity_when_Animal_Attacked_2)),
         activity_when_Animal_Attacked_3 = case_when(index == 1 & n.Animal_Attack == 2 ~ NA_character_, T ~ as.character(activity_when_Animal_Attacked_3)))

dx <- dx %>%
  mutate(activity_when_Animal_Attacked_1 = case_when(index == 1 & n.Animal_Attack == 3 ~ activity.when.bit, T ~ as.character(activity_when_Animal_Attacked_1)),
         activity_when_Animal_Attacked_2 = case_when(index == 1 & n.Animal_Attack == 3 ~ activity.when.bit1, T ~ as.character(activity_when_Animal_Attacked_2)),
         activity_when_Animal_Attacked_3 = case_when(index == 1 & n.Animal_Attack == 3 ~ activity.when.bit2, T ~ as.character(activity_when_Animal_Attacked_3)))

# index = 2
dx <- dx %>%
  mutate(activity_when_Animal_Attacked_1 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ activity.when.bit1, T ~ as.character(activity_when_Animal_Attacked_1)),
         activity_when_Animal_Attacked_2 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ NA_character_, T ~ as.character(activity_when_Animal_Attacked_2)),
         activity_when_Animal_Attacked_3 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ NA_character_, T ~ as.character(activity_when_Animal_Attacked_3)))

dx <- dx %>%
  mutate(activity_when_Animal_Attacked_1 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ activity.when.bit2, T ~ as.character(activity_when_Animal_Attacked_1)),
         activity_when_Animal_Attacked_2 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ NA_character_, T ~ as.character(activity_when_Animal_Attacked_2)),
         activity_when_Animal_Attacked_3 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ NA_character_, T ~ as.character(activity_when_Animal_Attacked_3)))

dx <- dx %>%
  mutate(activity_when_Animal_Attacked_1 = case_when(index == 2 & n.Animal_Attack == 2 ~ activity.when.bit1, T ~ as.character(activity_when_Animal_Attacked_1)),
         activity_when_Animal_Attacked_2 = case_when(index == 2 & n.Animal_Attack == 2 ~ activity.when.bit2, T ~ as.character(activity_when_Animal_Attacked_2)),
         activity_when_Animal_Attacked_3 = case_when(index == 2 & n.Animal_Attack == 2 ~ NA_character_, T ~ as.character(activity_when_Animal_Attacked_3)))

# index = 3
dx <- dx %>%
  mutate(activity_when_Animal_Attacked_1 = case_when(index == 3 & n.Animal_Attack == 1 ~ activity.when.bit2, T ~ as.character(activity_when_Animal_Attacked_1)),
         activity_when_Animal_Attacked_2 = case_when(index == 3 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(activity_when_Animal_Attacked_2)),
         activity_when_Animal_Attacked_3 = case_when(index == 3 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(activity_when_Animal_Attacked_3)))

# index = 4
dx <- dx %>%
  mutate(activity_when_Animal_Attacked_1 = case_when(index == 4 & n.Animal_Attack == 1 ~ activity.when.bit3, T ~ as.character(activity_when_Animal_Attacked_1)),
         activity_when_Animal_Attacked_2 = case_when(index == 4 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(activity_when_Animal_Attacked_2)),
         activity_when_Animal_Attacked_3 = case_when(index == 4 & n.Animal_Attack == 1 ~ NA_character_, T ~ as.character(activity_when_Animal_Attacked_3)))

# Remove the old columns
dx <- subset(dx, select = -c(activity.when.bit, activity.when.bit1, activity.when.bit2, activity.when.bit3))


## Days disabled ----
dx$days_disabled_Animal_Attack_1 <- NA_real_
dx$days_disabled_Animal_Attack_2 <- NA_real_
dx$days_disabled_Animal_Attack_3 <- NA_real_

dx <- relocate(dx, c(days_disabled_Animal_Attack_1, days_disabled_Animal_Attack_2,
                     days_disabled_Animal_Attack_3), .after = activity_when_Animal_Attacked_3)

# index = 1
dx <- dx %>%
  mutate(days_disabled_Animal_Attack_1 = case_when(index == 1 & n.Animal_Attack == 1 ~ days.disabled.bite, T ~ as.numeric(days_disabled_Animal_Attack_1)),
         days_disabled_Animal_Attack_2 = case_when(index == 1 & n.Animal_Attack == 1 ~ NA_real_, T ~ as.numeric(days_disabled_Animal_Attack_2)),
         days_disabled_Animal_Attack_3 = case_when(index == 1 & n.Animal_Attack == 1 ~ NA_real_, T ~ as.numeric(days_disabled_Animal_Attack_3)))

dx <- dx %>%
  mutate(days_disabled_Animal_Attack_1 = case_when(index == 1 & n.Animal_Attack == 2 ~ days.disabled.bite, T ~ as.numeric(days_disabled_Animal_Attack_1)),
         days_disabled_Animal_Attack_2 = case_when(index == 1 & n.Animal_Attack == 2 ~ days.disabled.bite1, T ~ as.numeric(days_disabled_Animal_Attack_2)),
         days_disabled_Animal_Attack_3 = case_when(index == 1 & n.Animal_Attack == 2 ~ NA_real_, T ~ as.numeric(days_disabled_Animal_Attack_3)))

dx <- dx %>%
  mutate(days_disabled_Animal_Attack_1 = case_when(index == 1 & n.Animal_Attack == 3 ~ days.disabled.bite, T ~ as.numeric(days_disabled_Animal_Attack_1)),
         days_disabled_Animal_Attack_2 = case_when(index == 1 & n.Animal_Attack == 3 ~ days.disabled.bite1, T ~ as.numeric(days_disabled_Animal_Attack_2)),
         days_disabled_Animal_Attack_3 = case_when(index == 1 & n.Animal_Attack == 3 ~ days.disabled.bite2, T ~ as.numeric(days_disabled_Animal_Attack_3)))

# index = 2
dx <- dx %>%
  mutate(days_disabled_Animal_Attack_1 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ days.disabled.bite1, T ~ as.numeric(days_disabled_Animal_Attack_1)),
         days_disabled_Animal_Attack_2 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ NA_real_, T ~ as.numeric(days_disabled_Animal_Attack_2)),
         days_disabled_Animal_Attack_3 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ NA_real_, T ~ as.numeric(days_disabled_Animal_Attack_3)))

dx <- dx %>%
  mutate(days_disabled_Animal_Attack_1 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ days.disabled.bite2, T ~ as.numeric(days_disabled_Animal_Attack_1)),
         days_disabled_Animal_Attack_2 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ NA_real_, T ~ as.numeric(days_disabled_Animal_Attack_2)),
         days_disabled_Animal_Attack_3 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ NA_real_, T ~ as.numeric(days_disabled_Animal_Attack_3)))

dx <- dx %>%
  mutate(days_disabled_Animal_Attack_1 = case_when(index == 2 & n.Animal_Attack == 2 ~ days.disabled.bite1, T ~ as.numeric(days_disabled_Animal_Attack_1)),
         days_disabled_Animal_Attack_2 = case_when(index == 2 & n.Animal_Attack == 2 ~ days.disabled.bite2, T ~ as.numeric(days_disabled_Animal_Attack_2)),
         days_disabled_Animal_Attack_3 = case_when(index == 2 & n.Animal_Attack == 2 ~ NA_real_, T ~ as.numeric(days_disabled_Animal_Attack_3)))

# index = 3
dx <- dx %>%
  mutate(days_disabled_Animal_Attack_1 = case_when(index == 3 & n.Animal_Attack == 1 ~ days.disabled.bite2, T ~ as.numeric(days_disabled_Animal_Attack_1)),
         days_disabled_Animal_Attack_2 = case_when(index == 3 & n.Animal_Attack == 1 ~ NA_real_, T ~ as.numeric(days_disabled_Animal_Attack_2)),
         days_disabled_Animal_Attack_3 = case_when(index == 3 & n.Animal_Attack == 1 ~ NA_real_, T ~ as.numeric(days_disabled_Animal_Attack_3)))

# index = 4
dx <- dx %>%
  mutate(days_disabled_Animal_Attack_1 = case_when(index == 4 & n.Animal_Attack == 1 ~ days.disabled.bite3, T ~ as.numeric(days_disabled_Animal_Attack_1)),
         days_disabled_Animal_Attack_2 = case_when(index == 4 & n.Animal_Attack == 1 ~ NA_real_, T ~ as.numeric(days_disabled_Animal_Attack_2)),
         days_disabled_Animal_Attack_3 = case_when(index == 4 & n.Animal_Attack == 1 ~ NA_real_, T ~ as.numeric(days_disabled_Animal_Attack_3)))

# Remove the old columns
dx <- subset(dx, select = -c(days.disabled.bite, days.disabled.bite1, days.disabled.bite2, days.disabled.bite3))


## Almost died ----
dx$almost_died_Animal_Attack_1 <- NA_integer_
dx$almost_died_Animal_Attack_2 <- NA_integer_
dx$almost_died_Animal_Attack_3 <- NA_integer_

dx <- relocate(dx, c(almost_died_Animal_Attack_1, almost_died_Animal_Attack_2,
                     almost_died_Animal_Attack_3), .after = days_disabled_Animal_Attack_3)

# index = 1
dx <- dx %>%
  mutate(almost_died_Animal_Attack_1 = case_when(index == 1 & n.Animal_Attack == 1 ~ almost.died.bite, T ~ as.integer(almost_died_Animal_Attack_1)),
         almost_died_Animal_Attack_2 = case_when(index == 1 & n.Animal_Attack == 1 ~ NA_integer_, T ~ as.integer(almost_died_Animal_Attack_2)),
         almost_died_Animal_Attack_3 = case_when(index == 1 & n.Animal_Attack == 1 ~ NA_integer_, T ~ as.integer(almost_died_Animal_Attack_3)))

dx <- dx %>%
  mutate(almost_died_Animal_Attack_1 = case_when(index == 1 & n.Animal_Attack == 2 ~ almost.died.bite, T ~ as.integer(almost_died_Animal_Attack_1)),
         almost_died_Animal_Attack_2 = case_when(index == 1 & n.Animal_Attack == 2 ~ almost.died.bite1, T ~ as.integer(almost_died_Animal_Attack_2)),
         almost_died_Animal_Attack_3 = case_when(index == 1 & n.Animal_Attack == 2 ~ NA_integer_, T ~ as.integer(almost_died_Animal_Attack_3)))

dx <- dx %>%
  mutate(almost_died_Animal_Attack_1 = case_when(index == 1 & n.Animal_Attack == 3 ~ almost.died.bite, T ~ as.integer(almost_died_Animal_Attack_1)),
         almost_died_Animal_Attack_2 = case_when(index == 1 & n.Animal_Attack == 3 ~ almost.died.bite1, T ~ as.integer(almost_died_Animal_Attack_2)),
         almost_died_Animal_Attack_3 = case_when(index == 1 & n.Animal_Attack == 3 ~ almost.died.bite2, T ~ as.integer(almost_died_Animal_Attack_3)))

# index = 2
dx <- dx %>%
  mutate(almost_died_Animal_Attack_1 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ almost.died.bite1, T ~ as.integer(almost_died_Animal_Attack_1)),
         almost_died_Animal_Attack_2 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(almost_died_Animal_Attack_2)),
         almost_died_Animal_Attack_3 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(almost_died_Animal_Attack_3)))

dx <- dx %>%
  mutate(almost_died_Animal_Attack_1 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ almost.died.bite2, T ~ as.integer(almost_died_Animal_Attack_1)),
         almost_died_Animal_Attack_2 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(almost_died_Animal_Attack_2)),
         almost_died_Animal_Attack_3 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(almost_died_Animal_Attack_3)))

dx <- dx %>%
  mutate(almost_died_Animal_Attack_1 = case_when(index == 2 & n.Animal_Attack == 2 ~ almost.died.bite1, T ~ as.integer(almost_died_Animal_Attack_1)),
         almost_died_Animal_Attack_2 = case_when(index == 2 & n.Animal_Attack == 2 ~ almost.died.bite2, T ~ as.integer(almost_died_Animal_Attack_2)),
         almost_died_Animal_Attack_3 = case_when(index == 2 & n.Animal_Attack == 2 ~ NA_integer_, T ~ as.integer(almost_died_Animal_Attack_3)))

# index = 3
dx <- dx %>%
  mutate(almost_died_Animal_Attack_1 = case_when(index == 3 & n.Animal_Attack == 1 ~ almost.died.bite2, T ~ as.integer(almost_died_Animal_Attack_1)),
         almost_died_Animal_Attack_2 = case_when(index == 3 & n.Animal_Attack == 1 ~ NA_integer_, T ~ as.integer(almost_died_Animal_Attack_2)),
         almost_died_Animal_Attack_3 = case_when(index == 3 & n.Animal_Attack == 1 ~ NA_integer_, T ~ as.integer(almost_died_Animal_Attack_3)))

# index = 4
dx <- dx %>%
  mutate(almost_died_Animal_Attack_1 = case_when(index == 4 & n.Animal_Attack == 1 ~ almost.died.bite3, T ~ as.integer(almost_died_Animal_Attack_1)),
         almost_died_Animal_Attack_2 = case_when(index == 4 & n.Animal_Attack == 1 ~ NA_integer_, T ~ as.integer(almost_died_Animal_Attack_2)),
         almost_died_Animal_Attack_3 = case_when(index == 4 & n.Animal_Attack == 1 ~ NA_integer_, T ~ as.integer(almost_died_Animal_Attack_3)))

# Remove the old columns
dx <- subset(dx, select = -c(almost.died.bite, almost.died.bite1, almost.died.bite2, almost.died.bite3))


## Still bothers ----
dx$still_bothers_Animal_Attack_1 <- NA_integer_
dx$still_bothers_Animal_Attack_2 <- NA_integer_
dx$still_bothers_Animal_Attack_3 <- NA_integer_

dx <- relocate(dx, c(still_bothers_Animal_Attack_1, still_bothers_Animal_Attack_2,
                     still_bothers_Animal_Attack_3), .after = almost_died_Animal_Attack_3)

# index = 1
dx <- dx %>%
  mutate(still_bothers_Animal_Attack_1 = case_when(index == 1 & n.Animal_Attack == 1 ~ still.bothers.bite, T ~ as.integer(still_bothers_Animal_Attack_1)),
         still_bothers_Animal_Attack_2 = case_when(index == 1 & n.Animal_Attack == 1 ~ NA_integer_, T ~ as.integer(still_bothers_Animal_Attack_2)),
         still_bothers_Animal_Attack_3 = case_when(index == 1 & n.Animal_Attack == 1 ~ NA_integer_, T ~ as.integer(still_bothers_Animal_Attack_3)))

dx <- dx %>%
  mutate(still_bothers_Animal_Attack_1 = case_when(index == 1 & n.Animal_Attack == 2 ~ still.bothers.bite, T ~ as.integer(still_bothers_Animal_Attack_1)),
         still_bothers_Animal_Attack_2 = case_when(index == 1 & n.Animal_Attack == 2 ~ still.bothers.bite1, T ~ as.integer(still_bothers_Animal_Attack_2)),
         still_bothers_Animal_Attack_3 = case_when(index == 1 & n.Animal_Attack == 2 ~ NA_integer_, T ~ as.integer(still_bothers_Animal_Attack_3)))

dx <- dx %>%
  mutate(still_bothers_Animal_Attack_1 = case_when(index == 1 & n.Animal_Attack == 3 ~ still.bothers.bite, T ~ as.integer(still_bothers_Animal_Attack_1)),
         still_bothers_Animal_Attack_2 = case_when(index == 1 & n.Animal_Attack == 3 ~ still.bothers.bite1, T ~ as.integer(still_bothers_Animal_Attack_2)),
         still_bothers_Animal_Attack_3 = case_when(index == 1 & n.Animal_Attack == 3 ~ still.bothers.bite2, T ~ as.integer(still_bothers_Animal_Attack_3)))

# index = 2
dx <- dx %>%
  mutate(still_bothers_Animal_Attack_1 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ still.bothers.bite1, T ~ as.integer(still_bothers_Animal_Attack_1)),
         still_bothers_Animal_Attack_2 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(still_bothers_Animal_Attack_2)),
         still_bothers_Animal_Attack_3 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 2 ~ NA_integer_, T ~ as.integer(still_bothers_Animal_Attack_3)))

dx <- dx %>%
  mutate(still_bothers_Animal_Attack_1 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ still.bothers.bite2, T ~ as.integer(still_bothers_Animal_Attack_1)),
         still_bothers_Animal_Attack_2 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(still_bothers_Animal_Attack_2)),
         still_bothers_Animal_Attack_3 = case_when(index == 2 & n.Animal_Attack == 1 & cum == 3 ~ NA_integer_, T ~ as.integer(still_bothers_Animal_Attack_3)))

dx <- dx %>%
  mutate(still_bothers_Animal_Attack_1 = case_when(index == 2 & n.Animal_Attack == 2 ~ still.bothers.bite1, T ~ as.integer(still_bothers_Animal_Attack_1)),
         still_bothers_Animal_Attack_2 = case_when(index == 2 & n.Animal_Attack == 2 ~ still.bothers.bite2, T ~ as.integer(still_bothers_Animal_Attack_2)),
         still_bothers_Animal_Attack_3 = case_when(index == 2 & n.Animal_Attack == 2 ~ NA_integer_, T ~ as.integer(still_bothers_Animal_Attack_3)))

# index = 3
dx <- dx %>%
  mutate(still_bothers_Animal_Attack_1 = case_when(index == 3 & n.Animal_Attack == 1 ~ still.bothers.bite2, T ~ as.integer(still_bothers_Animal_Attack_1)),
         still_bothers_Animal_Attack_2 = case_when(index == 3 & n.Animal_Attack == 1 ~ NA_integer_, T ~ as.integer(still_bothers_Animal_Attack_2)),
         still_bothers_Animal_Attack_3 = case_when(index == 3 & n.Animal_Attack == 1 ~ NA_integer_, T ~ as.integer(still_bothers_Animal_Attack_3)))

# index = 4
dx <- dx %>%
  mutate(still_bothers_Animal_Attack_1 = case_when(index == 4 & n.Animal_Attack == 1 ~ still.bothers.bite3, T ~ as.integer(still_bothers_Animal_Attack_1)),
         still_bothers_Animal_Attack_2 = case_when(index == 4 & n.Animal_Attack == 1 ~ NA_integer_, T ~ as.integer(still_bothers_Animal_Attack_2)),
         still_bothers_Animal_Attack_3 = case_when(index == 4 & n.Animal_Attack == 1 ~ NA_integer_, T ~ as.integer(still_bothers_Animal_Attack_3)))

# Remove the old columns
dx <- subset(dx, select = -c(still.bothers.bite, still.bothers.bite1, still.bothers.bite2, still.bothers.bite3))


## Get back to original dataframe
df1 <- left_join(df1, dx)
df1 <- relocate(df1, c(what_Animal_Attacked_you_1:still_bothers_Animal_Attack_3), .after = time.since.last.Animal_Attack)
df1 <- subset(df1, select = -c(index, cum))

# Categorizing the age in the interval for an individual
df1 <- df1 %>%
  mutate(age.cat = case_when(exit > 0 & exit <= 5 ~ "0-5",
                             exit > 5 & exit <= 10 ~ "5-10",
                             exit > 10 & exit <= 15 ~ "10-15",
                             exit > 15 & exit <= 20 ~ "15-20",
                             exit > 20 & exit <= 25 ~ "20-25",
                             exit > 25 & exit <= 30 ~ "25-30",
                             exit > 30 & exit <= 35 ~ "30-35",
                             exit > 35 & exit <= 40 ~ "35-40",
                             exit > 40 & exit <= 45 ~ "40-45",
                             exit > 45 & exit <= 50 ~ "45-50",
                             exit > 50 & exit <= 55 ~ "50-55",
                             exit > 55 & exit <= 60 ~ "55-60",
                             exit > 60 ~ "60+"))

# Episodes ----------------------------------------------------------------

df1 <- df1 %>%
  mutate(event.episode = replace(cumsum(event), event == 0, 0), .by = pid, .after = event)

# Export final table to csv -----------------------------------------------

write.csv(df1, "Animal_Attack_combined_final_table.csv", row.names = F)

