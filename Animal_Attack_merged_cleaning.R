
# Libraries and data import -----------------------------------------------

library(tidyverse)

# Read raw data
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

df$activity.when.bit4 <- NA
df <- relocate(df, activity.when.bit4, .after = where.bit.body4)

# Counting Process Format -------------------------------------------------

dg <- read.csv("data_new_format.csv")

