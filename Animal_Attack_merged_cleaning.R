
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

df$activity.when.bit4 <- NA
df <- relocate(df, activity.when.bit4, .after = where.bit.body4)


# Arranging ages ----------------------------------------------------------

replicate(
  5,
  {
    df <- transform(df,
                     Animal_Attack_age = case_when(Animal_Attack_age > Animal_Attack_age1 ~ Animal_Attack_age1, T ~ Animal_Attack_age),
                     Animal_Attack_age1 = case_when(Animal_Attack_age > Animal_Attack_age1 ~ Animal_Attack_age, T ~ Animal_Attack_age1),
                     what.bit.you = case_when(Animal_Attack_age > Animal_Attack_age1 ~ what.bit.you1, T ~ what.bit.you),
                     what.bit.you1 = case_when(Animal_Attack_age > Animal_Attack_age1 ~ what.bit.you, T ~ what.bit.you1),
                     where.bit.body = case_when(Animal_Attack_age > Animal_Attack_age1 ~ where.bit.body1, T ~ where.bit.body),
                     where.bit.body1 = case_when(Animal_Attack_age > Animal_Attack_age1 ~ where.bit.body, T ~ where.bit.body1),
                     activity.when.bit = case_when(Animal_Attack_age > Animal_Attack_age1 ~ activity.when.bit1, T ~ activity.when.bit),
                     activity.when.bit1 = case_when(Animal_Attack_age > Animal_Attack_age1 ~ activity.when.bit, T ~ activity.when.bit1),
                     days.disabled.bite = case_when(Animal_Attack_age > Animal_Attack_age1 ~ days.disabled.bite1, T ~ days.disabled.bite),
                     days.disabled.bite1 = case_when(Animal_Attack_age > Animal_Attack_age1 ~ days.disabled.bite, T ~ days.disabled.bite1),
                     almost.died.bite = case_when(Animal_Attack_age > Animal_Attack_age1 ~ almost.died.bite1, T ~ almost.died.bite),
                     almost.died.bite1 = case_when(Animal_Attack_age > Animal_Attack_age1 ~ almost.died.bite, T ~ almost.died.bite1),
                     still.bothers.bite = case_when(Animal_Attack_age > Animal_Attack_age1 ~ still.bothers.bite1, T ~ still.bothers.bite),
                     still.bothers.bite1 = case_when(Animal_Attack_age > Animal_Attack_age1 ~ still.bothers.bite, T ~ still.bothers.bite1))

    df <- transform(df,
                     Animal_Attack_age1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 ~ Animal_Attack_age2, T ~ Animal_Attack_age1),
                     Animal_Attack_age2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 ~ Animal_Attack_age1, T ~ Animal_Attack_age2),
                     what.bit.you1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 ~ what.bit.you2, T ~ what.bit.you1),
                     what.bit.you2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 ~ what.bit.you1, T ~ what.bit.you2),
                     where.bit.body1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 ~ where.bit.body2, T ~ where.bit.body1),
                     where.bit.body2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 ~ where.bit.body1, T ~ where.bit.body2),
                     activity.when.bit1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 ~ activity.when.bit2, T ~ activity.when.bit1),
                     activity.when.bit2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 ~ activity.when.bit1, T ~ activity.when.bit2),
                     days.disabled.bite1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 ~ days.disabled.bite2, T ~ days.disabled.bite1),
                     days.disabled.bite2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 ~ days.disabled.bite1, T ~ days.disabled.bite2),
                     almost.died.bite1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 ~ almost.died.bite2, T ~ almost.died.bite1),
                     almost.died.bite2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 ~ almost.died.bite1, T ~ almost.died.bite2),
                     still.bothers.bite1 = case_when(Animal_Attack_age1 > Animal_Attack_age2 ~ still.bothers.bite2, T ~ still.bothers.bite1),
                     still.bothers.bite2 = case_when(Animal_Attack_age1 > Animal_Attack_age2 ~ still.bothers.bite1, T ~ still.bothers.bite2))

    df <- transform(df,
                     Animal_Attack_age2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 ~ Animal_Attack_age3, T ~ Animal_Attack_age2),
                     Animal_Attack_age3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 ~ Animal_Attack_age2, T ~ Animal_Attack_age3),
                     what.bit.you2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 ~ what.bit.you3, T ~ what.bit.you2),
                     what.bit.you3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 ~ what.bit.you2, T ~ what.bit.you3),
                     where.bit.body2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 ~ where.bit.body3, T ~ where.bit.body2),
                     where.bit.body3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 ~ where.bit.body2, T ~ where.bit.body3),
                     activity.when.bit2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 ~ activity.when.bit3, T ~ activity.when.bit2),
                     activity.when.bit3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 ~ activity.when.bit2, T ~ activity.when.bit3),
                     days.disabled.bite2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 ~ days.disabled.bite3, T ~ days.disabled.bite2),
                     days.disabled.bite3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 ~ days.disabled.bite2, T ~ days.disabled.bite3),
                     almost.died.bite2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 ~ almost.died.bite3, T ~ almost.died.bite2),
                     almost.died.bite3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 ~ almost.died.bite2, T ~ almost.died.bite3),
                     still.bothers.bite2 = case_when(Animal_Attack_age2 > Animal_Attack_age3 ~ still.bothers.bite3, T ~ still.bothers.bite2),
                     still.bothers.bite3 = case_when(Animal_Attack_age2 > Animal_Attack_age3 ~ still.bothers.bite2, T ~ still.bothers.bite3))

    df <- transform(df,
                     Animal_Attack_age3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 ~ Animal_Attack_age4, T ~ Animal_Attack_age3),
                     Animal_Attack_age4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 ~ Animal_Attack_age3, T ~ Animal_Attack_age4),
                     what.bit.you3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 ~ what.bit.you4, T ~ what.bit.you3),
                     what.bit.you4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 ~ what.bit.you3, T ~ what.bit.you4),
                     where.bit.body3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 ~ where.bit.body4, T ~ where.bit.body3),
                     where.bit.body4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 ~ where.bit.body3, T ~ where.bit.body4),
                     activity.when.bit3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 ~ activity.when.bit4, T ~ activity.when.bit3),
                     activity.when.bit4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 ~ activity.when.bit3, T ~ activity.when.bit4),
                     days.disabled.bite3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 ~ days.disabled.bite4, T ~ days.disabled.bite3),
                     days.disabled.bite4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 ~ days.disabled.bite3, T ~ days.disabled.bite4),
                     almost.died.bite3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 ~ almost.died.bite4, T ~ almost.died.bite3),
                     almost.died.bite4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 ~ almost.died.bite3, T ~ almost.died.bite4),
                     still.bothers.bite3 = case_when(Animal_Attack_age3 > Animal_Attack_age4 ~ still.bothers.bite4, T ~ still.bothers.bite3),
                     still.bothers.bite4 = case_when(Animal_Attack_age3 > Animal_Attack_age4 ~ still.bothers.bite3, T ~ still.bothers.bite4))
  }
)




dg <- subset(df, Animal_Attack_age > Animal_Attack_age3)





