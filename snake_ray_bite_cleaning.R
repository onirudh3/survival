library(tidyverse)
library(readxl)

# Import raw data, this will import sheet "db"
snake_df <- read_xls("threat_wide___sumACEs_for anirudh.xls")

# Create data frame with only the columns we need
snake_df <- snake_df[c("pid", "age", "male", "snake.or.ray.bite.ever", "snake.or.ray.bite.age", "snake.or.ray.bite.age1",
           "snake.or.ray.bite.age2")]

# View any duplicate rows
# View(snake_df[duplicated(snake_df$pid), ])
snake_df[duplicated(snake_df$pid), ]
# View(snake_df[duplicated(snake_df$pid) | duplicated(snake_df$pid, fromLast = TRUE), ])
snake_df[duplicated(snake_df$pid) | duplicated(snake_df$pid, fromLast = TRUE), ]

# Delete duplicate rows, keeping the latest observation for a person
# db1 <- db1[!duplicated(db$pid), ]
snake_df <- snake_df %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()

# Making sure that the ages are chronological for each observation
snake_df1 <- snake_df[c("snake.or.ray.bite.age", "snake.or.ray.bite.age1", "snake.or.ray.bite.age2")]
snake_df1[] <- t(apply(snake_df1, 1, function(x) x[order(x)]))
snake_df2 <- snake_df[c("pid")]
snake_df3 <- cbind(snake_df2, snake_df1)

# Export as csv
write.csv(snake_df3, "snake_bite_cleaned.csv", row.names = F)

# Have you been bit more than once in the same interval?
# View(snake_df3[(snake_df3$snake.or.ray.bite.age == snake_df3$snake.or.ray.bite.age1),])
snake_df3[(snake_df3$snake.or.ray.bite.age == snake_df3$snake.or.ray.bite.age1),] # 9 people have

# Is age = snake/ray bite age ever?
age_df <- snake_df[c("pid", "age")]

snake_df4 <- left_join(snake_df3, age_df)

View(snake_df4[(snake_df4$age == snake_df4$snake.or.ray.bite.age |
                  snake_df4$age == snake_df4$snake.or.ray.bite.age1 |
                  snake_df4$age == snake_df4$snake.or.ray.bite.age2),])
# Yes, for 6 individuals RPWM, YWP3, U92T, V9Q4, FVQR, 8FLL
