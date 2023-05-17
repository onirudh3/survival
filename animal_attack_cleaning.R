library(tidyverse)
library(readxl)

# Working directory
setwd("C:/Users/oniru/OneDrive/Tsimane/Survival Data")

# Import raw data, this will import sheet "db"
animal_attack_df <- read_xls("threat_wide___sumACEs_for anirudh.xls")

# Create data frame with only the columns we need
animal_attack_df <- animal_attack_df[c("pid", "age", "male", "animal.attack.ever", "animal.attack.age", 
                         "animal.attack.age1")]

# View any duplicate rows
# View(animal_attack_df[duplicated(animal_attack_df$pid), ])
animal_attack_df[duplicated(animal_attack_df$pid), ]
# View(animal_attack_df[duplicated(animal_attack_df$pid) | duplicated(animal_attack_df$pid, fromLast = TRUE), ])
animal_attack_df[duplicated(animal_attack_df$pid) | duplicated(animal_attack_df$pid, fromLast = TRUE), ]

# Delete duplicate rows, keeping the latest observation for a person
# db1 <- db1[!duplicated(db$pid), ]
animal_attack_df <- animal_attack_df %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()

# Making sure that the ages are chronological for each observation
animal_attack_df1 <- animal_attack_df[c("animal.attack.age", "animal.attack.age1")]
animal_attack_df1[] <- t(apply(animal_attack_df1, 1, function(x) x[order(x)]))
animal_attack_df2 <- animal_attack_df[c("pid")]
animal_attack_df3 <- cbind(animal_attack_df2, animal_attack_df1)

# Have you been attacked more than once in the same interval?
# View(animal_attack_df3[(animal_attack_df3$animal.attack.age == animal_attack_df3$animal.attack.age1),])
animal_attack_df3[(animal_attack_df3$animal.attack.age == animal_attack_df3$animal.attack.age1),] # 0 people have

rm(animal_attack_df,animal_attack_df1,animal_attack_df2)