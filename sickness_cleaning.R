library(tidyverse)
library(readxl)

# Import raw data, this will import sheet "db"
sick_df <- read_xls("threat_wide___sumACEs_for anirudh.xls")

# Create data frame with only the columns we need
sick_df <- sick_df[c("pid", "age", "male", "sickness", "sickness.age", "sickness.age1",
           "sickness.age2")]

# View any duplicate rows
# View(sick_df[duplicated(sick_df$pid), ])
sick_df[duplicated(sick_df$pid), ]
# View(sick_df[duplicated(sick_df$pid) | duplicated(sick_df$pid, fromLast = TRUE), ])
sick_df[duplicated(sick_df$pid) | duplicated(sick_df$pid, fromLast = TRUE), ]

# Delete duplicate rows, keeping the latest observation for a person
# db1 <- db1[!duplicated(db$pid), ]
sick_df <- sick_df %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()

# Making sure that the ages are chronological for each observation
sick_df1 <- sick_df[c("sickness.age", "sickness.age1", "sickness.age2")]
sick_df1[] <- t(apply(sick_df1, 1, function(x) x[order(x)]))
sick_df2 <- sick_df[c("pid")]
sick_df3 <- cbind(sick_df2, sick_df1)

# Export as csv
write.csv(sick_df3, "sickness_cleaned.csv", row.names = F)

# Have you been sick more than once in the same interval?
# View(sick_df3[(sick_df3$sickness.age == sick_df3$sickness.age1),])
sick_df3[(sick_df3$sickness.age == sick_df3$sickness.age1),] # 17 people have

# Is age = sickness age ever?
age_df <- sick_df[c("pid", "age")]

sick_df4 <- left_join(sick_df3, age_df)

View(sick_df4[(sick_df4$age == sick_df4$sickness.age |
                          sick_df4$age == sick_df4$sickness.age1 |
                          sick_df4$age == sick_df4$sickness.age2),])
# Yes, for 31 individuals!
