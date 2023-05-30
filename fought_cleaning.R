library(tidyverse)
library(readxl)

# Import raw data, this will import sheet "db"
fought_df <- read_xls("threat_wide___sumACEs_for anirudh.xls")

# Create data frame with only the columns we need
fought_df <- fought_df[c("pid", "age", "male", "Fought.other", "fought.age",
                         "fought.age1",
                     "fought.age2")]

# View any duplicate rows
# View(fought_df[duplicated(fought_df$pid), ])
fought_df[duplicated(fought_df$pid), ]
# View(fought_df[duplicated(fought_df$pid) | duplicated(fought_df$pid,
# fromLast = TRUE), ])
fought_df[duplicated(fought_df$pid) | duplicated(fought_df$pid,
                                                 fromLast = TRUE), ]

# Delete duplicate rows, keeping the latest observation for a person
# db1 <- db1[!duplicated(db$pid), ]
fought_df <- fought_df %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()

# Making sure that the ages are chronological for each observation
fought_df1 <- fought_df[c("fought.age", "fought.age1", "fought.age2")]
fought_df1[] <- t(apply(fought_df1, 1, function(x) x[order(x)]))
fought_df2 <- fought_df[c("pid")]
fought_df3 <- cbind(fought_df2, fought_df1)

# Export as csv
write.csv(fought_df3, "fought_cleaned.csv", row.names = F)

# Have you fought more than once in the same interval?
# View(fought_df3[(fought_df3$fought.age == fought_df3$fought.age1),])
fought_df3[(fought_df3$fought.age == fought_df3$fought.age1),] # 7 people have

# Is age = fought age ever?
age_df <- fought_df[c("pid", "age")]

fought_df4 <- left_join(fought_df3, age_df)

View(fought_df4[(fought_df4$age == fought_df4$fought.age |
                   fought_df4$age == fought_df4$fought.age1 |
                   fought_df4$age == fought_df4$fought.age2),])
# Yes, for 2 individuals RXEC, PPB4
