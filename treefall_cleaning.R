# Libraries
library(tidyverse)
library(readxl)

# Import raw data, this will import sheet "db"
db <- read_xls("threat_wide___sumACEs_for anirudh.xls")

# Create data frame with only the columns we need
db <- db[c("pid", "age", "male", "tree.fall.ever", "TIPO1",
           "other.serious.accident.age", "TIPO2",
           "other.serious.accident.age1", "TIPO3",
           "other.serious.accident.age2", "TIPO4",
           "other.serious.accident.age3", "TIPO5",
           "other.serious.accident.age5", "TIPO6",
           "other.serious.accident.age5")]

# Delete rows where no tree fall ever occurred
# db <- db[(!db$tree.fall.ever == 0), ]

# Creating column counting number of tree falls per person
db$n.tree.fall <- rowSums(db == "b", na.rm = T)

# Counting stuff
# plyr::count(db, c("TIPO4")) # There are no treefalls in TIPO4, TIPO5, TIPO6

# Get treefall age 1
db$tf.age1 <- if_else(db$TIPO1 == "b", db$other.serious.accident.age, NA_real_)

# Get treefall age 2
db$tf.age2 <- if_else(db$TIPO2 == "b", db$other.serious.accident.age1, NA_real_)

# Get treefall age 3
db$tf.age3 <- if_else(db$TIPO3 == "b", db$other.serious.accident.age2, NA_real_)

# Create a dataframe with the columns we want
db1 <- db[c("pid", "age", "male", "tree.fall.ever", "tf.age1", "tf.age2", "tf.age3", "n.tree.fall")]

# View any duplicate rows
# View(db1[duplicated(db1$pid), ])
db1[duplicated(db1$pid), ]
# View(db1[duplicated(db1$pid) | duplicated(db1$pid, fromLast = TRUE), ])
db1[duplicated(db1$pid) | duplicated(db1$pid, fromLast = TRUE), ]

# Delete duplicate rows, keeping the latest observation for a person
# db1 <- db1[!duplicated(db$pid), ]
db1 <- db1 %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()

# How many ever fell from tree?
plyr::count(db1, c("tree.fall.ever"))

# Create a table for "male" to later add to the final table
male_table <- db1[c("pid", "male")]

# Moving values so there is no NA in tf.age1
db1 <- dedupewider::na_move(db1, cols = names(db1)[grepl("^tf.age\\d$",
                                                         names(db1))])
# Trying to check if more than one tree fall occurred in one interval
# View(db1[(db1$tf.age1 == db1$tf.age2), ])
db1[(db1$tf.age1 == db1$tf.age2), ] # There are three rows where this happened for PIDs 5001, 5027, HRP4

# Sorting the ages horizontally
db2 <- db1[c("tf.age1", "tf.age2", "tf.age3")]
db2[] <- t(apply(db2, 1, function(x) x[order(x)]))
db1 <- db1[c("pid", "age", "male", "tree.fall.ever", "n.tree.fall")]
db1 <- cbind(db1, db2)
# ^This is the version of the table that has treefall age1, age2... just like
# the raw data for other mortality types

# How many out of 388 people experienced tree falls?
db1 %>%
  filter(tree.fall.ever == 1) %>%
  summarise(count = n_distinct(pid)) # 165 out of 388 people experienced tree fall

# For splitting, we need ID, age, event, and the corresponding ages
db3 <- db1[c("pid", "tree.fall.ever", "age", "tf.age1", "tf.age2", "tf.age3")]

# Export as csv
write.csv(db1, "raw_data_no_duplicates.csv", row.names = F)

# Creating the row splits
db4 <- db3 %>%
  mutate(start = 0, end = age) %>%
  select(-tree.fall.ever) %>%
  gather(tree.fall.ever, enter, -pid) %>%
  group_by(pid) %>%
  arrange(pid, enter) %>%
  filter(!is.na(enter)) %>%
  mutate(exit = lead(enter)) %>%
  filter(!is.na(exit), !grepl("time_to_risk_out_start", tree.fall.ever)) %>%
  mutate(event = lead(grepl("time_to_event", tree.fall.ever), default = 0)) %>%
  select(pid, enter, exit, event) %>%
  ungroup()

# Cleaning up
db4 <- subset(db4, enter != exit)
db5 <- db1[c("pid", "age", "n.tree.fall", "tf.age1", "tf.age2", "tf.age3")]
db6 <- left_join(db4, db5, by = "pid")
db6$event <- ifelse(db6$exit == db6$age, 0, 1)

# Sorting out the number of tree falls.
# Note that this is context specific.
# In this case, at most, 2 tree falls have occurred in an interval.
# And as mentioned earlier, only 3 observations document this.
db6$n.tree.fall <- ifelse(db6$n.tree.fall == 0 | db6$n.tree.fall == 1, db6$n.tree.fall,
                          ifelse(db6$tf.age1 == db6$tf.age2 & db6$tf.age1 == db6$exit, 2, 1))
db6$n.tree.fall <- ifelse(db6$event == 0, 0, db6$n.tree.fall)

# Adding region column
region_df <- read_xls("threat_wide___sumACEs_for anirudh.xls")
region_df <- region_df[c("pid", "region", "age")]
region_df <- region_df %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()
region_df <- region_df[c("pid", "region")]
db6 <- left_join(db6, region_df, by = "pid")

# Final table
db6 <- left_join(db6, male_table, by = "pid")
db6 <- db6[c("pid", "age", "male", "region", "enter", "exit", "event", "n.tree.fall")]
rm(male_table)
rm(db1, db2, db3, db4, db5)

# Export as csv
write.csv(db6, "treefall_cleaned.csv", row.names = F)
