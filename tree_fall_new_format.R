# Libraries
library(tidyverse)
library(eha)
library(survival)

# Load data
df_final <- read.csv("treefall_final_table.csv")
raw_df <- read.csv("raw_data_no_duplicates.csv")
df_final <- df_final[c("pid", "age", "male", "region", "enter", "exit",
                       "event")]

#################### WORKING OUT RE-FORMATTING ISSUES ##########################

# Reformat the data frame
# ?survSplit()
cut_vector <- c(1:80)
df <- survSplit(df_final, cut = cut_vector, start = "enter", end = "exit",
                event = "event")

# Did it work correctly?
sum(raw_df$age)
# Exact sum of ages is 13,254.9397672827

raw_df$age_rounded_up <- ceiling(raw_df$age)
sum(raw_df$age_rounded_up)
# Shows 13451, ideally should match no. of rows in df which is showing 13463

# What are these 12 rows where the problem is coming from?
df_row <- df %>% count(pid)
raw_df <- left_join(raw_df, df_row)
View(raw_df[(raw_df$n != raw_df$age_rounded_up),])
# So 12 individuals reported precise decimal tree fall times, so it is creating
# extra interval
id_list <- c("ALJ4", "TTWM", "3TUC", "VCV2", "DJB7", "D7FT", "VHKK", "VBYN",
             "ISPN", "9XE5", "QBCD", "X9HY")

# Let us fix this so only last interval till age for an individual is not a year


######################### RE-FORMATTING ########################################

# Rounding up the tree fall times so that we get the desired final output
raw_df$tf.age1 <- ceiling(raw_df$tf.age1)
raw_df$tf.age2 <-  ceiling(raw_df$tf.age2)

# Let's see if it worked
# View(filter(raw_df, pid %in% id_list))

# Recycled code from tree fall cleaning script
# For splitting, we need ID, age, event, and the corresponding ages
db3 <- raw_df[c("pid", "tree.fall.ever", "age", "tf.age1", "tf.age2",
                "tf.age3")]

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
db5 <- raw_df[c("pid", "age", "tf.age1",
                "tf.age2", "tf.age3")]
db6 <- left_join(db4, db5, by = "pid")
db6$event <- ifelse(db6$exit == db6$age, 0, 1)

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
db6 <- db6[c("pid", "age", "region", "enter", "exit", "event")]

# Re-formatted with one year intervals
cut_vector <- c(1:80)
df <- survSplit(db6, cut = cut_vector, start = "enter", end = "exit",
                event = "event")

rm(db3, db4, db5, df_row, region_df, cut_vector, id_list)

# So we get 13454, the final pieces of the puzzle are beginning to reveal
# themselves. The three individuals who reported tree fall age = age.
df_final <- df_final[c("pid", "age", "region", "enter", "exit", "event")]
anti <- anti_join(db6, df_final)
plyr::count(anti$pid)
# 3WPX, DYJA, F9DJ are the problem, where age = age of tree fall

# Let's fix this
