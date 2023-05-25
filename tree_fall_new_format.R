# Libraries
library(tidyverse)
library(eha)
library(survival)

# Load data
df_final <- read.csv("treefall_final_table.csv")
df_final <- df_final[c("pid", "age", "male", "region", "enter", "exit", "event")]

# Reformat the data frame
# ?make.communal

db4 <- db3 %>% # db3 <- db1[c("pid", "tree.fall.ever", "age", "tf.age1", "tf.age2", "tf.age3")]
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

View(logrye)
View(logrye[, 2, drop = F])
View(scania)

scand <- make.communal(scania, logrye[, 2, drop = FALSE],
                       start = 1801.75)


df <- survSplit(df_final,
                cut = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
                        17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
                        31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44,
                        45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58,
                        59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72,
                        73, 74, 75, 76),
                start = "enter", end = "exit", event = "event")


# ?survSplit()

df %>%
  group_by(Year) %>%
  filter(dist <= 1100 & !duplicated(key)) %>%
  summarise(RR = sum(RR), dist = sum(dist))

