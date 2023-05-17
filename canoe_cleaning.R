# Libraries
library(tidyverse)
library(readxl)

# Working directory
setwd("C:/Users/oniru/OneDrive/Tsimane/Survival Data")

# Import raw data, this will import sheet "ds"
ds <- read_xls("threat_wide___sumACEs_for anirudh.xls")

# Create data frame with only the columns we need
ds <- ds[c("pid", "age", "male","canoe.capsize.ever", "TIPO1", 
           "other.serious.accident.age", "TIPO2", 
           "other.serious.accident.age1", "TIPO3", 
           "other.serious.accident.age2", "TIPO4", 
           "other.serious.accident.age3", "TIPO5", 
           "other.serious.accident.age5", "TIPO6", 
           "other.serious.accident.age5")]

# Delete rows where no canoe capsize ever occurred
# ds <- ds[(!ds$canoe.capsize.ever == 0), ]

# Creating column counting number of canoe capsizes per person
ds$n.canoe.capsize <- rowSums(ds == "a", na.rm = T)

# Counting stuff
# plyr::count(ds, c("TIPO4")) # There are no canoe capsizes in TIPO4, TIPO5, TIPO6

# Get capsize age 1
ds$cc.age1 <- if_else(ds$TIPO1 == "a", ds$other.serious.accident.age, NA_real_)

# Get capsize age 2
ds$cc.age2 <- if_else(ds$TIPO2 == "a", ds$other.serious.accident.age1, NA_real_)

# Get capsize age 3
ds$cc.age3 <- if_else(ds$TIPO3 == "a", ds$other.serious.accident.age2, NA_real_)

# Create a dataframe with the columns we want
ds1 <- ds[c("pid", "age", "male", "canoe.capsize.ever", "cc.age1", "cc.age2", "cc.age3", "n.canoe.capsize")]

# View any duplicate rows
# View(ds1[duplicated(ds1$pid), ])
ds1[duplicated(ds1$pid), ]
# View(ds1[duplicated(ds1$pid) | duplicated(ds1$pid, fromLast = TRUE), ])
ds1[duplicated(ds1$pid) | duplicated(ds1$pid, fromLast = TRUE), ]

# Delete duplicate rows, keeping the latest observation for a person
# ds1 <- ds1[!duplicated(ds$pid), ]
ds1 <- ds1 %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()

# Moving values so there is no NA in cc.age1
ds1 <- dedupewider::na_move(ds1, cols = names(ds1)[grepl("^cc.age\\d$", 
                                                         names(ds1))])
# Trying to check if more than one canoe capsize occurred in one interval
# View(ds1[(ds1$cc.age1 == ds1$cc.age2), ])
ds1[(ds1$cc.age1 == ds1$cc.age2), ] # There is one row where this happened

# Sorting the ages horizontally
ds2 <- ds1[c("cc.age1", "cc.age2", "cc.age3")]
ds2[] <- t(apply(ds2, 1, function(x) x[order(x)]))
ds1 <- ds1[c("pid", "age", "male", "canoe.capsize.ever", "n.canoe.capsize")]
ds1 <- cbind(ds1, ds2)

ds1 <- ds1[c("pid", "cc.age1", "cc.age2", "cc.age3")]