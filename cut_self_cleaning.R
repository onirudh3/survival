# Libraries
library(tidyverse)
library(readxl)

# Import raw data, this will import sheet "dc"
dc <- read_xls("threat_wide___sumACEs_for anirudh.xls")

# Create data frame with only the columns we need
dc <- dc[c("pid", "age", "male","cut.self.ever", "TIPO1",
           "other.serious.accident.age", "TIPO2",
           "other.serious.accident.age1", "TIPO3",
           "other.serious.accident.age2", "TIPO4",
           "other.serious.accident.age3", "TIPO5",
           "other.serious.accident.age4", "TIPO6",
           "other.serious.accident.age5")]

# Delete rows where no cut self ever occurred
# dc <- dc[(!dc$cut.self.ever == 0), ]

# Creating column counting number of cut selfs per person
dc$n.cut.self <- rowSums(dc == "c", na.rm = T)

# Counting stuff
plyr::count(dc, c("TIPO6")) # There are no cut selfs in TIPO4, TIPO5, TIPO6

# Get cut self age 1
dc$cut.age1 <- if_else(dc$TIPO1 == "c", dc$other.serious.accident.age, NA_real_)

# Get cut self age 2
dc$cut.age2 <- if_else(dc$TIPO2 == "c", dc$other.serious.accident.age1, NA_real_)

# Get cut self age 3
dc$cut.age3 <- if_else(dc$TIPO3 == "c", dc$other.serious.accident.age2, NA_real_)

# Get cut self age 4
dc$cut.age4 <- if_else(dc$TIPO4 == "c", dc$other.serious.accident.age3, NA_real_)

# Get cut self age 5
dc$cut.age5 <- if_else(dc$TIPO5 == "c", dc$other.serious.accident.age4, NA_real_)

# Get cut self age 6
dc$cut.age6 <- if_else(dc$TIPO6 == "c", dc$other.serious.accident.age5, NA_real_)

# Create a dataframe with the columns we want
dc1 <- dc[c("pid", "age", "male", "cut.self.ever", "cut.age1", "cut.age2", "cut.age3",
            "cut.age4", "cut.age5", "cut.age6", "n.cut.self")]

# View any duplicate rows
# View(dc1[duplicated(dc1$pid), ])
dc1[duplicated(dc1$pid), ]
# View(dc1[duplicated(dc1$pid) | duplicated(dc1$pid, fromLast = TRUE), ])
dc1[duplicated(dc1$pid) | duplicated(dc1$pid, fromLast = TRUE), ]

# Delete duplicate rows, keeping the latest observation for a person
# dc1 <- dc1[!duplicated(dc$pid), ]
dc1 <- dc1 %>%
  group_by(pid) %>%
  filter(age == max(age)) %>%
  ungroup()

# Moving values so there is no NA in cut.age1
dc1 <- dedupewider::na_move(dc1, cols = names(dc1)[grepl("^cut.age\\d$",
                                                         names(dc1))])
# Trying to check if more than one cut self occurred in one interval
# View(dc1[(dc1$cut.age1 == dc1$cut.age2), ])
dc1[(dc1$cut.age1 == dc1$cut.age2), ] # There is one row where this happened

# Sorting the ages horizontally
dc2 <- dc1[c("cut.age1", "cut.age2", "cut.age3", "cut.age4", "cut.age5", "cut.age6")]
dc2[] <- t(apply(dc2, 1, function(x) x[order(x)]))
dc1 <- dc1[c("pid", "age", "male", "cut.self.ever", "n.cut.self")]
dc1 <- cbind(dc1, dc2)

dc1 <- dc1[c("pid", "cut.age1", "cut.age2", "cut.age3", "cut.age4", "cut.age5", "cut.age6")]
