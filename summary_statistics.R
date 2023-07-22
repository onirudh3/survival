
# Libraries and data import -----------------------------------------------

library(tidyverse)

# Import data
df <- read.csv("data_new_format.csv")


# Tree Fall ---------------------------------------------------------------
df1 <- subset(df, tree.fall.during.interval == 1)
df1 <- df1 %>% 
  subset(!duplicated(pid))

# Sickness ----------------------------------------------------------------
df2 <- subset(df, sickness.during.interval == 1)
df2 <- df2 %>% 
  subset(!duplicated(pid))

# Animal Attack -----------------------------------------------------------
df3 <- subset(df, Animal_Attack.during.interval == 1)
df3 <- df3 %>% 
  subset(!duplicated(pid))

# Cut Self ----------------------------------------------------------------
df4 <- subset(df, cut.self.during.interval == 1)
df4 <- df4 %>% 
  subset(!duplicated(pid))

# Canoe Capsize -----------------------------------------------------------
df5 <- subset(df, canoe.capsize.during.interval == 1)
df5 <- df5 %>% 
  subset(!duplicated(pid))

# Fight -------------------------------------------------------------------
df6 <- subset(df, fought.during.interval == 1)
df6 <- df6 %>% 
  subset(!duplicated(pid))


# Summary statistics table ------------------------------------------------
summary_df <- data.frame("Tree Fall" = c(sum(df1$male == 1), sum(df1$male == 0), sum(df1$exit)),
                         "Sickness" = c(sum(df2$male == 1), sum(df2$male == 0), sum(df2$exit)),
                         "Animal Attack" = c(sum(df3$male == 1), sum(df3$male == 0), sum(df3$exit)),
                         "Cut Self" = c(sum(df4$male == 1), sum(df4$male == 0), sum(df4$exit)),
                         "Canoe Capsize" = c(sum(df5$male == 1), sum(df5$male == 0), sum(df5$exit)),
                         "Fight" = c(sum(df6$male == 1), sum(df6$male == 0), sum(df6$exit)))
summary_df <- round(summary_df, 2)
rownames(summary_df) <- c("Males Experiencing Risk", "Females Experiencing Risk", "Total No. of Risk Years")

stargazer::stargazer(summary_df, summary = F, )



