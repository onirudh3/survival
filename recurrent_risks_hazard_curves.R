
# Libraries and data ------------------------------------------------------

library(tidyverse)
library(survival)
library(bshazard)


# Cut Self ----------------------------------------------------------------

df <- read.csv("cut_self_final_table.csv")

## Anderson-Gill ----
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}

df_surv <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()

df_surv <- df_surv %>%
  mutate(age.cat = case_when(time > 0 & time <= 5 ~ "0-5",
                             time > 5 & time <= 10 ~ "5-10",
                             time > 10 & time <= 15 ~ "10-15",
                             time > 15 & time <= 20 ~ "15-20",
                             time > 20 & time <= 25 ~ "20-25",
                             time > 25 & time <= 30 ~ "25-30",
                             time > 30 & time <= 35 ~ "30-35",
                             time > 35 & time <= 40 ~ "35-40",
                             time > 40 & time <= 45 ~ "40-45",
                             time > 45 & time <= 50 ~ "45-50",
                             time > 50 & time <= 55 ~ "50-55",
                             time > 55 & time <= 60 ~ "55-60",
                             time > 60 ~ "60+"))
df_surv$age.cat <- factor(df_surv$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))
df_surv <- df_surv %>%
  group_by(age.cat, male) %>%
  summarise(mean_hazard = mean(hazard))
df_surv <- df_surv %>% rename("sex" = "male")
df_surv <- df_surv %>% mutate(sex = case_when(sex == 0 ~ "F", T ~ "M"))
write.csv(df_surv, "cut_self_mean_hazard_anderson_gill.csv", row.names = F)

## Prentice-Williams[1] ----
df_surv <- group_by(df, male) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ strata(event.episode), data = ., verbose = FALSE))) %>%
  ungroup()

df_surv <- df_surv %>%
  mutate(age.cat = case_when(time > 0 & time <= 5 ~ "0-5",
                             time > 5 & time <= 10 ~ "5-10",
                             time > 10 & time <= 15 ~ "10-15",
                             time > 15 & time <= 20 ~ "15-20",
                             time > 20 & time <= 25 ~ "20-25",
                             time > 25 & time <= 30 ~ "25-30",
                             time > 30 & time <= 35 ~ "30-35",
                             time > 35 & time <= 40 ~ "35-40",
                             time > 40 & time <= 45 ~ "40-45",
                             time > 45 & time <= 50 ~ "45-50",
                             time > 50 & time <= 55 ~ "50-55",
                             time > 55 & time <= 60 ~ "55-60",
                             time > 60 ~ "60+"))
df_surv$age.cat <- factor(df_surv$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                                      "20-25", "25-30", "30-35", "35-40",
                                                      "40-45", "45-50", "50-55", "55-60",
                                                      "60+"))
df_surv <- df_surv %>%
  group_by(age.cat, male) %>%
  summarise(mean_hazard = mean(hazard))
df_surv <- df_surv %>% rename("sex" = "male")
df_surv <- df_surv %>% mutate(sex = case_when(sex == 0 ~ "F", T ~ "M"))
write.csv(df_surv, "cut_self_mean_hazard_prentice_williams_1.csv", row.names = F)

## Prentice-Williams[2] ---- ERROR
# df$enter <- 0
# df <- df %>%
#   mutate(exit = c(exit[1], diff(exit)), .by = pid)
# df_surv <- group_by(df, male) %>%
#   do(as.data.frame(bshazard(Surv(enter, exit, event) ~ strata(event.episode), data = ., verbose = FALSE))) %>%
#   ungroup()
#
# df_surv <- df_surv %>%
#   mutate(age.cat = case_when(time > 0 & time <= 5 ~ "0-5",
#                              time > 5 & time <= 10 ~ "5-10",
#                              time > 10 & time <= 15 ~ "10-15",
#                              time > 15 & time <= 20 ~ "15-20",
#                              time > 20 & time <= 25 ~ "20-25",
#                              time > 25 & time <= 30 ~ "25-30",
#                              time > 30 & time <= 35 ~ "30-35",
#                              time > 35 & time <= 40 ~ "35-40",
#                              time > 40 & time <= 45 ~ "40-45",
#                              time > 45 & time <= 50 ~ "45-50",
#                              time > 50 & time <= 55 ~ "50-55",
#                              time > 55 & time <= 60 ~ "55-60",
#                              time > 60 ~ "60+"))
# df_surv$age.cat <- factor(df_surv$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
#                                                       "20-25", "25-30", "30-35", "35-40",
#                                                       "40-45", "45-50", "50-55", "55-60",
#                                                       "60+"))
# df_surv <- df_surv %>%
#   group_by(age.cat, male) %>%
#   summarise(mean_hazard = mean(hazard))
# df_surv <- df_surv %>% rename("sex" = "male")
# df_surv <- df_surv %>% mutate(sex = case_when(sex == 0 ~ "F", T ~ "M"))
# write.csv(df_surv, "cut_self_mean_hazard_prentice_williams_2.csv", row.names = F)
