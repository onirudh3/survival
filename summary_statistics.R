
# Libraries and data import -----------------------------------------------

library(tidyverse)


# Sickness ----------------------------------------------------------------

df <- read.csv("sickness_time_to_first_risk_short_interval.csv")
df <- subset(df, sickness.during.interval == 1)

df <- df %>%
  mutate(days_disabled_sickness_1 =
           case_when(!is.na(days_disabled_sickness_2) ~
                       days_disabled_sickness_1 +
                       days_disabled_sickness_2,
                     T ~ days_disabled_sickness_1))
df <- df %>%
  mutate(days_disabled_sickness_1 =
           case_when(!is.na(days_disabled_sickness_3) ~
                       days_disabled_sickness_1 +
                       days_disabled_sickness_3,
                     T ~ days_disabled_sickness_1))

perc_risk <- scales::percent(nrow(df) / 388, accuracy = 0.1)

first_exposure <- min(df$exit)

perc_report_disability <- scales::percent(summarise(subset(df, !is.na(days_disabled_sickness_1)),
                                                    perc = n() / nrow(df))[1, ], accuracy = 0.1)

median_days_disabled <- median(subset(df, !is.na(days_disabled_sickness_1))$days_disabled_sickness_1)

df_sickness <- data.frame(perc_risk, first_exposure, perc_report_disability, median_days_disabled)
rownames(df_sickness) <- c("Sickness")

# Cut Self ----------------------------------------------------------------

df <- read.csv("cut_self_time_to_first_risk_short_interval.csv")
df <- subset(df, cut.self.during.interval == 1)

df <- df %>%
  mutate(cut_self_days_disabled_1 =
           case_when(!is.na(cut_self_days_disabled_2) ~
                       cut_self_days_disabled_1 +
                       cut_self_days_disabled_2,
                     T ~ cut_self_days_disabled_1))
df <- df %>%
  mutate(cut_self_days_disabled_1 =
           case_when(!is.na(cut_self_days_disabled_3) ~
                       cut_self_days_disabled_1 +
                       cut_self_days_disabled_3,
                     T ~ cut_self_days_disabled_1))

perc_risk <- scales::percent(nrow(df) / 388, accuracy = 0.1)

first_exposure <- min(df$exit)

perc_report_disability <- scales::percent(summarise(subset(df, !is.na(cut_self_days_disabled_1)),
                                                    perc = n() / nrow(df))[1, ], accuracy = 0.1)

median_days_disabled <- median(subset(df, !is.na(cut_self_days_disabled_1))$cut_self_days_disabled_1)

df_cut_self <- data.frame(perc_risk, first_exposure, perc_report_disability, median_days_disabled)
rownames(df_cut_self) <- c("Cut Self")

# Animal Attack ----------------------------------------------------------------

df <- read.csv("Animal_Attack_time_to_first_risk_short_interval.csv")
df <- subset(df, Animal_Attack.during.interval == 1)

df <- df %>%
  mutate(days_disabled_Animal_Attacked_1 =
           case_when(!is.na(days_disabled_Animal_Attacked_2) ~
                       days_disabled_Animal_Attacked_1 +
                       days_disabled_Animal_Attacked_2,
                     T ~ days_disabled_Animal_Attacked_1))
df <- df %>%
  mutate(days_disabled_Animal_Attacked_1 =
           case_when(!is.na(days_disabled_Animal_Attacked_3) ~
                       days_disabled_Animal_Attacked_1 +
                       days_disabled_Animal_Attacked_3,
                     T ~ days_disabled_Animal_Attacked_1))

perc_risk <- scales::percent(nrow(df) / 388, accuracy = 0.1)

first_exposure <- min(df$exit)

perc_report_disability <- scales::percent(summarise(subset(df, !is.na(days_disabled_Animal_Attacked_1)),
                                                    perc = n() / nrow(df))[1, ], accuracy = 0.1)

median_days_disabled <- median(subset(df, !is.na(days_disabled_Animal_Attacked_1))$days_disabled_Animal_Attacked_1)

df_Animal_Attack <- data.frame(perc_risk, first_exposure, perc_report_disability, median_days_disabled)
rownames(df_Animal_Attack) <- c("Animal Attack")

# Tree Fall ----------------------------------------------------------------

df <- read.csv("tree_fall_time_to_first_risk_short_interval.csv")
df <- subset(df, tree.fall.during.interval == 1)

df <- df %>%
  mutate(tree_fall_days_disabled_1 =
           case_when(!is.na(tree_fall_days_disabled_2) ~
                       tree_fall_days_disabled_1 +
                       tree_fall_days_disabled_2,
                     T ~ tree_fall_days_disabled_1))

perc_risk <- scales::percent(nrow(df) / 388, accuracy = 0.1)

first_exposure <- min(df$exit)

perc_report_disability <- scales::percent(summarise(subset(df, !is.na(tree_fall_days_disabled_1)),
                                                    perc = n() / nrow(df))[1, ], accuracy = 0.1)

median_days_disabled <- median(subset(df, !is.na(tree_fall_days_disabled_1))$tree_fall_days_disabled_1)

df_tree_fall <- data.frame(perc_risk, first_exposure, perc_report_disability, median_days_disabled)
rownames(df_tree_fall) <- c("Tree Fall")

# Fight ----------------------------------------------------------------

df <- read.csv("fought_time_to_first_risk_short_interval.csv")
df <- subset(df, fought.during.interval == 1)

df <- df %>%
  mutate(fought_days_injured_1 =
           case_when(!is.na(fought_days_injured_2) ~
                       fought_days_injured_1 +
                       fought_days_injured_2,
                     T ~ fought_days_injured_1))
df <- df %>%
  mutate(fought_days_injured_1 =
           case_when(!is.na(fought_days_injured_3) ~
                       fought_days_injured_1 +
                       fought_days_injured_3,
                     T ~ fought_days_injured_1))

perc_risk <- scales::percent(nrow(df) / 388, accuracy = 0.1)

first_exposure <- min(df$exit)

perc_report_disability <- scales::percent(summarise(subset(df, !is.na(fought_days_injured_1)),
                                                    perc = n() / nrow(df))[1, ], accuracy = 0.1)

median_days_disabled <- median(subset(df, !is.na(fought_days_injured_1))$fought_days_injured_1)

df_fight <- data.frame(perc_risk, first_exposure, perc_report_disability, median_days_disabled)
rownames(df_fight) <- c("Fight")

# Canoe Capsize ----------------------------------------------------------------

df <- read.csv("canoe_capsize_time_to_first_risk_short_interval.csv")
df <- subset(df, canoe.capsize.during.interval == 1)

df <- df %>%
  mutate(canoe_capsize_days_disabled_1 =
           case_when(!is.na(canoe_capsize_days_disabled_2) ~
                       canoe_capsize_days_disabled_1 +
                       canoe_capsize_days_disabled_2,
                     T ~ canoe_capsize_days_disabled_1))

perc_risk <- scales::percent(nrow(df) / 388, accuracy = 0.1)

first_exposure <- min(df$exit)

perc_report_disability <- scales::percent(summarise(subset(df, !is.na(canoe_capsize_days_disabled_1)),
                                                    perc = n() / nrow(df))[1, ], accuracy = 0.1)

median_days_disabled <- median(subset(df, !is.na(canoe_capsize_days_disabled_1))$canoe_capsize_days_disabled_1)

df_canoe_capsize <- data.frame(perc_risk, first_exposure, perc_report_disability, median_days_disabled)
rownames(df_canoe_capsize) <- c("Canoe Capsize")



# Combine dfs -------------------------------------------------------------

combined_df <- bind_rows(df_sickness, df_cut_self, df_Animal_Attack, df_tree_fall,
                         df_fight, df_canoe_capsize)

stargazer::stargazer(combined_df, summary = F, flip = T)





