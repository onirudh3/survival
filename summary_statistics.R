
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

df <- df %>%
  mutate(almost_died_sickness_1 = case_when(almost_died_sickness_1 == 1 |
                                              almost_died_sickness_2 == 1 |
                                              almost_died_sickness_3 == 1 ~ 1, T ~ almost_died_sickness_1))

sample <- nrow(df)

perc_risk <- scales::percent(nrow(df) / 388, accuracy = 1)

sample_18 <- nrow(subset(df, exit <= 18))

perc_risk_18 <- scales::percent(nrow(subset(df, exit <= 18)) / 388, accuracy = 1)

avg_n_risk <- mean(df$n.sickness)
min_n_risk <- min(df$n.sickness)
max_n_risk <- max(df$n.sickness)

first_exposure <- median(df$exit)
first_exposure_min <- min(df$exit)
first_exposure_max <- max(df$exit)

perc_report_disability <- scales::percent(summarise(subset(df, days_disabled_sickness_1 > 0),
                                                           perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died <- scales::percent(summarise(subset(df, almost_died_sickness_1 == 1),
                                                     perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_sample <- scales::percent(summarise(subset(df, days_disabled_sickness_1 > 0),
                                                    perc = n() / nrow(df))[1, ], accuracy = 1)

perc_almost_died_sample <- scales::percent(summarise(subset(df, almost_died_sickness_1 == 1),
                                              perc = n() / nrow(df))[1, ], accuracy = 1)

perc_report_disability_less_than_18 <- scales::percent(summarise(subset(df, days_disabled_sickness_1 > 0 & exit < 18),
                                                                 perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died_less_than_18 <- scales::percent(summarise(subset(df, almost_died_sickness_1 == 1 & exit < 18),
                                              perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_less_than_18_sample <- scales::percent(summarise(subset(df, days_disabled_sickness_1 > 0 & exit < 18),
                                                                 perc = n() / nrow(subset(df, exit < 18)))[1, ], accuracy = 1)

perc_almost_died_less_than_18_sample <- scales::percent(summarise(subset(df, almost_died_sickness_1 == 1 & exit < 18),
                                                           perc = n() / nrow(subset(df, exit < 18)))[1, ], accuracy = 1)

perc_report_disability_more_than_18 <- scales::percent(summarise(subset(df, days_disabled_sickness_1 > 0 & exit >= 18),
                                                                 perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died_more_than_18 <- scales::percent(summarise(subset(df, almost_died_sickness_1 == 1 & exit >= 18),
                                                           perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_more_than_18_sample <- scales::percent(summarise(subset(df, days_disabled_sickness_1 > 0 & exit >= 18),
                                                                        perc = n() / nrow(subset(df, exit >= 18)))[1, ], accuracy = 1)

perc_almost_died_more_than_18_sample <- scales::percent(summarise(subset(df, almost_died_sickness_1 == 1 & exit >= 18),
                                                                  perc = n() / nrow(subset(df, exit >= 18)))[1, ], accuracy = 1)





median_days_disabled <- median(subset(df, !is.na(days_disabled_sickness_1))$days_disabled_sickness_1)
min_days_disabled <- min(subset(df, !is.na(days_disabled_sickness_1))$days_disabled_sickness_1)
max_days_disabled <- max(subset(df, !is.na(days_disabled_sickness_1))$days_disabled_sickness_1)

df_sickness <- data.frame(sample, perc_risk, sample_18, perc_risk_18, avg_n_risk, min_n_risk, max_n_risk, first_exposure,
                          first_exposure_min, first_exposure_max, perc_report_disability,
                          perc_report_disability_sample, perc_almost_died, perc_almost_died_sample,
                          median_days_disabled, min_days_disabled, max_days_disabled,
                          perc_report_disability_less_than_18, perc_report_disability_less_than_18_sample,
                          perc_report_disability_more_than_18, perc_report_disability_more_than_18_sample,
                          perc_almost_died_less_than_18, perc_almost_died_less_than_18_sample,
                          perc_almost_died_more_than_18, perc_almost_died_more_than_18_sample)
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

df <- df %>%
  mutate(cut_self_almost_died_1 = case_when(cut_self_almost_died_1 == 1 |
                                              cut_self_almost_died_2 == 1 |
                                              cut_self_almost_died_3 == 1 ~ 1, T ~ cut_self_almost_died_1))

sample <- nrow(df)

perc_risk <- scales::percent(nrow(df) / 388, accuracy = 1)

sample_18 <- nrow(subset(df, exit <= 18))

perc_risk_18 <- scales::percent(nrow(subset(df, exit <= 18)) / 388, accuracy = 1)

avg_n_risk <- mean(df$n.cut.self)
min_n_risk <- min(df$n.cut.self)
max_n_risk <- max(df$n.cut.self)

first_exposure <- median(df$exit)
first_exposure_min <- min(df$exit)
first_exposure_max <- max(df$exit)

perc_report_disability <- scales::percent(summarise(subset(df, cut_self_days_disabled_1 > 0),
                                                    perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died <- scales::percent(summarise(subset(df, cut_self_almost_died_1 == 1),
                                              perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_sample <- scales::percent(summarise(subset(df, cut_self_days_disabled_1 > 0),
                                                    perc = n() / nrow(df))[1, ], accuracy = 1)

perc_almost_died_sample <- scales::percent(summarise(subset(df, cut_self_almost_died_1 == 1),
                                              perc = n() / nrow(df))[1, ], accuracy = 1)

perc_report_disability_less_than_18 <- scales::percent(summarise(subset(df, cut_self_days_disabled_1 > 0 & exit < 18),
                                                                 perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died_less_than_18 <- scales::percent(summarise(subset(df, cut_self_almost_died_1 == 1 & exit < 18),
                                                           perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_less_than_18_sample <- scales::percent(summarise(subset(df, cut_self_days_disabled_1 > 0 & exit < 18),
                                                                        perc = n() / nrow(subset(df, exit < 18)))[1, ], accuracy = 1)

perc_almost_died_less_than_18_sample <- scales::percent(summarise(subset(df, cut_self_almost_died_1 == 1 & exit < 18),
                                                                  perc = n() / nrow(subset(df, exit < 18)))[1, ], accuracy = 1)

perc_report_disability_more_than_18 <- scales::percent(summarise(subset(df, cut_self_days_disabled_1 > 0 & exit >= 18),
                                                                 perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died_more_than_18 <- scales::percent(summarise(subset(df, cut_self_almost_died_1 == 1 & exit >= 18),
                                                           perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_more_than_18_sample <- scales::percent(summarise(subset(df, cut_self_days_disabled_1 > 0 & exit >= 18),
                                                                        perc = n() / nrow(subset(df, exit >= 18)))[1, ], accuracy = 1)

perc_almost_died_more_than_18_sample <- scales::percent(summarise(subset(df, cut_self_almost_died_1 == 1 & exit >= 18),
                                                                  perc = n() / nrow(subset(df, exit >= 18)))[1, ], accuracy = 1)

median_days_disabled <- median(subset(df, !is.na(cut_self_days_disabled_1))$cut_self_days_disabled_1)
min_days_disabled <- min(subset(df, !is.na(cut_self_days_disabled_1))$cut_self_days_disabled_1)
max_days_disabled <- max(subset(df, !is.na(cut_self_days_disabled_1))$cut_self_days_disabled_1)

df_cut_self <- data.frame(sample, perc_risk, sample_18, perc_risk_18, avg_n_risk, min_n_risk, max_n_risk, first_exposure,
                          first_exposure_min, first_exposure_max, perc_report_disability,
                          perc_report_disability_sample, perc_almost_died, perc_almost_died_sample,
                          median_days_disabled, min_days_disabled, max_days_disabled,
                          perc_report_disability_less_than_18, perc_report_disability_less_than_18_sample,
                          perc_report_disability_more_than_18, perc_report_disability_more_than_18_sample,
                          perc_almost_died_less_than_18, perc_almost_died_less_than_18_sample,
                          perc_almost_died_more_than_18, perc_almost_died_more_than_18_sample)
rownames(df_cut_self) <- c("Cut Self")

# Animal Attack ----------------------------------------------------------------

df <- read.csv("Animal_Attack_combined_time_to_first_risk_short_interval.csv")
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

df <- df %>%
  mutate(almost_died_Animal_Attacked_1 = case_when(almost_died_Animal_Attacked_1 == 1 |
                                                     almost_died_Animal_Attacked_2 == 1 |
                                                     almost_died_Animal_Attacked_3 == 1 ~ 1,
                                                   T ~ almost_died_Animal_Attacked_1))

sample <- nrow(df)

perc_risk <- scales::percent(nrow(df) / 388, accuracy = 1)

sample_18 <- nrow(subset(df, exit <= 18))

perc_risk_18 <- scales::percent(nrow(subset(df, exit <= 18)) / 388, accuracy = 1)

avg_n_risk <- mean(df$n.Animal_Attack)
min_n_risk <- min(df$n.Animal_Attack)
max_n_risk <- max(df$n.Animal_Attack)

first_exposure <- median(df$exit)
first_exposure_min <- min(df$exit)
first_exposure_max <- max(df$exit)

perc_report_disability <- scales::percent(summarise(subset(df, days_disabled_Animal_Attacked_1 > 0),
                                                    perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died <- scales::percent(summarise(subset(df, almost_died_Animal_Attacked_1 == 1),
                                              perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_sample <- scales::percent(summarise(subset(df, days_disabled_Animal_Attacked_1 > 0),
                                                    perc = n() / nrow(df))[1, ], accuracy = 1)

perc_almost_died_sample <- scales::percent(summarise(subset(df, almost_died_Animal_Attacked_1 == 1),
                                              perc = n() / nrow(df))[1, ], accuracy = 1)

perc_report_disability_less_than_18 <- scales::percent(summarise(subset(df, days_disabled_Animal_Attacked_1 > 0 & exit < 18),
                                                                 perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died_less_than_18 <- scales::percent(summarise(subset(df, almost_died_Animal_Attacked_1 == 1 & exit < 18),
                                                           perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_less_than_18_sample <- scales::percent(summarise(subset(df, days_disabled_Animal_Attacked_1 > 0 & exit < 18),
                                                                        perc = n() / nrow(subset(df, exit < 18)))[1, ], accuracy = 1)

perc_almost_died_less_than_18_sample <- scales::percent(summarise(subset(df, almost_died_Animal_Attacked_1 == 1 & exit < 18),
                                                                  perc = n() / nrow(subset(df, exit < 18)))[1, ], accuracy = 1)

perc_report_disability_more_than_18 <- scales::percent(summarise(subset(df, days_disabled_Animal_Attacked_1 > 0 & exit >= 18),
                                                                 perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died_more_than_18 <- scales::percent(summarise(subset(df, almost_died_Animal_Attacked_1 == 1 & exit >= 18),
                                                           perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_more_than_18_sample <- scales::percent(summarise(subset(df, days_disabled_Animal_Attacked_1 > 0 & exit >= 18),
                                                                        perc = n() / nrow(subset(df, exit >= 18)))[1, ], accuracy = 1)

perc_almost_died_more_than_18_sample <- scales::percent(summarise(subset(df, almost_died_Animal_Attacked_1 == 1 & exit >= 18),
                                                                  perc = n() / nrow(subset(df, exit >= 18)))[1, ], accuracy = 1)

median_days_disabled <- median(subset(df, !is.na(days_disabled_Animal_Attacked_1))$days_disabled_Animal_Attacked_1)
min_days_disabled <- min(subset(df, !is.na(days_disabled_Animal_Attacked_1))$days_disabled_Animal_Attacked_1)
max_days_disabled <- max(subset(df, !is.na(days_disabled_Animal_Attacked_1))$days_disabled_Animal_Attacked_1)

df_Animal_Attack <- data.frame(sample, perc_risk, sample_18, perc_risk_18, avg_n_risk, min_n_risk, max_n_risk, first_exposure,
                               first_exposure_min, first_exposure_max, perc_report_disability,
                               perc_report_disability_sample, perc_almost_died, perc_almost_died_sample,
                               median_days_disabled, min_days_disabled, max_days_disabled,
                               perc_report_disability_less_than_18, perc_report_disability_less_than_18_sample,
                               perc_report_disability_more_than_18, perc_report_disability_more_than_18_sample,
                               perc_almost_died_less_than_18, perc_almost_died_less_than_18_sample,
                               perc_almost_died_more_than_18, perc_almost_died_more_than_18_sample)
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

df <- df %>%
  mutate(tree_fall_almost_died_1 = case_when(tree_fall_almost_died_1 == 1 |
                                               tree_fall_almost_died_2 == 1 ~ 1,
                                                   T ~ tree_fall_almost_died_1))

sample <- nrow(df)

perc_risk <- scales::percent(nrow(df) / 388, accuracy = 1)

sample_18 <- nrow(subset(df, exit <= 18))

perc_risk_18 <- scales::percent(nrow(subset(df, exit <= 18)) / 388, accuracy = 1)

avg_n_risk <- mean(df$n.tree.fall)
min_n_risk <- min(df$n.tree.fall)
max_n_risk <- max(df$n.tree.fall)

first_exposure <- median(df$exit)
first_exposure_min <- min(df$exit)
first_exposure_max <- max(df$exit)

perc_report_disability <- scales::percent(summarise(subset(df, tree_fall_days_disabled_1 > 0),
                                                    perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died <- scales::percent(summarise(subset(df, tree_fall_almost_died_1 == 1),
                                              perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_sample <- scales::percent(summarise(subset(df, tree_fall_days_disabled_1 > 0),
                                                    perc = n() / nrow(df))[1, ], accuracy = 1)

perc_almost_died_sample <- scales::percent(summarise(subset(df, tree_fall_almost_died_1 == 1),
                                              perc = n() / nrow(df))[1, ], accuracy = 1)

perc_report_disability_less_than_18 <- scales::percent(summarise(subset(df, tree_fall_days_disabled_1 > 0 & exit < 18),
                                                                 perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died_less_than_18 <- scales::percent(summarise(subset(df, tree_fall_almost_died_1 == 1 & exit < 18),
                                                           perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_less_than_18_sample <- scales::percent(summarise(subset(df, tree_fall_days_disabled_1 > 0 & exit < 18),
                                                                        perc = n() / nrow(subset(df, exit < 18)))[1, ], accuracy = 1)

perc_almost_died_less_than_18_sample <- scales::percent(summarise(subset(df, tree_fall_almost_died_1 == 1 & exit < 18),
                                                                  perc = n() / nrow(subset(df, exit < 18)))[1, ], accuracy = 1)

perc_report_disability_more_than_18 <- scales::percent(summarise(subset(df, tree_fall_days_disabled_1 > 0 & exit >= 18),
                                                                 perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died_more_than_18 <- scales::percent(summarise(subset(df, tree_fall_almost_died_1 == 1 & exit >= 18),
                                                           perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_more_than_18_sample <- scales::percent(summarise(subset(df, tree_fall_days_disabled_1 > 0 & exit >= 18),
                                                                        perc = n() / nrow(subset(df, exit >= 18)))[1, ], accuracy = 1)

perc_almost_died_more_than_18_sample <- scales::percent(summarise(subset(df, tree_fall_almost_died_1 == 1 & exit >= 18),
                                                                  perc = n() / nrow(subset(df, exit >= 18)))[1, ], accuracy = 1)

median_days_disabled <- median(subset(df, !is.na(tree_fall_days_disabled_1))$tree_fall_days_disabled_1)
min_days_disabled <- min(subset(df, !is.na(tree_fall_days_disabled_1))$tree_fall_days_disabled_1)
max_days_disabled <- max(subset(df, !is.na(tree_fall_days_disabled_1))$tree_fall_days_disabled_1)

df_tree_fall <- data.frame(sample, perc_risk, sample_18, perc_risk_18, avg_n_risk, min_n_risk, max_n_risk, first_exposure,
                           first_exposure_min, first_exposure_max, perc_report_disability,
                           perc_report_disability_sample, perc_almost_died, perc_almost_died_sample,
                           median_days_disabled, min_days_disabled, max_days_disabled,
                           perc_report_disability_less_than_18, perc_report_disability_less_than_18_sample,
                           perc_report_disability_more_than_18, perc_report_disability_more_than_18_sample,
                           perc_almost_died_less_than_18, perc_almost_died_less_than_18_sample,
                           perc_almost_died_more_than_18, perc_almost_died_more_than_18_sample)
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

df <- df %>%
  mutate(fought_almost_died_1 = case_when(fought_almost_died_1 == 1 |
                                            fought_almost_died_2 == 1 |
                                            fought_almost_died_3 == 1 ~ 1,
                                                   T ~ fought_almost_died_1))

sample <- nrow(df)

perc_risk <- scales::percent(nrow(df) / 388, accuracy = 1)

sample_18 <- nrow(subset(df, exit <= 18))

perc_risk_18 <- scales::percent(nrow(subset(df, exit <= 18)) / 388, accuracy = 1)

avg_n_risk <- mean(df$n.fought)
min_n_risk <- min(df$n.fought)
max_n_risk <- max(df$n.fought)

first_exposure <- median(df$exit)
first_exposure_min <- min(df$exit)
first_exposure_max <- max(df$exit)

perc_report_disability <- scales::percent(summarise(subset(df, fought_days_injured_1 > 0),
                                                    perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died <- scales::percent(summarise(subset(df, fought_almost_died_1 == 1),
                                              perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_sample <- scales::percent(summarise(subset(df, fought_days_injured_1 > 0),
                                                    perc = n() / nrow(df))[1, ], accuracy = 1)

perc_almost_died_sample <- scales::percent(summarise(subset(df, fought_almost_died_1 == 1),
                                              perc = n() / nrow(df))[1, ], accuracy = 1)

perc_report_disability_less_than_18 <- scales::percent(summarise(subset(df, fought_days_injured_1 > 0 & exit < 18),
                                                                 perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died_less_than_18 <- scales::percent(summarise(subset(df, fought_almost_died_1 == 1 & exit < 18),
                                                           perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_less_than_18_sample <- scales::percent(summarise(subset(df, fought_days_injured_1 > 0 & exit < 18),
                                                                        perc = n() / nrow(subset(df, exit < 18)))[1, ], accuracy = 1)

perc_almost_died_less_than_18_sample <- scales::percent(summarise(subset(df, fought_almost_died_1 == 1 & exit < 18),
                                                                  perc = n() / nrow(subset(df, exit < 18)))[1, ], accuracy = 1)

perc_report_disability_more_than_18 <- scales::percent(summarise(subset(df, fought_days_injured_1 > 0 & exit >= 18),
                                                                 perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died_more_than_18 <- scales::percent(summarise(subset(df, fought_almost_died_1 == 1 & exit >= 18),
                                                           perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_more_than_18_sample <- scales::percent(summarise(subset(df, fought_days_injured_1 > 0 & exit >= 18),
                                                                        perc = n() / nrow(subset(df, exit >= 18)))[1, ], accuracy = 1)

perc_almost_died_more_than_18_sample <- scales::percent(summarise(subset(df, fought_almost_died_1 == 1 & exit >= 18),
                                                                  perc = n() / nrow(subset(df, exit >= 18)))[1, ], accuracy = 1)

median_days_disabled <- median(subset(df, !is.na(fought_days_injured_1))$fought_days_injured_1)
min_days_disabled <- min(subset(df, !is.na(fought_days_injured_1))$fought_days_injured_1)
max_days_disabled <- max(subset(df, !is.na(fought_days_injured_1))$fought_days_injured_1)

df_fight <- data.frame(sample, perc_risk, sample_18, perc_risk_18, avg_n_risk, min_n_risk, max_n_risk, first_exposure,
                       first_exposure_min, first_exposure_max, perc_report_disability,
                       perc_report_disability_sample, perc_almost_died, perc_almost_died_sample,
                       median_days_disabled, min_days_disabled, max_days_disabled,
                       perc_report_disability_less_than_18, perc_report_disability_less_than_18_sample,
                       perc_report_disability_more_than_18, perc_report_disability_more_than_18_sample,
                       perc_almost_died_less_than_18, perc_almost_died_less_than_18_sample,
                       perc_almost_died_more_than_18, perc_almost_died_more_than_18_sample)
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

df <- df %>%
  mutate(canoe_capsize_almost_died_1 = case_when(canoe_capsize_almost_died_1 == 1 |
                                                   canoe_capsize_almost_died_2 == 1 ~ 1,
                                             T ~ canoe_capsize_almost_died_1))

sample <- nrow(df)

perc_risk <- scales::percent(nrow(df) / 388, accuracy = 1)

sample_18 <- nrow(subset(df, exit <= 18))

perc_risk_18 <- scales::percent(nrow(subset(df, exit <= 18)) / 388, accuracy = 1)

avg_n_risk <- mean(df$n.canoe.capsize)
min_n_risk <- min(df$n.canoe.capsize)
max_n_risk <- max(df$n.canoe.capsize)

first_exposure <- median(df$exit)
first_exposure_min <- min(df$exit)
first_exposure_max <- max(df$exit)

perc_report_disability <- scales::percent(summarise(subset(df, canoe_capsize_days_disabled_1 > 0),
                                                    perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died <- scales::percent(summarise(subset(df, canoe_capsize_almost_died_1 == 1),
                                              perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_sample <- scales::percent(summarise(subset(df, canoe_capsize_days_disabled_1 > 0),
                                                    perc = n() / nrow(df))[1, ], accuracy = 1)

perc_almost_died_sample <- scales::percent(summarise(subset(df, canoe_capsize_almost_died_1 == 1),
                                              perc = n() / nrow(df))[1, ], accuracy = 1)

perc_report_disability_less_than_18 <- scales::percent(summarise(subset(df, canoe_capsize_days_disabled_1 > 0 & exit < 18),
                                                                 perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died_less_than_18 <- scales::percent(summarise(subset(df, canoe_capsize_almost_died_1 == 1 & exit < 18),
                                                           perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_less_than_18_sample <- scales::percent(summarise(subset(df, canoe_capsize_days_disabled_1 > 0 & exit < 18),
                                                                        perc = n() / nrow(subset(df, exit < 18)))[1, ], accuracy = 1)

perc_almost_died_less_than_18_sample <- scales::percent(summarise(subset(df, canoe_capsize_almost_died_1 == 1 & exit < 18),
                                                                  perc = n() / nrow(subset(df, exit < 18)))[1, ], accuracy = 1)

perc_report_disability_more_than_18 <- scales::percent(summarise(subset(df, canoe_capsize_days_disabled_1 > 0 & exit >= 18),
                                                                 perc = n() / 388)[1, ], accuracy = 1)

perc_almost_died_more_than_18 <- scales::percent(summarise(subset(df, canoe_capsize_almost_died_1 == 1 & exit >= 18),
                                                           perc = n() / 388)[1, ], accuracy = 1)

perc_report_disability_more_than_18_sample <- scales::percent(summarise(subset(df, canoe_capsize_days_disabled_1 > 0 & exit >= 18),
                                                                        perc = n() / nrow(subset(df, exit >= 18)))[1, ], accuracy = 1)

perc_almost_died_more_than_18_sample <- scales::percent(summarise(subset(df, canoe_capsize_almost_died_1 == 1 & exit >= 18),
                                                                  perc = n() / nrow(subset(df, exit >= 18)))[1, ], accuracy = 1)

median_days_disabled <- median(subset(df, !is.na(canoe_capsize_days_disabled_1))$canoe_capsize_days_disabled_1)
min_days_disabled <- min(subset(df, !is.na(canoe_capsize_days_disabled_1))$canoe_capsize_days_disabled_1)
max_days_disabled <- max(subset(df, !is.na(canoe_capsize_days_disabled_1))$canoe_capsize_days_disabled_1)

df_canoe_capsize <- data.frame(sample, perc_risk, sample_18, perc_risk_18, avg_n_risk, min_n_risk, max_n_risk, first_exposure,
                               first_exposure_min, first_exposure_max, perc_report_disability,
                               perc_report_disability_sample, perc_almost_died, perc_almost_died_sample,
                               median_days_disabled, min_days_disabled, max_days_disabled,
                               perc_report_disability_less_than_18, perc_report_disability_less_than_18_sample,
                               perc_report_disability_more_than_18, perc_report_disability_more_than_18_sample,
                               perc_almost_died_less_than_18, perc_almost_died_less_than_18_sample,
                               perc_almost_died_more_than_18, perc_almost_died_more_than_18_sample)
rownames(df_canoe_capsize) <- c("Canoe Capsize")




# No. of occurrences entire data ------------------------------------------

dc <- read.csv("data_new_format.csv")

dx <- dc %>%
  group_by(pid) %>%
  summarise(sum_n_sickness = sum(n.sickness),
            sum_n_cut_self = sum(n.cut.self),
            sum_n_Animal_Attack = sum(n.Animal_Attack),
            sum_n_tree_fall = sum(n.tree.fall),
            sum_n_fight = sum(n.fought),
            sum_n_canoe_capsize = sum(n.canoe.capsize))

df1 <- dx %>%
  summarise(avg_n_risk_individual = mean(sum_n_sickness),
            min_n_risk_individual = min(sum_n_sickness),
            max_n_risk_individual = max(sum_n_sickness))

df_sickness <- cbind(df_sickness, df1)

df2 <- dx %>%
  summarise(avg_n_risk_individual = mean(sum_n_cut_self),
            min_n_risk_individual = min(sum_n_cut_self),
            max_n_risk_individual = max(sum_n_cut_self))

df_cut_self <- cbind(df_cut_self, df2)

df3 <- dx %>%
  summarise(avg_n_risk_individual = mean(sum_n_Animal_Attack),
            min_n_risk_individual = min(sum_n_Animal_Attack),
            max_n_risk_individual = max(sum_n_Animal_Attack))

df_Animal_Attack <- cbind(df_Animal_Attack, df3)

df4 <- dx %>%
  summarise(avg_n_risk_individual = mean(sum_n_tree_fall),
            min_n_risk_individual = min(sum_n_tree_fall),
            max_n_risk_individual = max(sum_n_tree_fall))

df_tree_fall <- cbind(df_tree_fall, df4)

df5 <- dx %>%
  summarise(avg_n_risk_individual = mean(sum_n_fight),
            min_n_risk_individual = min(sum_n_fight),
            max_n_risk_individual = max(sum_n_fight))

df_fight <- cbind(df_fight, df5)

df6 <- dx %>%
  summarise(avg_n_risk_individual = mean(sum_n_canoe_capsize),
            min_n_risk_individual = min(sum_n_canoe_capsize),
            max_n_risk_individual = max(sum_n_canoe_capsize))

df_canoe_capsize <- cbind(df_canoe_capsize, df6)


# Combine dfs -------------------------------------------------------------

combined_df <- bind_rows(df_sickness, df_cut_self, df_Animal_Attack, df_tree_fall,
                         df_fight, df_canoe_capsize)

stargazer::stargazer(combined_df, summary = F, flip = T)

