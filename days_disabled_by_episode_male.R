
# Libraries and data ------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(grid)

# Data
df <- read.csv("data_new_format.csv")

df <- subset(df, male == 1)

# Sickness ----------------------------------------------------------------

dx <- subset(df, sickness.during.interval == 1)

dx <- dx %>%
  mutate(total_days_disabled =
           case_when(!is.na(days_disabled_sickness_2) ~
                       days_disabled_sickness_1 +
                       days_disabled_sickness_2,
                     T ~ days_disabled_sickness_1))

dx <- dx %>%
  mutate(total_days_disabled =
           case_when(!is.na(days_disabled_sickness_3) ~
                       days_disabled_sickness_1 +
                       days_disabled_sickness_3,
                     T ~ days_disabled_sickness_1))

dx <- subset(dx, select = c(sickness.during.interval.episode, total_days_disabled))

dx$sickness.during.interval.episode <- as.factor(dx$sickness.during.interval.episode)

# Test for group differences
kruskal.test(total_days_disabled ~ sickness.during.interval.episode, data = dx)

# Plot
p <- dx %>% ggplot(aes(x = sickness.during.interval.episode, y = total_days_disabled)) +
  geom_violin() +
  theme_classic() +
  ggtitle("Sickness") +
  labs(subtitle = "p-value = 0.46") +
  theme(plot.title = element_text(size = 30, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Episode") +
  ylab("Total days disabled")
p
saveRDS(p, file = "Days Disabled Episodes/sickness_male.RDS")

# Cut Self ----------------------------------------------------------------

dx <- subset(df, cut.self.during.interval == 1)

dx <- dx %>%
  mutate(total_days_disabled =
           case_when(!is.na(cut_self_days_disabled_2) ~
                       cut_self_days_disabled_1 +
                       cut_self_days_disabled_2,
                     T ~ cut_self_days_disabled_1))

dx <- dx %>%
  mutate(total_days_disabled =
           case_when(!is.na(cut_self_days_disabled_3) ~
                       cut_self_days_disabled_1 +
                       cut_self_days_disabled_3,
                     T ~ cut_self_days_disabled_1))

dx <- subset(dx, select = c(cut.self.during.interval.episode, total_days_disabled))

dx$cut.self.during.interval.episode <- as.factor(dx$cut.self.during.interval.episode)

# Test for group differences
kruskal.test(total_days_disabled ~ cut.self.during.interval.episode, data = dx)

# Plot
p <- dx %>% ggplot(aes(x = cut.self.during.interval.episode, y = total_days_disabled)) +
  geom_violin() +
  theme_classic() +
  ggtitle("Cut Self") +
  labs(subtitle = "p-value = 0.00") +
  theme(plot.title = element_text(size = 30, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Episode") +
  ylab("Total days disabled")
p
saveRDS(p, file = "Days Disabled Episodes/cut_self_male.RDS")

# Animal Attack -----------------------------------------------------------

dx <- subset(df, Animal_Attack.during.interval == 1)

dx <- dx %>%
  mutate(total_days_disabled =
           case_when(!is.na(days_disabled_Animal_Attacked_2) ~
                       days_disabled_Animal_Attacked_1 +
                       days_disabled_Animal_Attacked_2,
                     T ~ days_disabled_Animal_Attacked_1))

dx <- dx %>%
  mutate(total_days_disabled =
           case_when(!is.na(days_disabled_Animal_Attacked_3) ~
                       days_disabled_Animal_Attacked_1 +
                       days_disabled_Animal_Attacked_3,
                     T ~ days_disabled_Animal_Attacked_1))

dx <- subset(dx, select = c(Animal_Attack.during.interval.episode, total_days_disabled))

dx$Animal_Attack.during.interval.episode <- as.factor(dx$Animal_Attack.during.interval.episode)

# Test for group differences
kruskal.test(total_days_disabled ~ Animal_Attack.during.interval.episode, data = dx)

# Plot
p <- dx %>% ggplot(aes(x = Animal_Attack.during.interval.episode, y = total_days_disabled)) +
  geom_violin() +
  theme_classic() +
  ggtitle("Animal Attack") +
  labs(subtitle = "p-value = 0.31") +
  theme(plot.title = element_text(size = 30, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Episode") +
  ylab("Total days disabled")
p
saveRDS(p, file = "Days Disabled Episodes/Animal_Attack_male.RDS")

# Tree Fall ---------------------------------------------------------------

dx <- subset(df, tree.fall.during.interval == 1)

dx <- dx %>%
  mutate(total_days_disabled =
           case_when(!is.na(tree_fall_days_disabled_2) ~
                       tree_fall_days_disabled_1 +
                       tree_fall_days_disabled_2,
                     T ~ tree_fall_days_disabled_1))

dx <- subset(dx, select = c(tree.fall.during.interval.episode, total_days_disabled))

dx$tree.fall.during.interval.episode <- as.factor(dx$tree.fall.during.interval.episode)

# Test for group differences
kruskal.test(total_days_disabled ~ tree.fall.during.interval.episode, data = dx)

# Plot
p <- dx %>% ggplot(aes(x = tree.fall.during.interval.episode, y = total_days_disabled)) +
  geom_violin() +
  theme_classic() +
  ggtitle("Tree Fall") +
  labs(subtitle = "p-value = 0.82") +
  theme(plot.title = element_text(size = 30, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Episode") +
  ylab("Total days disabled")
p
saveRDS(p, file = "Days Disabled Episodes/tree_fall_male.RDS")

# Fight -------------------------------------------------------------------

dx <- subset(df, fought.during.interval == 1)

dx <- dx %>%
  mutate(total_days_disabled =
           case_when(!is.na(fought_days_injured_2) ~
                       fought_days_injured_1 +
                       fought_days_injured_2,
                     T ~ fought_days_injured_1))

dx <- dx %>%
  mutate(total_days_disabled =
           case_when(!is.na(fought_days_injured_3) ~
                       fought_days_injured_1 +
                       fought_days_injured_3,
                     T ~ fought_days_injured_1))

dx <- subset(dx, select = c(fought.during.interval.episode, total_days_disabled))

dx$fought.during.interval.episode <- as.factor(dx$fought.during.interval.episode)

# Test for group differences
kruskal.test(total_days_disabled ~ fought.during.interval.episode, data = dx)

# Plot
p <- dx %>% ggplot(aes(x = fought.during.interval.episode, y = total_days_disabled)) +
  geom_violin() +
  theme_classic() +
  ggtitle("Fight") +
  labs(subtitle = "p-value = 0.95") +
  theme(plot.title = element_text(size = 30, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Episode") +
  ylab("Total days disabled")
p
saveRDS(p, file = "Days Disabled Episodes/fight_male.RDS")

# Canoe Capsize -----------------------------------------------------------

dx <- subset(df, canoe.capsize.during.interval == 1)

dx <- dx %>%
  mutate(total_days_disabled =
           case_when(!is.na(canoe_capsize_days_disabled_2) ~
                       canoe_capsize_days_disabled_1 +
                       canoe_capsize_days_disabled_2,
                     T ~ canoe_capsize_days_disabled_1))

dx <- subset(dx, select = c(canoe.capsize.during.interval.episode, total_days_disabled))

dx$canoe.capsize.during.interval.episode <- as.factor(dx$canoe.capsize.during.interval.episode)

# Test for group differences
kruskal.test(total_days_disabled ~ canoe.capsize.during.interval.episode, data = dx)

# Plot
p <- dx %>% ggplot(aes(x = canoe.capsize.during.interval.episode, y = total_days_disabled)) +
  geom_violin() +
  theme_classic() +
  ggtitle("Canoe Capsize") +
  labs(subtitle = "p-value = 0.56") +
  theme(plot.title = element_text(size = 30, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Episode") +
  ylab("Total days disabled")
p
saveRDS(p, file = "Days Disabled Episodes/canoe_capsize_male.RDS")

# Panel plot --------------------------------------------------------------

figure <- ggarrange(readRDS("Days Disabled Episodes/sickness_male.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Days Disabled Episodes/cut_self_male.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Days Disabled Episodes/Animal_Attack_male.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Days Disabled Episodes/tree_fall_male.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Days Disabled Episodes/fight_male.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Days Disabled Episodes/canoe_capsize_male.RDS") + rremove("ylab") + rremove("xlab"))

pdf(file = "Days Disabled Episodes/panel_plot_male.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Total days disabled", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Episode", gp = gpar(cex = 1.3)))
dev.off()
