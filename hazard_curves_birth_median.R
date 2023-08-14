
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(survival)
library(bshazard)
library(ggfortify)
library(ggpubr)
library(grid)
library(scales)
library(cowplot)


# Global median ----------------------------------------------------------

# Sickness
df <- read.csv("sickness_time_to_first_risk_long_interval.csv")
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, birth_pre_median) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Sickness Plots/hazard_function_time_to_first_risk_by_birth_pre_median.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Sickness Plots/hazard_function_time_to_first_risk_by_birth_pre_median.RDS")

# Cut Self
df <- read.csv("cut_self_time_to_first_risk_long_interval.csv")
hazards <- group_by(df, birth_pre_median) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Cut Self Plots/hazard_function_time_to_first_risk_by_birth_pre_median.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Cut Self Plots/hazard_function_time_to_first_risk_by_birth_pre_median.RDS")

# Animal Attack
df <- read.csv("Animal_Attack_time_to_first_risk_long_interval.csv")
hazards <- group_by(df, birth_pre_median) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_birth_pre_median.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_birth_pre_median.RDS")

# Tree Fall
df <- read.csv("tree_fall_time_to_first_risk_long_interval.csv")
hazards <- group_by(df, birth_pre_median) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Tree Fall Plots/hazard_function_time_to_first_risk_by_birth_pre_median.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Tree Fall Plots/hazard_function_time_to_first_risk_by_birth_pre_median.RDS")

# Fight
df <- read.csv("fought_time_to_first_risk_long_interval.csv")
hazards <- group_by(df, birth_pre_median) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Fight Plots/hazard_function_time_to_first_risk_by_birth_pre_median.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.1), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Fight Plots/hazard_function_time_to_first_risk_by_birth_pre_median.RDS")

# Canoe Capsize
df <- read.csv("canoe_capsize_time_to_first_risk_long_interval.csv")
hazards <- group_by(df, birth_pre_median) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Canoe Capsize Plots/hazard_function_time_to_first_risk_by_birth_pre_median.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Canoe Capsize Plots/hazard_function_time_to_first_risk_by_birth_pre_median.RDS")

## Panel plot ----
figure <- ggarrange(readRDS("Sickness Plots/hazard_function_time_to_first_risk_by_birth_pre_median.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "green") + rremove("ylab") + rremove("xlab"),
                    readRDS("Cut Self Plots/hazard_function_time_to_first_risk_by_birth_pre_median.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_birth_pre_median.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Tree Fall Plots/hazard_function_time_to_first_risk_by_birth_pre_median.RDS") + rremove("ylab") + rremove("xlab") + annotate("rect", xmin = 0, xmax = 18, ymin = 0, ymax = Inf, alpha = 0.05, fill = "black") +
                      annotate("rect", xmin = 18, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Fight Plots/hazard_function_time_to_first_risk_by_birth_pre_median.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "red") + rremove("ylab") + rremove("xlab"),
                    readRDS("Canoe Capsize Plots/hazard_function_time_to_first_risk_by_birth_pre_median.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    common.legend = T, legend = "bottom")

pdf(file = "Panel Plots/hazard_by_birth_pre_median.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Hazard", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()


# Male median ----------------------------------------------------------

# Sickness
df <- read.csv("sickness_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 1)
hazards <- group_by(df, birth_pre_median) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Sickness Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.1), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Sickness Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.RDS")

# Cut Self
df <- read.csv("cut_self_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 1)
hazards <- group_by(df, birth_pre_median) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Cut Self Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Cut Self Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.RDS")

# Animal Attack
df <- read.csv("Animal_Attack_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 1)
hazards <- group_by(df, birth_pre_median) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.RDS")

# Tree Fall
df <- read.csv("tree_fall_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 1)
hazards <- group_by(df, birth_pre_median) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Tree Fall Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Tree Fall Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.RDS")

# Fight
df <- read.csv("fought_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 1)
hazards <- group_by(df, birth_pre_median) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Fight Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Fight Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.RDS")

# Canoe Capsize
df <- read.csv("canoe_capsize_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 1)
hazards <- group_by(df, birth_pre_median) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Canoe Capsize Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Canoe Capsize Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.RDS")

## Panel plot ----
figure <- ggarrange(readRDS("Sickness Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "green") + rremove("ylab") + rremove("xlab"),
                    readRDS("Cut Self Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Tree Fall Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.RDS") + rremove("ylab") + rremove("xlab") + annotate("rect", xmin = 0, xmax = 18, ymin = 0, ymax = Inf, alpha = 0.05, fill = "black") +
                      annotate("rect", xmin = 18, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Fight Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "red") + rremove("ylab") + rremove("xlab"),
                    readRDS("Canoe Capsize Plots/hazard_function_time_to_first_risk_by_birth_pre_median_male.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    common.legend = T, legend = "bottom")

pdf(file = "Panel Plots/hazard_by_birth_pre_median_male.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Hazard", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()


# Female median ----------------------------------------------------------

# Sickness
df <- read.csv("sickness_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 0)
hazards <- group_by(df, birth_pre_median) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Sickness Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Sickness Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.RDS")

# Cut Self
df <- read.csv("cut_self_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 0)
hazards <- group_by(df, birth_pre_median) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Cut Self Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Cut Self Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.RDS")

# Animal Attack ERROR
# df <- read.csv("Animal_Attack_time_to_first_risk_long_interval.csv")
# df <- subset(df, male == 0)
# hazards <- group_by(df, birth_pre_median) %>%
#   do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
#   ungroup()
pdf(file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.pdf", height = 5)
# p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
#   geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
#   labs(color = "", x = "Age [years]", y = "Hazard") +
#   theme_classic(base_size = 14) +
#   ggtitle("Animal Attack") +
#   theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
#   guides(fill = F) +
#   scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
#   scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p <- ggplot() +                      # Draw ggplot2 plot with text only
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Error, plot not generated for Animal Attack.") +
  theme_void()
p
dev.off()
saveRDS(p, file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.RDS")

# Tree Fall
df <- read.csv("tree_fall_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 0)
hazards <- group_by(df, birth_pre_median) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Tree Fall Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
# p <- ggplot() +                      # Draw ggplot2 plot with text only
#   annotate("text",
#            x = 1,
#            y = 1,
#            size = 4,
#            label = "Error, plot not generated for Tree Fall.") +
#   theme_void()
p
dev.off()
saveRDS(p, file = "Tree Fall Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.RDS")

# Fight
df <- read.csv("fought_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 0)
hazards <- group_by(df, birth_pre_median) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Fight Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
# p <- ggplot() +                      # Draw ggplot2 plot with text only
#   annotate("text",
#            x = 1,
#            y = 1,
#            size = 4,
#            label = "Error, plot not generated for Fight.") +
#   theme_void()
p
dev.off()
saveRDS(p, file = "Fight Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.RDS")

# Canoe Capsize
df <- read.csv("canoe_capsize_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 0)
hazards <- group_by(df, birth_pre_median) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Canoe Capsize Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = birth_pre_median)) + geom_line(aes(col = birth_pre_median)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = birth_pre_median), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Canoe Capsize Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.RDS")

## Panel plot ----
figure <- ggarrange(readRDS("Sickness Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "green") + rremove("ylab") + rremove("xlab"),
                    readRDS("Cut Self Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.RDS"),
                    readRDS("Tree Fall Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.RDS") + rremove("ylab") + rremove("xlab") + annotate("rect", xmin = 0, xmax = 18, ymin = 0, ymax = Inf, alpha = 0.05, fill = "black") +
                      annotate("rect", xmin = 18, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Fight Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "red") + rremove("ylab") + rremove("xlab"),
                    readRDS("Canoe Capsize Plots/hazard_function_time_to_first_risk_by_birth_pre_median_female.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    common.legend = T, legend = "bottom")

pdf(file = "Panel Plots/hazard_by_birth_pre_median_female.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Hazard", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()


