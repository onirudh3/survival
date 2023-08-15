
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(survival)
library(bshazard)
library(ggfortify)
library(ggpubr)
library(grid)
library(scales)
library(cowplot)


# Global tercile ----------------------------------------------------------

# Sickness
df <- read.csv("sickness_time_to_first_risk_long_interval.csv")
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time, hazard, lower.ci, upper.ci))
}
hazards <- group_by(df, tercile) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Sickness Plots/hazard_function_time_to_first_risk_by_tercile.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Sickness Plots/hazard_function_time_to_first_risk_by_tercile.RDS")

# Cut Self
df <- read.csv("cut_self_time_to_first_risk_long_interval.csv")
hazards <- group_by(df, tercile) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Cut Self Plots/hazard_function_time_to_first_risk_by_tercile.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Cut Self Plots/hazard_function_time_to_first_risk_by_tercile.RDS")

# Animal Attack
df <- read.csv("Animal_Attack_combined_time_to_first_risk_long_interval.csv")
hazards <- group_by(df, tercile) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_tercile.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
# p <- ggplot() +                      # Draw ggplot2 plot with text only
#   annotate("text",
#            x = 1,
#            y = 1,
#            size = 4,
#            label = "Error, plot not generated for Animal Attack.") +
#   # ggtitle("Animal Attack") +
#   # theme(plot.title = element_text(size = 30, hjust = 0.5)) +
#   theme_void()
p
dev.off()
saveRDS(p, file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_tercile.RDS")

# Tree Fall ERROR
# df <- read.csv("tree_fall_time_to_first_risk_long_interval.csv")
# hazards <- group_by(df, tercile) %>%
#   do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
#   ungroup()
pdf(file = "Tree Fall Plots/hazard_function_time_to_first_risk_by_tercile.pdf", height = 5)
# p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
#   geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
#   labs(color = "", x = "Age [years]", y = "Hazard") +
#   theme_classic(base_size = 14) +
#   ggtitle("Tree Fall") +
#   theme(plot.title = element_text(size = 30, hjust = 0.5)) +
#   guides(fill = F) +
#   scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
#   scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, NA))
p <- ggplot() +                      # Draw ggplot2 plot with text only
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Error, plot not generated for Tree Fall.") +
  # ggtitle("Tree Fall") +
  # theme(plot.title = element_text(size = 70, hjust = 5)) +
  theme_void()
p
dev.off()
saveRDS(p, file = "Tree Fall Plots/hazard_function_time_to_first_risk_by_tercile.RDS")

# Fight
df <- read.csv("fought_time_to_first_risk_long_interval.csv")
hazards <- group_by(df, tercile) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Fight Plots/hazard_function_time_to_first_risk_by_tercile.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.1), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Fight Plots/hazard_function_time_to_first_risk_by_tercile.RDS")

# Canoe Capsize
df <- read.csv("canoe_capsize_time_to_first_risk_long_interval.csv")
hazards <- group_by(df, tercile) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Canoe Capsize Plots/hazard_function_time_to_first_risk_by_tercile.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Canoe Capsize Plots/hazard_function_time_to_first_risk_by_tercile.RDS")


## Panel plot ----
figure <- ggarrange(readRDS("Sickness Plots/hazard_function_time_to_first_risk_by_tercile.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "green") + rremove("ylab") + rremove("xlab"),
                    readRDS("Cut Self Plots/hazard_function_time_to_first_risk_by_tercile.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_tercile.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Tree Fall Plots/hazard_function_time_to_first_risk_by_tercile.RDS"),
                    readRDS("Fight Plots/hazard_function_time_to_first_risk_by_tercile.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "red") + rremove("ylab") + rremove("xlab"),
                    readRDS("Canoe Capsize Plots/hazard_function_time_to_first_risk_by_tercile.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"))

pdf(file = "Panel Plots/hazard_by_tercile.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Hazard", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()

# Male tercile ----------------------------------------------------------

# Sickness
df <- read.csv("sickness_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 1)
hazards <- group_by(df, tercile) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Sickness Plots/hazard_function_time_to_first_risk_by_tercile_male.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.1), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Sickness Plots/hazard_function_time_to_first_risk_by_tercile_male.RDS")

# Cut Self
df <- read.csv("cut_self_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 1)
hazards <- group_by(df, tercile) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Cut Self Plots/hazard_function_time_to_first_risk_by_tercile_male.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Cut Self Plots/hazard_function_time_to_first_risk_by_tercile_male.RDS")

# Animal Attack
df <- read.csv("Animal_Attack_combined_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 1)
hazards <- group_by(df, tercile) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_tercile_male.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
# p <- ggplot() +                      # Draw ggplot2 plot with text only
#   annotate("text",
#            x = 1,
#            y = 1,
#            size = 4,
#            label = "Error, plot not generated for Animal Attack.") +
#   # ggtitle("Animal Attack") +
#   # theme(plot.title = element_text(size = 30, hjust = 0.5)) +
#   theme_void()
p
dev.off()
saveRDS(p, file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_tercile_male.RDS")

# Tree Fall ERROR
# df <- read.csv("tree_fall_time_to_first_risk_long_interval.csv")
# df <- subset(df, male == 1)
# hazards <- group_by(df, tercile) %>%
#   do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
#   ungroup()
pdf(file = "Tree Fall Plots/hazard_function_time_to_first_risk_by_tercile_male.pdf", height = 5)
# p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
#   geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
#   labs(color = "", x = "Age [years]", y = "Hazard") +
#   theme_classic(base_size = 14) +
#   ggtitle("Tree Fall") +
#   theme(plot.title = element_text(size = 30, hjust = 0.5)) +
#   guides(fill = F) +
#   scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
#   scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, NA))
p <- ggplot() +                      # Draw ggplot2 plot with text only
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Error, plot not generated for Tree Fall.") +
  # ggtitle("Tree Fall") +
  # theme(plot.title = element_text(size = 70, hjust = 5)) +
  theme_void()
p
dev.off()
saveRDS(p, file = "Tree Fall Plots/hazard_function_time_to_first_risk_by_tercile_male.RDS")

# Fight ERROR
# df <- read.csv("fought_time_to_first_risk_long_interval.csv")
# df <- subset(df, male == 1)
# hazards <- group_by(df, tercile) %>%
#   do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
#   ungroup()
pdf(file = "Fight Plots/hazard_function_time_to_first_risk_by_tercile_male.pdf", height = 5)
# p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
#   geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
#   labs(color = "", x = "Age [years]", y = "Hazard") +
#   theme_classic(base_size = 14) +
#   ggtitle("Fight") +
#   theme(plot.title = element_text(size = 30, hjust = 0.5)) +
#   guides(fill = F) +
#   scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
#   scale_y_continuous(breaks = seq(0, 100, 0.1), expand = c(0, 0), limits = c(0, NA))
p <- ggplot() +                      # Draw ggplot2 plot with text only
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Error, plot not generated for Fight.") +
  # ggtitle("Tree Fall") +
  # theme(plot.title = element_text(size = 70, hjust = 5)) +
  theme_void()
p
dev.off()
saveRDS(p, file = "Fight Plots/hazard_function_time_to_first_risk_by_tercile_male.RDS")

# Canoe Capsize
df <- read.csv("canoe_capsize_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 1)
hazards <- group_by(df, tercile) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Canoe Capsize Plots/hazard_function_time_to_first_risk_by_tercile_male.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Canoe Capsize Plots/hazard_function_time_to_first_risk_by_tercile_male.RDS")


## Panel plot ----
figure <- ggarrange(readRDS("Sickness Plots/hazard_function_time_to_first_risk_by_tercile_male.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "green") + rremove("ylab") + rremove("xlab"),
                    readRDS("Cut Self Plots/hazard_function_time_to_first_risk_by_tercile_male.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_tercile_male.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Tree Fall Plots/hazard_function_time_to_first_risk_by_tercile_male.RDS"),
                    readRDS("Fight Plots/hazard_function_time_to_first_risk_by_tercile_male.RDS"),
                    readRDS("Canoe Capsize Plots/hazard_function_time_to_first_risk_by_tercile_male.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"))

pdf(file = "Panel Plots/hazard_by_tercile_male.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Hazard", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()

# Female tercile ----------------------------------------------------------

# Sickness
df <- read.csv("sickness_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 0)
hazards <- group_by(df, tercile) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Sickness Plots/hazard_function_time_to_first_risk_by_tercile_female.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Sickness Plots/hazard_function_time_to_first_risk_by_tercile_female.RDS")

# Cut Self
df <- read.csv("cut_self_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 0)
hazards <- group_by(df, tercile) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Cut Self Plots/hazard_function_time_to_first_risk_by_tercile_female.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Cut Self Plots/hazard_function_time_to_first_risk_by_tercile_female.RDS")

# Animal Attack
df <- read.csv("Animal_Attack_combined_time_to_first_risk_long_interval.csv")
df <- subset(df, male == 0)
hazards <- group_by(df, tercile) %>%
  do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
  ungroup()
pdf(file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_tercile_female.pdf", height = 5)
p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
  labs(color = "", x = "Age [years]", y = "Hazard") +
  theme_classic(base_size = 14) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.2, 0.9), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
  scale_y_continuous(breaks = seq(0, 100, 0.1), expand = c(0, 0), limits = c(0, NA))
# p <- ggplot() +                      # Draw ggplot2 plot with text only
#   annotate("text",
#            x = 1,
#            y = 1,
#            size = 4,
#            label = "Error, plot not generated for Animal Attack.") +
#   # ggtitle("Animal Attack") +
#   # theme(plot.title = element_text(size = 30, hjust = 0.5)) +
#   theme_void()
p
dev.off()
saveRDS(p, file = "Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_tercile_female.RDS")

# Tree Fall ERROR
# df <- read.csv("tree_fall_time_to_first_risk_long_interval.csv")
# df <- subset(df, male == 0)
# hazards <- group_by(df, tercile) %>%
#   do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
#   ungroup()
pdf(file = "Tree Fall Plots/hazard_function_time_to_first_risk_by_tercile_female.pdf", height = 5)
# p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
#   geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
#   labs(color = "", x = "Age [years]", y = "Hazard") +
#   theme_classic(base_size = 14) +
#   ggtitle("Tree Fall") +
#   theme(plot.title = element_text(size = 30, hjust = 0.5)) +
#   guides(fill = F) +
#   scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
#   scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, NA))
p <- ggplot() +                      # Draw ggplot2 plot with text only
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Error, plot not generated for Tree Fall.") +
  # ggtitle("Tree Fall") +
  # theme(plot.title = element_text(size = 70, hjust = 5)) +
  theme_void()
p
dev.off()
saveRDS(p, file = "Tree Fall Plots/hazard_function_time_to_first_risk_by_tercile_female.RDS")

# Fight
# df <- read.csv("fought_time_to_first_risk_long_interval.csv")
# df <- subset(df, male == 0)
# hazards <- group_by(df, tercile) %>%
#   do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
#   ungroup()
pdf(file = "Fight Plots/hazard_function_time_to_first_risk_by_tercile_female.pdf", height = 5)
# p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
#   geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
#   labs(color = "", x = "Age [years]", y = "Hazard") +
#   theme_classic(base_size = 14) +
#   ggtitle("Fight") +
#   theme(plot.title = element_text(size = 30, hjust = 0.5)) +
#   guides(fill = F) +
#   scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
#   scale_y_continuous(breaks = seq(0, 100, 0.1), expand = c(0, 0), limits = c(0, NA))
p <- ggplot() +                      # Draw ggplot2 plot with text only
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Error, plot not generated for Fight.") +
  # ggtitle("Tree Fall") +
  # theme(plot.title = element_text(size = 70, hjust = 5)) +
  theme_void()
p
dev.off()
saveRDS(p, file = "Fight Plots/hazard_function_time_to_first_risk_by_tercile_female.RDS")

# Canoe Capsize
# df <- read.csv("canoe_capsize_time_to_first_risk_long_interval.csv")
# df <- subset(df, male == 0)
# hazards <- group_by(df, tercile) %>%
#   do(as.data.frame(bshazard(Surv(enter, exit, event) ~ 1, data = ., verbose = FALSE))) %>%
#   ungroup()
pdf(file = "Canoe Capsize Plots/hazard_function_time_to_first_risk_by_tercile_female.pdf", height = 5)
# p <- ggplot(hazards, aes(x = time, y = hazard, group = tercile)) + geom_line(aes(col = tercile)) +
#   geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = tercile), alpha = 0.3) +
#   labs(color = "", x = "Age [years]", y = "Hazard") +
#   theme_classic(base_size = 14) +
#   ggtitle("Canoe Capsize") +
#   theme(plot.title = element_text(size = 30, hjust = 0.5)) +
#   guides(fill = F) +
#   scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 75)) +
#   scale_y_continuous(breaks = seq(0, 100, 0.05), expand = c(0, 0), limits = c(0, NA))
p <- ggplot() +                      # Draw ggplot2 plot with text only
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Error, plot not generated for Canoe Capsize.") +
  # ggtitle("Tree Fall") +
  # theme(plot.title = element_text(size = 70, hjust = 5)) +
  theme_void()
p
dev.off()
saveRDS(p, file = "Canoe Capsize Plots/hazard_function_time_to_first_risk_by_tercile_female.RDS")


## Panel plot ----
figure <- ggarrange(readRDS("Sickness Plots/hazard_function_time_to_first_risk_by_tercile_female.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "green") + rremove("ylab") + rremove("xlab"),
                    readRDS("Cut Self Plots/hazard_function_time_to_first_risk_by_tercile_female.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_tercile_female.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Tree Fall Plots/hazard_function_time_to_first_risk_by_tercile_female.RDS"),
                    readRDS("Fight Plots/hazard_function_time_to_first_risk_by_tercile_female.RDS"),
                    readRDS("Canoe Capsize Plots/hazard_function_time_to_first_risk_by_tercile_female.RDS"))

pdf(file = "Panel Plots/hazard_by_tercile_female.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Hazard", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()
