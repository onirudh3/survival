
# Libraries ---------------------------------------------------------------

library(ggplot2)
library(survival)
library(bshazard)
library(ggfortify)
library(ggpubr)
library(grid)
library(scales)
library(cowplot)


# Global birth_tercile ----------------------------------------------------------

# Sickness
df <- read.csv("sickness_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = df, conf.type = "log-log")
pdf(file = "Sickness Plots/survival_function_time_to_first_risk_by_birth_tercile.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced tree fall") +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.75, 0.7), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Sickness Plots/survival_function_time_to_first_risk_by_birth_tercile.RDS")

# Cut Self
df <- read.csv("cut_self_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = df, conf.type = "log-log")
pdf(file = "Cut Self Plots/survival_function_time_to_first_risk_by_birth_tercile.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced tree fall") +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.75, 0.7), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Cut Self Plots/survival_function_time_to_first_risk_by_birth_tercile.RDS")

# Animal Attack
df <- read.csv("Animal_Attack_combined_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = df, conf.type = "log-log")
pdf(file = "Animal Attack Combined Plots/survival_function_time_to_first_risk_by_birth_tercile.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced tree fall") +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.75, 0.7), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Animal Attack Combined Plots/survival_function_time_to_first_risk_by_birth_tercile.RDS")

# Tree Fall
df <- read.csv("tree_fall_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = df, conf.type = "log-log")
pdf(file = "Tree Fall Plots/survival_function_time_to_first_risk_by_birth_tercile.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced tree fall") +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.8, 0.25), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Tree Fall Plots/survival_function_time_to_first_risk_by_birth_tercile.RDS")

# Fight
df <- read.csv("fought_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = df, conf.type = "log-log")
pdf(file = "Fight Plots/survival_function_time_to_first_risk_by_birth_tercile.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced Fight") +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.75, 0.2), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Fight Plots/survival_function_time_to_first_risk_by_birth_tercile.RDS")

# Canoe Capsize
df <- read.csv("canoe_capsize_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = df, conf.type = "log-log")
pdf(file = "Canoe Capsize Plots/survival_function_time_to_first_risk_by_birth_tercile.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced Canoe Capsize") +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.8, 0.2), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Canoe Capsize Plots/survival_function_time_to_first_risk_by_birth_tercile.RDS")

## Panel plot ----
figure <- ggarrange(readRDS("Sickness Plots/survival_function_time_to_first_risk_by_birth_tercile.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "green") + rremove("ylab") + rremove("xlab"),
                    readRDS("Cut Self Plots/survival_function_time_to_first_risk_by_birth_tercile.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Animal Attack Combined Plots/survival_function_time_to_first_risk_by_birth_tercile.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Tree Fall Plots/survival_function_time_to_first_risk_by_birth_tercile.RDS") + rremove("ylab") + rremove("xlab") + annotate("rect", xmin = 0, xmax = 18, ymin = 0, ymax = Inf, alpha = 0.05, fill = "black") +
                      annotate("rect", xmin = 18, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Fight Plots/survival_function_time_to_first_risk_by_birth_tercile.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "red") + rremove("ylab") + rremove("xlab"),
                    readRDS("Canoe Capsize Plots/survival_function_time_to_first_risk_by_birth_tercile.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    common.legend = T, legend = "bottom")

pdf(file = "Panel Plots/survival_by_birth_tercile.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Percentage Not Experienced Risk", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()


# Male birth_tercile ------------------------------------------------------------

# Sickness
df <- read.csv("sickness_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = subset(df, male == 1), conf.type = "log-log")
pdf(file = "Sickness Plots/survival_function_time_to_first_risk_by_birth_tercile_male.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced tree fall") +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.75, 0.7), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Sickness Plots/survival_function_time_to_first_risk_by_birth_tercile_male.RDS")

# Cut Self
df <- read.csv("cut_self_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = subset(df, male == 1), conf.type = "log-log")
pdf(file = "Cut Self Plots/survival_function_time_to_first_risk_by_birth_tercile_male.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced tree fall") +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.75, 0.7), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Cut Self Plots/survival_function_time_to_first_risk_by_birth_tercile_male.RDS")

# Animal Attack
df <- read.csv("Animal_Attack_combined_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = subset(df, male == 1), conf.type = "log-log")
pdf(file = "Animal Attack Combined Plots/survival_function_time_to_first_risk_by_birth_tercile_male.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced tree fall") +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.75, 0.65), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Animal Attack Combined Plots/survival_function_time_to_first_risk_by_birth_tercile_male.RDS")

# Tree Fall
df <- read.csv("tree_fall_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = subset(df, male == 1), conf.type = "log-log")
pdf(file = "Tree Fall Plots/survival_function_time_to_first_risk_by_birth_tercile_male.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced tree fall") +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.81, 0.2), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Tree Fall Plots/survival_function_time_to_first_risk_by_birth_tercile_male.RDS")

# Fight
df <- read.csv("fought_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = subset(df, male == 1), conf.type = "log-log")
pdf(file = "Fight Plots/survival_function_time_to_first_risk_by_birth_tercile_male.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced Fight") +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.73, 0.2), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Fight Plots/survival_function_time_to_first_risk_by_birth_tercile_male.RDS")

# Canoe Capsize
df <- read.csv("canoe_capsize_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = subset(df, male == 1), conf.type = "log-log")
pdf(file = "Canoe Capsize Plots/survival_function_time_to_first_risk_by_birth_tercile_male.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced Canoe Capsize") +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.8, 0.2), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Canoe Capsize Plots/survival_function_time_to_first_risk_by_birth_tercile_male.RDS")

## Panel plot ----
figure <- ggarrange(readRDS("Sickness Plots/survival_function_time_to_first_risk_by_birth_tercile_male.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "green") + rremove("ylab") + rremove("xlab"),
                    readRDS("Cut Self Plots/survival_function_time_to_first_risk_by_birth_tercile_male.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Animal Attack Combined Plots/survival_function_time_to_first_risk_by_birth_tercile_male.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Tree Fall Plots/survival_function_time_to_first_risk_by_birth_tercile_male.RDS") + rremove("ylab") + rremove("xlab") + annotate("rect", xmin = 0, xmax = 18, ymin = 0, ymax = Inf, alpha = 0.05, fill = "black") +
                      annotate("rect", xmin = 18, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Fight Plots/survival_function_time_to_first_risk_by_birth_tercile_male.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "red") + rremove("ylab") + rremove("xlab"),
                    readRDS("Canoe Capsize Plots/survival_function_time_to_first_risk_by_birth_tercile_male.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    common.legend = T, legend = "bottom")

pdf(file = "Panel Plots/survival_by_birth_tercile_male.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Percentage Not Experienced Risk", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()


# female birth_tercile ------------------------------------------------------------

# Sickness
df <- read.csv("sickness_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = subset(df, male == 0), conf.type = "log-log")
pdf(file = "Sickness Plots/survival_function_time_to_first_risk_by_birth_tercile_female.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced tree fall") +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.75, 0.7), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Sickness Plots/survival_function_time_to_first_risk_by_birth_tercile_female.RDS")

# Cut Self
df <- read.csv("cut_self_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = subset(df, male == 0), conf.type = "log-log")
pdf(file = "Cut Self Plots/survival_function_time_to_first_risk_by_birth_tercile_female.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced tree fall") +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.75, 0.7), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Cut Self Plots/survival_function_time_to_first_risk_by_birth_tercile_female.RDS")

# Animal Attack
df <- read.csv("Animal_Attack_combined_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = subset(df, male == 0), conf.type = "log-log")
pdf(file = "Animal Attack Combined Plots/survival_function_time_to_first_risk_by_birth_tercile_female.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced tree fall") +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.75, 0.65), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Animal Attack Combined Plots/survival_function_time_to_first_risk_by_birth_tercile_female.RDS")

# Tree Fall
df <- read.csv("tree_fall_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = subset(df, male == 0), conf.type = "log-log")
pdf(file = "Tree Fall Plots/survival_function_time_to_first_risk_by_birth_tercile_female.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced tree fall") +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.81, 0.2), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Tree Fall Plots/survival_function_time_to_first_risk_by_birth_tercile_female.RDS")

# Fight
df <- read.csv("fought_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = subset(df, male == 0), conf.type = "log-log")
pdf(file = "Fight Plots/survival_function_time_to_first_risk_by_birth_tercile_female.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced Fight") +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.73, 0.2), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Fight Plots/survival_function_time_to_first_risk_by_birth_tercile_female.RDS")

# Canoe Capsize
df <- read.csv("canoe_capsize_time_to_first_risk_long_interval.csv")
fit <- survfit(Surv(exit, event) ~ birth_tercile, data = subset(df, male == 0), conf.type = "log-log")
pdf(file = "Canoe Capsize Plots/survival_function_time_to_first_risk_by_birth_tercile_female.pdf", height = 5)
p <- autoplot(fit) +
  geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
  theme_classic(base_size = 14) +
  labs(color = "", x = "Age [years]", y = "Proportion not having experienced Canoe Capsize") +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.8, 0.2), legend.background = element_blank()) +
  guides(fill = F) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))
p
dev.off()
saveRDS(p, file = "Canoe Capsize Plots/survival_function_time_to_first_risk_by_birth_tercile_female.RDS")

## Panel plot ----
figure <- ggarrange(readRDS("Sickness Plots/survival_function_time_to_first_risk_by_birth_tercile_female.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "green") + rremove("ylab") + rremove("xlab"),
                    readRDS("Cut Self Plots/survival_function_time_to_first_risk_by_birth_tercile_female.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Animal Attack Combined Plots/survival_function_time_to_first_risk_by_birth_tercile_female.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Tree Fall Plots/survival_function_time_to_first_risk_by_birth_tercile_female.RDS") + rremove("ylab") + rremove("xlab") + annotate("rect", xmin = 0, xmax = 18, ymin = 0, ymax = Inf, alpha = 0.05, fill = "black") +
                      annotate("rect", xmin = 18, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Fight Plots/survival_function_time_to_first_risk_by_birth_tercile_female.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "red") + rremove("ylab") + rremove("xlab"),
                    readRDS("Canoe Capsize Plots/survival_function_time_to_first_risk_by_birth_tercile_female.RDS") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    common.legend = T, legend = "bottom")

pdf(file = "Panel Plots/survival_by_birth_tercile_female.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Percentage Not Experienced Risk", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()



