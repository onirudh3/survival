

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(survival)
library(bshazard)
library(ggfortify)
library(ggpubr)
library(grid)
library(scales)
library(cowplot)
library(survminer)
library(stargazer)

# Sickness ----------------------------------------------------------------

# df <- read.csv("sickness_time_to_first_risk_long_interval.csv")

df <- read.csv("sickness_final_table.csv")
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
p <- ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "orange2") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "pink3") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.087))
saveRDS(p, file = "Sickness Plots/all_exposures_hazard_curve.RDS")


# Cut Self ----------------------------------------------------------------

# df <- read.csv("cut_self_time_to_first_risk_long_interval.csv")

df <- read.csv("cut_self_final_table.csv")
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
p <- ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "orange2") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "pink3") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.087))
saveRDS(p, file = "Cut Self Plots/all_exposures_hazard_curve.RDS")

# Animal Attack ----------------------------------------------------------------

# df <- read.csv("Animal_Attack_combined_time_to_first_risk_long_interval.csv")

df <- read.csv("Animal_Attack_combined_final_table.csv")
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
p <- ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "orange2") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "pink3") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.087))
saveRDS(p, file = "Animal Attack Combined Plots/all_exposures_hazard_curve.RDS")

# Tree Fall ----------------------------------------------------------------

# df <- read.csv("tree_fall_time_to_first_risk_long_interval.csv")

df <- read.csv("tree_fall_final_table.csv")
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
p <- ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "orange2") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "pink3") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.087))
saveRDS(p, file = "Tree Fall Plots/all_exposures_hazard_curve.RDS")

# Fight ----------------------------------------------------------------

# df <- read.csv("fought_time_to_first_risk_long_interval.csv")

df <- read.csv("fought_final_table.csv")
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
p <- ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "orange2") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "pink3") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.087))
saveRDS(p, file = "Fight Plots/all_exposures_hazard_curve.RDS")

# Canoe Capsize ----------------------------------------------------------------

# df <- read.csv("canoe_capsize_time_to_first_risk_long_interval.csv")

df <- read.csv("canoe_capsize_final_table.csv")
fit <- bshazard(Surv(enter, exit, event) ~ 1, data = df)
df_surv <- data.frame(time = fit$time, hazard = fit$hazard,
                      lower.ci = fit$lower.ci, upper.ci = fit$upper.ci)
p <- ggplot(df_surv, aes(x = time, y = hazard)) +
  geom_line(color = "orange2") +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2,
              fill = "pink3") +
  theme_classic(base_size = 14) +
  xlab("Age [years]") +
  ylab("Hazard") +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_y_continuous(breaks = seq(0, 100, 0.02), expand = c(0, 0), limits = c(0, 0.087))
saveRDS(p, file = "Canoe Capsize Plots/all_exposures_hazard_curve.RDS")


# Panel plot --------------------------------------------------------------

figure <- ggarrange(readRDS("Sickness Plots/all_exposures_hazard_curve.RDS") + rremove("ylab") + rremove("xlab") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "green"),
                    readRDS("Cut Self Plots/all_exposures_hazard_curve.RDS") + rremove("ylab") + rremove("xlab") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black"),
                    readRDS("Animal Attack Combined Plots/all_exposures_hazard_curve.RDS") + rremove("ylab") + rremove("xlab") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black"),
                    readRDS("Tree Fall Plots/all_exposures_hazard_curve.RDS") + rremove("ylab") + rremove("xlab") + annotate("rect", xmin = 0, xmax = 18, ymin = 0, ymax = Inf, alpha = 0.05, fill = "black") +
                      annotate("rect", xmin = 18, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black") + rremove("ylab") + rremove("xlab"),
                    readRDS("Fight Plots/all_exposures_hazard_curve.RDS") + rremove("ylab") + rremove("xlab") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.05, fill = "red"),
                    readRDS("Canoe Capsize Plots/all_exposures_hazard_curve.RDS") + rremove("ylab") + rremove("xlab") + annotate("rect", xmin = 0, xmax = 75, ymin = 0, ymax = Inf, alpha = 0.1, fill = "black"))
pdf(file = "Panel Plots/hazard_all_exposures.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Hazard", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()
