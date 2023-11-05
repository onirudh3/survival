
# Libraries and data import -----------------------------------------------
library(dplyr)
library(ggplot2)
library(plyr)
library(ggpubr)
library(grid)

# Data
df <- read.csv("data_new_format.csv")

# Sickness ----------------------------------------------------------------

indiv.stats <- ddply(df, c("pid", "sickness.during.interval.episode"),
                     summarize, exit = exit)
indiv.stats <- subset(indiv.stats, sickness.during.interval.episode != 0)

indiv.stats  <- indiv.stats[order(indiv.stats$sickness.during.interval.episode,
                                  -indiv.stats$exit),]

indiv.stats$order <- c(1:length(indiv.stats$pid))

# Median episode time for each event (use these number for the vertical lines on the plot)
medianepi <- ddply(indiv.stats, "sickness.during.interval.episode", summarize,
                   medep = median(exit))

# Code for coloring the plot and figuring out where to put the episode numbers
medianepi <- ddply(indiv.stats, "sickness.during.interval.episode", summarize,
                   medep = median(order))

# Plot
p <- indiv.stats %>%
  ggplot(aes(x = exit, y = order)) +
  geom_rect(mapping = aes(
    xmin = 0,
    xmax = exit,
    ymin = order,
    ymax = order + 1,
    fill = factor(sickness.during.interval.episode))) +
  geom_rect(xmin = 18, xmax = 18, ymin = 1, ymax = 342, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 22, y = 171, label = "18") +
  geom_rect(xmin = 23.9, xmax = 23.9, ymin = 342, ymax = 483, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 28, y = 413, label = "23.9") +
  geom_rect(xmin = 27.5, xmax = 27.5, ymin = 483, ymax = 510, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 31, y = 495, label = "27.5") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Sickness") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/sickness_survival_times_episodes_global.RDS")

# Cut Self ----------------------------------------------------------------

indiv.stats <- ddply(df, c("pid", "cut.self.during.interval.episode"),
                     summarize, exit = exit)
indiv.stats <- subset(indiv.stats, cut.self.during.interval.episode != 0)

indiv.stats  <- indiv.stats[order(indiv.stats$cut.self.during.interval.episode,
                                  -indiv.stats$exit),]

indiv.stats$order <- c(1:length(indiv.stats$pid))

# Median episode time for each event (use these number for the vertical lines on the plot)
medianepi <- ddply(indiv.stats, "cut.self.during.interval.episode", summarize,
                   medep = median(exit))

# Code for coloring the plot and figuring out where to put the episode numbers
medianepi <- ddply(indiv.stats, "cut.self.during.interval.episode", summarize,
                   medep = median(order))

# Plot
p <- indiv.stats %>%
  ggplot(aes(x = exit, y = order)) +
  geom_rect(mapping = aes(
    xmin = 0,
    xmax = exit,
    ymin = order,
    ymax = order + 1,
    fill = factor(cut.self.during.interval.episode))) +
  geom_rect(xmin = 18, xmax = 18, ymin = 1, ymax = 312, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 22, y = 160, label = "18") +
  geom_rect(xmin = 20.5, xmax = 20.5, ymin = 312, ymax = 460, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 25, y = 380, label = "20.5") +
  geom_rect(xmin = 22, xmax = 22, ymin = 460, ymax = 516, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 28, y = 490, label = "22") +
  geom_rect(xmin = 25, xmax = 25, ymin = 516, ymax = 537, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 29, y = 525, label = "25") +
  geom_rect(xmin = 37.23, xmax = 37.23, ymin = 537, ymax = 540, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 41, y = 538, label = "37.2") +
  geom_rect(xmin = 27.19, xmax = 27.19, ymin = 540, ymax = 543, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 32, y = 542, label = "27.2") +
  scale_y_continuous(breaks = medianepi$medep[c(1, 2, 3, 4, 6)], labels = c("1", "2", "3", "4", "6")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Cut Self") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/cut_self_survival_times_episodes_global.RDS")

# Animal Attack -----------------------------------------------------------

indiv.stats <- ddply(df, c("pid", "Animal_Attack.during.interval.episode"),
                     summarize, exit = exit)
indiv.stats <- subset(indiv.stats, Animal_Attack.during.interval.episode != 0)

indiv.stats  <- indiv.stats[order(indiv.stats$Animal_Attack.during.interval.episode,
                                  -indiv.stats$exit),]

indiv.stats$order <- c(1:length(indiv.stats$pid))

# Median episode time for each event (use these number for the vertical lines on the plot)
medianepi <- ddply(indiv.stats, "Animal_Attack.during.interval.episode", summarize,
                   medep = median(exit))

# Code for coloring the plot and figuring out where to put the episode numbers
medianepi <- ddply(indiv.stats, "Animal_Attack.during.interval.episode", summarize,
                   medep = median(order))

# Plot
p <- indiv.stats %>%
  ggplot(aes(x = exit, y = order)) +
  geom_rect(mapping = aes(
    xmin = 0,
    xmax = exit,
    ymin = order,
    ymax = order + 1,
    fill = factor(Animal_Attack.during.interval.episode))) +
  geom_rect(xmin = 18, xmax = 18, ymin = 1, ymax = 189, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 22, y = 100, label = "18") +
  geom_rect(xmin = 23, xmax = 23, ymin = 189, ymax = 266, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 27, y = 230, label = "23") +
  geom_rect(xmin = 27, xmax = 27, ymin = 266, ymax = 288, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 31, y = 277, label = "27") +
  geom_rect(xmin = 45.5, xmax = 45.5, ymin = 288, ymax = 295, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 50, y = 293, label = "45.5") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Animal Attack") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/Animal_Attack_survival_times_episodes_global.RDS")

# Tree Fall ---------------------------------------------------------------

indiv.stats <- ddply(df, c("pid", "tree.fall.during.interval.episode"),
                      summarize, exit = exit)
indiv.stats <- subset(indiv.stats, tree.fall.during.interval.episode != 0)

indiv.stats  <- indiv.stats[order(indiv.stats$tree.fall.during.interval.episode,
                                  -indiv.stats$exit),]

indiv.stats$order <- c(1:length(indiv.stats$pid))

# Median episode time for each event (use these number for the vertical lines on the plot)
medianepi <- ddply(indiv.stats, "tree.fall.during.interval.episode", summarize,
                   medep = median(exit))

# Code for coloring the plot and figuring out where to put the episode numbers
medianepi <- ddply(indiv.stats, "tree.fall.during.interval.episode", summarize,
                   medep = median(order))

# Plot
p <- indiv.stats %>%
  ggplot(aes(x = exit, y = order)) +
  geom_rect(mapping = aes(
    xmin = 0,
    xmax = exit,
    ymin = order,
    ymax = order + 1,
    fill = factor(tree.fall.during.interval.episode))) +
  geom_rect(xmin = 12, xmax = 12, ymin = 1, ymax = 164, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 16, y = 80, label = "12") +
  geom_rect(xmin = 12.5, xmax = 12.5, ymin = 164, ymax = 182, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 17, y = 174, label = "12.5") +
  geom_rect(xmin = 16, xmax = 16, ymin = 182, ymax = 185, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 20, y = 183, label = "16") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Tree Fall") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/tree_fall_survival_times_episodes_global.RDS")

# Fight -------------------------------------------------------------------

indiv.stats <- ddply(df, c("pid", "fought.during.interval.episode"),
                     summarize, exit = exit)
indiv.stats <- subset(indiv.stats, fought.during.interval.episode != 0)

indiv.stats  <- indiv.stats[order(indiv.stats$fought.during.interval.episode,
                                  -indiv.stats$exit),]

indiv.stats$order <- c(1:length(indiv.stats$pid))

# Median episode time for each event (use these number for the vertical lines on the plot)
medianepi <- ddply(indiv.stats, "fought.during.interval.episode", summarize,
                   medep = median(exit))

# Code for coloring the plot and figuring out where to put the episode numbers
medianepi <- ddply(indiv.stats, "fought.during.interval.episode", summarize,
                   medep = median(order))

# Plot
p <- indiv.stats %>%
  ggplot(aes(x = exit, y = order)) +
  geom_rect(mapping = aes(
    xmin = 0,
    xmax = exit,
    ymin = order,
    ymax = order + 1,
    fill = factor(fought.during.interval.episode))) +
  geom_rect(xmin = 22.5, xmax = 22.5, ymin = 1, ymax = 107, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 27, y = 50, label = "22.5") +
  geom_rect(xmin = 24, xmax = 24, ymin = 107, ymax = 147, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 28, y = 120, label = "24") +
  geom_rect(xmin = 31, xmax = 31, ymin = 147, ymax = 165, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 35, y = 157, label = "31") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Fight") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/fight_survival_times_episodes_global.RDS")

# Canoe Capsize -----------------------------------------------------------

indiv.stats <- ddply(df, c("pid", "canoe.capsize.during.interval.episode"),
                     summarize, exit = exit)
indiv.stats <- subset(indiv.stats, canoe.capsize.during.interval.episode != 0)

indiv.stats  <- indiv.stats[order(indiv.stats$canoe.capsize.during.interval.episode,
                                  -indiv.stats$exit),]

indiv.stats$order <- c(1:length(indiv.stats$pid))

# Median episode time for each event (use these number for the vertical lines on the plot)
medianepi <- ddply(indiv.stats, "canoe.capsize.during.interval.episode", summarize,
                   medep = median(exit))

# Code for coloring the plot and figuring out where to put the episode numbers
medianepi <- ddply(indiv.stats, "canoe.capsize.during.interval.episode", summarize,
                   medep = median(order))

# Plot
p <- indiv.stats %>%
  ggplot(aes(x = exit, y = order)) +
  geom_rect(mapping = aes(
    xmin = 0,
    xmax = exit,
    ymin = order,
    ymax = order + 1,
    fill = factor(canoe.capsize.during.interval.episode))) +
  geom_rect(xmin = 19, xmax = 19, ymin = 1, ymax = 94, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 23, y = 50, label = "19") +
  geom_rect(xmin = 18.5, xmax = 18.5, ymin = 94, ymax = 102, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 23, y = 99, label = "18.5") +
  geom_rect(xmin = 30, xmax = 30, ymin = 102, ymax = 103, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 35, y = 102.5, label = "31") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Canoe Capsize") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/canoe_capsize_survival_times_episodes_global.RDS")



# Panel plot --------------------------------------------------------------

figure <- ggarrange(readRDS("Survival Times Episodes/sickness_survival_times_episodes_global.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Survival Times Episodes/cut_self_survival_times_episodes_global.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Survival Times Episodes/Animal_Attack_survival_times_episodes_global.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Survival Times Episodes/tree_fall_survival_times_episodes_global.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Survival Times Episodes/fight_survival_times_episodes_global.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Survival Times Episodes/canoe_capsize_survival_times_episodes_global.RDS") + rremove("ylab") + rremove("xlab"))

pdf(file = "Survival Times Episodes/panel_plot_survival_times_episodes_global.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Episode", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()
