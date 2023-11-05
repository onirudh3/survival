
# Libraries and data import -----------------------------------------------
library(dplyr)
library(ggplot2)
library(plyr)
library(ggpubr)
library(grid)

# Data
df <- read.csv("data_new_format.csv")

# Sickness ----------------------------------------------------------------

indiv.stats <- ddply(subset(df, male == 0), c("pid", "sickness.during.interval.episode"),
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
  geom_rect(xmin = 15, xmax = 15, ymin = 1, ymax = 165, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 19, y = 83, label = "15") +
  geom_rect(xmin = 23, xmax = 23, ymin = 165, ymax = 237, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 27, y = 200, label = "23") +
  geom_rect(xmin = 25, xmax = 25, ymin = 237, ymax = 247, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 29, y = 242, label = "25") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Sickness") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/sickness_survival_times_episodes_female.RDS")

# Cut Self ----------------------------------------------------------------

indiv.stats <- ddply(subset(df, male == 0), c("pid", "cut.self.during.interval.episode"),
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
  geom_rect(xmin = 15, xmax = 15, ymin = 1, ymax = 149, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 19, y = 75, label = "15") +
  geom_rect(xmin = 18.5, xmax = 18.5, ymin = 149, ymax = 223, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 23, y = 185, label = "18.5") +
  geom_rect(xmin = 18.7, xmax = 18.7, ymin = 223, ymax = 253, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 23, y = 238, label = "18.7") +
  geom_rect(xmin = 20.5, xmax = 20.5, ymin = 253, ymax = 265, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 25, y = 259, label = "20.5") +
  geom_rect(xmin = 41.5, xmax = 41.5, ymin = 265, ymax = 268, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 46, y = 267, label = "41.5") +
  geom_rect(xmin = 20.4, xmax = 20.4, ymin = 268, ymax = 269, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 24, y = 268.5, label = "20.4") +
  scale_y_continuous(breaks = medianepi$medep[c(1, 2, 3, 4, 6)], labels = c("1", "2", "3", "4", "6")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Cut Self") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/cut_self_survival_times_episodes_female.RDS")

# Animal Attack -----------------------------------------------------------

indiv.stats <- ddply(subset(df, male == 0), c("pid", "Animal_Attack.during.interval.episode"),
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
  geom_rect(xmin = 15, xmax = 15, ymin = 1, ymax = 54, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 19, y = 27, label = "15") +
  geom_rect(xmin = 19.1, xmax = 19.1, ymin = 54, ymax = 66, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 23, y = 60, label = "19.1") +
  geom_rect(xmin = 11, xmax = 11, ymin = 66, ymax = 67, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 15, y = 66.5, label = "11") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Animal Attack") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/Animal_Attack_survival_times_episodes_female.RDS")

# Tree Fall ---------------------------------------------------------------

indiv.stats <- ddply(subset(df, male == 0), c("pid", "tree.fall.during.interval.episode"),
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
  geom_rect(xmin = 11, xmax = 11, ymin = 1, ymax = 80, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 15, y = 40, label = "11") +
  geom_rect(xmin = 13, xmax = 13, ymin = 80, ymax = 91, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 17, y = 85, label = "13") +
  geom_rect(xmin = 16, xmax = 16, ymin = 91, ymax = 93, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 20, y = 92, label = "16") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Tree Fall") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/tree_fall_survival_times_episodes_female.RDS")

# Fight -------------------------------------------------------------------

indiv.stats <- ddply(subset(df, male == 0), c("pid", "fought.during.interval.episode"),
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
  geom_rect(xmin = 23, xmax = 23, ymin = 1, ymax = 29, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 27, y = 15, label = "23") +
  geom_rect(xmin = 22, xmax = 22, ymin = 29, ymax = 37, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 26, y = 34, label = "22") +
  geom_rect(xmin = 26, xmax = 26, ymin = 37, ymax = 39, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 30, y = 38, label = "26") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Fight") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/fight_survival_times_episodes_female.RDS")

# Canoe Capsize -----------------------------------------------------------

indiv.stats <- ddply(subset(df, male == 0), c("pid", "canoe.capsize.during.interval.episode"),
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
  geom_rect(xmin = 18, xmax = 18, ymin = 1, ymax = 28, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 22, y = 14, label = "18") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Canoe Capsize") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/canoe_capsize_survival_times_episodes_female.RDS")



# Panel plot --------------------------------------------------------------

figure <- ggarrange(readRDS("Survival Times Episodes/sickness_survival_times_episodes_female.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Survival Times Episodes/cut_self_survival_times_episodes_female.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Survival Times Episodes/Animal_Attack_survival_times_episodes_female.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Survival Times Episodes/tree_fall_survival_times_episodes_female.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Survival Times Episodes/fight_survival_times_episodes_female.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Survival Times Episodes/canoe_capsize_survival_times_episodes_female.RDS") + rremove("ylab") + rremove("xlab"))

pdf(file = "Survival Times Episodes/panel_plot_survival_times_episodes_female.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Episode", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()
