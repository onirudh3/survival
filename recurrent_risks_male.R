
# Libraries and data import -----------------------------------------------
library(dplyr)
library(ggplot2)
library(plyr)
library(ggpubr)
library(grid)

# Data
df <- read.csv("data_new_format.csv")

# Sickness ----------------------------------------------------------------

indiv.stats <- ddply(subset(df, male == 1), c("pid", "sickness.during.interval.episode"),
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
  geom_rect(xmin = 20, xmax = 20, ymin = 1, ymax = 178, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 24, y = 100, label = "20") +
  geom_rect(xmin = 26, xmax = 26, ymin = 178, ymax = 247, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 30, y = 210, label = "26") +
  geom_rect(xmin = 29, xmax = 29, ymin = 247, ymax = 260, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 33, y = 255, label = "29") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Sickness") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/sickness_survival_times_episodes_male.RDS")

# Cut Self ----------------------------------------------------------------

indiv.stats <- ddply(subset(df, male == 1), c("pid", "cut.self.during.interval.episode"),
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
  geom_rect(xmin = 22, xmax = 22, ymin = 1, ymax = 164, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 26, y = 82, label = "22") +
  geom_rect(xmin = 26, xmax = 26, ymin = 164, ymax = 238, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 30, y = 200, label = "26") +
  geom_rect(xmin = 30, xmax = 30, ymin = 238, ymax = 264, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 34, y = 250, label = "30") +
  geom_rect(xmin = 31, xmax = 31, ymin = 264, ymax = 272, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 35, y = 269, label = "31") +
  geom_rect(xmin = 33, xmax = 33, ymin = 272, ymax = 273, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 37, y = 272.5, label = "33") +
  geom_rect(xmin = 34, xmax = 34, ymin = 273, ymax = 274, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 38, y = 273.5, label = "34") +
  scale_y_continuous(breaks = medianepi$medep[c(1, 2, 3, 4, 6)], labels = c("1", "2", "3", "4", "6")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Cut Self") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/cut_self_survival_times_episodes_male.RDS")

# Animal Attack -----------------------------------------------------------

indiv.stats <- ddply(subset(df, male == 1), c("pid", "Animal_Attack.during.interval.episode"),
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
  geom_rect(xmin = 18, xmax = 18, ymin = 1, ymax = 136, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 22, y = 68, label = "18") +
  geom_rect(xmin = 24, xmax = 24, ymin = 136, ymax = 201, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 27, y = 170, label = "24") +
  geom_rect(xmin = 29, xmax = 29, ymin = 201, ymax = 222, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 31, y = 210, label = "29") +
  geom_rect(xmin = 45.5, xmax = 45.5, ymin = 222, ymax = 227, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 50, y = 224, label = "45.5") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Animal Attack") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/Animal_Attack_survival_times_episodes_male.RDS")

# Tree Fall ---------------------------------------------------------------

indiv.stats <- ddply(subset(df, male == 1), c("pid", "tree.fall.during.interval.episode"),
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
  geom_rect(xmin = 16, xmax = 16, ymin = 1, ymax = 85, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 20, y = 80, label = "16") +
  geom_rect(xmin = 11, xmax = 11, ymin = 85, ymax = 92, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 17, y = 89, label = "11") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Tree Fall") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/tree_fall_survival_times_episodes_male.RDS")

# Fight -------------------------------------------------------------------

indiv.stats <- ddply(subset(df, male == 1), c("pid", "fought.during.interval.episode"),
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
  geom_rect(xmin = 22, xmax = 22, ymin = 1, ymax = 79, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 26, y = 40, label = "22") +
  geom_rect(xmin = 24, xmax = 24, ymin = 79, ymax = 111, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 28, y = 100, label = "24") +
  geom_rect(xmin = 31, xmax = 31, ymin = 111, ymax = 126, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 35, y = 118, label = "31") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Fight") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/fight_survival_times_episodes_male.RDS")

# Canoe Capsize -----------------------------------------------------------

indiv.stats <- ddply(subset(df, male == 1), c("pid", "canoe.capsize.during.interval.episode"),
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
  geom_rect(xmin = 19, xmax = 19, ymin = 1, ymax = 67, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 23, y = 35, label = "19") +
  geom_rect(xmin = 18.5, xmax = 18.5, ymin = 67, ymax = 75, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 23, y = 71, label = "18.5") +
  geom_rect(xmin = 30, xmax = 30, ymin = 75, ymax = 76, colour = "black",
            linetype = "dashed") +
  geom_text(size = 3, x = 34, y = 75.5, label = "30") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  ggtitle("Canoe Capsize") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))
p
saveRDS(p, file = "Survival Times Episodes/canoe_capsize_survival_times_episodes_male.RDS")



# Panel plot --------------------------------------------------------------

figure <- ggarrange(readRDS("Survival Times Episodes/sickness_survival_times_episodes_male.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Survival Times Episodes/cut_self_survival_times_episodes_male.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Survival Times Episodes/Animal_Attack_survival_times_episodes_male.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Survival Times Episodes/tree_fall_survival_times_episodes_male.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Survival Times Episodes/fight_survival_times_episodes_male.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Survival Times Episodes/canoe_capsize_survival_times_episodes_male.RDS") + rremove("ylab") + rremove("xlab"))

pdf(file = "Survival Times Episodes/panel_plot_survival_times_episodes_male.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Episode", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()
