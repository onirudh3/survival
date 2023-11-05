
# Libraries and data import -----------------------------------------------
library(dplyr)
library(ggplot2)
library(plyr)

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
indiv.stats %>%
  ggplot(aes(x = exit, y = order)) +
  geom_rect(mapping = aes(
    xmin = 0,
    xmax = exit,
    ymin = order,
    ymax = order + 1,
    fill = factor(sickness.during.interval.episode))) +
  geom_rect(xmin = 18, xmax = 18, ymin = 1, ymax = 342, colour = "black",
            linetype = "dashed") +
  geom_rect(xmin = 23.9, xmax = 23.9, ymin = 342, ymax = 483, colour = "black",
            linetype = "dashed") +
  geom_rect(xmin = 27.5, xmax = 27.5, ymin = 483, ymax = 510, colour = "black",
            linetype = "dashed") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 80)) +
  ggtitle("Sickness") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(size = 50, hjust = 0.5))


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
indiv.stats %>%
  ggplot(aes(x = exit, y = order)) +
  geom_rect(mapping = aes(
    xmin = 0,
    xmax = exit,
    ymin = order,
    ymax = order + 1,
    fill = factor(cut.self.during.interval.episode))) +
  geom_rect(xmin = 18, xmax = 18, ymin = 1, ymax = 312, colour = "black",
            linetype = "dashed") +
  geom_rect(xmin = 20.5, xmax = 20.5, ymin = 312, ymax = 460, colour = "black",
            linetype = "dashed") +
  geom_rect(xmin = 22, xmax = 22, ymin = 460, ymax = 516, colour = "black",
            linetype = "dashed") +
  geom_rect(xmin = 25, xmax = 25, ymin = 516, ymax = 537, colour = "black",
            linetype = "dashed") +
  geom_rect(xmin = 37.23, xmax = 37.23, ymin = 537, ymax = 540, colour = "black",
            linetype = "dashed") +
  geom_rect(xmin = 27.19, xmax = 27.19, ymin = 540, ymax = 543, colour = "black",
            linetype = "dashed") +
  scale_y_continuous(breaks = medianepi$medep[c(1, 2, 3, 4, 6)], labels = c("1", "2", "3", "4", "6")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 80)) +
  ggtitle("Cut Self") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(size = 50, hjust = 0.5))


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
indiv.stats %>%
  ggplot(aes(x = exit, y = order)) +
  geom_rect(mapping = aes(
    xmin = 0,
    xmax = exit,
    ymin = order,
    ymax = order + 1,
    fill = factor(Animal_Attack.during.interval.episode))) +
  geom_rect(xmin = 18, xmax = 18, ymin = 1, ymax = 189, colour = "black",
            linetype = "dashed") +
  geom_rect(xmin = 23, xmax = 23, ymin = 189, ymax = 266, colour = "black",
            linetype = "dashed") +
  geom_rect(xmin = 27, xmax = 27, ymin = 266, ymax = 288, colour = "black",
            linetype = "dashed") +
  geom_rect(xmin = 45.5, xmax = 45.5, ymin = 288, ymax = 295, colour = "black",
            linetype = "dashed") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 80)) +
  ggtitle("Animal Attack") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(size = 50, hjust = 0.5))


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
indiv.stats %>%
  ggplot(aes(x = exit, y = order)) +
  geom_rect(mapping = aes(
    xmin = 0,
    xmax = exit,
    ymin = order,
    ymax = order + 1,
    fill = factor(tree.fall.during.interval.episode))) +
  geom_rect(xmin = 12, xmax = 12, ymin = 1, ymax = 164, colour = "black",
            linetype = "dashed") +
  geom_rect(xmin = 12.5, xmax = 12.5, ymin = 164, ymax = 182, colour = "black",
            linetype = "dashed") +
  geom_rect(xmin = 16, xmax = 16, ymin = 182, ymax = 185, colour = "black",
            linetype = "dashed") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 80)) +
  ggtitle("Tree Fall") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(size = 50, hjust = 0.5))


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
indiv.stats %>%
  ggplot(aes(x = exit, y = order)) +
  geom_rect(mapping = aes(
    xmin = 0,
    xmax = exit,
    ymin = order,
    ymax = order + 1,
    fill = factor(fought.during.interval.episode))) +
  geom_rect(xmin = 22.5, xmax = 22.5, ymin = 1, ymax = 107, colour = "black",
            linetype = "dashed") +
  geom_rect(xmin = 24, xmax = 24, ymin = 107, ymax = 147, colour = "black",
            linetype = "dashed") +
  geom_rect(xmin = 31, xmax = 31, ymin = 147, ymax = 165, colour = "black",
            linetype = "dashed") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 80)) +
  ggtitle("Fight") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(size = 50, hjust = 0.5))


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
indiv.stats %>%
  ggplot(aes(x = exit, y = order)) +
  geom_rect(mapping = aes(
    xmin = 0,
    xmax = exit,
    ymin = order,
    ymax = order + 1,
    fill = factor(canoe.capsize.during.interval.episode))) +
  geom_rect(xmin = 19, xmax = 19, ymin = 1, ymax = 94, colour = "black",
            linetype = "dashed") +
  geom_rect(xmin = 18.5, xmax = 18.5, ymin = 94, ymax = 102, colour = "black",
            linetype = "dashed") +
  geom_rect(xmin = 30, xmax = 30, ymin = 102, ymax = 103, colour = "black",
            linetype = "dashed") +
  scale_y_continuous(breaks = medianepi$medep, labels = c("1", "2", "3")) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 80)) +
  ggtitle("Canoe Capsize") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(size = 50, hjust = 0.5))









