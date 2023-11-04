
# Libraries and data import -----------------------------------------------
library(dplyr)
library(ggplot2)
library(plyr)

# Data
df <- read.csv("data_new_format.csv")

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

# Plot --------------------------------------------------------------------
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
  ggtitle("Tree Fall: Survival Time by Episode") +
  xlab("Age in Years") +
  ylab("Episode") +
  theme_classic(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5))












