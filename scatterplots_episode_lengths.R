
# Libraries and data ------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(grid)

# Data
df <- read.csv("data_new_format.csv")


# Sickness ----------------------------------------------------------------

dc <- subset(df, select = c(pid, age, exit, sickness.during.interval,
                            sickness.during.interval.episode))
dc <- subset(dc, sickness.during.interval == 1)
dc <- dc %>%
  group_by(pid) %>%
  mutate(count = n())

# Episode 1 v 2
dx <- subset(dc, count == 2)
dz <- data.frame(ep1_time = dx[(dx$sickness.during.interval.episode == 1), ]$exit,
                 ep2_time = dx[(dx$sickness.during.interval.episode == 2), ]$exit)
dz <- dz %>% mutate(group = case_when(ep2_time %% 1 == 0 ~ 1, T ~ 2))
dz$group <- as.factor(dz$group)
p <- dz %>%
  ggplot(aes(ep1_time, ep2_time, shape = group)) +
  scale_shape_manual(values = c(20, 3)) +
  geom_point() +
  # ggtitle("Sickness") +
  xlab("Episode 1 Time") +
  ylab("Episode 2 Time") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76))
saveRDS(p, file = "Scatterplot Episode Time/sickness_1_v_2.RDS")

# Episode 2 v 3
dx <- subset(dc, count == 3)
dz <- data.frame(ep2_time = dx[(dx$sickness.during.interval.episode == 2), ]$exit,
                 ep3_time = dx[(dx$sickness.during.interval.episode == 3), ]$exit)
dz <- dz %>% mutate(group = case_when(ep3_time %% 1 == 0 ~ 1, T ~ 2))
dz$group <- as.factor(dz$group)
p <- dz %>%
  ggplot(aes(ep2_time, ep3_time, shape = group)) +
  scale_shape_manual(values = c(20, 3)) +
  geom_point() +
  # ggtitle("Sickness") +
  xlab("Episode 2 Time") +
  ylab("Episode 3 Time") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76))
saveRDS(p, file = "Scatterplot Episode Time/sickness_2_v_3.RDS")


# Cut Self ----------------------------------------------------------------

dc <- subset(df, select = c(pid, age, exit, cut.self.during.interval,
                            cut.self.during.interval.episode))
dc <- subset(dc, cut.self.during.interval == 1)
dc <- dc %>%
  group_by(pid) %>%
  mutate(count = n())

# Episode 1 v 2
dx <- subset(dc, count == 2)
dz <- data.frame(ep1_time = dx[(dx$cut.self.during.interval.episode == 1), ]$exit,
                 ep2_time = dx[(dx$cut.self.during.interval.episode == 2), ]$exit)
dz <- dz %>% mutate(group = case_when(ep2_time %% 1 == 0 ~ 1, T ~ 2))
dz$group <- as.factor(dz$group)
p <- dz %>%
  ggplot(aes(ep1_time, ep2_time, shape = group)) +
  scale_shape_manual(values = c(20, 3)) +
  geom_point() +
  # ggtitle("Cut Self") +
  xlab("Episode 1 Time") +
  ylab("Episode 2 Time") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76))
saveRDS(p, file = "Scatterplot Episode Time/cut_self_1_v_2.RDS")

# Episode 2 v 3
dx <- subset(dc, count == 3)
dz <- data.frame(ep2_time = dx[(dx$cut.self.during.interval.episode == 2), ]$exit,
                 ep3_time = dx[(dx$cut.self.during.interval.episode == 3), ]$exit)
dz <- dz %>% mutate(group = case_when(ep3_time %% 1 == 0 ~ 1, T ~ 2))
dz$group <- as.factor(dz$group)
p <- dz %>%
  ggplot(aes(ep2_time, ep3_time, shape = group)) +
  scale_shape_manual(values = c(20, 3)) +
  geom_point() +
  # ggtitle("Cut Self") +
  xlab("Episode 2 Time") +
  ylab("Episode 3 Time") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76))
saveRDS(p, file = "Scatterplot Episode Time/cut_self_2_v_3.RDS")

# Episode 3 v 4
dx <- subset(dc, count == 4)
dz <- data.frame(ep3_time = dx[(dx$cut.self.during.interval.episode == 3), ]$exit,
                 ep4_time = dx[(dx$cut.self.during.interval.episode == 4), ]$exit)
dz <- dz %>% mutate(group = case_when(ep4_time %% 1 == 0 ~ 1, T ~ 2))
dz$group <- as.factor(dz$group)
p <- dz %>%
  ggplot(aes(ep3_time, ep4_time, shape = group)) +
  scale_shape_manual(values = c(20, 3)) +
  geom_point() +
  # ggtitle("Cut Self") +
  xlab("Episode 3 Time") +
  ylab("Episode 4 Time") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76))
saveRDS(p, file = "Scatterplot Episode Time/cut_self_3_v_4.RDS")

# Episode 4 v 5
dx <- subset(dc, count == 5)
dz <- data.frame(ep4_time = dx[(dx$cut.self.during.interval.episode == 4), ]$exit,
                 ep5_time = dx[(dx$cut.self.during.interval.episode == 5), ]$exit)
dz <- dz %>% mutate(group = case_when(ep5_time %% 1 == 0 ~ 1, T ~ 2))
dz$group <- as.factor(dz$group)
p <- dz %>%
  ggplot(aes(ep4_time, ep5_time, shape = group)) +
  scale_shape_manual(values = c(20, 3)) +
  geom_point() +
  # ggtitle("Cut Self") +
  xlab("Episode 4 Time") +
  ylab("Episode 5 Time") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76))
saveRDS(p, file = "Scatterplot Episode Time/cut_self_4_v_5.RDS")

# Episode 5 v 6
dx <- subset(dc, count == 6)
dz <- data.frame(ep5_time = dx[(dx$cut.self.during.interval.episode == 5), ]$exit,
                 ep6_time = dx[(dx$cut.self.during.interval.episode == 6), ]$exit)
dz <- dz %>% mutate(group = case_when(ep6_time %% 1 == 0 ~ 1, T ~ 2))
dz$group <- as.factor(dz$group)
p <- dz %>%
  ggplot(aes(ep5_time, ep6_time, shape = group)) +
  scale_shape_manual(values = c(20, 3)) +
  geom_point() +
  # ggtitle("Cut Self") +
  xlab("Episode 5 Time") +
  ylab("Episode 6 Time") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76))
saveRDS(p, file = "Scatterplot Episode Time/cut_self_5_v_6.RDS")


# Animal Attack -----------------------------------------------------------

dc <- subset(df, select = c(pid, age, exit, Animal_Attack.during.interval,
                            Animal_Attack.during.interval.episode))
dc <- subset(dc, Animal_Attack.during.interval == 1)
dc <- dc %>%
  group_by(pid) %>%
  mutate(count = n())

# Episode 1 v 2
dx <- subset(dc, count == 2)
dz <- data.frame(ep1_time = dx[(dx$Animal_Attack.during.interval.episode == 1), ]$exit,
                 ep2_time = dx[(dx$Animal_Attack.during.interval.episode == 2), ]$exit)
dz <- dz %>% mutate(group = case_when(ep2_time %% 1 == 0 ~ 1, T ~ 2))
dz$group <- as.factor(dz$group)
p <- dz %>%
  ggplot(aes(ep1_time, ep2_time, shape = group)) +
  scale_shape_manual(values = c(20, 3)) +
  geom_point() +
  # ggtitle("Animal Attack") +
  xlab("Episode 1 Time") +
  ylab("Episode 2 Time") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76))
saveRDS(p, file = "Scatterplot Episode Time/Animal_Attack_1_v_2.RDS")

# Episode 2 v 3
dx <- subset(dc, count == 3)
dz <- data.frame(ep2_time = dx[(dx$Animal_Attack.during.interval.episode == 2), ]$exit,
                 ep3_time = dx[(dx$Animal_Attack.during.interval.episode == 3), ]$exit)
dz <- dz %>% mutate(group = case_when(ep3_time %% 1 == 0 ~ 1, T ~ 2))
dz$group <- as.factor(dz$group)
p <- dz %>%
  ggplot(aes(ep2_time, ep3_time, shape = group)) +
  scale_shape_manual(values = c(20, 3)) +
  geom_point() +
  # ggtitle("Animal Attack") +
  xlab("Episode 2 Time") +
  ylab("Episode 3 Time") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76))
saveRDS(p, file = "Scatterplot Episode Time/Animal_Attack_2_v_3.RDS")

# Episode 3 v 4
dx <- subset(dc, count == 4)
dz <- data.frame(ep3_time = dx[(dx$Animal_Attack.during.interval.episode == 3), ]$exit,
                 ep4_time = dx[(dx$Animal_Attack.during.interval.episode == 4), ]$exit)
dz <- dz %>% mutate(group = case_when(ep4_time %% 1 == 0 ~ 1, T ~ 2))
dz$group <- as.factor(dz$group)
p <- dz %>%
  ggplot(aes(ep3_time, ep4_time, shape = group)) +
  scale_shape_manual(values = c(20, 3)) +
  geom_point() +
  # ggtitle("Animal Attack") +
  xlab("Episode 3 Time") +
  ylab("Episode 4 Time") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76))
saveRDS(p, file = "Scatterplot Episode Time/Animal_Attack_3_v_4.RDS")


# Tree Fall ---------------------------------------------------------------

dc <- subset(df, select = c(pid, age, exit, tree.fall.during.interval,
                            tree.fall.during.interval.episode))
dc <- subset(dc, tree.fall.during.interval == 1)
dc <- dc %>%
  group_by(pid) %>%
  mutate(count = n())

# Episode 1 v 2
dx <- subset(dc, count == 2)
dz <- data.frame(ep1_time = dx[(dx$tree.fall.during.interval.episode == 1), ]$exit,
                 ep2_time = dx[(dx$tree.fall.during.interval.episode == 2), ]$exit)
dz <- dz %>% mutate(group = case_when(ep2_time %% 1 == 0 ~ 1, T ~ 2))
dz$group <- as.factor(dz$group)
p <- dz %>%
  ggplot(aes(ep1_time, ep2_time, shape = group)) +
  scale_shape_manual(values = c(20, 3)) +
  geom_point() +
  # ggtitle("Tree Fall") +
  xlab("Episode 1 Time") +
  ylab("Episode 2 Time") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76))
saveRDS(p, file = "Scatterplot Episode Time/tree_fall_1_v_2.RDS")

# Episode 2 v 3
dx <- subset(dc, count == 3)
dz <- data.frame(ep2_time = dx[(dx$tree.fall.during.interval.episode == 2), ]$exit,
                 ep3_time = dx[(dx$tree.fall.during.interval.episode == 3), ]$exit)
dz <- dz %>% mutate(group = case_when(ep3_time %% 1 == 0 ~ 1, T ~ 2))
dz$group <- as.factor(dz$group)
p <- dz %>%
  ggplot(aes(ep2_time, ep3_time, shape = group)) +
  scale_shape_manual(values = c(20, 3)) +
  geom_point() +
  # ggtitle("Tree Fall") +
  xlab("Episode 2 Time") +
  ylab("Episode 3 Time") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76))
saveRDS(p, file = "Scatterplot Episode Time/tree_fall_2_v_3.RDS")


# Fight -------------------------------------------------------------------

dc <- subset(df, select = c(pid, age, exit, fought.during.interval,
                            fought.during.interval.episode))
dc <- subset(dc, fought.during.interval == 1)
dc <- dc %>%
  group_by(pid) %>%
  mutate(count = n())

# Episode 1 v 2
dx <- subset(dc, count == 2)
dz <- data.frame(ep1_time = dx[(dx$fought.during.interval.episode == 1), ]$exit,
                 ep2_time = dx[(dx$fought.during.interval.episode == 2), ]$exit)
dz <- dz %>% mutate(group = case_when(ep2_time %% 1 == 0 ~ 1, T ~ 2))
dz$group <- as.factor(dz$group)
p <- dz %>%
  ggplot(aes(ep1_time, ep2_time, shape = group)) +
  scale_shape_manual(values = c(20, 3)) +
  geom_point() +
  # ggtitle("Fight") +
  xlab("Episode 1 Time") +
  ylab("Episode 2 Time") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76))
saveRDS(p, file = "Scatterplot Episode Time/fight_1_v_2.RDS")

# Episode 2 v 3
dx <- subset(dc, count == 3)
dz <- data.frame(ep2_time = dx[(dx$fought.during.interval.episode == 2), ]$exit,
                 ep3_time = dx[(dx$fought.during.interval.episode == 3), ]$exit)
dz <- dz %>% mutate(group = case_when(ep3_time %% 1 == 0 ~ 1, T ~ 2))
dz$group <- as.factor(dz$group)
p <- dz %>%
  ggplot(aes(ep2_time, ep3_time, shape = group)) +
  scale_shape_manual(values = c(20, 3)) +
  geom_point() +
  # ggtitle("Fight") +
  xlab("Episode 2 Time") +
  ylab("Episode 3 Time") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76))
saveRDS(p, file = "Scatterplot Episode Time/fight_2_v_3.RDS")


# Canoe Capsize -----------------------------------------------------------

dc <- subset(df, select = c(pid, age, exit, canoe.capsize.during.interval,
                            canoe.capsize.during.interval.episode))
dc <- subset(dc, canoe.capsize.during.interval == 1)
dc <- dc %>%
  group_by(pid) %>%
  mutate(count = n())

# Episode 1 v 2
dx <- subset(dc, count == 2)
dz <- data.frame(ep1_time = dx[(dx$canoe.capsize.during.interval.episode == 1), ]$exit,
                 ep2_time = dx[(dx$canoe.capsize.during.interval.episode == 2), ]$exit)
dz <- dz %>% mutate(group = case_when(ep2_time %% 1 == 0 ~ 1, T ~ 2))
dz$group <- as.factor(dz$group)
p <- dz %>%
  ggplot(aes(ep1_time, ep2_time, shape = group)) +
  scale_shape_manual(values = c(20, 3)) +
  geom_point() +
  # ggtitle("Canoe Capsize") +
  xlab("Episode 1 Time") +
  ylab("Episode 2 Time") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76))
saveRDS(p, file = "Scatterplot Episode Time/canoe_capsize_1_v_2.RDS")

# Episode 2 v 3
dx <- subset(dc, count == 3)
dz <- data.frame(ep2_time = dx[(dx$canoe.capsize.during.interval.episode == 2), ]$exit,
                 ep3_time = dx[(dx$canoe.capsize.during.interval.episode == 3), ]$exit)
dz <- dz %>% mutate(group = case_when(ep3_time %% 1 == 0 ~ 1, T ~ 2))
dz$group <- as.factor(dz$group)
p <- dz %>%
  ggplot(aes(ep2_time, ep3_time, shape = group)) +
  scale_shape_manual(values = c(20, 3)) +
  geom_point() +
  # ggtitle("Canoe Capsize") +
  xlab("Episode 2 Time") +
  ylab("Episode 3 Time") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, 76))
saveRDS(p, file = "Scatterplot Episode Time/canoe_capsize_2_v_3.RDS")


# Panel plots -------------------------------------------------------------

# Sickness
figure <- ggarrange(readRDS("Scatterplot Episode Time/sickness_1_v_2.RDS"),
                    readRDS("Scatterplot Episode Time/sickness_2_v_3.RDS"))

pdf(file = "Scatterplot Episode Time/panel_sickness.pdf", height = 7, width = 15)
annotate_figure(figure, top = textGrob("Sickness", rot = 0, vjust = 0.5, gp = gpar(cex = 5)))
dev.off()

# Cut Self
figure <- ggarrange(readRDS("Scatterplot Episode Time/cut_self_1_v_2.RDS"),
                    readRDS("Scatterplot Episode Time/cut_self_2_v_3.RDS"),
                    readRDS("Scatterplot Episode Time/cut_self_3_v_4.RDS"),
                    readRDS("Scatterplot Episode Time/cut_self_4_v_5.RDS"),
                    readRDS("Scatterplot Episode Time/cut_self_5_v_6.RDS"))

pdf(file = "Scatterplot Episode Time/panel_cut_self.pdf", height = 9, width = 15)
annotate_figure(figure, top = textGrob("Cut Self", rot = 0, vjust = 0.5, gp = gpar(cex = 5)))
dev.off()

# Animal Attack
figure <- ggarrange(readRDS("Scatterplot Episode Time/Animal_Attack_1_v_2.RDS"),
                    readRDS("Scatterplot Episode Time/Animal_Attack_2_v_3.RDS"),
                    readRDS("Scatterplot Episode Time/Animal_Attack_3_v_4.RDS"))

pdf(file = "Scatterplot Episode Time/panel_Animal_Attack.pdf", height = 10, width = 12)
annotate_figure(figure, top = textGrob("Animal Attack", rot = 0, vjust = 0.5, gp = gpar(cex = 5)))
dev.off()

# Tree Fall
figure <- ggarrange(readRDS("Scatterplot Episode Time/tree_fall_1_v_2.RDS"),
                    readRDS("Scatterplot Episode Time/tree_fall_2_v_3.RDS"))

pdf(file = "Scatterplot Episode Time/panel_tree_fall.pdf", height = 7, width = 15)
annotate_figure(figure, top = textGrob("Tree Fall", rot = 0, vjust = 0.5, gp = gpar(cex = 5)))
dev.off()

# Fight
figure <- ggarrange(readRDS("Scatterplot Episode Time/fight_1_v_2.RDS"),
                    readRDS("Scatterplot Episode Time/fight_2_v_3.RDS"))

pdf(file = "Scatterplot Episode Time/panel_fight.pdf", height = 7, width = 15)
annotate_figure(figure, top = textGrob("Fight", rot = 0, vjust = 0.5, gp = gpar(cex = 5)))
dev.off()

# Sickness
figure <- ggarrange(readRDS("Scatterplot Episode Time/canoe_capsize_1_v_2.RDS"),
                    readRDS("Scatterplot Episode Time/canoe_capsize_2_v_3.RDS"))

pdf(file = "Scatterplot Episode Time/panel_canoe_capsize.pdf", height = 7, width = 15)
annotate_figure(figure, top = textGrob("Canoe Capsize", rot = 0, vjust = 0.5, gp = gpar(cex = 5)))
dev.off()
