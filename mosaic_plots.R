
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(ggmosaic)
library(forcats)

library(survival)
library(bshazard)
library(ggfortify)
library(ggpubr)
library(grid)
library(scales)
library(cowplot)


# All Causes --------------------------------------------------------------

# Sickness
df <- read_xlsx("perceived_causes_sickness.xlsx")
df <- subset(df, type != "Total")
df$female <- df$prop_total_female
df$male <- df$prop_total_male
df <- subset(df, select = c(type, male, female, rank_global))
df <- pivot_longer(cols = c(male, female),
                   names_to = c("gender"),
                   df)
df <- df %>% rename("Freq" = "value")
df <- df %>%
  group_by(gender) %>%
  arrange(Freq)
df$type <- fct_reorder(df$type, df$rank_global, .desc = T)
pdf(file = "Sickness Plots/mosaic_plot_sickness.pdf", height = 6, width = 8)
df %>% ggplot() +
  geom_mosaic(aes(x = product(type),
                  weight = Freq,
                  fill = gender),
              divider = c("hspine" , "vspine")) +
  labs(x = "", y = "", fill = "") +
  guides(x = F) +
  theme_classic(base_size = 10) +
  theme(axis.line.x = element_line(color = "white"), axis.line.y = element_line(color = "white"),
        axis.ticks = element_blank()) +
  ggtitle("Sickness")
dev.off()

# Cut Self
df <- read_xlsx("perceived_causes_cut_self.xlsx")
df <- subset(df, activity != "Total")
df$female <- df$prop_total_female
df$male <- df$prop_total_male
df <- subset(df, select = c(activity, male, female, rank_global))

df <- pivot_longer(cols = c(male, female),
                   names_to = c("gender"),
                   df)
df <- df %>% rename("Freq" = "value")
df <- df %>%
  group_by(gender) %>%
  arrange(Freq)
df$activity <- fct_reorder(df$activity, df$rank_global, .desc = T)
pdf(file = "Cut Self Plots/mosaic_plot_cut_self.pdf", height = 6, width = 8)
df %>% ggplot() +
  geom_mosaic(aes(x = product(activity),
                  weight = Freq,
                  fill = gender),
              divider = c("hspine" , "vspine")) +
  labs(x = "", y = "", fill = "") +
  guides(x = F) +
  theme_classic(base_size = 10) +
  theme(axis.line.x = element_line(color = "white"), axis.line.y = element_line(color = "white")) +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5))
dev.off()

# Animal Attack
df <- read_xlsx("perceived_causes_Animal_Attack.xlsx")
df <- subset(df, activity != "Total")
df$female <- df$prop_total_female
df$male <- df$prop_total_male
df <- subset(df, select = c(activity, male, female, rank_global))

df <- pivot_longer(cols = c(male, female),
                   names_to = c("gender"),
                   df)
df <- df %>% rename("Freq" = "value")
df <- df %>%
  group_by(gender) %>%
  arrange(Freq)
df$activity <- fct_reorder(df$activity, df$rank_global, .desc = T)
pdf(file = "Animal Attack Combined Plots/mosaic_plot_Animal_Attack.pdf", height = 6, width = 8)
df %>% ggplot() +
  geom_mosaic(aes(x = product(activity),
                  weight = Freq,
                  fill = gender),
              divider = c("hspine" , "vspine")) +
  labs(x = "", y = "", fill = "") +
  guides(x = F) +
  theme_classic(base_size = 10) +
  theme(axis.line.x = element_line(color = "white"), axis.line.y = element_line(color = "white")) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5))
dev.off()

# Tree Fall
df <- read_xlsx("perceived_causes_tree_fall.xlsx")
df <- subset(df, activity != "Total")
df$female <- df$prop_total_female
df$male <- df$prop_total_male
df <- subset(df, select = c(activity, male, female, rank_global))

df <- pivot_longer(cols = c(male, female),
                   names_to = c("gender"),
                   df)
df <- df %>% rename("Freq" = "value")
df <- df %>%
  group_by(gender) %>%
  arrange(Freq)
df$activity <- fct_reorder(df$activity, df$rank_global, .desc = T)
pdf(file = "Tree Fall Plots/mosaic_plot_tree_fall.pdf", height = 6, width = 8)
df %>% ggplot() +
  geom_mosaic(aes(x = product(activity),
                  weight = Freq,
                  fill = gender),
              divider = c("hspine" , "vspine")) +
  labs(x = "", y = "", fill = "") +
  guides(x = F) +
  theme_classic(base_size = 10) +
  theme(axis.line.x = element_line(color = "white"), axis.line.y = element_line(color = "white")) +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30, hjust = 0.5))
dev.off()

# Fight
df <- read_xlsx("perceived_causes_fight.xlsx")
df <- subset(df, cause != "Total")
df$female <- df$prop_total_female
df$male <- df$prop_total_male
df <- subset(df, select = c(cause, male, female, rank_global))

df <- pivot_longer(cols = c(male, female),
                   names_to = c("gender"),
                   df)
df <- df %>% rename("Freq" = "value")
df <- df %>%
  group_by(gender) %>%
  arrange(Freq)
df$cause <- fct_reorder(df$cause, df$rank_global, .desc = T)
pdf(file = "Fight Plots/mosaic_plot_fight.pdf", height = 6, width = 8)
df %>% ggplot() +
  geom_mosaic(aes(x = product(cause),
                  weight = Freq,
                  fill = gender),
              divider = c("hspine" , "vspine")) +
  labs(x = "", y = "", fill = "") +
  guides(x = F) +
  theme_classic(base_size = 10) +
  theme(axis.line.x = element_line(color = "white"), axis.line.y = element_line(color = "white")) +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30, hjust = 0.5))
dev.off()

# Canoe Capsize
df <- read_xlsx("perceived_causes_canoe_capsize.xlsx")
df <- subset(df, activity != "Total")
df$female <- df$prop_total_female
df$male <- df$prop_total_male
df <- subset(df, select = c(activity, male, female, rank_global))

df <- pivot_longer(cols = c(male, female),
                   names_to = c("gender"),
                   df)
df <- df %>% rename("Freq" = "value")
df <- df %>%
  group_by(gender) %>%
  arrange(Freq)
df$activity <- fct_reorder(df$activity, df$rank_global, .desc = T)
pdf(file = "Canoe Capsize Plots/mosaic_plot_canoe_capsize.pdf", height = 6, width = 8)
df %>% ggplot() +
  geom_mosaic(aes(x = product(activity),
                  weight = Freq,
                  fill = gender),
              divider = c("hspine" , "vspine")) +
  labs(x = "", y = "", fill = "") +
  guides(x = F) +
  theme_classic(base_size = 10) +
  theme(axis.line.x = element_line(color = "white"), axis.line.y = element_line(color = "white")) +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30, hjust = 0.5))
dev.off()



# Top 3 Causes ------------------------------------------------------------

# Sickness
df <- read_xlsx("perceived_causes_sickness.xlsx")
df <- subset(df, type != "Total")
df$female <- df$prop_total_female
df$male <- df$prop_total_male
df <- subset(df, select = c(type, male, female, rank_global))
df <- pivot_longer(cols = c(male, female),
                   names_to = c("gender"),
                   df)
df <- df %>% rename("Freq" = "value")
df <- df %>%
  group_by(gender) %>%
  arrange(Freq)
df$type <- fct_reorder(df$type, df$rank_global, .desc = T)
pdf(file = "Sickness Plots/mosaic_plot_sickness_top_3.pdf", height = 6, width = 8)
p <- df %>% ggplot() +
  geom_mosaic(aes(x = product(type),
                  weight = Freq,
                  fill = gender),
              divider = c("hspine" , "vspine")) +
  labs(x = "", y = "", fill = "") +
  guides(x = F) +
  theme_classic(base_size = 25) +
  theme(axis.line.x = element_line(color = "white"), axis.line.y = element_line(color = "white"),
        axis.ticks = element_blank()) +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_productlist(labels = c("",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "Witchcraft",
                                 "Respiratory",
                                 "Viral"))
p
dev.off()
saveRDS(p, file = "Sickness Plots/mosaic_plot_sickness_top_3.RDS")

# Cut Self
df <- read_xlsx("perceived_causes_cut_self.xlsx")
df <- subset(df, activity != "Total")
df$female <- df$prop_total_female
df$male <- df$prop_total_male
df <- subset(df, select = c(activity, male, female, rank_global))

df <- pivot_longer(cols = c(male, female),
                   names_to = c("gender"),
                   df)
df <- df %>% rename("Freq" = "value")
df <- df %>%
  group_by(gender) %>%
  arrange(Freq)
df$activity <- fct_reorder(df$activity, df$rank_global, .desc = T)
pdf(file = "Cut Self Plots/mosaic_plot_cut_self_top_3.pdf", height = 6, width = 8)
p <- df %>% ggplot() +
  geom_mosaic(aes(x = product(activity),
                  weight = Freq,
                  fill = gender),
              divider = c("hspine" , "vspine")) +
  labs(x = "", y = "", fill = "") +
  guides(x = F) +
  theme_classic(base_size = 25) +
  theme(axis.line.x = element_line(color = "white"), axis.line.y = element_line(color = "white"),
        axis.ticks = element_blank()) +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_productlist(labels = c("",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "Making Tools",
                                 "Cooking",
                                 "Clearing Field"))
p
dev.off()
saveRDS(p, file = "Cut Self Plots/mosaic_plot_cut_self_top_3.RDS")

# Animal Attack
df <- read_xlsx("perceived_causes_Animal_Attack.xlsx")
df <- subset(df, activity != "Total")
df$female <- df$prop_total_female
df$male <- df$prop_total_male
df <- subset(df, select = c(activity, male, female, rank_global))

df <- pivot_longer(cols = c(male, female),
                   names_to = c("gender"),
                   df)
df <- df %>% rename("Freq" = "value")
df <- df %>%
  group_by(gender) %>%
  arrange(Freq)
df$activity <- fct_reorder(df$activity, df$rank_global, .desc = T)
pdf(file = "Animal Attack Combined Plots/mosaic_plot_Animal_Attack_top_3.pdf", height = 6, width = 8)
p <- df %>% ggplot() +
  geom_mosaic(aes(x = product(activity),
                  weight = Freq,
                  fill = gender),
              divider = c("hspine" , "vspine")) +
  labs(x = "", y = "", fill = "") +
  guides(x = F) +
  theme_classic(base_size = 25) +
  theme(axis.line.x = element_line(color = "white"), axis.line.y = element_line(color = "white"),
        axis.ticks = element_blank()) +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_productlist(labels = c("",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "Bathing",
                                 "Walking",
                                 "Fishing"))
p
dev.off()
saveRDS(p, file = "Animal Attack Combined Plots/mosaic_plot_Animal_Attack_top_3.RDS")

# Tree Fall
df <- read_xlsx("perceived_causes_tree_fall.xlsx")
df <- subset(df, activity != "Total")
df$female <- df$prop_total_female
df$male <- df$prop_total_male
df <- subset(df, select = c(activity, male, female, rank_global))

df <- pivot_longer(cols = c(male, female),
                   names_to = c("gender"),
                   df)
df <- df %>% rename("Freq" = "value")
df <- df %>%
  group_by(gender) %>%
  arrange(Freq)
df$activity <- fct_reorder(df$activity, df$rank_global, .desc = T)
pdf(file = "Tree Fall Plots/mosaic_plot_tree_fall_top_3.pdf", height = 6, width = 8)
p <- df %>% ggplot() +
  geom_mosaic(aes(x = product(activity),
                  weight = Freq,
                  fill = gender),
              divider = c("hspine" , "vspine")) +
  labs(x = "", y = "", fill = "") +
  guides(x = F) +
  theme_classic(base_size = 25) +
  theme(axis.line.x = element_line(color = "white"), axis.line.y = element_line(color = "white"),
        axis.ticks = element_blank()) +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30, hjust = 0.5))
p
dev.off()
saveRDS(p, file = "Tree Fall Plots/mosaic_plot_tree_fall_top_3.RDS")

# Fight
df <- read_xlsx("perceived_causes_fight.xlsx")
df <- subset(df, cause != "Total")
df$female <- df$prop_total_female
df$male <- df$prop_total_male
df <- subset(df, select = c(cause, male, female, rank_global))

df <- pivot_longer(cols = c(male, female),
                   names_to = c("gender"),
                   df)
df <- df %>% rename("Freq" = "value")
df <- df %>%
  group_by(gender) %>%
  arrange(Freq)
df$cause <- fct_reorder(df$cause, df$rank_global, .desc = T)
pdf(file = "Fight Plots/mosaic_plot_fight_top_3.pdf", height = 6, width = 8)
p <- df %>% ggplot() +
  geom_mosaic(aes(x = product(cause),
                  weight = Freq,
                  fill = gender),
              divider = c("hspine" , "vspine")) +
  labs(x = "", y = "", fill = "") +
  guides(x = F) +
  theme_classic(base_size = 25) +
  theme(axis.line.x = element_line(color = "white"), axis.line.y = element_line(color = "white"),
        axis.ticks = element_blank()) +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_productlist(labels = c("",
                                 "",
                                 "",
                                 "Resource Competition",
                                 "Partner Competition",
                                 "Drunk"))
p
dev.off()
saveRDS(p, file = "Fight Plots/mosaic_plot_fight_top_3.RDS")

# Canoe Capsize
df <- read_xlsx("perceived_causes_canoe_capsize.xlsx")
df <- subset(df, activity != "Total")
df$female <- df$prop_total_female
df$male <- df$prop_total_male
df <- subset(df, select = c(activity, male, female, rank_global))

df <- pivot_longer(cols = c(male, female),
                   names_to = c("gender"),
                   df)
df <- df %>% rename("Freq" = "value")
df <- df %>%
  group_by(gender) %>%
  arrange(Freq)
df$activity <- fct_reorder(df$activity, df$rank_global, .desc = T)
pdf(file = "Canoe Capsize Plots/mosaic_plot_canoe_capsize_top_3.pdf", height = 6, width = 8)
p <- df %>% ggplot() +
  geom_mosaic(aes(x = product(activity),
                  weight = Freq,
                  fill = gender),
              divider = c("hspine" , "vspine")) +
  labs(x = "", y = "", fill = "") +
  guides(x = F) +
  theme_classic(base_size = 25) +
  theme(axis.line.x = element_line(color = "white"), axis.line.y = element_line(color = "white"),
        axis.ticks = element_blank()) +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_productlist(labels = c("",
                                 "Crossing River with Platanos",
                                 "Fishing",
                                 "Crossing River"))
p
dev.off()
saveRDS(p, file = "Canoe Capsize Plots/mosaic_plot_canoe_capsize_top_3.RDS")



# Panel plot --------------------------------------------------------------

figure <- ggarrange(readRDS("Sickness Plots/mosaic_plot_sickness_top_3.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Cut Self Plots/mosaic_plot_cut_self_top_3.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Animal Attack Combined Plots/mosaic_plot_Animal_Attack_top_3.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Tree Fall Plots/mosaic_plot_tree_fall_top_3.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Fight Plots/mosaic_plot_fight_top_3.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("Canoe Capsize Plots/mosaic_plot_canoe_capsize_top_3.RDS") + rremove("ylab") + rremove("xlab"),
                    common.legend = T,
                    legend = "bottom")
pdf(file = "Panel Plots/mosaic_panel_plot.pdf", height = 15, width = 20)
annotate_figure(figure, bottom = grid::textGrob("Top 3 Causes/Activities", gp = gpar(cex = 1.7)))
dev.off()
