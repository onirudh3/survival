
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(ggmosaic)
library(forcats)

# Sickness ----------------------------------------------------------------

df <- read_xlsx("perceived_causes_sickness.xlsx")
df <- subset(df, type != "Total")
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
  theme(axis.line.x = element_line(color = "white"), axis.line.y = element_line(color = "white")) +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5))
dev.off()

# Cut Self ----------------------------------------------------------------

df <- read_xlsx("perceived_causes_cut_self.xlsx")
df <- subset(df, activity != "Total")
df <- subset(df, select = c(activity, male, female, rank_global))

df <- pivot_longer(cols = c(male, female),
                   names_to = c("gender"),
                   df)
df <- df %>% rename("Freq" = "value")
df <- df %>%
  group_by(gender) %>%
  arrange(Freq)

df$type <- fct_reorder(df$activity, df$rank_global, .desc = T)

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

# Animal Attack ----------------------------------------------------------------

df <- read_xlsx("perceived_causes_Animal_Attack.xlsx")
df <- subset(df, activity != "Total")
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

# Tree Fall ----------------------------------------------------------------

df <- read_xlsx("perceived_causes_tree_fall.xlsx")
df <- subset(df, activity != "Total")
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

# Fight ----------------------------------------------------------------

df <- read_xlsx("perceived_causes_fight.xlsx")
df <- subset(df, cause != "Total")
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

# Canoe Capsize ----------------------------------------------------------------

df <- read_xlsx("perceived_causes_canoe_capsize.xlsx")
df <- subset(df, activity != "Total")
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





