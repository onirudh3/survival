
# Libraries and data ------------------------------------------------------

library(tidyverse)
library(survival)
# library(ggfortify)
library(survminer)
library(ggpubr)
library(grid)

# Sickness ----------------------------------------------------------------

df <- read.csv("sickness_final_table.csv")
df <- subset(df, !(event.episode %in% c(0)))
fit <- survfit(Surv(exit, event) ~ event.episode, df, conf.type = "log-log")
p <- ggsurvplot(fit, conf.int = TRUE, title = "Sickness", font.title = "30",
                surv.scale = "percent", legend.labs = c("1", "2", "3"),
                legend.title = "Episode", axes.offset = F, pval = T,
                break.x.by = 5, legend = c(0.75, 0.7), xlim = c(0, 75),
                xlab = "", ylab = "")
p$plot + theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced risk")
saveRDS(p$plot + theme(plot.title = element_text(size = 30, hjust = 0.5)),
        file = "Survival by Episode/sickness.RDS")

# autoplot(fit) +
#   geom_segment(aes(x = 0, y = 0.5, xend = 75, yend = 0.5), lty = 2) +
#   theme_classic(base_size = 14) +
#   labs(color = "Episode", x = "Age [years]", y = "Proportion not having experienced risk") +
#   ggtitle("Sickness") +
#   theme(plot.title = element_text(size = 30, hjust = 0.5), legend.position = c(0.75, 0.7),
#         legend.background = element_blank()) +
#   guides(fill = "none") +
#   scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
#   scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


# Cut Self ----------------------------------------------------------------

df <- read.csv("cut_self_final_table.csv")
df <- subset(df, !(event.episode %in% c(0)))
fit <- survfit(Surv(exit, event) ~ event.episode, df, conf.type = "log-log")
p <- ggsurvplot(fit, conf.int = TRUE, title = "Cut Self", font.title = "30",
                surv.scale = "percent", legend.labs = c("1", "2", "3", "4", "5", "6"),
                legend.title = "Episode", axes.offset = F, pval = T,
                break.x.by = 5, legend = c(0.75, 0.7), xlim = c(0, 75),
                xlab = "", ylab = "")
p$plot + theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced risk")
saveRDS(p$plot + theme(plot.title = element_text(size = 30, hjust = 0.5)),
        file = "Survival by Episode/cut_self.RDS")


# Animal Attack -----------------------------------------------------------

df <- read.csv("Animal_Attack_combined_final_table.csv")
df <- subset(df, !(event.episode %in% c(0)))
fit <- survfit(Surv(exit, event) ~ event.episode, df, conf.type = "log-log")
p <- ggsurvplot(fit, conf.int = TRUE, title = "Animal Attack", font.title = "30",
                surv.scale = "percent", legend.labs = c("1", "2", "3", "4"),
                legend.title = "Episode", axes.offset = F, pval = T,
                break.x.by = 5, legend = c(0.75, 0.7), xlim = c(0, 75),
                xlab = "", ylab = "")
p$plot + theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced risk")
saveRDS(p$plot + theme(plot.title = element_text(size = 30, hjust = 0.5)),
        file = "Survival by Episode/Animal_Attack.RDS")


# Tree Fall ---------------------------------------------------------------

df <- read.csv("tree_fall_final_table.csv")
df <- subset(df, !(event.episode %in% c(0)))
fit <- survfit(Surv(exit, event) ~ event.episode, df, conf.type = "log-log")
p <- ggsurvplot(fit, conf.int = TRUE, title = "Tree Fall", font.title = "30",
                surv.scale = "percent", legend.labs = c("1", "2", "3"),
                legend.title = "Episode", axes.offset = F, pval = T,
                break.x.by = 5, legend = c(0.75, 0.7), xlim = c(0, 75),
                xlab = "", ylab = "")
p$plot + theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced risk")
saveRDS(p$plot + theme(plot.title = element_text(size = 30, hjust = 0.5)),
        file = "Survival by Episode/tree_fall.RDS")


# Fight -------------------------------------------------------------------

df <- read.csv("fought_final_table.csv")
df <- subset(df, !(event.episode %in% c(0)))
fit <- survfit(Surv(exit, event) ~ event.episode, df, conf.type = "log-log")
p <- ggsurvplot(fit, conf.int = TRUE, title = "Fight", font.title = "30",
                surv.scale = "percent", legend.labs = c("1", "2", "3"),
                legend.title = "Episode", axes.offset = F, pval = T,
                break.x.by = 5, legend = c(0.75, 0.7), xlim = c(0, 75),
                xlab = "", ylab = "")
p$plot + theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced risk")
saveRDS(p$plot + theme(plot.title = element_text(size = 30, hjust = 0.5)),
        file = "Survival by Episode/fight.RDS")


# Canoe Capsize -----------------------------------------------------------

df <- read.csv("canoe_capsize_final_table.csv")
df <- subset(df, !(event.episode %in% c(0)))
df$event.episode <- as.character(df$event.episode)
fit <- survfit(Surv(exit, event) ~ event.episode, df, conf.type = "log-log")
p <- ggsurvplot(fit, conf.int = TRUE, title = "Canoe Capsize", font.title = "30",
           surv.scale = "percent", legend.labs = c("1", "2", "3"),
           legend.title = "Episode", axes.offset = F, pval = T,
           break.x.by = 5, legend = c(0.75, 0.7), xlim = c(0, 75),
           xlab = "", ylab = "")
p$plot + theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  xlab("Age [years]") +
  ylab("Proportion not having experienced risk")
saveRDS(p$plot + theme(plot.title = element_text(size = 30, hjust = 0.5)),
        file = "Survival by Episode/canoe_capsize.RDS")

# Panel plot --------------------------------------------------------------

figure <- ggarrange(readRDS("Survival by Episode/sickness.RDS"),
                    readRDS("Survival by Episode/cut_self.RDS"),
                    readRDS("Survival by Episode/Animal_Attack.RDS"),
                    readRDS("Survival by Episode/tree_fall.RDS"),
                    readRDS("Survival by Episode/fight.RDS"),
                    readRDS("Survival by Episode/canoe_capsize.RDS"))

pdf(file = "Survival by Episode/panel_plot.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("Proportion not having experienced risk",
                                        rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age [Years]", gp = gpar(cex = 1.3)))
dev.off()
