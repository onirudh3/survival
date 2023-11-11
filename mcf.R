
# Libraries and data ------------------------------------------------------

library(reda)

# Data
df <- read.csv("data_new_format.csv")

# Rename male
df <- df %>% mutate(male = case_when(male == 1 ~ "Male", T ~ "Female"))

# Sickness ----------------------------------------------------------------

mcf <- mcf(Recur(exit, pid, sickness.during.interval, check = "soft") ~ 1, data = df)

p <- plot(mcf, conf.int = T, mark.time = F, addOrigin = T, col = 2) +
  ggplot2::xlab("Age (Years)") +
  ggplot2::theme_classic() +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 80, 5), limits = c(0, 77)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 4.5, 0.5), limits = c(0, 4.3))
p
saveRDS(p, file = "MCF/sickness_global.RDS")


# By sex
mcf <- mcf(Recur(exit, pid, sickness.during.interval, check = "soft") ~ male, data = df)

p <- plot(mcf, conf.int = T, mark.time = F, addOrigin = T, col = 2, legendName = "") +
  ggplot2::xlab("Age (Years)") +
  ggplot2::theme_classic() +
  ggtitle("Sickness") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 80, 5), limits = c(0, 77)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 4.5, 0.5), limits = c(0, 4.3))
p
saveRDS(p, file = "MCF/sickness_by_sex.RDS")


# Cut Self ----------------------------------------------------------------

mcf <- mcf(Recur(exit, pid, cut.self.during.interval, check = "soft") ~ 1, data = df)

p <- plot(mcf, conf.int = T, mark.time = F, addOrigin = T, col = 2) +
  ggplot2::xlab("Age (Years)") +
  ggplot2::theme_classic() +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 80, 5), limits = c(0, 77)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 4.5, 0.5), limits = c(0, 4.3))
p
saveRDS(p, file = "MCF/cut_self_global.RDS")


# By sex
mcf <- mcf(Recur(exit, pid, cut.self.during.interval, check = "soft") ~ male, data = df)

p <- plot(mcf, conf.int = T, mark.time = F, addOrigin = T, col = 2, legendName = "") +
  ggplot2::xlab("Age (Years)") +
  ggplot2::theme_classic() +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 80, 5), limits = c(0, 77)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 4.5, 0.5), limits = c(0, 4.3))
p
saveRDS(p, file = "MCF/cut_self_by_sex.RDS")


# Animal Attack -----------------------------------------------------------

mcf <- mcf(Recur(exit, pid, Animal_Attack.during.interval, check = "soft") ~ 1, data = df)

p <- plot(mcf, conf.int = T, mark.time = F, addOrigin = T, col = 2) +
  ggplot2::xlab("Age (Years)") +
  ggplot2::theme_classic() +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 80, 5), limits = c(0, 77)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 4.5, 0.5), limits = c(0, 4.3))
p
saveRDS(p, file = "MCF/Animal_Attack_global.RDS")


# By sex
mcf <- mcf(Recur(exit, pid, Animal_Attack.during.interval, check = "soft") ~ male, data = df)

p <- plot(mcf, conf.int = T, mark.time = F, addOrigin = T, col = 2, legendName = "") +
  ggplot2::xlab("Age (Years)") +
  ggplot2::theme_classic() +
  ggtitle("Animal Attack") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 80, 5), limits = c(0, 77)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 4.5, 0.5), limits = c(0, 4.3))
p
saveRDS(p, file = "MCF/Animal_Attack_by_sex.RDS")


# Tree Fall ---------------------------------------------------------------

mcf <- mcf(Recur(exit, pid, tree.fall.during.interval, check = "soft") ~ 1, data = df)

p <- plot(mcf, conf.int = T, mark.time = F, addOrigin = T, col = 2) +
  ggplot2::xlab("Age (Years)") +
  ggplot2::theme_classic() +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 80, 5), limits = c(0, 77)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 4.5, 0.5), limits = c(0, 4.3))
p
saveRDS(p, file = "MCF/tree_fall_global.RDS")


# By sex
mcf <- mcf(Recur(exit, pid, tree.fall.during.interval, check = "soft") ~ male, data = df)

p <- plot(mcf, conf.int = T, mark.time = F, addOrigin = T, col = 2, legendName = "") +
  ggplot2::xlab("Age (Years)") +
  ggplot2::theme_classic() +
  ggtitle("Tree Fall") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 80, 5), limits = c(0, 77)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 4.5, 0.5), limits = c(0, 4.3))
p
saveRDS(p, file = "MCF/tree_fall_by_sex.RDS")


# Fight -------------------------------------------------------------------

mcf <- mcf(Recur(exit, pid, fought.during.interval, check = "soft") ~ 1, data = df)

p <- plot(mcf, conf.int = T, mark.time = F, addOrigin = T, col = 2) +
  ggplot2::xlab("Age (Years)") +
  ggplot2::theme_classic() +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 80, 5), limits = c(0, 77)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 4.5, 0.5), limits = c(0, 4.3))
p
saveRDS(p, file = "MCF/fight_global.RDS")


# By sex
mcf <- mcf(Recur(exit, pid, fought.during.interval, check = "soft") ~ male, data = df)

p <- plot(mcf, conf.int = T, mark.time = F, addOrigin = T, col = 2, legendName = "") +
  ggplot2::xlab("Age (Years)") +
  ggplot2::theme_classic() +
  ggtitle("Fight") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 80, 5), limits = c(0, 77)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 4.5, 0.5), limits = c(0, 4.3))
p
saveRDS(p, file = "MCF/fight_by_sex.RDS")


# Canoe Capsize -----------------------------------------------------------

mcf <- mcf(Recur(exit, pid, canoe.capsize.during.interval, check = "soft") ~ 1, data = df)

p <- plot(mcf, conf.int = T, mark.time = F, addOrigin = T, col = 2) +
  ggplot2::xlab("Age (Years)") +
  ggplot2::theme_classic() +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 80, 5), limits = c(0, 77)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 4.5, 0.5), limits = c(0, 4.3))
p
saveRDS(p, file = "MCF/canoe_capsize_global.RDS")


# By sex
mcf <- mcf(Recur(exit, pid, canoe.capsize.during.interval, check = "soft") ~ male, data = df)

p <- plot(mcf, conf.int = T, mark.time = F, addOrigin = T, col = 2, legendName = "") +
  ggplot2::xlab("Age (Years)") +
  ggplot2::theme_classic() +
  ggtitle("Canoe Capsize") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 80, 5), limits = c(0, 77)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 4.5, 0.5), limits = c(0, 4.3))
p
saveRDS(p, file = "MCF/canoe_capsize_by_sex.RDS")


# Panel plots -------------------------------------------------------------

# Global
figure <- ggarrange(readRDS("MCF/sickness_global.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("MCF/cut_self_global.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("MCF/Animal_Attack_global.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("MCF/tree_fall_global.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("MCF/fight_global.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("MCF/canoe_capsize_global.RDS") + rremove("ylab") + rremove("xlab"))

pdf(file = "MCF/panel_plot_global.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("MCF Estimates", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()

# By sex
figure <- ggarrange(readRDS("MCF/sickness_by_sex.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("MCF/cut_self_by_sex.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("MCF/Animal_Attack_by_sex.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("MCF/tree_fall_by_sex.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("MCF/fight_by_sex.RDS") + rremove("ylab") + rremove("xlab"),
                    readRDS("MCF/canoe_capsize_by_sex.RDS") + rremove("ylab") + rremove("xlab"),
                    common.legend = T, legend = "bottom")

pdf(file = "MCF/panel_plot_by_sex.pdf", height = 9, width = 15)
annotate_figure(figure, left = textGrob("MCF Estimates", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = grid::textGrob("Age (Years)", gp = gpar(cex = 1.3)))
dev.off()
