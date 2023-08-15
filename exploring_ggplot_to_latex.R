

library(tidyverse)
library(ggpubr)
library(showtext)
library(grid)

font_add(family = "CMU", regular = "C:/Users/oniru/AppData/Local/Microsoft/Windows/Fonts/cmunrm.ttf")
showtext_auto()


figure <- ggarrange(readRDS("Sickness Plots/hazard_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab") + theme(text = element_text(family = "CMU")),
                    readRDS("Cut Self Plots/hazard_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab") + theme(text = element_text(family = "CMU")),
                    readRDS("Animal Attack Combined Plots/hazard_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab") + theme(text = element_text(family = "CMU")),
                    readRDS("Tree Fall Plots/hazard_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab") + theme(text = element_text(family = "CMU")),
                    readRDS("Fight Plots/hazard_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab") + theme(text = element_text(family = "CMU")),
                    readRDS("Canoe Capsize Plots/hazard_function_time_to_first_risk_by_gender.RDS") + rremove("ylab") + rremove("xlab") + theme(text = element_text(family = "CMU")),
                    common.legend = T, legend = "bottom")

# pdf(file = "Panel Plots/plot.pdf", height = 9, width = 15)
annotate_figure(figure, left = text_grob("Hazard", rot = 90, vjust = 1, size = 15, family = "CMU"),
                     bottom = text_grob("Age (Years)", size = 15, family = "CMU"))
# dev.off()

# This is not needed
