
# Libraries and data import -----------------------------------------------

library(coxme)
library(xtable)
library(tidyverse)

# Import time-to-first-risk data
df <- read.csv("tree_fall_time_to_first_risk_short_interval.csv")

# Extract estimates from coxme objects
extract_coxme_table <- function (mod){
  beta <- mod$coefficients
  exp_beta <- exp(beta)
  nvar <- length(beta)
  nfrail <- nrow(mod$var) - nvar
  se <- sqrt(diag(mod$var)[nfrail + 1:nvar])
  p <- signif(1 - pchisq((beta/se) ^ 2, 1), 2)
  table = data.frame(cbind(beta, exp_beta, se, p))
  return(table)
}


# Summary Table Mixed Effects ---------------------------------------------

model1a <- coxph(Surv(exit, tree.fall.during.interval) ~ 1, data = df)

model1b <- coxme(Surv(exit, tree.fall.during.interval) ~ 1 + (1 | pid),
                 data = df)

model1c <- coxme(Surv(exit, tree.fall.during.interval) ~ 1 + (1 | pid) +
                   (1 | house.id), data = df)

model1d <- coxme(Surv(exit, tree.fall.during.interval) ~ 1 + (1 | pid) +
                   (1 | house.id) + strata(region), data = df)

model1e <- coxme(Surv(exit, tree.fall.during.interval) ~ 1 + (1 | pid) +
                   (1 | house.id) + strata(region) + male, data = df)

model1f <- coxme(Surv(exit, tree.fall.during.interval) ~ 1 + (1 | pid) +
                   (1 | house.id) + strata(region) + male +
                   strata(tercile), data = df)

model1g <- coxme(Surv(exit, tree.fall.during.interval) ~ 1 + (1 | pid) +
                   strata(region) + male + strata(tercile), data = df)


## Comparing model fit
aov_test <- anova(model1a, model1b, model1c, model1d, model1e, model1f, model1g)
aov_test

aic_test <- AIC(model1a, model1b, model1c, model1d, model1e, model1f, model1g)
aic_test

## Tabulating
df_coxme <- cbind(aov_test, aic_test)
df_coxme <- subset(df_coxme, select = -c(Df))
df_coxme <- data.frame(t(df_coxme))
colnames(df_coxme) <- c("1", "2", "3", "4", "5", "6", "7")

addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 5
addtorow$pos[[3]] <- 5
addtorow$pos[[4]] <- 5
addtorow$pos[[5]] <- 5
addtorow$pos[[6]] <- 5
addtorow$pos[[7]] <- 5
addtorow$pos[[8]] <- 5
addtorow$command <- c('\\hline',
                      '\\hline',
                      'PID RE & No & Yes & Yes & Yes & Yes & Yes & Yes \\\\\n',
                      'House ID RE & No & No & Yes & Yes & Yes & Yes & No \\\\\n',
                      'Region FE (Stratified) & No & No & No & Yes & Yes & Yes & Yes \\\\\n',
                      'Male FE (Unstratified) & No & No & No & No & Yes & Yes & Yes \\\\\n',
                      'Tercile FE (Stratified) & No & No & No & No & No & Yes & Yes \\\\\n',
                      '\\hline')
x <- xtable(df_coxme, caption = "Tree Fall Mixed Effects Specifications")
align(x) <- "lccccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/mixed_effects_summary.tex")



# RE PID, House ID --------------------------------------------------------


# Sex ---------------------------------------------------------------------
# PH not violated for sex
model2 <- coxme(Surv(exit, tree.fall.during.interval) ~ male + (1 | pid) + (1 | house.id), df)
results <- extract_coxme_table(model2)
b <- data.frame(confint(model2))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex.tex")
c <- data.frame(t(data.frame(VarCorr(model2))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar.tex")

# Sickness
model2a <- coxme(Surv(exit, tree.fall.during.interval) ~ male + sickness.during.interval +
                   (1 | pid) + (1 | house.id), df)
saveRDS(model2a, file = "Tree Fall Tables/sex_sickness.RDS")
results <- extract_coxme_table(model2a)
b <- data.frame(confint(model2a))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Sickness")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_sickness.tex")
c <- data.frame(t(data.frame(VarCorr(model2a))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_sickness.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_sickness.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_sickness.RDS")))
dev.off()

# Animal Attack
model2b <- coxme(Surv(exit, tree.fall.during.interval) ~ male + Animal_Attack.during.interval +
                   (1 | pid) + (1 | house.id), df)
saveRDS(model2b, file = "Tree Fall Tables/sex_Animal_Attack.RDS")
results <- extract_coxme_table(model2b)
b <- data.frame(confint(model2b))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Animal Attack")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_Animal_Attack.tex")
c <- data.frame(t(data.frame(VarCorr(model2b))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_Animal_Attack.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_Animal_Attack.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_Animal_Attack.RDS")))
dev.off()

# Cut Self
model2c <- coxme(Surv(exit, tree.fall.during.interval) ~ male + cut.self.during.interval +
                   (1 | pid) + (1 | house.id), df)
saveRDS(model2c, file = "Tree Fall Tables/sex_cut_self.RDS")
results <- extract_coxme_table(model2c)
b <- data.frame(confint(model2c))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Cut Self")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_cut_self.tex")
c <- data.frame(t(data.frame(VarCorr(model2c))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_cut_self.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_cut_self.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_cut_self.RDS")))
dev.off()

# Canoe Capsize
moedl2d <- coxme(Surv(exit, tree.fall.during.interval) ~ male + canoe.capsize.during.interval +
                   (1 | pid) + (1 | house.id), df)
saveRDS(moedl2d, file = "Tree Fall Tables/sex_canoe_capsize.RDS")
results <- extract_coxme_table(moedl2d)
b <- data.frame(confint(moedl2d))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Canoe Capsize")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_canoe_capsize.tex")
c <- data.frame(t(data.frame(VarCorr(moedl2d))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_canoe_capsize.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_canoe_capsize.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_canoe_capsize.RDS")))
dev.off()

# Fight
moedl2e <- coxme(Surv(exit, tree.fall.during.interval) ~ male + fought.during.interval +
                   (1 | pid) + (1 | house.id), df)
saveRDS(moedl2e, file = "Tree Fall Tables/sex_fight.RDS")
results <- extract_coxme_table(moedl2e)
b <- data.frame(confint(moedl2e))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Fight")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_fight.tex")
c <- data.frame(t(data.frame(VarCorr(moedl2e))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_fight.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_fight.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_fight.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_fight.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_fight.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_sickness.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_sickness.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_Animal_Attack.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_Animal_Attack.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_cut_self.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_cut_self.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_canoe_capsize.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_canoe_capsize.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Tree Fall Tables/sex_fight.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/sex_sickness.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/sex_cut_self.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/sex_canoe_capsize.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/sex_Animal_Attack.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "fought.during.interval" ~ "Fight",
                               covariate == "sickness.during.interval" ~ "Sickness",
                               covariate == "cut.self.during.interval" ~ "Cut Self",
                               covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Tree Fall Plots/sex_panel_plot.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = p.y), stat = "identity", width = 0.9, alpha = 0.35) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "PID and House ID RE, Male FE") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("black", "lightcoral")) +
  theme_classic(base_size = 20) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable:
Tree Fall") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 19)) + theme(legend.position = "none")
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Tree Fall Plots/sex_panel_plot.RDS")


# Sex + Interaction -------------------------------------------------------

# PH not violated for sex
model2interaction <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + (1 | pid) + (1 | house.id), df)
results <- extract_coxme_table(model2interaction)
b <- data.frame(confint(model2interaction))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_interaction.tex")
c <- data.frame(t(data.frame(VarCorr(model2interaction))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_interaction.tex")

# Sickness
model2interactiona <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + sickness.during.interval +
                              (1 | pid) + (1 | house.id), df)
saveRDS(model2interactiona, file = "Tree Fall Tables/sex_sickness_interaction.RDS")
results <- extract_coxme_table(model2interactiona)
b <- data.frame(confint(model2interactiona))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Sickness", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_sickness_interaction.tex")
c <- data.frame(t(data.frame(VarCorr(model2interactiona))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_sickness_interaction.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_sickness_interaction.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_sickness_interaction.RDS")))
dev.off()

# Animal Attack Error in ikmat %*% fcoef : NA/NaN/Inf in foreign function call (arg 9)
# model2interactionb <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + Animal_Attack.during.interval +
#                               (1 | pid) + (1 | house.id), df)
# saveRDS(model2interactionb, file = "Tree Fall Tables/sex_Animal_Attack_interaction.RDS")
# results <- extract_coxme_table(model2interactionb)
# b <- data.frame(confint(model2interactionb))
# results <- cbind(results, b)
# results <- round(results, 3)
# results <- results %>% rename("Coef" = "beta",
#                               "exp(Coef)" = "exp_beta",
#                               "SE" = "se",
#                               "Lower CI" = "X2.5..",
#                               "Upper CI" = "X97.5..")
# rownames(results) <- c("Male", "Animal Attack", "Male x Age")
# results <- data.frame(t(results))
# results <- data.frame(t(results))
# addtorow <- list()
# addtorow$pos <- list()
# addtorow$pos[[1]] <- -1
# addtorow$pos[[2]] <- 3
# addtorow$pos[[3]] <- 3
# addtorow$pos[[4]] <- 3
# addtorow$pos[[5]] <- 3
# addtorow$command <- c('\\hline ',
#                       '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
#                       'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
#                       'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
#                       '\\hline ')
# x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
# align(x) <- "lcccccc"
# print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_Animal_Attack_interaction.tex")
# c <- data.frame(t(data.frame(VarCorr(model2interactionb))))
# colnames(c) <- c("RE Variance")
# print(xtable(c), file = "Tree Fall Tables/sexvar_Animal_Attack_interaction.tex")
# # Plot Schoenfeld residuals
# pdf(file = "Tree Fall Plots/sex_schoenfeld_Animal_Attack_interaction.pdf", height = 5, width = 7)
# survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_Animal_Attack_interaction.RDS")))
# dev.off()

# Cut Self
model2interactionc <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + cut.self.during.interval +
                              (1 | pid) + (1 | house.id), df)
saveRDS(model2interactionc, file = "Tree Fall Tables/sex_cut_self_interaction.RDS")
results <- extract_coxme_table(model2interactionc)
b <- data.frame(confint(model2interactionc))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Cut Self", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_cut_self_interaction.tex")
c <- data.frame(t(data.frame(VarCorr(model2interactionc))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_cut_self_interaction.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_cut_self_interaction.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_cut_self_interaction.RDS")))
dev.off()

# Canoe Capsize
moedl2d <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + canoe.capsize.during.interval +
                   (1 | pid) + (1 | house.id), df)
saveRDS(moedl2d, file = "Tree Fall Tables/sex_canoe_capsize_interaction.RDS")
results <- extract_coxme_table(moedl2d)
b <- data.frame(confint(moedl2d))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Canoe Capsize", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_canoe_capsize_interaction.tex")
c <- data.frame(t(data.frame(VarCorr(moedl2d))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_canoe_capsize_interaction.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_canoe_capsize_interaction.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_canoe_capsize_interaction.RDS")))
dev.off()

# Fight
moedl2e <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + fought.during.interval +
                   (1 | pid) + (1 | house.id), df)
saveRDS(moedl2e, file = "Tree Fall Tables/sex_fight_interaction.RDS")
results <- extract_coxme_table(moedl2e)
b <- data.frame(confint(moedl2e))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Fight", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_fight_interaction.tex")
c <- data.frame(t(data.frame(VarCorr(moedl2e))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_fight_interaction.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_fight_interaction.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_fight_interaction.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_fight_interaction.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_fight_interaction.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_sickness_interaction.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_sickness_interaction.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_cut_self_interaction.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_cut_self_interaction.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_canoe_capsize_interaction.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_canoe_capsize_interaction.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Tree Fall Tables/sex_fight_interaction.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/sex_sickness_interaction.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/sex_cut_self_interaction.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/sex_canoe_capsize_interaction.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "fought.during.interval" ~ "Fight",
                               covariate == "sickness.during.interval" ~ "Sickness",
                               covariate == "cut.self.during.interval" ~ "Cut Self",
                               covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Tree Fall Plots/sex_panel_plot_interaction.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = p.y), stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID and House ID") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable: Tree Fall") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 17))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Tree Fall Plots/sex_panel_plot_interaction.RDS")

# Sex + Region ------------------------------------------------------------
# PH violated for region
model3 <- coxme(Surv(exit, tree.fall.during.interval) ~ male + strata(region) + (1 | pid) + (1 | house.id), df)
results <- extract_coxme_table(model3)
b <- data.frame(confint(model3))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region.tex")
c <- data.frame(t(data.frame(VarCorr(model3))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar.tex")

# Sickness
model3a <- coxme(Surv(exit, tree.fall.during.interval) ~ male + sickness.during.interval +
                   strata(region) + (1 | pid) + (1 | house.id), df)
saveRDS(model3a, file = "Tree Fall Tables/region_sickness.RDS")
results <- extract_coxme_table(model3a)
b <- data.frame(confint(model3a))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Sickness")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_sickness.tex")
c <- data.frame(t(data.frame(VarCorr(model3a))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_sickness.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/region_schoenfeld_sickness.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_sickness.RDS")))
dev.off()


# Animal Attack
model3b <- coxme(Surv(exit, tree.fall.during.interval) ~ male + Animal_Attack.during.interval +
                   strata(region) + (1 | pid) + (1 | house.id), df)
saveRDS(model3b, file = "Tree Fall Tables/region_Animal_Attack.RDS")
results <- extract_coxme_table(model3b)
b <- data.frame(confint(model3b))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Animal Attack")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_Animal_Attack.tex")
c <- data.frame(t(data.frame(VarCorr(model3b))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_Animal_Attack.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/region_schoenfeld_Animal_Attack.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_Animal_Attack.RDS")))
dev.off()

# Cut Self - ERROR

# Canoe Capsize - ERROR

# Fight
model3e <- coxme(Surv(exit, tree.fall.during.interval) ~ male + fought.during.interval +
                   strata(region) + (1 | pid) + (1 | house.id), df)
saveRDS(model3e, file = "Tree Fall Tables/region_fight.RDS")
results <- extract_coxme_table(model3e)
b <- data.frame(confint(model3e))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Fight")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_fight.tex")
c <- data.frame(t(data.frame(VarCorr(model3e))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_fight.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/region_schoenfeld_fight.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_fight.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Tree Fall Tables/region_fight.RDS")), data.frame(confint(readRDS("Tree Fall Tables/region_fight.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/region_sickness.RDS")), data.frame(confint(readRDS("Tree Fall Tables/region_sickness.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/region_Animal_Attack.RDS")), data.frame(confint(readRDS("Tree Fall Tables/region_Animal_Attack.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Tree Fall Tables/region_fight.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/region_sickness.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/region_Animal_Attack.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "fought.during.interval" ~ "Fight",
                               covariate == "sickness.during.interval" ~ "Sickness",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Tree Fall Plots/region_panel_plot.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = p.y), stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID and House ID") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable: Tree Fall") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 19))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Tree Fall Plots/region_panel_plot.RDS")

# Sex + Region + Interaction ----------------------------------------------
# PH violated for region
model3interaction <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + strata(region) + (1 | pid) + (1 | house.id), df)
results <- extract_coxme_table(model3interaction)
b <- data.frame(confint(model3interaction))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_interaction.tex")
c <- data.frame(t(data.frame(VarCorr(model3interaction))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_interaction.tex")

# Sickness ERROR, huge confidence intervals
# model3interactiona <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + sickness.during.interval +
#                               strata(region) + (1 | pid) + (1 | house.id), df)
# saveRDS(model3interactiona, file = "Tree Fall Tables/region_sickness_interaction.RDS")
# results <- extract_coxme_table(model3interactiona)
# b <- data.frame(confint(model3interactiona))
# results <- cbind(results, b)
# results <- round(results, 3)
# results <- results %>% rename("Coef" = "beta",
#                               "exp(Coef)" = "exp_beta",
#                               "SE" = "se",
#                               "Lower CI" = "X2.5..",
#                               "Upper CI" = "X97.5..")
# rownames(results) <- c("Male", "Sickness", "Male x Age")
# results <- data.frame(t(results))
# results <- data.frame(t(results))
# addtorow <- list()
# addtorow$pos <- list()
# addtorow$pos[[1]] <- -1
# addtorow$pos[[2]] <- 3
# addtorow$pos[[3]] <- 3
# addtorow$pos[[4]] <- 3
# addtorow$pos[[5]] <- 3
# addtorow$command <- c('\\hline ',
#                       '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
#                       'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
#                       'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
#                       '\\hline ')
# x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
# align(x) <- "lcccccc"
# print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_sickness_interaction.tex")
# c <- data.frame(t(data.frame(VarCorr(model3interactiona))))
# colnames(c) <- c("RE Variance")
# print(xtable(c), file = "Tree Fall Tables/regionvar_sickness_interaction.tex")
# # Plot Schoenfeld residuals
# pdf(file = "Tree Fall Plots/region_schoenfeld_sickness_interaction.pdf", height = 5, width = 7)
# survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_sickness_interaction.RDS")))
# dev.off()


# Animal Attack
model3interactionb <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + Animal_Attack.during.interval +
                              strata(region) + (1 | pid) + (1 | house.id), df)
saveRDS(model3interactionb, file = "Tree Fall Tables/region_Animal_Attack_interaction.RDS")
results <- extract_coxme_table(model3interactionb)
b <- data.frame(confint(model3interactionb))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Animal Attack", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_Animal_Attack_interaction.tex")
c <- data.frame(t(data.frame(VarCorr(model3interactionb))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_Animal_Attack_interaction.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/region_schoenfeld_Animal_Attack_interaction.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_Animal_Attack_interaction.RDS")))
dev.off()

# Cut Self
model3interactionc <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + cut.self.during.interval +
                              strata(region) + (1 | pid) + (1 | house.id), df)
saveRDS(model3interactionc, file = "Tree Fall Tables/region_cut_self_interaction.RDS")
results <- extract_coxme_table(model3interactionc)
b <- data.frame(confint(model3interactionc))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Cut Self", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_cut_self_interaction.tex")
c <- data.frame(t(data.frame(VarCorr(model3interactionc))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_cut_self_interaction.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/region_schoenfeld_cut_self_interaction.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_cut_self_interaction.RDS")))
dev.off()

# Canoe Capsize
model3interactiond <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + canoe.capsize.during.interval +
                              strata(region) + (1 | pid) + (1 | house.id), df)
saveRDS(model3interactiond, file = "Tree Fall Tables/region_canoe_capsize_interaction.RDS")
results <- extract_coxme_table(model3interactiond)
b <- data.frame(confint(model3interactiond))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Canoe Capsize", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_canoe_capsize_interaction.tex")
c <- data.frame(t(data.frame(VarCorr(model3interactiond))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_canoe_capsize_interaction.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/region_schoenfeld_canoe_capsize_interaction.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_canoe_capsize_interaction.RDS")))
dev.off()

# Fight
model3interactione <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + fought.during.interval +
                              strata(region) + (1 | pid) + (1 | house.id), df)
saveRDS(model3interactione, file = "Tree Fall Tables/region_fight_interaction.RDS")
results <- extract_coxme_table(model3interactione)
b <- data.frame(confint(model3interactione))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Fight", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_fight_interaction.tex")
c <- data.frame(t(data.frame(VarCorr(model3interactione))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_fight_interaction.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/region_schoenfeld_fight_interaction.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_fight_interaction.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Tree Fall Tables/region_fight_interaction.RDS")), data.frame(confint(readRDS("Tree Fall Tables/region_fight_interaction.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/region_cut_self_interaction.RDS")), data.frame(confint(readRDS("Tree Fall Tables/region_cut_self_interaction.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/region_canoe_capsize_interaction.RDS")), data.frame(confint(readRDS("Tree Fall Tables/region_canoe_capsize_interaction.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/region_Animal_Attack_interaction.RDS")), data.frame(confint(readRDS("Tree Fall Tables/region_Animal_Attack_interaction.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Tree Fall Tables/region_fight_interaction.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/region_cut_self_interaction.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/region_canoe_capsize_interaction.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/region_Animal_Attack_interaction.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "fought.during.interval" ~ "Fight",
                               covariate == "cut.self.during.interval" ~ "Cut Self",
                               covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Tree Fall Plots/region_panel_plot_interaction.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = p.y), stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID and House ID") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable: Tree Fall") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, NA))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Tree Fall Plots/region_panel_plot_interaction.RDS")

# Sex + Region + Calendar Year Tercile ------------------------------------
# PH violated for tercile
model4 <- coxme(Surv(exit, tree.fall.during.interval) ~ male + strata(region) +
                  strata(tercile) + (1 | pid) + (1 | house.id), df)
results <- extract_coxme_table(model4)
b <- data.frame(confint(model4))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile.tex")
c <- data.frame(t(data.frame(VarCorr(model4))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar.tex")

# Sickness
model4a <- coxme(Surv(exit, tree.fall.during.interval) ~ male + sickness.during.interval +
                   strata(region) + strata(tercile) + (1 | pid) + (1 | house.id), df)
saveRDS(model4a, file = "Tree Fall Tables/tercile_sickness.RDS")
results <- extract_coxme_table(model4a)
b <- data.frame(confint(model4a))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Sickness")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_sickness.tex")
c <- data.frame(t(data.frame(VarCorr(model4a))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_sickness.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_sickness.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_sickness.RDS")))
dev.off()

# Animal Attack
model4b <- coxme(Surv(exit, tree.fall.during.interval) ~ male + Animal_Attack.during.interval +
                   strata(region) + strata(tercile) + (1 | pid) + (1 | house.id), df)
saveRDS(model4b, file = "Tree Fall Tables/tercile_Animal_Attack.RDS")
results <- extract_coxme_table(model4b)
b <- data.frame(confint(model4b))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Animal Attack")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_Animal_Attack.tex")
c <- data.frame(t(data.frame(VarCorr(model4b))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_Animal_Attack.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_Animal_Attack.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_Animal_Attack.RDS")))
dev.off()

# Cut Self
model4c <- coxme(Surv(exit, tree.fall.during.interval) ~ male + cut.self.during.interval +
                   strata(region) + strata(tercile) + (1 | pid) + (1 | house.id), df)
saveRDS(model4c, file = "Tree Fall Tables/tercile_cut_self.RDS")
results <- extract_coxme_table(model4c)
b <- data.frame(confint(model4c))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Cut Self")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_cut_self.tex")
c <- data.frame(t(data.frame(VarCorr(model4c))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_cut_self.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_cut_self.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_cut_self.RDS")))
dev.off()

# Canoe Capsize
model4d <- coxme(Surv(exit, tree.fall.during.interval) ~ male + canoe.capsize.during.interval +
                   strata(region) + strata(tercile) + (1 | pid) + (1 | house.id), df)
saveRDS(model4d, file = "Tree Fall Tables/tercile_canoe_capsize.RDS")
results <- extract_coxme_table(model4d)
b <- data.frame(confint(model4d))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Canoe Capsize")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_canoe_capsize.tex")
c <- data.frame(t(data.frame(VarCorr(model4d))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_canoe_capsize.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_canoe_capsize.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_canoe_capsize.RDS")))
dev.off()

# Fight
model4e <- coxme(Surv(exit, tree.fall.during.interval) ~ male + fought.during.interval +
                   strata(region) + strata(tercile) + (1 | pid) + (1 | house.id), df)
saveRDS(model4e, file = "Tree Fall Tables/tercile_fight.RDS")
results <- extract_coxme_table(model4e)
b <- data.frame(confint(model4e))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Fight")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_fight.tex")
c <- data.frame(t(data.frame(VarCorr(model4e))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_fight.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_fight.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_fight.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_fight.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_fight.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_sickness.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_sickness.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_Animal_Attack.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_Animal_Attack.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_cut_self.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_cut_self.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_canoe_capsize.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_canoe_capsize.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_fight.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_sickness.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_Animal_Attack.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_cut_self.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_canoe_capsize.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "fought.during.interval" ~ "Fight",
                               covariate == "sickness.during.interval" ~ "Sickness",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack",
                               covariate == "cut.self.during.interval" ~ "Cut Self",
                               covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Tree Fall Plots/tercile_panel_plot.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = p.y), stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID and House ID") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable: Tree Fall") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 10.5))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Tree Fall Plots/tercile_panel_plot.RDS")


# Sex + Region + Tercile + Interaction ------------------------------------

# PH violated for tercile
model4interaction <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + strata(region) +
                             strata(tercile) + (1 | pid) + (1 | house.id), df)
results <- extract_coxme_table(model4interaction)
b <- data.frame(confint(model4interaction))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_interaction.tex")
c <- data.frame(t(data.frame(VarCorr(model4interaction))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_interaction.tex")


# Sickness
model4interactiona <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + sickness.during.interval +
                              strata(region) + strata(tercile) + (1 | pid) + (1 | house.id), df)
saveRDS(model4interactiona, file = "Tree Fall Tables/tercile_sickness_interaction.RDS")
results <- extract_coxme_table(model4interactiona)
b <- data.frame(confint(model4interactiona))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Sickness", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_sickness_interaction.tex")
c <- data.frame(t(data.frame(VarCorr(model4interactiona))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_sickness_interaction.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_sickness_interaction.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_sickness_interaction.RDS")))
dev.off()

# Animal Attack
model4interactionb <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + Animal_Attack.during.interval +
                              strata(region) + strata(tercile) + (1 | pid) + (1 | house.id), df)
saveRDS(model4interactionb, file = "Tree Fall Tables/tercile_Animal_Attack_interaction.RDS")
results <- extract_coxme_table(model4interactionb)
b <- data.frame(confint(model4interactionb))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Animal Attack", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_Animal_Attack_interaction.tex")
c <- data.frame(t(data.frame(VarCorr(model4interactionb))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_Animal_Attack_interaction.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_Animal_Attack_interaction.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_Animal_Attack_interaction.RDS")))
dev.off()

# Cut Self
model4interactionc <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + cut.self.during.interval +
                              strata(region) + strata(tercile) + (1 | pid) + (1 | house.id), df)
saveRDS(model4interactionc, file = "Tree Fall Tables/tercile_cut_self_interaction.RDS")
results <- extract_coxme_table(model4interactionc)
b <- data.frame(confint(model4interactionc))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Cut Self", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_cut_self_interaction.tex")
c <- data.frame(t(data.frame(VarCorr(model4interactionc))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_cut_self_interaction.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_cut_self_interaction.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_cut_self_interaction.RDS")))
dev.off()

# Canoe Capsize
model4interactiond <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + canoe.capsize.during.interval +
                              strata(region) + strata(tercile) + (1 | pid) + (1 | house.id), df)
saveRDS(model4interactiond, file = "Tree Fall Tables/tercile_canoe_capsize_interaction.RDS")
results <- extract_coxme_table(model4interactiond)
b <- data.frame(confint(model4interactiond))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Canoe Capsize", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_canoe_capsize_interaction.tex")
c <- data.frame(t(data.frame(VarCorr(model4interactiond))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_canoe_capsize_interaction.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_canoe_capsize_interaction.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_canoe_capsize_interaction.RDS")))
dev.off()

# Fight
model4interactione <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + fought.during.interval +
                              strata(region) + strata(tercile) + (1 | pid) + (1 | house.id), df)
saveRDS(model4interactione, file = "Tree Fall Tables/tercile_fight_interaction.RDS")
results <- extract_coxme_table(model4interactione)
b <- data.frame(confint(model4interactione))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Fight", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_fight_interaction.tex")
c <- data.frame(t(data.frame(VarCorr(model4interactione))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_fight_interaction.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_fight_interaction.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_fight_interaction.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_fight_interaction.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_fight_interaction.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_sickness_interaction.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_sickness_interaction.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_Animal_Attack_interaction.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_Animal_Attack_interaction.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_cut_self_interaction.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_cut_self_interaction.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_canoe_capsize_interaction.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_canoe_capsize_interaction.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_fight_interaction.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_sickness_interaction.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_Animal_Attack_interaction.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_cut_self_interaction.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_canoe_capsize_interaction.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "fought.during.interval" ~ "Fight",
                               covariate == "sickness.during.interval" ~ "Sickness",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack",
                               covariate == "cut.self.during.interval" ~ "Cut Self",
                               covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Tree Fall Plots/tercile_panel_plot_interaction.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = p.y), stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID and House ID") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable: Tree Fall") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 10.5))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Tree Fall Plots/tercile_panel_plot_interaction.RDS")



# RE PID ------------------------------------------------------------------


# Sex ---------------------------------------------------------------------
# PH not violated for sex
model5 <- coxme(Surv(exit, tree.fall.during.interval) ~ male + (1 | pid), df)
results <- extract_coxme_table(model5)
b <- data.frame(confint(model5))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_1.tex")
c <- data.frame(t(data.frame(VarCorr(model5))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_1.tex")

# Sickness
model5a <- coxme(Surv(exit, tree.fall.during.interval) ~ male + sickness.during.interval +
                   (1 | pid), df)
saveRDS(model5a, file = "Tree Fall Tables/sex_sickness_1.RDS")
results <- extract_coxme_table(model5a)
b <- data.frame(confint(model5a))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Sickness")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_sickness_1.tex")
c <- data.frame(t(data.frame(VarCorr(model5a))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_sickness_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_sickness_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_sickness_1.RDS")))
dev.off()

# Animal Attack
model5b <- coxme(Surv(exit, tree.fall.during.interval) ~ male + Animal_Attack.during.interval +
                   (1 | pid), df)
saveRDS(model5b, file = "Tree Fall Tables/sex_Animal_Attack_1.RDS")
results <- extract_coxme_table(model5b)
b <- data.frame(confint(model5b))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Animal Attack")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_Animal_Attack_1.tex")
c <- data.frame(t(data.frame(VarCorr(model5b))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_Animal_Attack_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_Animal_Attack_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_Animal_Attack_1.RDS")))
dev.off()

# Cut Self
model5c <- coxme(Surv(exit, tree.fall.during.interval) ~ male + cut.self.during.interval +
                   (1 | pid), df)
saveRDS(model5c, file = "Tree Fall Tables/sex_cut_self_1.RDS")
results <- extract_coxme_table(model5c)
b <- data.frame(confint(model5c))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Cut Self")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_cut_self_1.tex")
c <- data.frame(t(data.frame(VarCorr(model5c))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_cut_self_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_cut_self_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_cut_self_1.RDS")))
dev.off()

# Canoe Capsize
moedl2d <- coxme(Surv(exit, tree.fall.during.interval) ~ male + canoe.capsize.during.interval +
                   (1 | pid), df)
saveRDS(moedl2d, file = "Tree Fall Tables/sex_canoe_capsize_1.RDS")
results <- extract_coxme_table(moedl2d)
b <- data.frame(confint(moedl2d))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Canoe Capsize")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_canoe_capsize_1.tex")
c <- data.frame(t(data.frame(VarCorr(moedl2d))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_canoe_capsize_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_canoe_capsize_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_canoe_capsize_1.RDS")))
dev.off()

# Fight
moedl2e <- coxme(Surv(exit, tree.fall.during.interval) ~ male + fought.during.interval +
                   (1 | pid), df)
saveRDS(moedl2e, file = "Tree Fall Tables/sex_fight_1.RDS")
results <- extract_coxme_table(moedl2e)
b <- data.frame(confint(moedl2e))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Fight")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_fight_1.tex")
c <- data.frame(t(data.frame(VarCorr(moedl2e))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_fight_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_fight_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_fight_1.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_fight_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_fight_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_sickness_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_sickness_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_Animal_Attack_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_Animal_Attack_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_cut_self_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_cut_self_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_canoe_capsize_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_canoe_capsize_1.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Tree Fall Tables/sex_fight_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/sex_sickness_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/sex_cut_self_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/sex_canoe_capsize_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/sex_Animal_Attack_1.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "fought.during.interval" ~ "Fight",
                               covariate == "sickness.during.interval" ~ "Sickness",
                               covariate == "cut.self.during.interval" ~ "Cut Self",
                               covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Tree Fall Plots/sex_panel_plot_1.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = p.y), stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable: Tree Fall") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 17))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Tree Fall Plots/sex_panel_plot_1.RDS")


# Sex + Interaction -------------------------------------------------------

# PH not violated for sex
model5interaction <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + (1 | pid), df)
results <- extract_coxme_table(model5interaction)
b <- data.frame(confint(model5interaction))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_interaction_1.tex")
c <- data.frame(t(data.frame(VarCorr(model5interaction))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_interaction_1.tex")

# Sickness
model5interactiona <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + sickness.during.interval +
                              (1 | pid), df)
saveRDS(model5interactiona, file = "Tree Fall Tables/sex_sickness_interaction_1.RDS")
results <- extract_coxme_table(model5interactiona)
b <- data.frame(confint(model5interactiona))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Sickness", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_sickness_interaction_1.tex")
c <- data.frame(t(data.frame(VarCorr(model5interactiona))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_sickness_interaction_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_sickness_interaction_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_sickness_interaction_1.RDS")))
dev.off()

# Animal Attack
model5interactionb <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + Animal_Attack.during.interval +
                              (1 | pid), df)
saveRDS(model5interactionb, file = "Tree Fall Tables/sex_Animal_Attack_interaction_1.RDS")
results <- extract_coxme_table(model5interactionb)
b <- data.frame(confint(model5interactionb))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Animal Attack", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_Animal_Attack_interaction_1.tex")
c <- data.frame(t(data.frame(VarCorr(model5interactionb))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_Animal_Attack_interaction_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_Animal_Attack_interaction_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_Animal_Attack_interaction_1.RDS")))
dev.off()

# Cut Self
model5interactionc <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + cut.self.during.interval +
                              (1 | pid), df)
saveRDS(model5interactionc, file = "Tree Fall Tables/sex_cut_self_interaction_1.RDS")
results <- extract_coxme_table(model5interactionc)
b <- data.frame(confint(model5interactionc))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Cut Self", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_cut_self_interaction_1.tex")
c <- data.frame(t(data.frame(VarCorr(model5interactionc))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_cut_self_interaction_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_cut_self_interaction_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_cut_self_interaction_1.RDS")))
dev.off()

# Canoe Capsize
moedl2d <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + canoe.capsize.during.interval +
                   (1 | pid), df)
saveRDS(moedl2d, file = "Tree Fall Tables/sex_canoe_capsize_interaction_1.RDS")
results <- extract_coxme_table(moedl2d)
b <- data.frame(confint(moedl2d))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Canoe Capsize", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_canoe_capsize_interaction_1.tex")
c <- data.frame(t(data.frame(VarCorr(moedl2d))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_canoe_capsize_interaction_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_canoe_capsize_interaction_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_canoe_capsize_interaction_1.RDS")))
dev.off()

# Fight
moedl2e <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + fought.during.interval +
                   (1 | pid), df)
saveRDS(moedl2e, file = "Tree Fall Tables/sex_fight_interaction_1.RDS")
results <- extract_coxme_table(moedl2e)
b <- data.frame(confint(moedl2e))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Fight", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/sex_fight_interaction_1.tex")
c <- data.frame(t(data.frame(VarCorr(moedl2e))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/sexvar_fight_interaction_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/sex_schoenfeld_fight_interaction_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/sex_fight_interaction_1.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_fight_interaction_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_fight_interaction_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_sickness_interaction_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_sickness_interaction_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_Animal_Attack_interaction_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_Animal_Attack_interaction_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_cut_self_interaction_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_cut_self_interaction_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/sex_canoe_capsize_interaction_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/sex_canoe_capsize_interaction_1.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Tree Fall Tables/sex_fight_interaction_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/sex_sickness_interaction_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/sex_cut_self_interaction_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/sex_canoe_capsize_interaction_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/sex_Animal_Attack_interaction_1.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "fought.during.interval" ~ "Fight",
                               covariate == "sickness.during.interval" ~ "Sickness",
                               covariate == "cut.self.during.interval" ~ "Cut Self",
                               covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Tree Fall Plots/sex_panel_plot_interaction_1.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = p.y), stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable: Tree Fall") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 17))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Tree Fall Plots/sex_panel_plot_interaction_1.RDS")

# Sex + Region ------------------------------------------------------------
# PH violated for region
model6 <- coxme(Surv(exit, tree.fall.during.interval) ~ male + strata(region) + (1 | pid), df)
results <- extract_coxme_table(model6)
b <- data.frame(confint(model6))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_1.tex")
c <- data.frame(t(data.frame(VarCorr(model6))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_1.tex")

# Sickness
model6a <- coxme(Surv(exit, tree.fall.during.interval) ~ male + sickness.during.interval +
                   strata(region) + (1 | pid), df)
saveRDS(model6a, file = "Tree Fall Tables/region_sickness_1.RDS")
results <- extract_coxme_table(model6a)
b <- data.frame(confint(model6a))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Sickness")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_sickness_1.tex")
c <- data.frame(t(data.frame(VarCorr(model6a))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_sickness_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/region_schoenfeld_sickness_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_sickness_1.RDS")))
dev.off()


# Animal Attack
model6b <- coxme(Surv(exit, tree.fall.during.interval) ~ male + Animal_Attack.during.interval +
                   strata(region) + (1 | pid), df)
saveRDS(model6b, file = "Tree Fall Tables/region_Animal_Attack_1.RDS")
results <- extract_coxme_table(model6b)
b <- data.frame(confint(model6b))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Animal Attack")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_Animal_Attack_1.tex")
c <- data.frame(t(data.frame(VarCorr(model6b))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_Animal_Attack_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/region_schoenfeld_Animal_Attack_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_Animal_Attack_1.RDS")))
dev.off()


# Cut Self
model6c <- coxme(Surv(exit, tree.fall.during.interval) ~ male + cut.self.during.interval +
                   strata(region) + (1 | pid), df)
saveRDS(model6c, file = "Tree Fall Tables/region_cut_self_1.RDS")
results <- extract_coxme_table(model6c)
b <- data.frame(confint(model6c))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Cut Self")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_cut_self_1.tex")
c <- data.frame(t(data.frame(VarCorr(model6c))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_cut_self_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/region_schoenfeld_cut_self_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_cut_self_1.RDS")))
dev.off()

# Canoe Capsize
model6d <- coxme(Surv(exit, tree.fall.during.interval) ~ male + canoe.capsize.during.interval +
                   strata(region) + (1 | pid), df)
saveRDS(model6d, file = "Tree Fall Tables/region_canoe_capsize_1.RDS")
results <- extract_coxme_table(model6d)
b <- data.frame(confint(model6d))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Canoe Capsize")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_canoe_capsize_1.tex")
c <- data.frame(t(data.frame(VarCorr(model6d))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_canoe_capsize_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/region_schoenfeld_canoe_capsize_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_canoe_capsize_1.RDS")))
dev.off()

# Fight
model6e <- coxme(Surv(exit, tree.fall.during.interval) ~ male + fought.during.interval +
                   strata(region) + (1 | pid), df)
saveRDS(model6e, file = "Tree Fall Tables/region_fight_1.RDS")
results <- extract_coxme_table(model6e)
b <- data.frame(confint(model6e))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Fight")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_fight_1.tex")
c <- data.frame(t(data.frame(VarCorr(model6e))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_fight_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/region_schoenfeld_fight_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_fight_1.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Tree Fall Tables/region_fight_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/region_fight_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/region_sickness_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/region_sickness_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/region_cut_self_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/region_cut_self_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/region_canoe_capsize_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/region_canoe_capsize_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/region_Animal_Attack_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/region_Animal_Attack_1.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Tree Fall Tables/region_fight_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/region_sickness_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/region_cut_self_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/region_canoe_capsize_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/region_Animal_Attack_1.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "fought.during.interval" ~ "Fight",
                               covariate == "sickness.during.interval" ~ "Sickness",
                               covariate == "cut.self.during.interval" ~ "Cut Self",
                               covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Tree Fall Plots/region_panel_plot_1.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = p.y), stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable: Tree Fall") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 19))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Tree Fall Plots/region_panel_plot_1.RDS")


# Sex + Region + Interaction ----------------------------------------------

# PH violated for region
model6interaction <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + strata(region) + (1 | pid), df)
results <- extract_coxme_table(model6interaction)
b <- data.frame(confint(model6interaction))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_interaction_1.tex")
c <- data.frame(t(data.frame(VarCorr(model6interaction))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_interaction_1.tex")

# Sickness
model6interactiona <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + sickness.during.interval +
                              strata(region) + (1 | pid), df)
saveRDS(model6interactiona, file = "Tree Fall Tables/region_sickness_interaction_1.RDS")
results <- extract_coxme_table(model6interactiona)
b <- data.frame(confint(model6interactiona))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Sickness", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_sickness_interaction_1.tex")
c <- data.frame(t(data.frame(VarCorr(model6interactiona))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_sickness_interaction_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/region_schoenfeld_sickness_interaction_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_sickness_interaction_1.RDS")))
dev.off()


# Animal Attack
model6interactionb <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + Animal_Attack.during.interval +
                              strata(region) + (1 | pid), df)
saveRDS(model6interactionb, file = "Tree Fall Tables/region_Animal_Attack_interaction_1.RDS")
results <- extract_coxme_table(model6interactionb)
b <- data.frame(confint(model6interactionb))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Animal Attack", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_Animal_Attack_interaction_1.tex")
c <- data.frame(t(data.frame(VarCorr(model6interactionb))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_Animal_Attack_interaction_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/region_schoenfeld_Animal_Attack_interaction_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_Animal_Attack_interaction_1.RDS")))
dev.off()


# Cut Self
model6interactionc <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + cut.self.during.interval +
                              strata(region) + (1 | pid), df)
saveRDS(model6interactionc, file = "Tree Fall Tables/region_cut_self_interaction_1.RDS")
results <- extract_coxme_table(model6interactionc)
b <- data.frame(confint(model6interactionc))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Cut Self", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_cut_self_interaction_1.tex")
c <- data.frame(t(data.frame(VarCorr(model6interactionc))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_cut_self_interaction_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/region_schoenfeld_cut_self_interaction_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_cut_self_interaction_1.RDS")))
dev.off()

# Canoe Capsize
model6interactiond <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + canoe.capsize.during.interval +
                              strata(region) + (1 | pid), df)
saveRDS(model6interactiond, file = "Tree Fall Tables/region_canoe_capsize_interaction_1.RDS")
results <- extract_coxme_table(model6interactiond)
b <- data.frame(confint(model6interactiond))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Canoe Capsize", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_canoe_capsize_interaction_1.tex")
c <- data.frame(t(data.frame(VarCorr(model6interactiond))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_canoe_capsize_interaction_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/region_schoenfeld_canoe_capsize_interaction_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_canoe_capsize_interaction_1.RDS")))
dev.off()

# Fight
model6interactione <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + fought.during.interval +
                              strata(region) + (1 | pid), df)
saveRDS(model6interactione, file = "Tree Fall Tables/region_fight_interaction_1.RDS")
results <- extract_coxme_table(model6interactione)
b <- data.frame(confint(model6interactione))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Fight", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/region_fight_interaction_1.tex")
c <- data.frame(t(data.frame(VarCorr(model6interactione))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/regionvar_fight_interaction_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/region_schoenfeld_fight_interaction_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/region_fight_interaction_1.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Tree Fall Tables/region_fight_interaction_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/region_fight_interaction_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/region_sickness_interaction_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/region_sickness_interaction_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/region_cut_self_interaction_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/region_cut_self_interaction_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/region_canoe_capsize_interaction_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/region_canoe_capsize_interaction_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/region_Animal_Attack_interaction_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/region_Animal_Attack_interaction_1.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Tree Fall Tables/region_fight_interaction_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/region_sickness_interaction_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/region_cut_self_interaction_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/region_canoe_capsize_interaction_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/region_Animal_Attack_interaction_1.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "fought.during.interval" ~ "Fight",
                               covariate == "sickness.during.interval" ~ "Sickness",
                               covariate == "cut.self.during.interval" ~ "Cut Self",
                               covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Tree Fall Plots/region_panel_plot_interaction_1.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = p.y), stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable: Tree Fall") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 19))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Tree Fall Plots/region_panel_plot_interaction_1.RDS")

# Sex + Region + Calendar Year Tercile-------------------------------------
# PH violated for tercile
model7 <- coxme(Surv(exit, tree.fall.during.interval) ~ male + strata(region) +
                  strata(tercile) + (1 | pid), df)
results <- extract_coxme_table(model7)
b <- data.frame(confint(model7))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 1
addtorow$pos[[3]] <- 1
addtorow$pos[[4]] <- 1
addtorow$pos[[5]] <- 1
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_1.tex")
c <- data.frame(t(data.frame(VarCorr(model7))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_1.tex")

# Sickness
model7a <- coxme(Surv(exit, tree.fall.during.interval) ~ male + sickness.during.interval +
                   strata(region) + strata(tercile) + (1 | pid), df)
saveRDS(model7a, file = "Tree Fall Tables/tercile_sickness_1.RDS")
results <- extract_coxme_table(model7a)
b <- data.frame(confint(model7a))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Sickness")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_sickness_1.tex")
c <- data.frame(t(data.frame(VarCorr(model7a))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_sickness_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_sickness_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_sickness_1.RDS")))
dev.off()

# Animal Attack
model7b <- coxme(Surv(exit, tree.fall.during.interval) ~ male + Animal_Attack.during.interval +
                   strata(region) + strata(tercile) + (1 | pid), df)
saveRDS(model7b, file = "Tree Fall Tables/tercile_Animal_Attack_1.RDS")
results <- extract_coxme_table(model7b)
b <- data.frame(confint(model7b))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Animal Attack")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_Animal_Attack_1.tex")
c <- data.frame(t(data.frame(VarCorr(model7b))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_Animal_Attack_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_Animal_Attack_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_Animal_Attack_1.RDS")))
dev.off()

# Cut Self
model7c <- coxme(Surv(exit, tree.fall.during.interval) ~ male + cut.self.during.interval +
                   strata(region) + strata(tercile) + (1 | pid), df)
saveRDS(model7c, file = "Tree Fall Tables/tercile_cut_self_1.RDS")
results <- extract_coxme_table(model7c)
b <- data.frame(confint(model7c))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Cut Self")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_cut_self_1.tex")
c <- data.frame(t(data.frame(VarCorr(model7c))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_cut_self_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_cut_self_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_cut_self_1.RDS")))
dev.off()

# Canoe Capsize
model7d <- coxme(Surv(exit, tree.fall.during.interval) ~ male + canoe.capsize.during.interval +
                   strata(region) + strata(tercile) + (1 | pid), df)
saveRDS(model7d, file = "Tree Fall Tables/tercile_canoe_capsize_1.RDS")
results <- extract_coxme_table(model7d)
b <- data.frame(confint(model7d))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Canoe Capsize")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_canoe_capsize_1.tex")
c <- data.frame(t(data.frame(VarCorr(model7d))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_canoe_capsize_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_canoe_capsize_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_canoe_capsize_1.RDS")))
dev.off()

# Fight
model7e <- coxme(Surv(exit, tree.fall.during.interval) ~ male + fought.during.interval +
                   strata(region) + strata(tercile) + (1 | pid), df)
saveRDS(model7e, file = "Tree Fall Tables/tercile_fight_1.RDS")
results <- extract_coxme_table(model7e)
b <- data.frame(confint(model7e))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Fight")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_fight_1.tex")
c <- data.frame(t(data.frame(VarCorr(model7e))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_fight_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_fight_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_fight_1.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_fight_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_fight_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_sickness_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_sickness_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_Animal_Attack_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_Animal_Attack_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_cut_self_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_cut_self_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_canoe_capsize_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_canoe_capsize_1.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_fight_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_sickness_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_Animal_Attack_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_cut_self_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_canoe_capsize_1.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "fought.during.interval" ~ "Fight",
                               covariate == "sickness.during.interval" ~ "Sickness",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack",
                               covariate == "cut.self.during.interval" ~ "Cut Self",
                               covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Tree Fall Plots/tercile_panel_plot_1.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = p.y), stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable: Tree Fall") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 21))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Tree Fall Plots/tercile_panel_plot_1.RDS")


# Sex + Region + Tercile + Interaction ------------------------------------

# PH violated for tercile
model7interaction <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + strata(region) +
                             strata(tercile) + (1 | pid), df)
results <- extract_coxme_table(model7interaction)
b <- data.frame(confint(model7interaction))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 2
addtorow$pos[[3]] <- 2
addtorow$pos[[4]] <- 2
addtorow$pos[[5]] <- 2
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_interaction_1.tex")
c <- data.frame(t(data.frame(VarCorr(model7interaction))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_interaction_1.tex")

# Sickness
model7interactiona <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + sickness.during.interval +
                              strata(region) + strata(tercile) + (1 | pid), df)
saveRDS(model7interactiona, file = "Tree Fall Tables/tercile_sickness_interaction_1.RDS")
results <- extract_coxme_table(model7interactiona)
b <- data.frame(confint(model7interactiona))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Sickness", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_sickness_interaction_1.tex")
c <- data.frame(t(data.frame(VarCorr(model7interactiona))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_sickness_interaction_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_sickness_interaction_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_sickness_interaction_1.RDS")))
dev.off()

# Animal Attack
model7interactionb <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + Animal_Attack.during.interval +
                              strata(region) + strata(tercile) + (1 | pid), df)
saveRDS(model7interactionb, file = "Tree Fall Tables/tercile_Animal_Attack_interaction_1.RDS")
results <- extract_coxme_table(model7interactionb)
b <- data.frame(confint(model7interactionb))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Animal Attack", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_Animal_Attack_interaction_1.tex")
c <- data.frame(t(data.frame(VarCorr(model7interactionb))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_Animal_Attack_interaction_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_Animal_Attack_interaction_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_Animal_Attack_interaction_1.RDS")))
dev.off()

# Cut Self
model7interactionc <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + cut.self.during.interval +
                              strata(region) + strata(tercile) + (1 | pid), df)
saveRDS(model7interactionc, file = "Tree Fall Tables/tercile_cut_self_interaction_1.RDS")
results <- extract_coxme_table(model7interactionc)
b <- data.frame(confint(model7interactionc))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Cut Self", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_cut_self_interaction_1.tex")
c <- data.frame(t(data.frame(VarCorr(model7interactionc))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_cut_self_interaction_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_cut_self_interaction_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_cut_self_interaction_1.RDS")))
dev.off()

# Canoe Capsize Error in optim(par = -0.446287102628419, fn = function (theta, varlist,non-finite finite-difference value [1]
# model7interactiond <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + canoe.capsize.during.interval +
#                               strata(region) + strata(tercile) + (1 | pid), df)
# saveRDS(model7interactiond, file = "Tree Fall Tables/tercile_canoe_capsize_interaction_1.RDS")
# results <- extract_coxme_table(model7interactiond)
# b <- data.frame(confint(model7interactiond))
# results <- cbind(results, b)
# results <- round(results, 3)
# results <- results %>% rename("Coef" = "beta",
#                               "exp(Coef)" = "exp_beta",
#                               "SE" = "se",
#                               "Lower CI" = "X2.5..",
#                               "Upper CI" = "X97.5..")
# rownames(results) <- c("Male", "Canoe Capsize", "Male x Age")
# results <- data.frame(t(results))
# results <- data.frame(t(results))
# addtorow <- list()
# addtorow$pos <- list()
# addtorow$pos[[1]] <- -1
# addtorow$pos[[2]] <- 3
# addtorow$pos[[3]] <- 3
# addtorow$pos[[4]] <- 3
# addtorow$pos[[5]] <- 3
# addtorow$command <- c('\\hline ',
#                       '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
#                       'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
#                       'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
#                       '\\hline ')
# x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
# align(x) <- "lcccccc"
# print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_canoe_capsize_interaction_1.tex")
# c <- data.frame(t(data.frame(VarCorr(model7interactiond))))
# colnames(c) <- c("RE Variance")
# print(xtable(c), file = "Tree Fall Tables/tercilevar_canoe_capsize_interaction_1.tex")
# # Plot Schoenfeld residuals
# pdf(file = "Tree Fall Plots/tercile_schoenfeld_canoe_capsize_interaction_1.pdf", height = 5, width = 7)
# survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_canoe_capsize_interaction_1.RDS")))
# dev.off()

# Fight
model7interactione <- coxme(Surv(exit, tree.fall.during.interval) ~ male + male:exit + fought.during.interval +
                              strata(region) + strata(tercile) + (1 | pid), df)
saveRDS(model7interactione, file = "Tree Fall Tables/tercile_fight_interaction_1.RDS")
results <- extract_coxme_table(model7interactione)
b <- data.frame(confint(model7interactione))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Male", "Fight", "Male x Age")
results <- data.frame(t(results))
results <- data.frame(t(results))
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- 3
addtorow$pos[[3]] <- 3
addtorow$pos[[4]] <- 3
addtorow$pos[[5]] <- 3
addtorow$command <- c('\\hline ',
                      '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
                      'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
                      'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
                      '\\hline ')
x <- xtable(results, caption = "Tree Fall \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Tree Fall Tables/tercile_fight_interaction_1.tex")
c <- data.frame(t(data.frame(VarCorr(model7interactione))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Tree Fall Tables/tercilevar_fight_interaction_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Tree Fall Plots/tercile_schoenfeld_fight_interaction_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Tree Fall Tables/tercile_fight_interaction_1.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_fight_interaction_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_fight_interaction_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_sickness_interaction_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_sickness_interaction_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_Animal_Attack_interaction_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_Animal_Attack_interaction_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Tree Fall Tables/tercile_cut_self_interaction_1.RDS")), data.frame(confint(readRDS("Tree Fall Tables/tercile_cut_self_interaction_1.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_fight_interaction_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_sickness_interaction_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_Animal_Attack_interaction_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Tree Fall Tables/tercile_cut_self_interaction_1.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "fought.during.interval" ~ "Fight",
                               covariate == "sickness.during.interval" ~ "Sickness",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack",
                               covariate == "cut.self.during.interval" ~ "Cut Self"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Tree Fall Plots/tercile_panel_plot_interaction_1.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = p.y), stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable: Tree Fall") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 21))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Tree Fall Plots/tercile_panel_plot_interaction_1.RDS")
