# Libraries and data import -----------------------------------------------

library(coxme)
library(xtable)
library(tidyverse)

# Import time-to-first-risk data
df <- read.csv("fought_time_to_first_risk_short_interval.csv")

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

model1a <- coxph(Surv(exit, fought.during.interval) ~ 1, data = df)

model1b <- coxme(Surv(exit, fought.during.interval) ~ 1 + (1 | pid),
                 data = df)

model1c <- coxme(Surv(exit, fought.during.interval) ~ 1 + (1 | pid) +
                   (1 | house.id), data = df)

model1d <- coxme(Surv(exit, fought.during.interval) ~ 1 + (1 | pid) +
                   (1 | house.id) + strata(region), data = df)

model1e <- coxme(Surv(exit, fought.during.interval) ~ 1 + (1 | pid) +
                   (1 | house.id) + strata(region) + strata(male), data = df)

model1f <- coxme(Surv(exit, fought.during.interval) ~ 1 + (1 | pid) +
                   (1 | house.id) + strata(region) + strata(male) +
                   strata(tercile), data = df)

model1g <- coxme(Surv(exit, fought.during.interval) ~ 1 + (1 | pid) +
                   strata(region) + strata(male) + strata(tercile), data = df)

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
                      'Male FE (Stratified) & No & No & No & No & Yes & Yes & Yes \\\\\n',
                      'Tercile FE (Stratified) & No & No & No & No & No & Yes & Yes \\\\\n',
                      '\\hline')
x <- xtable(df_coxme, caption = "Fight Mixed Effects Specifications")
align(x) <- "lccccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/mixed_effects_summary.tex")


# PID and House ID RE -----------------------------------------------------


# Sex ---------------------------------------------------------------------
# PH violated for sex
# Tree Fall
model2a <- coxme(Surv(exit, fought.during.interval) ~ strata(male) +
                   tree.fall.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model2a, file = "Fight Tables/sex_tree_fall.RDS")
results <- extract_coxme_table(model2a)
b <- data.frame(confint(model2a))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Tree Fall")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/sex_tree_fall.tex")
c <- data.frame(t(data.frame(VarCorr(model2a))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/sexvar_tree_fall.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/sex_schoenfeld_tree_fall.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/sex_tree_fall.RDS")))
dev.off()

# Sickness Error in optim(par = c(-0.446287102628419, -0.446287102628419), fn = function (theta,  : non-finite finite-difference value [1]
# model2b <- coxme(Surv(exit, fought.during.interval) ~ strata(male) +
#                    sickness.during.interval + (1 | pid) + (1 | house.id), df)
# saveRDS(model2b, file = "Fight Tables/sex_sickness.RDS")
# results <- extract_coxme_table(model2b)
# b <- data.frame(confint(model2b))
# results <- cbind(results, b)
# results <- round(results, 3)
# results <- results %>% rename("Coef" = "beta",
#                               "exp(Coef)" = "exp_beta",
#                               "SE" = "se",
#                               "Lower CI" = "X2.5..",
#                               "Upper CI" = "X97.5..")
# rownames(results) <- c("Sickness")
# results <- data.frame(t(results))
# results <- data.frame(t(results))
# addtorow <- list()
# addtorow$pos <- list()
# addtorow$pos[[1]] <- -1
# addtorow$pos[[2]] <- 1
# addtorow$pos[[3]] <- 1
# addtorow$pos[[4]] <- 1
# addtorow$pos[[5]] <- 1
# addtorow$command <- c('\\hline ',
#                       '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
#                       'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
#                       'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
#                       '\\hline ')
# x <- xtable(results, caption = "Fight \\vspace{-1em}")
# align(x) <- "lcccccc"
# print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/sex_sickness.tex")
# c <- data.frame(t(data.frame(VarCorr(model2b))))
# colnames(c) <- c("RE Variance")
# print(xtable(c), file = "Fight Tables/sexvar_sickness.tex")
# # Plot Schoenfeld residuals
# pdf(file = "Fight Plots/sex_schoenfeld_sickness.pdf", height = 5, width = 7)
# survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/sex_sickness.RDS")))
# dev.off()


# Animal Attack
model2c <- coxme(Surv(exit, fought.during.interval) ~ strata(male) +
                   Animal_Attack.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model2c, file = "Fight Tables/sex_Animal_Attack.RDS")
results <- extract_coxme_table(model2c)
b <- data.frame(confint(model2c))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Animal Attack")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/sex_Animal_Attack.tex")
c <- data.frame(t(data.frame(VarCorr(model2c))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/sexvar_Animal_Attack.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/sex_schoenfeld_Animal_Attack.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/sex_Animal_Attack.RDS")))
dev.off()

# Cut Self Error in ikmat %*% fcoef : NA/NaN/Inf in foreign function call (arg 9)
# model2d <- coxme(Surv(exit, fought.during.interval) ~ strata(male) +
#                    cut.self.during.interval + (1 | pid) + (1 | house.id), df)
# saveRDS(model2d, file = "Fight Tables/sex_cut_self.RDS")
# results <- extract_coxme_table(model2d)
# b <- data.frame(confint(model2d))
# results <- cbind(results, b)
# results <- round(results, 3)
# results <- results %>% rename("Coef" = "beta",
#                               "exp(Coef)" = "exp_beta",
#                               "SE" = "se",
#                               "Lower CI" = "X2.5..",
#                               "Upper CI" = "X97.5..")
# rownames(results) <- c("Cut Self")
# results <- data.frame(t(results))
# results <- data.frame(t(results))
# addtorow <- list()
# addtorow$pos <- list()
# addtorow$pos[[1]] <- -1
# addtorow$pos[[2]] <- 1
# addtorow$pos[[3]] <- 1
# addtorow$pos[[4]] <- 1
# addtorow$pos[[5]] <- 1
# addtorow$command <- c('\\hline ',
#                       '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
#                       'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
#                       'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
#                       '\\hline ')
# x <- xtable(results, caption = "Fight \\vspace{-1em}")
# align(x) <- "lcccccc"
# print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/sex_cut_self.tex")
# c <- data.frame(t(data.frame(VarCorr(model2d))))
# colnames(c) <- c("RE Variance")
# print(xtable(c), file = "Fight Tables/sexvar_cut_self.tex")
# # Plot Schoenfeld residuals
# pdf(file = "Fight Plots/sex_schoenfeld_cut_self.pdf", height = 5, width = 7)
# survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/sex_cut_self.RDS")))
# dev.off()

# Canoe Capsize
model2e <- coxme(Surv(exit, fought.during.interval) ~ strata(male) +
                   canoe.capsize.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model2e, file = "Fight Tables/sex_canoe_capsize.RDS")
results <- extract_coxme_table(model2e)
b <- data.frame(confint(model2e))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Canoe Capsize")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/sex_canoe_capsize.tex")
c <- data.frame(t(data.frame(VarCorr(model2e))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/sexvar_canoe_capsize.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/sex_schoenfeld_canoe_capsize.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/sex_canoe_capsize.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Fight Tables/sex_canoe_capsize.RDS")), data.frame(confint(readRDS("Fight Tables/sex_canoe_capsize.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/sex_Animal_Attack.RDS")), data.frame(confint(readRDS("Fight Tables/sex_Animal_Attack.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/sex_tree_fall.RDS")), data.frame(confint(readRDS("Fight Tables/sex_tree_fall.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Fight Tables/sex_canoe_capsize.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/sex_Animal_Attack.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/sex_tree_fall.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack",
                               covariate == "tree.fall.during.interval" ~ "Tree Fall"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Fight Plots/sex_panel_plot.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = p.y), stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID and House ID") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable: Fight") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 17))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Fight Plots/sex_panel_plot.RDS")

# Sex + Region ------------------------------------------------------------
# PH violated for region
model3 <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) +
                  (1 | pid) + (1 | house.id), df)
c <- data.frame(t(data.frame(VarCorr(model3))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/regionvar.tex")

# Tree Fall
# model3a <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) +
#                    tree.fall.during.interval + (1 | pid) + (1 | house.id), df)
# saveRDS(model3a, file = "Fight Tables/region_tree_fall.RDS")
# results <- extract_coxme_table(model3a)
# b <- data.frame(confint(model3a))
# results <- cbind(results, b)
# results <- round(results, 3)
# results <- results %>% rename("Coef" = "beta",
#                               "exp(Coef)" = "exp_beta",
#                               "SE" = "se",
#                               "Lower CI" = "X2.5..",
#                               "Upper CI" = "X97.5..")
# rownames(results) <- c("Tree Fall")
# results <- data.frame(t(results))
# results <- data.frame(t(results))
# addtorow <- list()
# addtorow$pos <- list()
# addtorow$pos[[1]] <- -1
# addtorow$pos[[2]] <- 1
# addtorow$pos[[3]] <- 1
# addtorow$pos[[4]] <- 1
# addtorow$pos[[5]] <- 1
# addtorow$command <- c('\\hline ',
#                       '\\hline No. of Individuals &  &  &  \\multicolumn{2}{c}{388}  &  &  \\\\',
#                       'No. of Intervals &  &  &  \\multicolumn{2}{c}{10,738}  &  &  \\\\',
#                       'No. of Risk Years &  &  &  \\multicolumn{2}{c}{10,618.31}  &  & \\\\ ',
#                       '\\hline ')
# x <- xtable(results, caption = "Fight \\vspace{-1em}")
# align(x) <- "lcccccc"
# print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/region_tree_fall.tex")
# c <- data.frame(t(data.frame(VarCorr(model3a))))
# colnames(c) <- c("RE Variance")
# print(xtable(c), file = "Fight Tables/regionvar_tree_fall.tex")

# Sickness
model3b <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) +
                   sickness.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model3b, file = "Fight Tables/region_sickness.RDS")
results <- extract_coxme_table(model3b)
b <- data.frame(confint(model3b))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Sickness")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/region_sickness.tex")
c <- data.frame(t(data.frame(VarCorr(model3b))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/regionvar_sickness.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/region_schoenfeld_sickness.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/region_sickness.RDS")))
dev.off()


# Animal Attack
model3c <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) +
                   Animal_Attack.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model3c, file = "Fight Tables/region_Animal_Attack.RDS")
results <- extract_coxme_table(model3c)
b <- data.frame(confint(model3c))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Animal Attack")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/region_Animal_Attack.tex")
c <- data.frame(t(data.frame(VarCorr(model3c))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/regionvar_Animal_Attack.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/region_schoenfeld_Animal_Attack.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/region_Animal_Attack.RDS")))
dev.off()

# Cut Self
model3d <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) +
                   cut.self.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model3d, file = "Fight Tables/region_cut_self.RDS")
results <- extract_coxme_table(model3d)
b <- data.frame(confint(model3d))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Cut Self")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/region_cut_self.tex")
c <- data.frame(t(data.frame(VarCorr(model3d))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/regionvar_cut_self.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/region_schoenfeld_cut_self.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/region_cut_self.RDS")))
dev.off()

# Canoe Capsize
model3e <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) +
                   canoe.capsize.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model3e, file = "Fight Tables/region_canoe_capsize.RDS")
results <- extract_coxme_table(model3e)
b <- data.frame(confint(model3e))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Canoe Capsize")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/region_canoe_capsize.tex")
c <- data.frame(t(data.frame(VarCorr(model3e))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/regionvar_canoe_capsize.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/region_schoenfeld_canoe_capsize.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/region_canoe_capsize.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Fight Tables/region_canoe_capsize.RDS")), data.frame(confint(readRDS("Fight Tables/region_canoe_capsize.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/region_Animal_Attack.RDS")), data.frame(confint(readRDS("Fight Tables/region_Animal_Attack.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/region_sickness.RDS")), data.frame(confint(readRDS("Fight Tables/region_sickness.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/region_cut_self.RDS")), data.frame(confint(readRDS("Fight Tables/region_cut_self.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Fight Tables/region_canoe_capsize.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/region_Animal_Attack.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/region_sickness.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/region_cut_self.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack",
                               covariate == "sickness.during.interval" ~ "Sickness",
                               covariate == "cut.self.during.interval" ~ "Cut Self"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Fight Plots/region_panel_plot.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = p.y), stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID and House ID") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable: Fight") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 19))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Fight Plots/region_panel_plot.RDS")

# Sex + Region + Calendar Year Tercile ------------------------------------
# PH violated for tercile
model4 <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) +
                  strata(tercile) + (1 | pid) + (1 | house.id), df)
c <- data.frame(t(data.frame(VarCorr(model4))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/tercilevar.tex")

# Tree Fall
model4a <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) + strata(tercile) +
                   tree.fall.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model4a, file = "Fight Tables/tercile_tree_fall.RDS")
results <- extract_coxme_table(model4a)
b <- data.frame(confint(model4a))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Tree Fall")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/tercile_tree_fall.tex")
c <- data.frame(t(data.frame(VarCorr(model4a))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/tercilevar_tree_fall.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/tercile_schoenfeld_tree_fall.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/tercile_tree_fall.RDS")))
dev.off()

# Sickness
model4b <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) + strata(tercile) +
                   sickness.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model4b, file = "Fight Tables/tercile_sickness.RDS")
results <- extract_coxme_table(model4b)
b <- data.frame(confint(model4b))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Sickness")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/tercile_sickness.tex")
c <- data.frame(t(data.frame(VarCorr(model4b))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/tercilevar_sickness.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/tercile_schoenfeld_sickness.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/tercile_sickness.RDS")))
dev.off()

# Animal Attack
model4c <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) + strata(tercile) +
                   Animal_Attack.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model4c, file = "Fight Tables/tercile_Animal_Attack.RDS")
results <- extract_coxme_table(model4c)
b <- data.frame(confint(model4c))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Animal Attack")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/tercile_Animal_Attack.tex")
c <- data.frame(t(data.frame(VarCorr(model4c))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/tercilevar_Animal_Attack.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/tercile_schoenfeld_Animal_Attack.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/tercile_Animal_Attack.RDS")))
dev.off()

# Cut Self
model4d <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) + strata(tercile) +
                   cut.self.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model4d, file = "Fight Tables/tercile_cut_self.RDS")
results <- extract_coxme_table(model4d)
b <- data.frame(confint(model4d))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Cut Self")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/tercile_cut_self.tex")
c <- data.frame(t(data.frame(VarCorr(model4d))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/tercilevar_cut_self.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/tercile_schoenfeld_cut_self.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/tercile_cut_self.RDS")))
dev.off()

# Canoe Capsize
model4e <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) + strata(tercile) +
                   canoe.capsize.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model4e, file = "Fight Tables/tercile_canoe_capsize.RDS")
results <- extract_coxme_table(model4e)
b <- data.frame(confint(model4e))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Canoe Capsize")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/tercile_canoe_capsize.tex")
c <- data.frame(t(data.frame(VarCorr(model4e))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/tercilevar_canoe_capsize.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/tercile_schoenfeld_canoe_capsize.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/tercile_canoe_capsize.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Fight Tables/tercile_canoe_capsize.RDS")), data.frame(confint(readRDS("Fight Tables/tercile_canoe_capsize.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/tercile_tree_fall.RDS")), data.frame(confint(readRDS("Fight Tables/tercile_tree_fall.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/tercile_sickness.RDS")), data.frame(confint(readRDS("Fight Tables/tercile_sickness.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/tercile_Animal_Attack.RDS")), data.frame(confint(readRDS("Fight Tables/tercile_Animal_Attack.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/tercile_cut_self.RDS")), data.frame(confint(readRDS("Fight Tables/tercile_cut_self.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Fight Tables/tercile_canoe_capsize.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/tercile_tree_fall.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/tercile_sickness.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/tercile_Animal_Attack.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/tercile_cut_self.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize",
                               covariate == "tree.fall.during.interval" ~ "Tree Fall",
                               covariate == "sickness.during.interval" ~ "Sickness",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack",
                               covariate == "cut.self.during.interval" ~ "Cut Self"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Fight Plots/tercile_panel_plot.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = p.y), stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID and House ID") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable: Fight") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 10.5))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Fight Plots/tercile_panel_plot.RDS")



# PID RE ------------------------------------------------------------------

# Sex ---------------------------------------------------------------------
# PH violated for sex
# Tree Fall
model5a <- coxme(Surv(exit, fought.during.interval) ~ strata(male) +
                   tree.fall.during.interval + (1 | pid), df)
saveRDS(model5a, file = "Fight Tables/sex_tree_fall_1.RDS")
results <- extract_coxme_table(model5a)
b <- data.frame(confint(model5a))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Tree Fall")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/sex_tree_fall_1.tex")
c <- data.frame(t(data.frame(VarCorr(model5a))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/sexvar_tree_fall_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/sex_schoenfeld_tree_fall_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/sex_tree_fall_1.RDS")))
dev.off()

# Sickness
model5b <- coxme(Surv(exit, fought.during.interval) ~ strata(male) +
                   sickness.during.interval + (1 | pid), df)
saveRDS(model5b, file = "Fight Tables/sex_sickness_1.RDS")
results <- extract_coxme_table(model5b)
b <- data.frame(confint(model5b))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Sickness")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/sex_sickness_1.tex")
c <- data.frame(t(data.frame(VarCorr(model5b))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/sexvar_sickness_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/sex_schoenfeld_sickness_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/sex_sickness_1.RDS")))
dev.off()


# Animal Attack
model5c <- coxme(Surv(exit, fought.during.interval) ~ strata(male) +
                   Animal_Attack.during.interval + (1 | pid), df)
saveRDS(model5c, file = "Fight Tables/sex_Animal_Attack_1.RDS")
results <- extract_coxme_table(model5c)
b <- data.frame(confint(model5c))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Animal Attack")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/sex_Animal_Attack_1.tex")
c <- data.frame(t(data.frame(VarCorr(model5c))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/sexvar_Animal_Attack_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/sex_schoenfeld_Animal_Attack_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/sex_Animal_Attack_1.RDS")))
dev.off()

# Cut Self
model5d <- coxme(Surv(exit, fought.during.interval) ~ strata(male) +
                   cut.self.during.interval + (1 | pid), df)
saveRDS(model5d, file = "Fight Tables/sex_cut_self_1.RDS")
results <- extract_coxme_table(model5d)
b <- data.frame(confint(model5d))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Cut Self")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/sex_cut_self_1.tex")
c <- data.frame(t(data.frame(VarCorr(model5d))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/sexvar_cut_self_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/sex_schoenfeld_cut_self_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/sex_cut_self_1.RDS")))
dev.off()

# Canoe Capsize
model5e <- coxme(Surv(exit, fought.during.interval) ~ strata(male) +
                   canoe.capsize.during.interval + (1 | pid), df)
saveRDS(model5e, file = "Fight Tables/sex_canoe_capsize_1.RDS")
results <- extract_coxme_table(model5e)
b <- data.frame(confint(model5e))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Canoe Capsize")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/sex_canoe_capsize_1.tex")
c <- data.frame(t(data.frame(VarCorr(model5e))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/sexvar_canoe_capsize_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/sex_schoenfeld_canoe_capsize_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/sex_canoe_capsize_1.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Fight Tables/sex_canoe_capsize_1.RDS")), data.frame(confint(readRDS("Fight Tables/sex_canoe_capsize_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/sex_Animal_Attack_1.RDS")), data.frame(confint(readRDS("Fight Tables/sex_Animal_Attack_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/sex_sickness_1.RDS")), data.frame(confint(readRDS("Fight Tables/sex_sickness_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/sex_tree_fall_1.RDS")), data.frame(confint(readRDS("Fight Tables/sex_tree_fall_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/sex_cut_self_1.RDS")), data.frame(confint(readRDS("Fight Tables/sex_cut_self_1.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Fight Tables/sex_canoe_capsize_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/sex_Animal_Attack_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/sex_sickness_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/sex_tree_fall_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/sex_cut_self_1.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack",
                               covariate == "sickness.during.interval" ~ "Sickness",
                               covariate == "tree.fall.during.interval" ~ "Tree Fall",
                               covariate == "cut.self.during.interval" ~ "Cut Self"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Fight Plots/sex_panel_plot_1.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = p.y), stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable: Fight") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 17))
p
  dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Fight Plots/sex_panel_plot_1.RDS")

# Sex + Region ------------------------------------------------------------
# PH violated for region
model6 <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) +
                  (1 | pid), df)
c <- data.frame(t(data.frame(VarCorr(model6))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/regionvar_1.tex")

# Tree Fall
model6a <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) +
                   tree.fall.during.interval + (1 | pid), df)
saveRDS(model6a, file = "Fight Tables/region_tree_fall_1.RDS")
results <- extract_coxme_table(model6a)
b <- data.frame(confint(model6a))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Tree Fall")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/region_tree_fall_1.tex")
c <- data.frame(t(data.frame(VarCorr(model6a))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/regionvar_tree_fall_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/region_schoenfeld_tree_fall_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/region_tree_fall_1.RDS")))
dev.off()

# Sickness
model6b <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) +
                   sickness.during.interval + (1 | pid), df)
saveRDS(model6b, file = "Fight Tables/region_sickness_1.RDS")
results <- extract_coxme_table(model6b)
b <- data.frame(confint(model6b))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Sickness")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/region_sickness_1.tex")
c <- data.frame(t(data.frame(VarCorr(model6b))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/regionvar_sickness_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/region_schoenfeld_sickness_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/region_sickness_1.RDS")))
dev.off()

# Animal Attack
model6c <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) +
                   Animal_Attack.during.interval + (1 | pid), df)
saveRDS(model6c, file = "Fight Tables/region_Animal_Attack_1.RDS")
results <- extract_coxme_table(model6c)
b <- data.frame(confint(model6c))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Animal Attack")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/region_Animal_Attack_1.tex")
c <- data.frame(t(data.frame(VarCorr(model6c))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/regionvar_Animal_Attack_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/region_schoenfeld_Animal_Attack_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/region_Animal_Attack_1.RDS")))
dev.off()

# Cut Self
model6d <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) +
                   cut.self.during.interval + (1 | pid), df)
saveRDS(model6d, file = "Fight Tables/region_cut_self_1.RDS")
results <- extract_coxme_table(model6d)
b <- data.frame(confint(model6d))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Cut Self")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/region_cut_self_1.tex")
c <- data.frame(t(data.frame(VarCorr(model6d))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/regionvar_cut_self_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/region_schoenfeld_cut_self_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/region_cut_self_1.RDS")))
dev.off()

# Canoe Capsize
model6e <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) +
                   canoe.capsize.during.interval + (1 | pid), df)
saveRDS(model6e, file = "Fight Tables/region_canoe_capsize_1.RDS")
results <- extract_coxme_table(model6e)
b <- data.frame(confint(model6e))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Canoe Capsize")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/region_canoe_capsize_1.tex")
c <- data.frame(t(data.frame(VarCorr(model6e))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/regionvar_canoe_capsize_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/region_schoenfeld_canoe_capsize_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/region_canoe_capsize_1.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Fight Tables/region_canoe_capsize_1.RDS")), data.frame(confint(readRDS("Fight Tables/region_canoe_capsize_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/region_tree_fall_1.RDS")), data.frame(confint(readRDS("Fight Tables/region_tree_fall_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/region_Animal_Attack_1.RDS")), data.frame(confint(readRDS("Fight Tables/region_Animal_Attack_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/region_sickness_1.RDS")), data.frame(confint(readRDS("Fight Tables/region_sickness_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/region_cut_self_1.RDS")), data.frame(confint(readRDS("Fight Tables/region_cut_self_1.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Fight Tables/region_canoe_capsize_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/region_Animal_Attack_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/region_tree_fall_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/region_sickness_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/region_cut_self_1.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack",
                               covariate == "sickness.during.interval" ~ "Sickness",
                               covariate == "tree.fall.during.interval" ~ "Tree Fall",
                               covariate == "cut.self.during.interval" ~ "Cut Self"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Fight Plots/region_panel_plot_1.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = covariate), stat = "identity", width = 0.9, alpha = 0.8) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "PID RE, Male and Region FE") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("green3", "gray30", "gray30", "gray30", "gray30")) +
  theme_classic(base_size = 20) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable:
Fight") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 19)) + theme(legend.position = "none")
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Fight Plots/region_panel_plot_1.RDS")

# Sex + Region + Calendar Year Tercile ------------------------------------
# PH violated for tercile
model7 <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) +
                  strata(tercile) + (1 | pid), df)
c <- data.frame(t(data.frame(VarCorr(model7))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/tercilevar_1.tex")

# Tree Fall
model7a <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) + strata(tercile) +
                   tree.fall.during.interval + (1 | pid), df)
saveRDS(model7a, file = "Fight Tables/tercile_tree_fall_1.RDS")
results <- extract_coxme_table(model7a)
b <- data.frame(confint(model7a))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Tree Fall")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/tercile_tree_fall_1.tex")
c <- data.frame(t(data.frame(VarCorr(model7a))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/tercilevar_tree_fall_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/tercile_schoenfeld_tree_fall_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/tercile_tree_fall_1.RDS")))
dev.off()

# Sickness
model7b <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) + strata(tercile) +
                   sickness.during.interval + (1 | pid), df)
saveRDS(model7b, file = "Fight Tables/tercile_sickness_1.RDS")
results <- extract_coxme_table(model7b)
b <- data.frame(confint(model7b))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Sickness")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/tercile_sickness_1.tex")
c <- data.frame(t(data.frame(VarCorr(model7b))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/tercilevar_sickness_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/tercile_schoenfeld_sickness_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/tercile_sickness_1.RDS")))
dev.off()

# Animal Attack
model7c <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) + strata(tercile) +
                   Animal_Attack.during.interval + (1 | pid), df)
saveRDS(model7c, file = "Fight Tables/tercile_Animal_Attack_1.RDS")
results <- extract_coxme_table(model7c)
b <- data.frame(confint(model7c))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Animal Attack")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/tercile_Animal_Attack_1.tex")
c <- data.frame(t(data.frame(VarCorr(model7c))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/tercilevar_Animal_Attack_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/tercile_schoenfeld_Animal_Attack_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/tercile_Animal_Attack_1.RDS")))
dev.off()

# Cut Self
model7d <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) + strata(tercile) +
                   cut.self.during.interval + (1 | pid), df)
saveRDS(model7d, file = "Fight Tables/tercile_cut_self_1.RDS")
results <- extract_coxme_table(model7d)
b <- data.frame(confint(model7d))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Cut Self")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/tercile_cut_self_1.tex")
c <- data.frame(t(data.frame(VarCorr(model7d))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/tercilevar_cut_self_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/tercile_schoenfeld_cut_self_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/tercile_cut_self_1.RDS")))
dev.off()

# Canoe Capsize
model7e <- coxme(Surv(exit, fought.during.interval) ~ strata(male) + strata(region) + strata(tercile) +
                   canoe.capsize.during.interval + (1 | pid), df)
saveRDS(model7e, file = "Fight Tables/tercile_canoe_capsize_1.RDS")
results <- extract_coxme_table(model7e)
b <- data.frame(confint(model7e))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Canoe Capsize")
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
x <- xtable(results, caption = "Fight \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Fight Tables/tercile_canoe_capsize_1.tex")
c <- data.frame(t(data.frame(VarCorr(model7e))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Fight Tables/tercilevar_canoe_capsize_1.tex")
# Plot Schoenfeld residuals
pdf(file = "Fight Plots/tercile_schoenfeld_canoe_capsize_1.pdf", height = 5, width = 7)
survminer::ggcoxzph(cox.zph(readRDS("Fight Tables/tercile_canoe_capsize_1.RDS")))
dev.off()

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Fight Tables/tercile_canoe_capsize_1.RDS")), data.frame(confint(readRDS("Fight Tables/tercile_canoe_capsize_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/tercile_tree_fall_1.RDS")), data.frame(confint(readRDS("Fight Tables/tercile_tree_fall_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/tercile_sickness_1.RDS")), data.frame(confint(readRDS("Fight Tables/tercile_sickness_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/tercile_Animal_Attack_1.RDS")), data.frame(confint(readRDS("Fight Tables/tercile_Animal_Attack_1.RDS")))),
                            cbind(extract_coxme_table(readRDS("Fight Tables/tercile_cut_self_1.RDS")), data.frame(confint(readRDS("Fight Tables/tercile_cut_self_1.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Fight Tables/tercile_canoe_capsize_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/tercile_tree_fall_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/tercile_sickness_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/tercile_Animal_Attack_1.RDS"))$table),
                   data.frame(cox.zph(readRDS("Fight Tables/tercile_cut_self_1.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize",
                               covariate == "tree.fall.during.interval" ~ "Tree Fall",
                               covariate == "sickness.during.interval" ~ "Sickness",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack",
                               covariate == "cut.self.during.interval" ~ "Cut Self"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

df_hazard_plot$covariate <- factor(df_hazard_plot$covariate, levels = c("Sickness", "Cut Self", "Animal Attack",
                                                                        "Tree Fall", "Fight", "Canoe Capsize"))

pdf(file = "Fight Plots/tercile_panel_plot_1.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = covariate, y = exp_beta, fill = p.y), stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID") +
  geom_segment(aes(x = 0, y = 1, xend = 6.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) + scale_x_discrete(drop = F) +
  ggtitle("Outcome Variable: Fight") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 21))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Fight Plots/tercile_panel_plot_1.RDS")
