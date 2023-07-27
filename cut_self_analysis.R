# Libraries and data import -----------------------------------------------

library(coxme)

# Import time-to-first-risk data
df <- read.csv("cut_self_time_to_first_risk_short_interval.csv")

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

model1a <- coxph(Surv(exit, cut.self.during.interval) ~ 1, data = df)

model1b <- coxme(Surv(exit, cut.self.during.interval) ~ 1 + (1 | pid),
                 data = df)

model1c <- coxme(Surv(exit, cut.self.during.interval) ~ 1 + (1 | pid) +
                   (1 | house.id), data = df)

model1d <- coxme(Surv(exit, cut.self.during.interval) ~ 1 + (1 | pid) +
                   (1 | house.id) + (1 | strata(region)), data = df)

model1e <- readRDS("Cut Self Tables/coxme_calendar_year.RDS")

model1f <- readRDS("Cut Self Tables/coxme_calendar_year_tercile.RDS")

## Comparing model fit
aov_test <- anova(model1a, model1b, model1c, model1d, model1e, model1f)
aov_test

aic_test <- AIC(model1a, model1b, model1c, model1d, model1e, model1f)
aic_test

## Tabulating
df_coxme <- cbind(aov_test, aic_test)
df_coxme <- subset(df_coxme, select = -c(Df))
df_coxme <- data.frame(t(df_coxme))
colnames(df_coxme) <- c("1", "2", "3", "4", "5", "6")

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
addtorow$pos[[9]] <- 5
addtorow$command <- c('\\hline',
                      '\\hline',
                      'PID RE & No & Yes & Yes & Yes & Yes & Yes \\\\\n',
                      'House ID RE & No & No & Yes & Yes & Yes & Yes \\\\\n',
                      'Region FE & No & No & No & Yes & Yes & Yes \\\\\n',
                      'Male FE (Stratified) & No & No & No & No & Yes & Yes \\\\\n',
                      'Calendar Year FE & No & No & No & No & Yes & No \\\\\n',
                      'Tercile FE & No & No & No & No & No & Yes \\\\\n',
                      '\\hline')
x <- xtable(df_coxme, caption = "Cut Self Mixed Effects Specifications")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Cut Self Tables/mixed_effects_summary.tex")

# Sex ---------------------------------------------------------------------
# PH violated for sex
model2 <- coxme(Surv(exit, cut.self.during.interval) ~ strata(male) +
                  (1 | pid) + (1 | house.id), df)
c <- data.frame(t(data.frame(VarCorr(model2))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Cut Self Tables/sexvar.tex")

# Sex + Region ------------------------------------------------------------
# PH violated for region
model3 <- coxme(Surv(exit, cut.self.during.interval) ~ strata(male) + strata(region) +
                  (1 | pid) + (1 | house.id), df)
c <- data.frame(t(data.frame(VarCorr(model3))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Cut Self Tables/regionvar.tex")

# Tree Fall
model3a <- coxme(Surv(exit, cut.self.during.interval) ~ strata(male) + strata(region) +
                   tree.fall.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model3a, file = "Cut Self Tables/region_tree_fall.RDS")
results <- extract_coxme_table(model3a)
b <- data.frame(confint(model3a))
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
x <- xtable(results, caption = "Cut Self \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Cut Self Tables/region_tree_fall.tex")
c <- data.frame(t(data.frame(VarCorr(model3a))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Cut Self Tables/regionvar_tree_fall.tex")

# Sickness
model3b <- coxme(Surv(exit, cut.self.during.interval) ~ strata(male) + strata(region) +
                   sickness.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model3b, file = "Cut Self Tables/region_sickness.RDS")
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
x <- xtable(results, caption = "Cut Self \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Cut Self Tables/region_sickness.tex")
c <- data.frame(t(data.frame(VarCorr(model3b))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Cut Self Tables/regionvar_sickness.tex")

# Animal Attack
model3c <- coxme(Surv(exit, cut.self.during.interval) ~ strata(male) + strata(region) +
                   Animal_Attack.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model3c, file = "Cut Self Tables/region_Animal_Attack.RDS")
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
x <- xtable(results, caption = "Cut Self \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Cut Self Tables/region_Animal_Attack.tex")
c <- data.frame(t(data.frame(VarCorr(model3c))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Cut Self Tables/regionvar_Animal_Attack.tex")

# Canoe Capsize
model3d <- coxme(Surv(exit, cut.self.during.interval) ~ strata(male) + strata(region) +
                   canoe.capsize.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model3d, file = "Cut Self Tables/region_canoe_capsize.RDS")
results <- extract_coxme_table(model3d)
b <- data.frame(confint(model3d))
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
x <- xtable(results, caption = "Cut Self \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Cut Self Tables/region_canoe_capsize.tex")
c <- data.frame(t(data.frame(VarCorr(model3d))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Cut Self Tables/regionvar_canoe_capsize.tex")

# Fight
model3e <- coxme(Surv(exit, cut.self.during.interval) ~ strata(male) + strata(region) +
                   fought.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model3e, file = "Cut Self Tables/region_fight.RDS")
results <- extract_coxme_table(model3e)
b <- data.frame(confint(model3e))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Fight")
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
x <- xtable(results, caption = "Cut Self \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Cut Self Tables/region_fight.tex")
c <- data.frame(t(data.frame(VarCorr(model3e))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Cut Self Tables/regionvar_fight.tex")

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Cut Self Tables/region_fight.RDS")), data.frame(confint(readRDS("Cut Self Tables/region_fight.RDS")))),
                            cbind(extract_coxme_table(readRDS("Cut Self Tables/region_Animal_Attack.RDS")), data.frame(confint(readRDS("Cut Self Tables/region_Animal_Attack.RDS")))),
                            cbind(extract_coxme_table(readRDS("Cut Self Tables/region_tree_fall.RDS")), data.frame(confint(readRDS("Cut Self Tables/region_tree_fall.RDS")))),
                            cbind(extract_coxme_table(readRDS("Cut Self Tables/region_sickness.RDS")), data.frame(confint(readRDS("Cut Self Tables/region_sickness.RDS")))),
                            cbind(extract_coxme_table(readRDS("Cut Self Tables/region_canoe_capsize.RDS")), data.frame(confint(readRDS("Cut Self Tables/region_canoe_capsize.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Cut Self Tables/region_fight.RDS"))$table),
                   data.frame(cox.zph(readRDS("Cut Self Tables/region_Animal_Attack.RDS"))$table),
                   data.frame(cox.zph(readRDS("Cut Self Tables/region_tree_fall.RDS"))$table),
                   data.frame(cox.zph(readRDS("Cut Self Tables/region_sickness.RDS"))$table),
                   data.frame(cox.zph(readRDS("Cut Self Tables/region_canoe_capsize.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "fought.during.interval" ~ "Fight",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack",
                               covariate == "tree.fall.during.interval" ~ "Tree Fall",
                               covariate == "sickness.during.interval" ~ "sickness",
                               covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

pdf(file = "Cut Self Plots/region_panel_plot.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = reorder(covariate, -exp_beta), y = exp_beta, fill = p.y), stat = "identity") +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID and House ID") +
  geom_segment(aes(x = 0, y = 1, xend = 5.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) +
  ggtitle("Outcome Variable: Cut Self") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 19))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Cut Self Plots/region_panel_plot.RDS")

# Sex + Region + Calendar Year Tercile ------------------------------------
# PH violated for tercile
model4 <- coxme(Surv(exit, cut.self.during.interval) ~ strata(male) + strata(region) + strata(tercile) + (1 | pid) + (1 | house.id), df)
c <- data.frame(t(data.frame(VarCorr(model4))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Cut Self Tables/tercilevar.tex")

# Tree Fall
model4a <- coxme(Surv(exit, cut.self.during.interval) ~ strata(male) + strata(region) + strata(tercile) +
                   tree.fall.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model4a, file = "Cut Self Tables/tercile_tree_fall.RDS")
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
x <- xtable(results, caption = "Cut Self \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Cut Self Tables/tercile_tree_fall.tex")
c <- data.frame(t(data.frame(VarCorr(model4a))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Cut Self Tables/tercilevar_tree_fall.tex")

# Sickness
model4b <- coxme(Surv(exit, cut.self.during.interval) ~ strata(male) + strata(region) + strata(tercile) +
                   sickness.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model4b, file = "Cut Self Tables/tercile_sickness.RDS")
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
x <- xtable(results, caption = "Cut Self \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Cut Self Tables/tercile_sickness.tex")
c <- data.frame(t(data.frame(VarCorr(model4b))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Cut Self Tables/tercilevar_sickness.tex")

# Animal Attack
model4c <- coxme(Surv(exit, cut.self.during.interval) ~ strata(male) + strata(region) + strata(tercile) +
                   Animal_Attack.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model4c, file = "Cut Self Tables/tercile_Animal_Attack.RDS")
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
x <- xtable(results, caption = "Cut Self \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Cut Self Tables/tercile_Animal_Attack.tex")
c <- data.frame(t(data.frame(VarCorr(model4c))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Cut Self Tables/tercilevar_Animal_Attack.tex")

# Canoe Capsize
model4d <- coxme(Surv(exit, cut.self.during.interval) ~ strata(male) + strata(region) + strata(tercile) +
                   canoe.capsize.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model4d, file = "Cut Self Tables/tercile_canoe_capsize.RDS")
results <- extract_coxme_table(model4d)
b <- data.frame(confint(model4d))
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
x <- xtable(results, caption = "Cut Self \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Cut Self Tables/tercile_canoe_capsize.tex")
c <- data.frame(t(data.frame(VarCorr(model4d))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Cut Self Tables/tercilevar_canoe_capsize.tex")

# Fight
model4e <- coxme(Surv(exit, cut.self.during.interval) ~ strata(male) + strata(region) + strata(tercile) +
                   fought.during.interval + (1 | pid) + (1 | house.id), df)
saveRDS(model4e, file = "Cut Self Tables/tercile_fight.RDS")
results <- extract_coxme_table(model4e)
b <- data.frame(confint(model4e))
results <- cbind(results, b)
results <- round(results, 3)
results <- results %>% rename("Coef" = "beta",
                              "exp(Coef)" = "exp_beta",
                              "SE" = "se",
                              "Lower CI" = "X2.5..",
                              "Upper CI" = "X97.5..")
rownames(results) <- c("Fight")
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
x <- xtable(results, caption = "Cut Self \\vspace{-1em}")
align(x) <- "lcccccc"
print(x, caption.placement = "top", add.to.row = addtorow, file = "Cut Self Tables/tercile_fight.tex")
c <- data.frame(t(data.frame(VarCorr(model4e))))
colnames(c) <- c("RE Variance")
print(xtable(c), file = "Cut Self Tables/tercilevar_fight.tex")

# Plot
df_hazard_plot <- bind_rows(cbind(extract_coxme_table(readRDS("Cut Self Tables/tercile_fight.RDS")), data.frame(confint(readRDS("Cut Self Tables/tercile_fight.RDS")))),
                            cbind(extract_coxme_table(readRDS("Cut Self Tables/tercile_tree_fall.RDS")), data.frame(confint(readRDS("Cut Self Tables/tercile_tree_fall.RDS")))),
                            cbind(extract_coxme_table(readRDS("Cut Self Tables/tercile_sickness.RDS")), data.frame(confint(readRDS("Cut Self Tables/tercile_sickness.RDS")))),
                            cbind(extract_coxme_table(readRDS("Cut Self Tables/tercile_Animal_Attack.RDS")), data.frame(confint(readRDS("Cut Self Tables/tercile_Animal_Attack.RDS")))),
                            cbind(extract_coxme_table(readRDS("Cut Self Tables/tercile_canoe_capsize.RDS")), data.frame(confint(readRDS("Cut Self Tables/tercile_canoe_capsize.RDS")))))

df_hazard_plot <- round(df_hazard_plot, 3)
df_hazard_plot$X2.5.. <- exp(df_hazard_plot$X2.5..)
df_hazard_plot$X97.5.. <- exp(df_hazard_plot$X97.5..)
df_hazard_plot <- rownames_to_column(df_hazard_plot, "covariate")

ph_df <- bind_rows(data.frame(cox.zph(readRDS("Cut Self Tables/tercile_fight.RDS"))$table),
                   data.frame(cox.zph(readRDS("Cut Self Tables/tercile_tree_fall.RDS"))$table),
                   data.frame(cox.zph(readRDS("Cut Self Tables/tercile_sickness.RDS"))$table),
                   data.frame(cox.zph(readRDS("Cut Self Tables/tercile_Animal_Attack.RDS"))$table),
                   data.frame(cox.zph(readRDS("Cut Self Tables/tercile_canoe_capsize.RDS"))$table))
ph_df <- ph_df %>% filter(!duplicated(ph_df))
ph_df <- rownames_to_column(ph_df, "covariate")

df_hazard_plot <- left_join(df_hazard_plot, ph_df, by = "covariate")
df_hazard_plot <- df_hazard_plot %>%
  mutate(p.y = case_when(p.y <= 0.05 ~ "Violates PH", T ~ "Does Not Violate PH"),
         covariate = case_when(covariate == "fought.during.interval" ~ "Fight",
                               covariate == "tree.fall.during.interval" ~ "Tree Fall",
                               covariate == "sickness.during.interval" ~ "Sickness",
                               covariate == "Animal_Attack.during.interval" ~ "Animal Attack",
                               covariate == "canoe.capsize.during.interval" ~ "Canoe Capsize"))

df_hazard_plot$p.y <- as.factor(df_hazard_plot$p.y)
df_hazard_plot <- subset(df_hazard_plot, !is.na(covariate))

pdf(file = "Cut Self Plots/tercile_panel_plot.pdf", height = 6.5, width = 6)
p <- df_hazard_plot %>%
  ggplot() +
  geom_bar(aes(x = reorder(covariate, -exp_beta), y = exp_beta, fill = p.y), stat = "identity") +
  geom_errorbar(aes(x = covariate, ymin = X2.5.., ymax = X97.5..), width = 0.5, linewidth = 0.4) +
  labs(x = "", y = "Hazard Ratio", fill = "", subtitle = "Cox Model with RE for PID and House ID") +
  geom_segment(aes(x = 0, y = 1, xend = 5.6, yend = 1), lty = 2, col = "grey40", size = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "lightcoral")) +
  theme_classic(base_size = 16) +
  guides(x =  guide_axis(angle = 90)) +
  ggtitle("Outcome Variable: Cut Self") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 10.5))
p
dev.off()
saveRDS(p + labs(x = "", y = "", subtitle = ""), file = "Cut Self Plots/tercile_panel_plot.RDS")
