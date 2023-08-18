
# Libraries ---------------------------------------------------------------

library(dplyr)
library(survival)
library(stargazer)

# Sickness ----------------------------------------------------------------
df <- read.csv("sickness_time_to_first_risk_long_interval.csv")
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
df1 <- data.frame("time" = m1[["time"]],
                         "n.risk" = m1[["n.risk"]],
                         "events" = m1[["n.event"]],
                         "pct.risk" = m1[["surv"]])
df1$events <- cumsum(df1$events)
df1 <- df1[which.min(abs(df1$pct.risk - 0.5)), ]
df1$pct.risk <- round(df1$pct.risk, 2)
rownames(df1) <- "Sickness"

# Cut Self ----------------------------------------------------------------
df <- read.csv("cut_self_time_to_first_risk_long_interval.csv")
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
df2 <- data.frame("time" = m1[["time"]],
                  "n.risk" = m1[["n.risk"]],
                  "events" = m1[["n.event"]],
                  "pct.risk" = m1[["surv"]])
df2$events <- cumsum(df2$events)
df2 <- df2[which.min(abs(df2$pct.risk - 0.5)), ]
df2$pct.risk <- round(df2$pct.risk, 2)
rownames(df2) <- "Cut Self"

# Animal Attack ----------------------------------------------------------------
df <- read.csv("Animal_Attack_combined_time_to_first_risk_long_interval.csv")
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
df3 <- data.frame("time" = m1[["time"]],
                  "n.risk" = m1[["n.risk"]],
                  "events" = m1[["n.event"]],
                  "pct.risk" = m1[["surv"]])
df3$events <- cumsum(df3$events)
df3 <- df3[which.min(abs(df3$pct.risk - 0.5)), ]
df3$pct.risk <- round(df3$pct.risk, 2)
rownames(df3) <- "Animal Attack"

# Tree Fall ----------------------------------------------------------------
df <- read.csv("tree_fall_time_to_first_risk_long_interval.csv")
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
df4 <- data.frame("time" = m1[["time"]],
                  "n.risk" = m1[["n.risk"]],
                  "events" = m1[["n.event"]],
                  "pct.risk" = m1[["surv"]])
df4$events <- cumsum(df4$events)
df4 <- df4[which.min(abs(df4$pct.risk - 0.5)), ]
df4$pct.risk <- round(df4$pct.risk, 2)
rownames(df4) <- "Tree Fall"

# Fight ----------------------------------------------------------------
df <- read.csv("fought_time_to_first_risk_long_interval.csv")
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
df5 <- data.frame("time" = m1[["time"]],
                  "n.risk" = m1[["n.risk"]],
                  "events" = m1[["n.event"]],
                  "pct.risk" = m1[["surv"]])
df5$events <- cumsum(df5$events)
df5 <- df5[which.min(abs(df5$pct.risk - 0.5)), ]
df5$pct.risk <- round(df5$pct.risk, 2)
rownames(df5) <- "Fight"

# Canoe Capsize ----------------------------------------------------------------
df <- read.csv("canoe_capsize_time_to_first_risk_long_interval.csv")
m1 <- survfit(Surv(exit, event) ~ 1, data = df, conf.type = "log-log")
df6 <- data.frame("time" = m1[["time"]],
                  "n.risk" = m1[["n.risk"]],
                  "events" = m1[["n.event"]],
                  "pct.risk" = m1[["surv"]])
df6$events <- cumsum(df6$events)
df6 <- df6[which.min(abs(df6$pct.risk - 0.5)), ]
df6$pct.risk <- round(df6$pct.risk, 2)
rownames(df6) <- "Canoe Capsize"


# Combine dfs -------------------------------------------------------------

median_survival <- bind_rows(df1, df2, df3, df4, df5, df6)

median_survival <- median_survival %>% rename("Age (Years)" = "time",
                                    "No. at Risk" = "n.risk",
                                    "No. of Events" = "events",
                                    "Proportion at Risk" = "pct.risk")

stargazer(median_survival, summary = F, rownames = F)
