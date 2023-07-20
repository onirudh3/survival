



df <- read.csv("tree_fall_time_to_first_risk_long_interval.csv") # longer interval format

df_event <- subset(df, event == 1) # Tree fall occurred 183 times

plyr::count(df_event$pid) # 163 out of 388 ever faced tree fall





fit <- survfit(Surv(enter, exit, event) ~ 1, df)

ggsurvplot(fit)

gg <- ggsurvtable(fit, data = df, break.time.by = 5)






df_risk <- data.frame(fit$time, fit$n.risk, fit$n.event)





df <- subset(df, select = c(pid, enter, exit, event))
df <- df[c(1:10),]
fit <- survfit(Surv(enter, exit, event) ~ 1, df)
ggsurvplot(fit)
risk_df <- data.frame(fit$time, fit$n.risk, fit$n.event)
risk_df <- ggsurvtable(fit, data = df, break.time.by = 5)




dput(df)
