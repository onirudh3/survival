






tikz(file = "plot.tex", width = 5, height = 5)


ggplot(df_surv3, aes(x = time, y = hazard, fill = Sex)) +
  geom_line(aes(color = Sex)) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), alpha = 0.2) +
  theme_classic(base_size = 12) +
  xlab("Age in Years") +
  ylab("Probability of experiencing risk") +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(subtitle = "388 Individuals, 13,451 Intervals")


dev.off()


tikzTest()
tikzTest("\\char77")

library(tinytex)
tlmgr_search('/tikz.sty')    # search for tikz.sty
tlmgr_install('pgf')         # install the psnfss package
tlmgr_update()
tinytex::reinstall_tinytex(repository = "illinois")
