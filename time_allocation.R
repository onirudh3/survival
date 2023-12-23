
# Libraries and data ------------------------------------------------------

library(tidyverse)
library(readxl)

# Data
df <- read_excel("Time Allocation/raw time allocation data for anirudh.xlsx")


# Data cleaning -----------------------------------------------------------

# Select necessary columns
df <- subset(df, select = c(MidPId, ComID, hora, act1, obj1, act2, obj2, verify,
                            Visitor, AgeYr, sex))

# Male or female
df <- df %>% mutate(sex = case_when(sex == "0" ~ "F",
                                     sex == "1" ~ "M",
                                     T ~ sex))

# Make all codes lowercase
df <- df %>%
  mutate(across(4:7, tolower))

# We do not want visitors
plyr::count(df$Visitor) # How many rows are visitors
df <- subset(df, Visitor == 0)

# If activity code begins with O or A, remove row
df <- df %>%
  filter(!grepl("^[o|a]", act1, ignore.case = T))

# AgeYr stuff
df$AgeYr <- as.numeric(df$AgeYr) # Make AgeYr numeric
df <- subset(df, AgeYr > 0)
df <- subset(df, !is.na(AgeYr))

# Age category
df <- df %>%
  mutate(age.cat = case_when(AgeYr > 0 & AgeYr <= 5 ~ "0-5",
                             AgeYr > 5 & AgeYr <= 10 ~ "5-10",
                             AgeYr > 10 & AgeYr <= 15 ~ "10-15",
                             AgeYr > 15 & AgeYr <= 20 ~ "15-20",
                             AgeYr > 20 & AgeYr <= 25 ~ "20-25",
                             AgeYr > 25 & AgeYr <= 30 ~ "25-30",
                             AgeYr > 30 & AgeYr <= 35 ~ "30-35",
                             AgeYr > 35 & AgeYr <= 40 ~ "35-40",
                             AgeYr > 40 & AgeYr <= 45 ~ "40-45",
                             AgeYr > 45 & AgeYr <= 50 ~ "45-50",
                             AgeYr > 50 & AgeYr <= 55 ~ "50-55",
                             AgeYr > 55 & AgeYr <= 60 ~ "55-60",
                             AgeYr > 60 ~ "60+"))

# Make age.cat as factor
df$age.cat <- factor(df$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))

# Add variable to calculate proportion
df$row <- "0"


# Cut Self ----------------------------------------------------------------

activity_codes <- c("gcr", "gsp", "fbt", "fca", "fct", "fpl", "fps", "frp",
                    "fsl", "lbhs", "lgch", "lgcr", "lsbbt", "lsbhs", "lswd",
                    "lwbbt", "lwjc", "lwcn", "lwwd", "mgm", "mot", "rfk", "rff",
                    "rvg", "rrc", "rrcn", "plmu")

object_codes <- c("axe", "arrw", "mcht", "stik", "need", "hook", "cuch", "scis",
                  "syr")

# Activity groups
household_codes <- c("cuch", "scis", "syr", "gbt", "gcb", "gch", "gcl", "gcr",
                     "gcm", "gcw", "gdac", "gdr", "dr", "drp", "eat", "gex",
                     "gfn", "pgfg", "ins", "gfi", "gfw", "gwt", "ggr", "ggh",
                     "gjp", "gks", "gli", "glk", "gpta", "grd", "run", "gsw",
                     "gsp", "gsh", "gsi", "gsl", "gst", "gwm", "gsg", "gft",
                     "gfast", "gvo", "gwk", "gws")

food_process_codes <- c("fbt", "fch", "fca", "fcp", "fcu", "fct", "fdg", "fdr",
                        "fpr", "fgr", "fma", "fpl", "fpo", "fps", "frp", "fsv",
                        "fsh", "fsi", "fsl", "fst")

labor_codes <- c("lbhs", "lgbr", "lgch", "lgcr", "lgcl", "lgpl", "lgun", "lgwd",
                 "lghm", "lsbbt", "lsbhs", "lswd", "lwant", "lwbbt", "lwjc",
                 "lwcn", "lwmtr", "lwngo", "lwoth", "lwtch", "lwtr", "lwwd",
                 "gjat")

manufacture_codes <- c("stik", "need", "hook", "plmu", "mgm", "msp", "mwv",
                       "mfr", "mot", "mgl")

resource_codes <- c("axe", "arrw", "mcht", "rfp", "rfc", "rfb", "rfk", "rff",
                    "rfd", "rfn", "rfu", "rvg", "rht", "rhb", "rhr", "rhg",
                    "rhs", "rrc", "rrcn")

df$activity_group <- "0"

df <- df %>%
  mutate(activity_group = case_when(grepl(paste(household_codes, collapse = "|"), act1) |
                                      grepl(paste(household_codes, collapse = "|"), act2) ~ "Household",
                                    grepl(paste(household_codes, collapse = "|"), obj1) |
                                      grepl(paste(household_codes, collapse = "|"), obj2) ~ "Household",
                                    grepl(paste(food_process_codes, collapse = "|"), act1) |
                                      grepl(paste(food_process_codes, collapse = "|"), act2) ~ "Food Process",
                                    grepl(paste(food_process_codes, collapse = "|"), obj1) |
                                      grepl(paste(food_process_codes, collapse = "|"), obj2) ~ "Food Process",
                                    grepl(paste(labor_codes, collapse = "|"), act1) |
                                      grepl(paste(labor_codes, collapse = "|"), act2) ~ "Labor",
                                    grepl(paste(labor_codes, collapse = "|"), obj1) |
                                      grepl(paste(labor_codes, collapse = "|"), obj2) ~ "Labor",
                                    grepl(paste(manufacture_codes, collapse = "|"), act1) |
                                      grepl(paste(manufacture_codes, collapse = "|"), act2) ~ "Manufacture",
                                    grepl(paste(manufacture_codes, collapse = "|"), obj1) |
                                      grepl(paste(manufacture_codes, collapse = "|"), obj2) ~ "Manufacture",
                                    grepl(paste(resource_codes, collapse = "|"), act1) |
                                      grepl(paste(resource_codes, collapse = "|"), act2) ~ "Resource Acq",
                                    grepl(paste(resource_codes, collapse = "|"), obj1) |
                                      grepl(paste(resource_codes, collapse = "|"), obj2) ~ "Resource Acq",
                                    T ~ "Other"))


# Assign numerator for proportion
dx <- df %>%
  mutate(row = case_when(grepl(paste(activity_codes, collapse = "|"), act1) |
                           grepl(paste(activity_codes, collapse = "|"), act2) ~ "num",
                         grepl(paste(object_codes, collapse = "|"), obj1) |
                           grepl(paste(object_codes, collapse = "|"), obj2) ~ "num",
                         T ~ row))

## Less conservative ----

dz <- subset(dx, verify != 0)


### Times at risk ----

# Tabulate proportions per age category
prop <- dz %>%
  group_by(age.cat, sex, activity_group) %>%
  summarise(count = sum(row == "num"), total = n())

prop <- prop %>%
  group_by(age.cat, sex) %>%
  mutate(prop = count / sum(total))

prop <- subset(prop, activity_group != "Other")


#### Anderson-Gill ----

# Merge prop with mean hazard file
prop <- left_join(prop, read.csv("cut_self_mean_hazard_anderson_gill.csv"))

# Make age.cat as factor
prop$age.cat <- factor(prop$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))

# Plot
scaleFactor <- max(prop$prop) / max(prop$mean_hazard)
ggplot(prop, aes(x = age.cat)) +
  geom_col(aes(y = prop, fill = activity_group), position = "stack", alpha = 0.5, color = 'gray50') +
  geom_point(aes(y = mean_hazard * scaleFactor), size = 2, color = "blue") +
  geom_line(aes(y = mean_hazard * scaleFactor), group = 1, size = 1, color = "blue") +
  theme_classic(base_size = 15) +
  scale_fill_brewer('', palette = 'Dark2') +
  labs(x = "Age Category") +
  scale_y_continuous(name = "Proportion of Time",
                     sec.axis = sec_axis(~. / scaleFactor, name = "Hazard")) +
  theme(plot.title = element_text(size = 65, hjust = 0.5),
        strip.background = element_blank(),
        strip.placement = 'outside',
        axis.text.x = element_text(face = 3),
        panel.grid.major.x = element_blank(),
        axis.line.y.right = element_line(color = "blue"),
        axis.ticks.y.right = element_line(color = "blue"),
        axis.text.y.right = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "blue")) +
  ggtitle("Cut Self") +
  facet_wrap(.~sex, ncol = 1)


### Individuals at risk ----

# Remove individuals with less than 50 observations
dz <- dz %>%
  group_by(MidPId) %>%
  filter(n() >= 50)

# Tabulate proportions per MidPId per age category
prop <- dz %>%
  group_by(MidPId, AgeYr, sex) %>%
  summarise(count = sum(row == "num"), total = n(), prop = sum(row == "num") / n())

# Plot
prop %>%
  ggplot() +
  geom_point(aes(AgeYr, prop)) +
  geom_smooth(aes(AgeYr, prop, color = sex), method = "loess", se = F) +
  theme_classic(base_size = 25) +
  xlab("Age [Years]") +
  ylab("Proportion at Risk") +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 65, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA))


## More conservative ----

dz <- subset(dx, verify == 5)


### Times at risk ----

# Tabulate proportions per age category
prop <- dz %>%
  group_by(age.cat, sex, activity_group) %>%
  summarise(count = sum(row == "num"), total = n())

prop <- prop %>%
  group_by(age.cat, sex) %>%
  mutate(prop = count / sum(total))

prop <- subset(prop, activity_group != "Other")

# Plot
prop %>%
  ggplot(aes(paste0('', sex), prop, fill = activity_group)) +
  geom_col(position = "stack", alpha = 0.5, color = 'gray50') +
  facet_grid(.~age.cat, switch = 'x') +
  theme_classic(base_size = 15) +
  scale_x_discrete('Age Category', expand = c(0.5, 0.5)) +
  scale_fill_brewer('', palette = 'Dark2') +
  labs(y = 'Proportion of Time') +
  theme(plot.title = element_text(size = 65, hjust = 0.5),
        strip.background = element_blank(),
        strip.placement = 'outside',
        axis.text.x = element_text(face = 3),
        panel.grid.major.x = element_blank(),
        panel.spacing.x = unit(0, 'mm')) +
  ggtitle("Cut Self") +
  scale_y_continuous(expand = c(0, 0))


### Individuals at risk ----

# Remove individuals with less than 50 observations
dz <- dz %>%
  group_by(MidPId) %>%
  filter(n() >= 50)

# Tabulate proportions per MidPId per age category
prop <- dz %>%
  group_by(MidPId, AgeYr, sex) %>%
  summarise(count = sum(row == "num"), total = n(), prop = sum(row == "num") / n())

# Plot
prop %>%
  ggplot() +
  geom_point(aes(AgeYr, prop)) +
  geom_smooth(aes(AgeYr, prop, color = sex), method = "loess", se = F) +
  theme_classic(base_size = 25) +
  xlab("Age [Years]") +
  ylab("Proportion at Risk") +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 65, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0), limits = c(0, NA))
