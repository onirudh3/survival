
# Libraries and data ------------------------------------------------------

library(tidyverse)
library(readxl)

# Data
df <- read_excel("Time Allocation/raw time allocation data for anirudh.xlsx")


# Data cleaning -----------------------------------------------------------

# Select necessary columns
df <- subset(df, select = c(MidPId, ComID, hora, act1, obj1, act2, obj2, verify,
                            Visitor, AgeYr, sex))

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
  group_by(age.cat) %>%
  summarise(prop = sum(row == "num") / n())

# Plot
prop %>%
  ggplot(aes(age.cat, prop)) +
  geom_col() +
  theme_classic(base_size = 25) +
  xlab("Age Category") +
  ylab("Percentage of Times at Risk") +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 65, hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


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
  group_by(age.cat) %>%
  summarise(prop = sum(row == "num") / n())

# Plot
prop %>%
  ggplot(aes(age.cat, prop)) +
  geom_col() +
  theme_classic(base_size = 25) +
  xlab("Age Category") +
  ylab("Percentage of Times at Risk") +
  ggtitle("Cut Self") +
  theme(plot.title = element_text(size = 65, hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, NA))


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
