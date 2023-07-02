# Libraries and Data Import -----------------------------------------------
library(tidyverse)
library(survival)
library(eha)
library(stargazer)
library(bshazard)
library(muhaz)
library(biostat3)
library(ggfortify)
library(rms)
library(Greg)
library(survminer)
library(moonBook)

# Import data
df <- read.csv("data_new_format.csv") # format with 13,541 intervals

# Make region as factor
df$region <- as.factor(df$region)

# Change male = 1 to "Male" and male = 0 to "Female"
df$male <- ifelse(df$male == 1, "Male", "Female")

# Make age.cat as factor
df$age.cat <- factor(df$age.cat, levels = c("0-5", "5-10", "10-15", "15-20",
                                            "20-25", "25-30", "30-35", "35-40",
                                            "40-45", "45-50", "50-55", "55-60",
                                            "60+"))


# Checking some stuff -----------------------------------------------------
# df <- df %>%
#   subset(male == 1) %>%
#   subset(sickness.during.interval == 1) %>%
#   subset(age.cat == "10-15")
#
# plyr::count(df$pid)




# With Snake/Ray Bite and Animal Attack separate --------------------------


# Percentage of intervals in which all risks occur by age category ----
tree_fall_df <- df %>%
  count(age.cat, tree.fall.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(tree.fall.during.interval == 1)
tree_fall_df <- tree_fall_df %>%
  dplyr::rename("event" = "tree.fall.during.interval")
tree_fall_df$event <- ifelse(tree_fall_df$event == 1, "Tree Fall",
                             tree_fall_df$event)

sickness_df <- df %>%
  count(age.cat, sickness.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(sickness.during.interval == 1)
sickness_df <- sickness_df %>%
  dplyr::rename("event" = "sickness.during.interval")
sickness_df$event <- ifelse(sickness_df$event == 1,
                            "Sickness", sickness_df$event)

bite_df <- df %>%
  count(age.cat, bite.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(bite.during.interval == 1)
bite_df <- bite_df %>% dplyr::rename("event" = "bite.during.interval")
bite_df$event <- ifelse(bite_df$event == 1, "Snake/Ray Bite", bite_df$event)

fought_df <- df %>%
  count(age.cat, fought.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(fought.during.interval == 1)
fought_df <- fought_df %>% dplyr::rename("event" = "fought.during.interval")
fought_df$event <- ifelse(fought_df$event == 1, "Fight", fought_df$event)

animal_attack_df <- df %>%
  count(age.cat, animal.attack.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(animal.attack.during.interval == 1)
animal_attack_df <- animal_attack_df %>%
  dplyr::rename("event" = "animal.attack.during.interval")
animal_attack_df$event <- ifelse(animal_attack_df$event == 1, "Animal Attack",
                                 animal_attack_df$event)

canoe_capsize_df <- df %>%
  count(age.cat, canoe.capsize.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(canoe.capsize.during.interval == 1)
canoe_capsize_df <- canoe_capsize_df %>%
  dplyr::rename("event" = "canoe.capsize.during.interval")
canoe_capsize_df$event <- ifelse(canoe_capsize_df$event == 1, "Canoe Capsize",
                                 canoe_capsize_df$event)

cut_self_df <- df %>%
  count(age.cat, cut.self.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(cut.self.during.interval == 1)
cut_self_df <- cut_self_df %>%
  dplyr::rename("event" = "cut.self.during.interval")
cut_self_df$event <- ifelse(cut_self_df$event == 1, "Cut Self",
                            cut_self_df$event)

df2 <- rbind(tree_fall_df, sickness_df, bite_df, fought_df, animal_attack_df,
             canoe_capsize_df, cut_self_df)
rm(tree_fall_df, sickness_df, bite_df, fought_df, animal_attack_df,
   canoe_capsize_df, cut_self_df)


df2$event <- factor(df2$event, levels = c("Cut Self", "Sickness",
                                          "Snake/Ray Bite", "Tree Fall", "Fight",
                                          "Canoe Capsize", "Animal Attack"))

ggplot(df2, aes(x = age.cat, y = prop, group = event, col = event)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.6) +
  scale_y_continuous(breaks = seq(0, 0.013, 0.002), labels = scales::percent,
                     limits = c(0, 0.013)) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "Type") +
  labs(subtitle = "388 Individuals, 13,541 Intervals")

# Percentage of intervals by gender ---------------------------------------
## Males ----
df_male <- subset(df, male == "Male")

tree_fall_df_m <- df_male %>%
  count(age.cat, tree.fall.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(tree.fall.during.interval == 1)
tree_fall_df_m <- tree_fall_df_m %>%
  dplyr::rename("event" = "tree.fall.during.interval")
tree_fall_df_m$event <- ifelse(tree_fall_df_m$event == 1, "Tree Fall",
                               tree_fall_df_m$event)

sickness_df_m <- df_male %>%
  count(age.cat, sickness.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(sickness.during.interval == 1)
sickness_df_m <- sickness_df_m %>%
  dplyr::rename("event" = "sickness.during.interval")
sickness_df_m$event <- ifelse(sickness_df_m$event == 1,
                              "Sickness", sickness_df_m$event)

bite_df_m <- df_male %>%
  count(age.cat, bite.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(bite.during.interval == 1)
bite_df_m <- bite_df_m %>% dplyr::rename("event" = "bite.during.interval")
bite_df_m$event <- ifelse(bite_df_m$event == 1, "Snake/Ray Bite", bite_df_m$event)

fought_df_m <- df_male %>%
  count(age.cat, fought.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(fought.during.interval == 1)
fought_df_m <- fought_df_m %>% dplyr::rename("event" = "fought.during.interval")
fought_df_m$event <- ifelse(fought_df_m$event == 1, "Fight", fought_df_m$event)

animal_attack_df_m <- df_male %>%
  count(age.cat, animal.attack.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(animal.attack.during.interval == 1)
animal_attack_df_m <- animal_attack_df_m %>%
  dplyr::rename("event" = "animal.attack.during.interval")
animal_attack_df_m$event <- ifelse(animal_attack_df_m$event == 1, "Animal Attack",
                                   animal_attack_df_m$event)

canoe_capsize_df_m <- df_male %>%
  count(age.cat, canoe.capsize.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(canoe.capsize.during.interval == 1)
canoe_capsize_df_m <- canoe_capsize_df_m %>%
  dplyr::rename("event" = "canoe.capsize.during.interval")
canoe_capsize_df_m$event <- ifelse(canoe_capsize_df_m$event == 1, "Canoe Capsize",
                                   canoe_capsize_df_m$event)

cut_self_df_m <- df_male %>%
  count(age.cat, cut.self.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(cut.self.during.interval == 1)
cut_self_df_m <- cut_self_df_m %>%
  dplyr::rename("event" = "cut.self.during.interval")
cut_self_df_m$event <- ifelse(cut_self_df_m$event == 1, "Cut Self",
                              cut_self_df_m$event)

df2_m <- rbind(tree_fall_df_m, sickness_df_m, bite_df_m, fought_df_m, animal_attack_df_m,
             canoe_capsize_df_m, cut_self_df_m)
rm(tree_fall_df_m, sickness_df_m, bite_df_m, fought_df_m, animal_attack_df_m,
   canoe_capsize_df_m, cut_self_df_m)

df2_m <- df2_m[c("age.cat", "event", "prop")]

df2_m$event <- factor(df2_m$event, levels = c("Cut Self", "Sickness",
                                          "Snake/Ray Bite", "Tree Fall", "Fight",
                                          "Canoe Capsize", "Animal Attack"))

df2_m <- complete(df2_m, age.cat, event)

# Plot
ggplot(df2_m, aes(x = age.cat, y = prop, group = event, col = event)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.6) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "Type") +
  labs(subtitle = "202 Males") +
  scale_y_continuous(breaks = seq(0, 0.013, 0.002), labels = scales::percent,
                     limits = c(0, 0.013))

## Females ----
df_female <- subset(df, male == "Female")

tree_fall_df_f <- df_female %>%
  count(age.cat, tree.fall.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(tree.fall.during.interval == 1)
tree_fall_df_f <- tree_fall_df_f %>%
  dplyr::rename("event" = "tree.fall.during.interval")
tree_fall_df_f$event <- ifelse(tree_fall_df_f$event == 1, "Tree Fall",
                               tree_fall_df_f$event)

sickness_df_f <- df_female %>%
  count(age.cat, sickness.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(sickness.during.interval == 1)
sickness_df_f <- sickness_df_f %>%
  dplyr::rename("event" = "sickness.during.interval")
sickness_df_f$event <- ifelse(sickness_df_f$event == 1,
                              "Sickness", sickness_df_f$event)

bite_df_f <- df_female %>%
  count(age.cat, bite.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(bite.during.interval == 1)
bite_df_f <- bite_df_f %>% dplyr::rename("event" = "bite.during.interval")
bite_df_f$event <- ifelse(bite_df_f$event == 1, "Snake/Ray Bite", bite_df_f$event)

fought_df_f <- df_female %>%
  count(age.cat, fought.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(fought.during.interval == 1)
fought_df_f <- fought_df_f %>% dplyr::rename("event" = "fought.during.interval")
fought_df_f$event <- ifelse(fought_df_f$event == 1, "Fight", fought_df_f$event)

animal_attack_df_f <- df_female %>%
  count(age.cat, animal.attack.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(animal.attack.during.interval == 1)
animal_attack_df_f <- animal_attack_df_f %>%
  dplyr::rename("event" = "animal.attack.during.interval")
animal_attack_df_f$event <- ifelse(animal_attack_df_f$event == 1, "Animal Attack",
                                   animal_attack_df_f$event)

canoe_capsize_df_f <- df_female %>%
  count(age.cat, canoe.capsize.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(canoe.capsize.during.interval == 1)
canoe_capsize_df_f <- canoe_capsize_df_f %>%
  dplyr::rename("event" = "canoe.capsize.during.interval")
canoe_capsize_df_f$event <- ifelse(canoe_capsize_df_f$event == 1, "Canoe Capsize",
                                   canoe_capsize_df_f$event)

cut_self_df_f <- df_female %>%
  count(age.cat, cut.self.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(cut.self.during.interval == 1)
cut_self_df_f <- cut_self_df_f %>%
  dplyr::rename("event" = "cut.self.during.interval")
cut_self_df_f$event <- ifelse(cut_self_df_f$event == 1, "Cut Self",
                              cut_self_df_f$event)

df2_f <- rbind(tree_fall_df_f, sickness_df_f, bite_df_f, fought_df_f, animal_attack_df_f,
               canoe_capsize_df_f, cut_self_df_f)
rm(tree_fall_df_f, sickness_df_f, bite_df_f, fought_df_f, animal_attack_df_f,
   canoe_capsize_df_f, cut_self_df_f)

df2_f <- df2_f[c("age.cat", "event", "prop")]

df2_f$event <- factor(df2_f$event, levels = c("Cut Self", "Sickness",
                                              "Snake/Ray Bite", "Tree Fall", "Fight",
                                              "Canoe Capsize", "Animal Attack"))

df2_f <- complete(df2_f, age.cat, event)


# Plot
ggplot(df2_f, aes(x = age.cat, y = prop, group = event, col = event)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.6) +
  scale_y_continuous(breaks = seq(0, 0.013, 0.002), labels = scales::percent,
                     limits = c(0, 0.013)) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "Type") +
  labs(subtitle = "186 Females")


## Male over female ratio --------------------------------------------------
df2_m <- df2_m %>% dplyr::rename("prop_m" = "prop")
df2_m <- df2_m[c("age.cat", "event", "prop_m")]

df2_f <- df2_f %>% dplyr::rename("prop_f" = "prop")
df2_f <- df2_f[c("age.cat", "event", "prop_f")]

df2_ratio <- left_join(df2_m, df2_f)
df2_ratio$prop_m_by_f <- df2_ratio$prop_m / df2_ratio$prop_f

ggplot(df2_ratio, aes(x = age.cat, y = prop_m_by_f, group = event, col = event)) +
  geom_point(size = 2.5) +
  geom_segment(aes(x = 0, y = 1, xend = 13.3, yend = 1), lty = 2, col = "lavender") +
  geom_line(linewidth = 1.6) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Ratio of Percentage (Male/Female)") +
  labs(color = "Type") +
  labs(subtitle = "388 Individuals, 13,541 Intervals") +
  scale_y_continuous(breaks = seq(0, 10, 1))


# Percentage of individuals experiencing risk by age category -------------
df <- df %>%
  group_by(age.cat) %>%
  mutate(individuals.in.age.category = length(unique(pid)))

tree_fall_df <- df %>%
  group_by(age.cat) %>%
  filter(tree.fall.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
tree_fall_df$prop <- tree_fall_df$individuals.experiencing.risk / tree_fall_df$individuals.in.age.category
tree_fall_df <- tree_fall_df[c("age.cat", "prop")]
tree_fall_df <- tree_fall_df[!duplicated(tree_fall_df), ]
tree_fall_df$risk.type <- "Tree Fall"

sickness_df <- df %>%
  group_by(age.cat) %>%
  filter(sickness.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
sickness_df$prop <- sickness_df$individuals.experiencing.risk / sickness_df$individuals.in.age.category
sickness_df <- sickness_df[c("age.cat", "prop")]
sickness_df <- sickness_df[!duplicated(sickness_df), ]
sickness_df$risk.type <- "Sickness"

snake_bite_df <- df %>%
  group_by(age.cat) %>%
  filter(bite.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
snake_bite_df$prop <- snake_bite_df$individuals.experiencing.risk / snake_bite_df$individuals.in.age.category
snake_bite_df <- snake_bite_df[c("age.cat", "prop")]
snake_bite_df <- snake_bite_df[!duplicated(snake_bite_df), ]
snake_bite_df$risk.type <- "Snake/Ray Bite"

fought_df <- df %>%
  group_by(age.cat) %>%
  filter(fought.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
fought_df$prop <- fought_df$individuals.experiencing.risk / fought_df$individuals.in.age.category
fought_df <- fought_df[c("age.cat", "prop")]
fought_df <- fought_df[!duplicated(fought_df), ]
fought_df$risk.type <- "Fight"

animal_attack_df <- df %>%
  group_by(age.cat) %>%
  filter(animal.attack.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
animal_attack_df$prop <- animal_attack_df$individuals.experiencing.risk / animal_attack_df$individuals.in.age.category
animal_attack_df <- animal_attack_df[c("age.cat", "prop")]
animal_attack_df <- animal_attack_df[!duplicated(animal_attack_df), ]
animal_attack_df$risk.type <- "Animal Attack"

canoe_capsize_df <- df %>%
  group_by(age.cat) %>%
  filter(canoe.capsize.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
canoe_capsize_df$prop <- canoe_capsize_df$individuals.experiencing.risk / canoe_capsize_df$individuals.in.age.category
canoe_capsize_df <- canoe_capsize_df[c("age.cat", "prop")]
canoe_capsize_df <- canoe_capsize_df[!duplicated(canoe_capsize_df), ]
canoe_capsize_df$risk.type <- "Canoe Capsize"

cut_self_df <- df %>%
  group_by(age.cat) %>%
  filter(cut.self.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
cut_self_df$prop <- cut_self_df$individuals.experiencing.risk / cut_self_df$individuals.in.age.category
cut_self_df <- cut_self_df[c("age.cat", "prop")]
cut_self_df <- cut_self_df[!duplicated(cut_self_df), ]
cut_self_df$risk.type <- "Cut Self"

# Combine all df's
df2 <- rbind(tree_fall_df, sickness_df, snake_bite_df, fought_df, animal_attack_df,
             canoe_capsize_df, cut_self_df)
rm(tree_fall_df, sickness_df, snake_bite_df, fought_df, animal_attack_df,
   canoe_capsize_df, cut_self_df)

# Plot
df2$risk.type <- factor(df2$risk.type, levels = c("Cut Self", "Sickness",
                                          "Snake/Ray Bite", "Tree Fall", "Fight",
                                          "Canoe Capsize", "Animal Attack"))
ggplot(df2, aes(x = age.cat, y = prop, group = risk.type, col = risk.type)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.6) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.5, 0.1), limits = c(0, 0.4)) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Individuals") +
  labs(color = "Type") +
  labs(subtitle = "388 Individuals, 13,541 Intervals")

rm(df2)

# Percentage of individuals by gender -------------------------------------
df <- df %>%
  group_by(male, age.cat) %>%
  mutate(individuals.in.age.category = length(unique(pid)))

tree_fall_df <- df %>%
  group_by(male, age.cat) %>%
  filter(tree.fall.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
tree_fall_df$prop <- tree_fall_df$individuals.experiencing.risk / tree_fall_df$individuals.in.age.category
tree_fall_df <- tree_fall_df[c("age.cat", "male", "prop")]
tree_fall_df <- tree_fall_df[!duplicated(tree_fall_df), ]
tree_fall_df$risk.type <- "Tree Fall"

sickness_df <- df %>%
  group_by(male, age.cat) %>%
  filter(sickness.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
sickness_df$prop <- sickness_df$individuals.experiencing.risk / sickness_df$individuals.in.age.category
sickness_df <- sickness_df[c("age.cat", "male", "prop")]
sickness_df <- sickness_df[!duplicated(sickness_df), ]
sickness_df$risk.type <- "Sickness"

snake_bite_df <- df %>%
  group_by(male, age.cat) %>%
  filter(bite.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
snake_bite_df$prop <- snake_bite_df$individuals.experiencing.risk / snake_bite_df$individuals.in.age.category
snake_bite_df <- snake_bite_df[c("age.cat", "male", "prop")]
snake_bite_df <- snake_bite_df[!duplicated(snake_bite_df), ]
snake_bite_df$risk.type <- "Snake/Ray Bite"

fought_df <- df %>%
  group_by(male, age.cat) %>%
  filter(fought.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
fought_df$prop <- fought_df$individuals.experiencing.risk / fought_df$individuals.in.age.category
fought_df <- fought_df[c("age.cat", "male", "prop")]
fought_df <- fought_df[!duplicated(fought_df), ]
fought_df$risk.type <- "Fight"

animal_attack_df <- df %>%
  group_by(male, age.cat) %>%
  filter(animal.attack.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
animal_attack_df$prop <- animal_attack_df$individuals.experiencing.risk / animal_attack_df$individuals.in.age.category
animal_attack_df <- animal_attack_df[c("age.cat", "male", "prop")]
animal_attack_df <- animal_attack_df[!duplicated(animal_attack_df), ]
animal_attack_df$risk.type <- "Animal Attack"

canoe_capsize_df <- df %>%
  group_by(male, age.cat) %>%
  filter(canoe.capsize.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
canoe_capsize_df$prop <- canoe_capsize_df$individuals.experiencing.risk / canoe_capsize_df$individuals.in.age.category
canoe_capsize_df <- canoe_capsize_df[c("age.cat", "male", "prop")]
canoe_capsize_df <- canoe_capsize_df[!duplicated(canoe_capsize_df), ]
canoe_capsize_df$risk.type <- "Canoe Capsize"

cut_self_df <- df %>%
  group_by(male, age.cat) %>%
  filter(cut.self.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
cut_self_df$prop <- cut_self_df$individuals.experiencing.risk / cut_self_df$individuals.in.age.category
cut_self_df <- cut_self_df[c("age.cat", "male", "prop")]
cut_self_df <- cut_self_df[!duplicated(cut_self_df), ]
cut_self_df$risk.type <- "Cut Self"

df2 <- bind_rows(tree_fall_df, sickness_df, snake_bite_df, fought_df, animal_attack_df,
             canoe_capsize_df, cut_self_df)
rm(tree_fall_df, sickness_df, snake_bite_df, fought_df, animal_attack_df,
   canoe_capsize_df, cut_self_df)

df2$risk.type <- factor(df2$risk.type, levels = c("Cut Self", "Sickness",
                                                  "Snake/Ray Bite", "Tree Fall", "Fight",
                                                  "Canoe Capsize", "Animal Attack"))

# Plot for males
df_male <- subset(df2, male == "Male")
df_male <- df_male[c("age.cat", "risk.type", "prop")]

df_male <- df_male |>
  ungroup() |>
  complete(age.cat, risk.type)

ggplot(df_male, aes(x = age.cat, y = prop, group = risk.type, col = risk.type)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.6) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.5, 0.1), limits = c(0, 0.4)) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Individuals") +
  labs(color = "Type") +
  labs(subtitle = "202 Males")

# Plot for females
df_female <- subset(df2, male == "Female")
df_female <- df_female[c("age.cat", "risk.type", "prop")]

df_female <- df_female |>
  ungroup() |>
  complete(age.cat, risk.type)

ggplot(df_female, aes(x = age.cat, y = prop, group = risk.type, col = risk.type)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.6) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.5, 0.1), limits = c(0, 0.4)) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Individuals") +
  labs(color = "Type") +
  labs(subtitle = "186 Females")

## Male over female ratio --------------------------------------------------

df_male2 <- df_male[c("age.cat", "risk.type", "prop")]
df_male2 <- df_male2 %>%  dplyr::rename("prop_m" = "prop")
df_female2 <- df_female[c("age.cat", "risk.type", "prop")]
df_female2 <- df_female2 %>%  dplyr::rename("prop_f" = "prop")
df3 <- left_join(df_female2, df_male2)
df3$prop_ratio_m_by_f <- df3$prop_m / df3$prop_f

ggplot(df3, aes(x = age.cat, y = prop_ratio_m_by_f, group = risk.type,
                color = risk.type)) +
  geom_line(linewidth = 1.6) +
  geom_segment(aes(x = 0, y = 1, xend = 13.3, yend = 1), lty = 2, col = "lavender") +
  geom_point(size = 2.5) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age in Years") +
  ylab("Ratio of Percentage of Risk Occurrence (Male/Female)") +
  labs(subtitle = "388 Individuals, 13,541 Intervals") +
  labs(color = "Type") +
  scale_y_continuous(breaks = seq(0, 10, 1))

rm(df_female, df_female2, df_male, df_male2, df2, df3)






# With Snake/Ray Bite and Animal Attack combined, as Animal Attack (c) ----

# Percentage of intervals in which all risks occur by age category ----
tree_fall_df <- df %>%
  count(age.cat, tree.fall.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(tree.fall.during.interval == 1)
tree_fall_df <- tree_fall_df %>%
  dplyr::rename("event" = "tree.fall.during.interval")
tree_fall_df$event <- ifelse(tree_fall_df$event == 1, "Tree Fall",
                             tree_fall_df$event)

sickness_df <- df %>%
  count(age.cat, sickness.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(sickness.during.interval == 1)
sickness_df <- sickness_df %>%
  dplyr::rename("event" = "sickness.during.interval")
sickness_df$event <- ifelse(sickness_df$event == 1,
                            "Sickness", sickness_df$event)

fought_df <- df %>%
  count(age.cat, fought.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(fought.during.interval == 1)
fought_df <- fought_df %>% dplyr::rename("event" = "fought.during.interval")
fought_df$event <- ifelse(fought_df$event == 1, "Fight", fought_df$event)

Animal_Attack_df <- df %>%
  count(age.cat, Animal_Attack.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(Animal_Attack.during.interval == 1)
Animal_Attack_df <- Animal_Attack_df %>%
  dplyr::rename("event" = "Animal_Attack.during.interval")
Animal_Attack_df$event <- ifelse(Animal_Attack_df$event == 1, "Animal Attack (c)",
                                 Animal_Attack_df$event)

canoe_capsize_df <- df %>%
  count(age.cat, canoe.capsize.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(canoe.capsize.during.interval == 1)
canoe_capsize_df <- canoe_capsize_df %>%
  dplyr::rename("event" = "canoe.capsize.during.interval")
canoe_capsize_df$event <- ifelse(canoe_capsize_df$event == 1, "Canoe Capsize",
                                 canoe_capsize_df$event)

cut_self_df <- df %>%
  count(age.cat, cut.self.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(cut.self.during.interval == 1)
cut_self_df <- cut_self_df %>%
  dplyr::rename("event" = "cut.self.during.interval")
cut_self_df$event <- ifelse(cut_self_df$event == 1, "Cut Self",
                            cut_self_df$event)

df2 <- rbind(tree_fall_df, sickness_df, fought_df, Animal_Attack_df,
             canoe_capsize_df, cut_self_df)
rm(tree_fall_df, sickness_df, fought_df, Animal_Attack_df,
   canoe_capsize_df, cut_self_df)


df2$event <- factor(df2$event, levels = c("Cut Self", "Sickness", "Tree Fall", "Fight",
                                          "Canoe Capsize", "Animal Attack (c)"))

ggplot(df2, aes(x = age.cat, y = prop, group = event, col = event)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.6) +
  scale_y_continuous(breaks = seq(0, 0.013, 0.002), labels = scales::percent,
                     limits = c(0, 0.013)) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "Type") +
  labs(subtitle = "388 Individuals, 13,541 Intervals")



# Percentage of intervals by gender ---------------------------------------
## Males ----
df_male <- subset(df, male == "Male")

tree_fall_df_m <- df_male %>%
  count(age.cat, tree.fall.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(tree.fall.during.interval == 1)
tree_fall_df_m <- tree_fall_df_m %>%
  dplyr::rename("event" = "tree.fall.during.interval")
tree_fall_df_m$event <- ifelse(tree_fall_df_m$event == 1, "Tree Fall",
                               tree_fall_df_m$event)

sickness_df_m <- df_male %>%
  count(age.cat, sickness.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(sickness.during.interval == 1)
sickness_df_m <- sickness_df_m %>%
  dplyr::rename("event" = "sickness.during.interval")
sickness_df_m$event <- ifelse(sickness_df_m$event == 1,
                              "Sickness", sickness_df_m$event)

fought_df_m <- df_male %>%
  count(age.cat, fought.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(fought.during.interval == 1)
fought_df_m <- fought_df_m %>% dplyr::rename("event" = "fought.during.interval")
fought_df_m$event <- ifelse(fought_df_m$event == 1, "Fight", fought_df_m$event)

Animal_Attack_df_m <- df_male %>%
  count(age.cat, Animal_Attack.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(Animal_Attack.during.interval == 1)
Animal_Attack_df_m <- Animal_Attack_df_m %>%
  dplyr::rename("event" = "Animal_Attack.during.interval")
Animal_Attack_df_m$event <- ifelse(Animal_Attack_df_m$event == 1, "Animal Attack (c)",
                                   Animal_Attack_df_m$event)

canoe_capsize_df_m <- df_male %>%
  count(age.cat, canoe.capsize.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(canoe.capsize.during.interval == 1)
canoe_capsize_df_m <- canoe_capsize_df_m %>%
  dplyr::rename("event" = "canoe.capsize.during.interval")
canoe_capsize_df_m$event <- ifelse(canoe_capsize_df_m$event == 1, "Canoe Capsize",
                                   canoe_capsize_df_m$event)

cut_self_df_m <- df_male %>%
  count(age.cat, cut.self.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(cut.self.during.interval == 1)
cut_self_df_m <- cut_self_df_m %>%
  dplyr::rename("event" = "cut.self.during.interval")
cut_self_df_m$event <- ifelse(cut_self_df_m$event == 1, "Cut Self",
                              cut_self_df_m$event)

df2_m <- rbind(tree_fall_df_m, sickness_df_m, fought_df_m, Animal_Attack_df_m,
               canoe_capsize_df_m, cut_self_df_m)
rm(tree_fall_df_m, sickness_df_m, fought_df_m, Animal_Attack_df_m,
   canoe_capsize_df_m, cut_self_df_m)

df2_m <- df2_m[c("age.cat", "event", "prop")]

df2_m$event <- factor(df2_m$event, levels = c("Cut Self", "Sickness", "Tree Fall", "Fight",
                                              "Canoe Capsize", "Animal Attack (c)"))

df2_m <- complete(df2_m, age.cat, event)

# Plot
ggplot(df2_m, aes(x = age.cat, y = prop, group = event, col = event)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.6) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "Type") +
  labs(subtitle = "202 Males") +
  scale_y_continuous(breaks = seq(0, 0.015, 0.002), labels = scales::percent,
                     limits = c(0, NA))

## Females ----
df_female <- subset(df, male == "Female")

tree_fall_df_f <- df_female %>%
  count(age.cat, tree.fall.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(tree.fall.during.interval == 1)
tree_fall_df_f <- tree_fall_df_f %>%
  dplyr::rename("event" = "tree.fall.during.interval")
tree_fall_df_f$event <- ifelse(tree_fall_df_f$event == 1, "Tree Fall",
                               tree_fall_df_f$event)

sickness_df_f <- df_female %>%
  count(age.cat, sickness.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(sickness.during.interval == 1)
sickness_df_f <- sickness_df_f %>%
  dplyr::rename("event" = "sickness.during.interval")
sickness_df_f$event <- ifelse(sickness_df_f$event == 1,
                              "Sickness", sickness_df_f$event)

fought_df_f <- df_female %>%
  count(age.cat, fought.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(fought.during.interval == 1)
fought_df_f <- fought_df_f %>% dplyr::rename("event" = "fought.during.interval")
fought_df_f$event <- ifelse(fought_df_f$event == 1, "Fight", fought_df_f$event)

Animal_Attack_df_f <- df_female %>%
  count(age.cat, Animal_Attack.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(Animal_Attack.during.interval == 1)
Animal_Attack_df_f <- Animal_Attack_df_f %>%
  dplyr::rename("event" = "Animal_Attack.during.interval")
Animal_Attack_df_f$event <- ifelse(Animal_Attack_df_f$event == 1, "Animal Attack (c)",
                                   Animal_Attack_df_f$event)

canoe_capsize_df_f <- df_female %>%
  count(age.cat, canoe.capsize.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(canoe.capsize.during.interval == 1)
canoe_capsize_df_f <- canoe_capsize_df_f %>%
  dplyr::rename("event" = "canoe.capsize.during.interval")
canoe_capsize_df_f$event <- ifelse(canoe_capsize_df_f$event == 1, "Canoe Capsize",
                                   canoe_capsize_df_f$event)

cut_self_df_f <- df_female %>%
  count(age.cat, cut.self.during.interval) %>%
  mutate(prop = prop.table(n)) %>%
  filter(cut.self.during.interval == 1)
cut_self_df_f <- cut_self_df_f %>%
  dplyr::rename("event" = "cut.self.during.interval")
cut_self_df_f$event <- ifelse(cut_self_df_f$event == 1, "Cut Self",
                              cut_self_df_f$event)

df2_f <- rbind(tree_fall_df_f, sickness_df_f, fought_df_f, Animal_Attack_df_f,
               canoe_capsize_df_f, cut_self_df_f)
rm(tree_fall_df_f, sickness_df_f, fought_df_f, Animal_Attack_df_f,
   canoe_capsize_df_f, cut_self_df_f)

df2_f <- df2_f[c("age.cat", "event", "prop")]

df2_f$event <- factor(df2_f$event, levels = c("Cut Self", "Sickness", "Tree Fall", "Fight",
                                              "Canoe Capsize", "Animal Attack (c)"))

df2_f <- complete(df2_f, age.cat, event)


# Plot
ggplot(df2_f, aes(x = age.cat, y = prop, group = event, col = event)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.6) +
  scale_y_continuous(breaks = seq(0, 0.013, 0.002), labels = scales::percent,
                     limits = c(0, 0.013)) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Intervals") +
  labs(color = "Type") +
  labs(subtitle = "186 Females")


## Male over female ratio --------------------------------------------------
df2_m <- df2_m %>% dplyr::rename("prop_m" = "prop")
df2_m <- df2_m[c("age.cat", "event", "prop_m")]

df2_f <- df2_f %>% dplyr::rename("prop_f" = "prop")
df2_f <- df2_f[c("age.cat", "event", "prop_f")]

df2_ratio <- left_join(df2_m, df2_f)
df2_ratio$prop_m_by_f <- df2_ratio$prop_m / df2_ratio$prop_f

ggplot(df2_ratio, aes(x = age.cat, y = prop_m_by_f, group = event, col = event)) +
  geom_point(size = 2.5) +
  geom_segment(aes(x = 0, y = 1, xend = 13.3, yend = 1), lty = 2, col = "lavender") +
  geom_line(linewidth = 1.6) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Ratio of Percentage (Male/Female)") +
  labs(color = "Type") +
  labs(subtitle = "388 Individuals, 13,541 Intervals") +
  scale_y_continuous(breaks = seq(0, 10, 1))


# Percentage of individuals experiencing risk by age category -------------
df <- df %>%
  group_by(age.cat) %>%
  mutate(individuals.in.age.category = length(unique(pid)))

tree_fall_df <- df %>%
  group_by(age.cat) %>%
  filter(tree.fall.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
tree_fall_df$prop <- tree_fall_df$individuals.experiencing.risk / tree_fall_df$individuals.in.age.category
tree_fall_df <- tree_fall_df[c("age.cat", "prop")]
tree_fall_df <- tree_fall_df[!duplicated(tree_fall_df), ]
tree_fall_df$risk.type <- "Tree Fall"

sickness_df <- df %>%
  group_by(age.cat) %>%
  filter(sickness.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
sickness_df$prop <- sickness_df$individuals.experiencing.risk / sickness_df$individuals.in.age.category
sickness_df <- sickness_df[c("age.cat", "prop")]
sickness_df <- sickness_df[!duplicated(sickness_df), ]
sickness_df$risk.type <- "Sickness"

fought_df <- df %>%
  group_by(age.cat) %>%
  filter(fought.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
fought_df$prop <- fought_df$individuals.experiencing.risk / fought_df$individuals.in.age.category
fought_df <- fought_df[c("age.cat", "prop")]
fought_df <- fought_df[!duplicated(fought_df), ]
fought_df$risk.type <- "Fight"

Animal_Attack_df <- df %>%
  group_by(age.cat) %>%
  filter(Animal_Attack.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
Animal_Attack_df$prop <- Animal_Attack_df$individuals.experiencing.risk / Animal_Attack_df$individuals.in.age.category
Animal_Attack_df <- Animal_Attack_df[c("age.cat", "prop")]
Animal_Attack_df <- Animal_Attack_df[!duplicated(Animal_Attack_df), ]
Animal_Attack_df$risk.type <- "Animal Attack (c)"

canoe_capsize_df <- df %>%
  group_by(age.cat) %>%
  filter(canoe.capsize.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
canoe_capsize_df$prop <- canoe_capsize_df$individuals.experiencing.risk / canoe_capsize_df$individuals.in.age.category
canoe_capsize_df <- canoe_capsize_df[c("age.cat", "prop")]
canoe_capsize_df <- canoe_capsize_df[!duplicated(canoe_capsize_df), ]
canoe_capsize_df$risk.type <- "Canoe Capsize"

cut_self_df <- df %>%
  group_by(age.cat) %>%
  filter(cut.self.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
cut_self_df$prop <- cut_self_df$individuals.experiencing.risk / cut_self_df$individuals.in.age.category
cut_self_df <- cut_self_df[c("age.cat", "prop")]
cut_self_df <- cut_self_df[!duplicated(cut_self_df), ]
cut_self_df$risk.type <- "Cut Self"

# Combine all df's
df2 <- rbind(tree_fall_df, sickness_df, fought_df, Animal_Attack_df,
             canoe_capsize_df, cut_self_df)
rm(tree_fall_df, sickness_df, fought_df, Animal_Attack_df,
   canoe_capsize_df, cut_self_df)

# Plot
df2$risk.type <- factor(df2$risk.type, levels = c("Cut Self", "Sickness", "Tree Fall", "Fight",
                                                  "Canoe Capsize", "Animal Attack (c)"))
ggplot(df2, aes(x = age.cat, y = prop, group = risk.type, col = risk.type)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.6) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.5, 0.1), limits = c(0, 0.4)) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Individuals") +
  labs(color = "Type") +
  labs(subtitle = "388 Individuals, 13,541 Intervals")

rm(df2)

# Percentage of individuals by gender -------------------------------------
df <- df %>%
  group_by(male, age.cat) %>%
  mutate(individuals.in.age.category = length(unique(pid)))

tree_fall_df <- df %>%
  group_by(male, age.cat) %>%
  filter(tree.fall.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
tree_fall_df$prop <- tree_fall_df$individuals.experiencing.risk / tree_fall_df$individuals.in.age.category
tree_fall_df <- tree_fall_df[c("age.cat", "male", "prop")]
tree_fall_df <- tree_fall_df[!duplicated(tree_fall_df), ]
tree_fall_df$risk.type <- "Tree Fall"

sickness_df <- df %>%
  group_by(male, age.cat) %>%
  filter(sickness.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
sickness_df$prop <- sickness_df$individuals.experiencing.risk / sickness_df$individuals.in.age.category
sickness_df <- sickness_df[c("age.cat", "male", "prop")]
sickness_df <- sickness_df[!duplicated(sickness_df), ]
sickness_df$risk.type <- "Sickness"

fought_df <- df %>%
  group_by(male, age.cat) %>%
  filter(fought.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
fought_df$prop <- fought_df$individuals.experiencing.risk / fought_df$individuals.in.age.category
fought_df <- fought_df[c("age.cat", "male", "prop")]
fought_df <- fought_df[!duplicated(fought_df), ]
fought_df$risk.type <- "Fight"

Animal_Attack_df <- df %>%
  group_by(male, age.cat) %>%
  filter(Animal_Attack.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
Animal_Attack_df$prop <- Animal_Attack_df$individuals.experiencing.risk / Animal_Attack_df$individuals.in.age.category
Animal_Attack_df <- Animal_Attack_df[c("age.cat", "male", "prop")]
Animal_Attack_df <- Animal_Attack_df[!duplicated(Animal_Attack_df), ]
Animal_Attack_df$risk.type <- "Animal Attack (c)"

canoe_capsize_df <- df %>%
  group_by(male, age.cat) %>%
  filter(canoe.capsize.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
canoe_capsize_df$prop <- canoe_capsize_df$individuals.experiencing.risk / canoe_capsize_df$individuals.in.age.category
canoe_capsize_df <- canoe_capsize_df[c("age.cat", "male", "prop")]
canoe_capsize_df <- canoe_capsize_df[!duplicated(canoe_capsize_df), ]
canoe_capsize_df$risk.type <- "Canoe Capsize"

cut_self_df <- df %>%
  group_by(male, age.cat) %>%
  filter(cut.self.during.interval == 1) %>%
  mutate(individuals.experiencing.risk = length(unique(pid)))
cut_self_df$prop <- cut_self_df$individuals.experiencing.risk / cut_self_df$individuals.in.age.category
cut_self_df <- cut_self_df[c("age.cat", "male", "prop")]
cut_self_df <- cut_self_df[!duplicated(cut_self_df), ]
cut_self_df$risk.type <- "Cut Self"

df2 <- bind_rows(tree_fall_df, sickness_df, fought_df, Animal_Attack_df,
                 canoe_capsize_df, cut_self_df)
rm(tree_fall_df, sickness_df, fought_df, Animal_Attack_df,
   canoe_capsize_df, cut_self_df)

df2$risk.type <- factor(df2$risk.type, levels = c("Cut Self", "Sickness", "Tree Fall", "Fight",
                                                  "Canoe Capsize", "Animal Attack (c)"))

# Plot for males
df_male <- subset(df2, male == "Male")
df_male <- df_male[c("age.cat", "risk.type", "prop")]

df_male <- df_male |>
  ungroup() |>
  complete(age.cat, risk.type)

ggplot(df_male, aes(x = age.cat, y = prop, group = risk.type, col = risk.type)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.6) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.5, 0.1), limits = c(0, 0.4)) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Individuals") +
  labs(color = "Type") +
  labs(subtitle = "202 Males")

# Plot for females
df_female <- subset(df2, male == "Female")
df_female <- df_female[c("age.cat", "risk.type", "prop")]

df_female <- df_female |>
  ungroup() |>
  complete(age.cat, risk.type)

ggplot(df_female, aes(x = age.cat, y = prop, group = risk.type, col = risk.type)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.6) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.5, 0.1), limits = c(0, 0.4)) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age of Occurrence") +
  ylab("Percentage of Individuals") +
  labs(color = "Type") +
  labs(subtitle = "186 Females")

## Male over female ratio --------------------------------------------------

df_male2 <- df_male[c("age.cat", "risk.type", "prop")]
df_male2 <- df_male2 %>%  dplyr::rename("prop_m" = "prop")
df_female2 <- df_female[c("age.cat", "risk.type", "prop")]
df_female2 <- df_female2 %>%  dplyr::rename("prop_f" = "prop")
df3 <- left_join(df_female2, df_male2)
df3$prop_ratio_m_by_f <- df3$prop_m / df3$prop_f

ggplot(df3, aes(x = age.cat, y = prop_ratio_m_by_f, group = risk.type,
                color = risk.type)) +
  geom_line(linewidth = 1.6) +
  geom_segment(aes(x = 0, y = 1, xend = 13.3, yend = 1), lty = 2, col = "lavender") +
  geom_point(size = 2.5) +
  theme_classic(base_size = 20) +
  ggtitle("RISKS") +
  theme(plot.title = element_text(size = 40)) +
  xlab("Age in Years") +
  ylab("Ratio of Percentage of Risk Occurrence (Male/Female)") +
  labs(subtitle = "388 Individuals, 13,541 Intervals") +
  labs(color = "Type") +
  scale_y_continuous(breaks = seq(0, 10, 1))

rm(df_female, df_female2, df_male, df_male2, df2, df3)



