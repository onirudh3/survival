subjects <- data.frame(
  id = 1:3,
  event = c(0, 1, 1),
  time_to_event_1 = c(NA, 5, 20),
  time_to_event_2 = c(NA, 15, NA),
  time_to_risk_out_start_1 = c(NA, NA, 9),
  time_to_risk_out_end_1 = c(NA, NA, 11),
  time_to_risk_out_start_2 = NA,
  time_to_risk_out_end_2 = NA
)

subjects1 <- subjects %>%
  mutate(start = 0,
         end = 36) %>%
  select(-event) %>%
  gather(event, t0, -id) %>%
  group_by(id) %>%
  arrange(id, t0) %>%
  filter(!is.na(t0)) %>%
  mutate(t1 = lead(t0)) %>%
  filter(!is.na(t1),
         !grepl("time_to_risk_out_start", event)) %>%
  mutate(outcome = lead(grepl("time_to_event", event), default = 0)) %>%
  select(id, t0, t1, outcome) %>%
  ungroup()

subjects <- subjects %>% 
  mutate(start = 0, end = 36)

df <- subjects %>% 
  gather(event, t0, -id)
