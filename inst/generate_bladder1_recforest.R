library(dplyr)

bladder1_recforest <- survival::bladder1 %>%
  group_by(id) %>%
  mutate(
    is_last = row_number() == n(),
    across(
      c(treatment, number, size, recur, start, stop, rtumor, rsize, enum),
      list(prev = ~ lag(.x)),
      .names = "prev_{.col}"
    )
  ) %>%
  mutate(
    status = ifelse(is_last & status == 1, 0, status),
    across(
      starts_with("prev_"),
      ~ ifelse(is_last & status == 0 & lag(status) == 1, .x, get(sub("prev_", "", cur_column()))),
      .names = "{.col}"
    )
  ) %>%
  ungroup() %>%
  select(-is_last, starts_with("prev_")) %>%
  mutate(
    t.start = start,
    t.stop = stop,
    death = as.numeric(status == 2),
    event = as.numeric(status == 1)
  ) %>%
  select(id, t.start, t.stop, treatment, number, size, death, event)
