require(tidyverse)
require(labelled)
require(ggrepel)

vulnerable_cols <- c(
  "Q29DiscriminationMe",
  "Q41ATSI_Final",
  "Q43LOTE",
  "Q45DisabilityYS",
  "Q1Gender_Final"
)

df <- ys |> 
  select(weight_final_1, Year, K6_class, 
         any_of(vulnerable_cols)) |> 
  mutate(risk = if_else(K6_class == 3, "high", "low"),
         `Gender diverse` = case_when(
           Q1Gender_Final == 3 & Year > 2015 ~ "Yes", 
           Year > 2015 ~ "No",
           TRUE ~ NA_character_),
         Female = if_else(Q1Gender_Final == 2, "Yes", "No")) |>
  select(-K6_class, -Q1Gender_Final) |> 
  as_factor() 

rename_map <- labelled::var_label(df, unlist=F, null_action = "fill") |> 
  map(.f = ~str_remove(.x, ".* = |K6 ")) |> 
  unlist()

vulnerable <- df %>%
  rename_with(~rename_map[.x], .cols = names(rename_map)) |> 
  mutate(across(where(is.factor), as.character)) %>%
  gather(key, val, -Year, -risk, -weight_final_1) |> 
  filter(str_detect(val, "Yes|No|Indigenous|gender")) |> 
  mutate(key = recode(key, `Two categories ATSI status` = "Indigenous"),
         val = recode_factor(val, Indigenous = "Yes", `Non-Indigenous` = "No",
                             `No, English only` = "No")) |> 
  mutate(key = fct_relevel(key, "Gender diverse", 
                           "Experienced unfair treatment in the last year",
                           "Identify as a person with disability")) |> 
  filter(!is.na(risk)) 

facet_title <- vulnerable |> 
  count(key, val) |> 
  group_by(key) |> 
  add_tally(n, name = "tally") |> 
  filter(val == "Yes") |> 
  transmute(key, 
            key_label = paste0(str_wrap(key, 27), 
                               "\n(", round(n / tally*100, 0), "% of respondents)"),
            key_label = fct_reorder(key_label, as.numeric(key)))

vulnerabl.p <- vulnerable |> 
  count(Year, key, val, risk, wt = weight_final_1) |> 
  spread(risk, n) |> 
  left_join(facet_title, by = join_by(key)) |> 
  mutate(
    p = high/(high + low)) 

highlights <- vulnerabl.p |>
  group_by(key_label, val) |>
  mutate(p.max = p == max(p)) |>
  filter(p.max) |> 
  select(Year, key_label, val, p)

fig_4 <- ggplot(vulnerabl.p, aes(x = Year, y = p)) +
  geom_hline(aes(yintercept = 0), alpha = 0) +
  geom_vline(aes(xintercept = 2010), alpha = 0) +
  geom_line(aes(linetype = val)) +
  geom_text_repel(data = highlights, aes(label = val),
                  fontface = "bold", size = 3.5, nudge_x = 0.5, 
                  point.size = 3, min.segment.length = Inf) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2024)) +
  facet_wrap(~key_label) +
  labs(
    title = "Young people in vulnerable groups have higher psychological
distress, and the gap has not improved over time",
    x = "", y = "",
    subtitle = "Prevalance (%) of psychological distress in vulnerable Australians aged 15-19") +
  theme(
    legend.position = "none",
    strip.text = element_text(hjust = 1, vjust = 1))
