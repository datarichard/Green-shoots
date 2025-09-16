require(tidyverse)

ys <- read_rds("data/ys_preprocessed.rds")

k6_percents <- select(ys, 
    Year, K6_class, weight_final_1) |> 
  mutate(High = K6_class == 3) |> 
  filter(!is.na(High)) |> 
  percent(Year, High, wt = weight_final_1) |> 
  mutate(N = sum(n)) |> 
  filter(High) |> 
  ungroup()

highlights <- k6_percents |> 
  mutate(m = proportion == max(proportion[Year > 2019])) |> 
  filter(
    Year == first(Year) |m| Year == last(Year))


fig_1 <- ggplot(k6_percents, aes(x = Year, y = proportion)) +
  geom_vline(aes(xintercept = 2011), alpha = 0) +
  # geom_vline(aes(xintercept = 2022), color = "grey80") +
  geom_hline(aes(yintercept = 0), alpha = 0) +
  geom_hline(aes(yintercept = .3), alpha = 0) +
  geom_line(color = "grey50") +
  geom_point(data = highlights, size = 2, stroke = 1, 
             color = "grey50") +
  annotate(geom = "text", x = 2021.5, y = .29, label = "2022", 
           color = "grey70", fontface = "bold") +
  geom_text(data = highlights, 
            aes(label = paste0(round(proportion*100, 1), "%")),
            fontface = "bold", color = "grey50", nudge_x = 1) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2024)) +
  labs(
    x = "", y = "",
    # title = "Has psychological distress recently peaked?",
    subtitle = "Prevalence (%) of psychological distress in Australians aged 15-19"
  )