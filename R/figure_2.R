require(tidyverse)

ys <- read_rds("data/ys_preprocessed.rds")

reds <- RColorBrewer::brewer.pal(3, "Reds")
blues <- RColorBrewer::brewer.pal(3, "Blues")

k6_symptoms <- ys |> 
  select(Year, weight_final_1, starts_with("K6 ")) |> 
  gather(Symptoms, Val, `K6 Sad`:`K6 Worthless`) |> 
  mutate(Symptoms = str_remove(Symptoms, "K6 "),
         Symptoms = fct_relevel(Symptoms, "Sad", "Hopeless", "Worthless")) |> 
  filter(!is.na(Val)) |> 
  summarise(
    val = weighted.mean(Val, w = weight_final_1),
    .by = c(Year, Symptoms)
  )

fig_2 <- ggplot(k6_symptoms, 
                aes(x = Year, y = val, color = Symptoms, group = Symptoms)) +
  geom_vline(aes(xintercept = 2011), color = "white", linewidth = .5) +
  geom_vline(aes(xintercept = 2022), color = "grey80") +
  geom_hline(aes(yintercept = 1.5), alpha = 0) +
  geom_hline(aes(yintercept = 3), alpha = 0) +
  annotate(geom = "text", x = 2022.75, y = 3.1, label = "2022", 
           color = "grey70", fontface = "bold") +
  geom_line() +
  guides(color = guide_legend(reverse = T)) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2024)) +
  scale_color_manual(values = c(blues, reds),
                     guide = guide_legend(ncol = 1)) +
  labs(
    x = "", 
    y = "",
    subtitle = "Average symptom severity (from 1 'absent' to 5 'all the time') in Australians aged 15-19",
    title = "Psychological distress has been primarily driven by anxiety
symptoms in young people",
  ) +
  hrbrthemes::theme_ipsum(grid = "Y",
                          plot_margin = margin(t = 10, r = 30, b = 0, l = 0)) +
  theme(plot.title.position = "plot",
        plot.title = element_text(margin = margin(l = 10, b = 10)),
        plot.subtitle = element_text(margin = margin(l = 10)))
   