require(tidyverse)

ys <- read_rds("data/ys_preprocessed.rds")

# Mental health covariate trends
mhi_cols <- c(
  Q19PWIY_whole = '"Dissatisfied" with life',
  Q20MHSelfPerceived = '"Poor" self-rated\nmental health',
  Q23TheFuture = '"Negative" feelings about\nthe future'
)



wellbeing <- ys |> 
  select(UniqueID, Year, weight_final_1, all_of(names(mhi_cols))) |> 
  haven::zap_labels() |> 
  mutate(
    Q19PWIY_whole = as.numeric(Q19PWIY_whole < 5),
    Q20MHSelfPerceived = as.numeric(Q20MHSelfPerceived == 1),
    Q23TheFuture = as.numeric(Q23TheFuture < 3)
  ) |> 
  gather(key, val, 4:6) |> 
  mutate(
    key = recode_factor(key, !!!mhi_cols)) |> 
  filter(!is.na(val)) |>
  percent(Year, key, val, wt = weight_final_1) |> 
  mutate(N = sum(n)) |> 
  filter(val == 1) |> 
  ungroup()


highlights <- wellbeing |> 
  group_by(key) |> 
  mutate(m = proportion == max(proportion[Year < 2024])) |> 
  filter(Year == last(Year) | m | Year == first(Year))

fig_3 <- select(wellbeing, Year, key, proportion) |> 
  # spread(Year, proportion) |> 
  # gather("Year", "proportion", -key) |> 
  # mutate(Year = parse_number(Year)) |> 
  ggplot(aes(x = Year, color = key)) +
  geom_vline(aes(xintercept = 2011), alpha = 0) +
  geom_vline(aes(xintercept = 2025), alpha = 0) +
  geom_hline(aes(yintercept = 0), alpha = 0) +
  geom_hline(aes(yintercept = .3), alpha = 0) +
  geom_line(aes(y = proportion, group = key)) +
  geom_point(
    data = highlights, aes(y = proportion)) +
  geom_text(
    data = highlights, 
    aes(y = proportion, label = paste0(round(proportion*100), "%")),
    nudge_y = .025, nudge_x = 0.5, fontface = "bold", size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2024)) +
  scale_color_manual(values = c("grey40", "grey40", "salmon")) +
  facet_wrap(~key) +
  labs(
    subtitle = "Percent of Australians aged 15-19",
    title = "Percentage of young people with negative feelings about the
future are higher than ever",
    x = "", y = "") +
  # hrbrthemes::theme_ipsum(grid = "Y",
  #      plot_margin = margin(t = 5, r = 0, b = 0, l = 0)) +
  theme(legend.position = "none",
        strip.text = element_text(hjust = 1, vjust = 1),
        # panel.spacing = unit(.5, "lines"),
        axis.text.y = element_blank())
# plot.title.position = "plot")
