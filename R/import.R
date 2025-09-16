# Import ####
# RW Morris Sept 15th 2025
# This script was run on the raw data from the Mission Australia youth survey 
# (raw data is unavailable)
# 
# Import the raw-data from Mission Australia, perform some preprocessing (e.g.,
# select relevant columns), and then save in \data directory for further 
# analysis. 
require(tidyverse)
library(haven)

## paths ####
ys_paths <- list.files(
  path    = "../data/YS Geospatial PD_2012_2024", 
  pattern = "*.sav$",
  full    =T
)

k6_paths <- list.files(
  path    = "../data/YS Geospatial_K6_12_24", 
  pattern = "*._K6.sav$",
  full    =T
)

## read .sav ####
# Youth survey details including Q25K6SUM_AIFS (low, med, high distress), 
ys <- map(ys_paths, read_sav) |> 
  set_names(str_remove(ys_paths, "../data/YS Geospatial PD_2012_2024/YS "))

# Youth survey K6 scores including Q25K6SUM (summed score 6-30)
k6 <- map(k6_paths, read_sav) |> 
  set_names(str_remove(k6_paths, "../data/YS Geospatial_K6_12_24/YS "))

# Youth survey K6 scores including Q25K6SUM (summed score 6-30)
k6_list <- map(k6_paths, read_sav) 

k6_key <- k6_list[[1]] |> 
  labelled::var_label() |> 
  enframe() |> 
  unnest(value) |>
  filter(str_detect(name, "^Q")) |> 
  mutate(value = str_remove(value, paste0(name, "..")),
         value = fct_relevel(value, "K6 Sad", "K6 Hopeless", "K6 Worthless")) |>
  arrange(value) |> 
  select(value, name) |> 
  deframe()

# K6 Sum = 6 to 30, K6 symptoms = 1 to 5
k6 <- k6_list |> 
  set_names(str_remove(k6_paths, "data/YS Geospatial_K6_12_24/YS ")) |> 
  # Reverse the score so higher scores = more distress
  map(.f = ~{mutate(.x, across(Q25Feel_1:Q25Feel_6, ~ 6 - .x)) |> 
      rename(!!!k6_key)}) |> 
  # Correct the summed score (2024 has values K6 == 96)
  map(.f = ~{mutate(.x, `K6 Sum` = na_if(`K6 Sum`, 96))}) |> 
  bind_rows()



## select cols ####
# Select other variables needed for report 
# 
# Primary keys
id_cols <- c("UniqueID", "Year", "weight_final_1")

# Demographics for table 1
demog_cols <- c(
  "Q1Gender_Final", "Q3Age_Final", "Q2State", "OLDCountryofBirth", "Q43LOTE",
  "Q41ATSI_Final", "Q5Studying", "Q11PaidWork", "Q37AwayFromHome"
)

# Mental health covariate trends
mhi_cols <- c(
  Q19PWIY_whole, # '"Dissatisfied" with life',
  Q20MHSelfPerceived, # '"Poor" self-rated\nmental health',
  Q23TheFuture # '"Negative" feelings about\nthe future'
)

# At-risk groups
vulnerable_cols <- c(
  "Q29DiscriminationMe",
  "Q41ATSI_Final",
  "Q43LOTE",
  "Q45DisabilityYS",
  "Q1Gender_Final",
  "Q35ResidentialSetting"
)

# select cols, join with K6 and write to data
map_dfr(ys, ~select(.x, 
                    all_of(id_cols), 
                    K6_class = Q25K6SUM_AIFS,
                    all_of(mhi_cols),
                    any_of(vulnerable_cols),
                    any_of(demog_cols)
)) |> # 263,507 by 17
  inner_join(k6, by = join_by("UniqueID")) |> # 263,507 by 24
  write_rds("../data/ys_preprocessed.rds") 
