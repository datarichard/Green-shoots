# Table 1. Demographics ####
library(labelled)
library(flextable)
library(gtsummary)

demographics <- read_rds("data/ys_preprocessed.rds") |> 
  select(UniqueID, Year, Q1Gender_Final, Q3Age_Final, Q2State,  
         OLDCountryofBirth, Q43LOTE, Q41ATSI_Final, Q45DisabilityYS, 
         Q11PaidWork, Q5Studying, Q37AwayFromHome)

demographics |> 
  filter(Year %in% c(2019, 2022, 2024)) |> 
  as_factor() |> 
  rename(Age = Q3Age_Final) |> 
  mutate(#Age = if_else(Age == 19, 18, Age),
    NEET = str_detect(Q5Studying, "Yes", negate = T) &
      str_detect(Q11PaidWork, "Yes", negate = T),
    Studying = str_detect(Q5Studying, "Yes")) |> 
  select(-UniqueID, -Q5Studying, -Q11PaidWork) -> df

var_labels <- labelled::get_variable_labels(df, null_action = "fill") |> 
  map(~str_remove(.x, ".* = ")) |> 
  map(~str_replace(.x, "Two categories ATSI status", "Indigenous (ATSI)")) |> 
  map(~str_replace(.x, "Country they were born in", "Born outside Australia")) 

table_1 <- df |> 
  mutate(
    OLDCountryofBirth = OLDCountryofBirth == "Other",
    Q43LOTE = Q43LOTE == "Yes",
    Q41ATSI_Final = Q41ATSI_Final == "Indigenous",
    Q45DisabilityYS = Q45DisabilityYS == "Yes",
    Q37AwayFromHome = Q37AwayFromHome == "Yes"
  ) |> 
  set_variable_labels(.labels = var_labels, .strict=F) |> 
  tbl_summary(by = "Year",
              missing = "no") |> 
  as_flex_table() |> 
  align(j = 2:4, align = "right", part = "all")