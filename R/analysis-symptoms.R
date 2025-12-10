# Linear trend analysis of symptoms ####
# 
# Setup ####
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

# Preprocess data ####
ys <- read_rds("data/ys_preprocessed.rds")

df <- ys |> 
  select(Year, ID = UniqueID, starts_with("K6 "), -`K6 Sum`) |> 
  gather(Symptoms, Val, `K6 Sad`:`K6 Worthless`) |> 
  mutate(Symptoms = str_remove(Symptoms, "K6 "),
         Symptoms = fct_relevel(Symptoms, "Sad", "Hopeless", "Worthless"),
         dim = if_else(Symptoms %in% c("Worthless", "Hopeless", "Sad"),
                       "Dep", "Anx"),
         # Year = as.factor(Year)
  ) |> 
  filter(!is.na(Val)) 
summary(df)

# Fit model from 2012 to 2019 ####
fit <- lm(Val ~ Symptoms*Year, data = filter(df, Year < 2020))

summary(fit)
emtrends(fit, "Symptoms", var = "Year") |> pairs()

# contrast             estimate     SE     df t.ratio p.value
# Sad - Hopeless       -0.00122 0.0019 950477  -0.643  0.9878
# Sad - Worthless      -0.00261 0.0019 950477  -1.372  0.7438
# Sad - Effort         -0.01172 0.0019 950477  -6.172  <.0001
# Sad - Nervous        -0.02151 0.0019 950477 -11.333  <.0001
# Sad - Restless       -0.02671 0.0019 950477 -14.061  <.0001
# Hopeless - Worthless -0.00138 0.0019 950477  -0.728  0.9786
# Hopeless - Effort    -0.01050 0.0019 950477  -5.518  <.0001
# Hopeless - Nervous   -0.02029 0.0019 950477 -10.670  <.0001
# Hopeless - Restless  -0.02548 0.0019 950477 -13.393  <.0001
# Worthless - Effort   -0.00911 0.0019 950477  -4.795  <.0001
# Worthless - Nervous  -0.01891 0.0019 950477  -9.951  <.0001
# Worthless - Restless -0.02410 0.0019 950477 -12.677  <.0001
# Effort - Nervous     -0.00979 0.0019 950477  -5.155  <.0001
# Effort - Restless    -0.01499 0.0019 950477  -7.884  <.0001
# Nervous - Restless   -0.00519 0.0019 950477  -2.732  0.0690
# 
# Trends in Sad, Hopeless and Worthless are all significantly different from 
# trends in Effort, Nervous, Restless, but not significantly different from each
# other.


# (check whether above result is sensitive to dependency)
library(lme4)
library(lmerTest)

fit <- lmer(Val ~ Symptoms*Year + (1|ID), data = filter(df, Year < 2020)) 
emtrends(fit, "Symptoms", var = "Year") |> pairs()

# contrast              estimate      SE  df z.ratio p.value
# Sad - Hopeless       -0.000975 0.00127 Inf  -0.768  0.9729
# Sad - Worthless      -0.002609 0.00127 Inf  -2.056  0.3107
# Sad - Effort         -0.011589 0.00127 Inf  -9.133  <.0001
# Sad - Nervous        -0.021577 0.00127 Inf -17.011  <.0001
# Sad - Restless       -0.026600 0.00127 Inf -20.960  <.0001
# Hopeless - Worthless -0.001634 0.00127 Inf  -1.285  0.7934
# Hopeless - Effort    -0.010614 0.00127 Inf  -8.348  <.0001
# Hopeless - Nervous   -0.020602 0.00127 Inf -16.209  <.0001
# Hopeless - Restless  -0.025624 0.00127 Inf -20.151  <.0001
# Worthless - Effort   -0.008980 0.00127 Inf  -7.071  <.0001
# Worthless - Nervous  -0.018968 0.00127 Inf -14.939  <.0001
# Worthless - Restless -0.023990 0.00127 Inf -18.886  <.0001
# Effort - Nervous     -0.009988 0.00127 Inf  -7.867  <.0001
# Effort - Restless    -0.015010 0.00127 Inf -11.818  <.0001
# Nervous - Restless   -0.005023 0.00127 Inf  -3.956  0.0011
# 
# Random intercept model supports the same conclusion above.



# Fit model from 2019 to 2022 ####
fit <- lm(Val ~ Symptoms*Year, data = filter(df, Year %in% 2019:2022))

summary(fit)
emtrends(fit, "Symptoms", var = "Year") |> summary(infer = T)

# Symptoms  Year.trend      SE     df lower.CL upper.CL t.ratio p.value
# Sad         -0.02045 0.00370 530099 -0.02771 -0.01320  -5.523  <.0001
# Hopeless    -0.00313 0.00372 530099 -0.01042  0.00416  -0.842  0.4000
# Worthless    0.00598 0.00371 530099 -0.00129  0.01324   1.611  0.1071
# Effort       0.01899 0.00371 530099  0.01172  0.02626   5.122  <.0001
# Nervous      0.01918 0.00371 530099  0.01191  0.02644   5.173  <.0001
# Restless     0.05413 0.00371 530099  0.04687  0.06140  14.596  <.0001
# 
# Trends in anxiety symptoms were signficant and positive between 2019 and 2022.
# Trends in depressive symptoms were either negative or non-significant.


# Not run
library(ggeffects)

ggpredict(fit, terms = c("Year", "Symptoms")) |> plot()
ggeffects::hypothesis_test(fit, terms = c("Year", "Symptoms"))

broom.mixed::tidy(fit, effects = "fixed")

