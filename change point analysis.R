# Changepoint analysis 
# 
# https://lindeloev.github.io/mcp/index.html
# 
# Setup
library(tidyverse)
library(mcp)
library(patchwork)

source("R/helpers.R")

ys <- read_rds("data/ys_preprocessed.rds") |> 
  select(State = Q2State, Gender = Q1Gender_Final, 
         Year, K6_class, weight_final_1) |> 
  mutate(State = as_factor(State),
         Gender = as_factor(Gender)) |> 
  haven::zap_labels()

# Changepoint (homogenous) ####
#
# We fit a Bayesian changepoint model using the mcp package in R (Lindeløv, 
# 2020) to examine whether the temporal trend in psychological distress 
# prevalence changed over the study period. The outcome was the proportion of 
# respondents reporting high psychological distress (K6 scale), modeled as a 
# binomial response with the number of trials equal to the sample size for each
# year. The model consisted of two linear segments:
#  1) An intercept and slope for Year.
#  2) A slope for Year without an intercept, allowing the level and trend to 
#  shift at the changepoint.
#  
# We specified a weakly informative prior for the changepoint (a truncated 
# normal distribution centered at 2 years, SD = 3, bounded between –3 and 5 
# years) to balance plausibility and flexibility. Weakly informative priors are
# recommended in Bayesian models when domain knowledge suggests a reasonable 
# range but strong constraints are unwarranted (Gelman et al., 2017). In this 
# case, the prior reflects the expectation that any shift in trend would likely
# occur within the observed time frame, while allowing substantial uncertainty
# so that the data primarily determine the posterior estimate. This approach
# helps stabilize estimation without imposing unrealistic assumptions, reducing
# the risk of overfitting and improving interpretability.
#     
# We did not use a uniform prior because, although noninformative, it can 
# allocate probability mass to implausible changepoint values and slow 
# convergence. A weakly informative prior constrains the estimate to a plausible
# region based on substantive knowledge while still permitting flexibility, 
# improving computational efficiency and reducing the risk of spurious 
# changepoint detection without imposing strong assumptions.
# 
# 
#
k6_percents <- ys |> 
  mutate(High = K6_class == 3) |> 
  filter(!is.na(High)) |> 
  percent(Year, High, wt = weight_final_1) |> 
  mutate(
    Year = Year - 2018, # 0 = 2018
    N = round(sum(n)),
    n = round(n)) |> 
  filter(High) |> 
  ungroup() |> 
  select(Year, y = n, N, proportion)

## Fit ####
fit <- mcp(
  model = list(
    y | trials(N) ~ 1 + Year,
    ~ 0 + Year
  ),
  data = k6_percents,
  family = binomial(),
  iter = 5000,
  prior = list(cp_1 = "dnorm(2, 3) T(-3, 5)")
  # prior = list(cp_1 = "dunif(0, 6)")
)


summary(fit)
# Population-level parameters:
# name   mean  lower upper Rhat n.eff
# cp_1  3.978  3.718  4.29    1 18501
# 
# The change point is between 3.7 and 4.3 (2021.7 and 2022.3)

plot(fit) +
  scale_x_continuous(labels = ~.x + 2018) +
  labs(
    title = "Estimated trend in psychological distress prevalence with Bayesian
changepoint model (2012–2024)",
    subtitle = "Prevalence of psychological distress in young people aged 15-19",
    y = "")

plot_pars(fit)

## Model comparison ####
#
# To evaluate whether the changepoint model provided a better fit than a null 
# model without a changepoint, we compared the two models using approximate 
# leave-one-out cross-validation (LOO) implemented in the loo package in R 
# (Vehtari et al., 2017). The null model included a single linear trend for Year
# without a changepoint. Both models were fit using a binomial likelihood and we
# computed the expected log predictive density (ELPD) for each model and 
# compared them to determine model evidence. A positive difference in ELPD 
# indicates that the changepoint model predicts the data better than the null
# model, with the magnitude of the difference and its standard error providing 
# evidence strength. This approach is recommended for Bayesian model comparison
# because it accounts for predictive accuracy rather than relying on point 
# estimates or information criteria alone.

null <- mcp(
  model = list(
    y | trials(N) ~ 1 + Year),
  data = k6_percents,
  iter = 5000,
  family = binomial()
  )

loo::loo_compare(loo(fit), loo(null))
#       elpd_diff se_diff
# model1    0.0       0.0 
# model2 -272.7     148.9 
# 
# The changepoint model demonstrated superior predictive performance compared to
# the null model, with an ELPD difference of 272.7 (SE = 148.9). Although the 
# ELPD difference is large, the standard error is also relatively large 
# (ELPD/SE ratio < 2, Vehtari et al., 2017), which suggests the changepoint 
# model provides only marginal evidence of a structural shift over a single 
# linear trend during this brief time period (13 years). Nevertheless, the 
# posterior estimate was precise, indicating that the changepoint occurred in 
# year 2022 (95% credible interval: 2021 to 2023), consistent with a shift in 
# psychological distress prevalence during the COVID-19 pandemic. The narrow 
# credible interval and convergence diagnostics (R̂ = 1.00, n_eff = 2,074)
# indicate high certainty in the timing estimate and good model convergence.
# 
# Note: elpd_diff/se_diff ratio p-value = .06 (2*pnorm(272.7/148.9, 
# lower.tail=F))

hypothesis(fit, c("cp_1 > 3"))
#     hypothesis      mean     lower    upper p  BF
# 1 cp_1 - 3 > 0 0.9722861 0.7096328 1.287802 1 Inf
# 
# The model is certain the change point is sometime after 2021


# Varying changepoints ####
# 
# Relax the homogeneity assumption by allowing changepoints to vary (randomly)
# by State
k6_state <- ys |> 
  mutate(High = K6_class == 3) |> 
  filter(!is.na(High)) |> 
  percent(State, Year, High, wt = weight_final_1) |> 
  mutate(
    Year = Year - 2018,
    N = round(sum(n)),
    n = round(n)) |> 
  filter(High) |> 
  ungroup() |> 
  filter(State %in% c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "ACT", "NT")) |> 
  select(State, Year, y = n, N, proportion)


fit <- mcp(
  model = list(
    y | trials(N) ~ 1 + Year + (1 + Year|State),
    1 + (1|State) ~ 0 + Year + (1|State)
  ),
  data = k6_state,
  family = binomial(),
  iter = 5000,
  prior = list(cp_1 = "dnorm(3, 1) T(-3, 5)")
)

fixef(fit)
ranef(fit)

plot(fit, facet_by = "State")
# This model assumes all States have the same slope before and after the 
# changepoint, which is not true. So we try State-specific models below.

# State-specific models ####

k6_state_nested <- k6_state |> 
  group_by(State) |> 
  nest() |> 
  mutate(
    fit = map(data, ~mcp(
      model = list(
        y | trials(N) ~ 1 + Year,
        ~ 0 + Year
      ),
      data = .x,
      family = binomial(),
      iter = 5000,
      prior = list(cp_1 = "dnorm(2, 3) T(-3, 5)")
      # prior = list(cp_1 = "dunif(-3, 6)")
    )) |> 
      set_names(State)
  )

# k6_state$fit <- set_names(x = k6_state$fit, nm = k6_state$State) 


map_df(k6_state_nested$fit, fixef, .id = "State")

# Null model
k6_state_nested <- k6_state_nested |> 
  mutate(
    null = map(data, ~mcp(
      model = list(y | trials(N) ~ 1 + Year),
      data = .x,
      family = binomial(),
      iter = 5000
    )) |> 
      set_names(State)
  )

# Model comparison
k6_state_nested <- k6_state_nested |> 
  mutate(
    model_compare = map2(.x = fit, .y = null, ~{
      loo::loo_compare(loo(.x), loo(.y))
    })
  )

# Results
k6_state_nested |> 
  transmute(
    winner = map_chr(model_compare, ~dimnames(.x)[[1]][1]),
    win.ratio = map_dbl(model_compare, 
                        ~{abs(.x[2, 1] / .x[2, 2])}),
    p.value = 2*pnorm(win.ratio, lower.tail=F)
    )

# State winner win.ratio p.value
# NSW   model1     1.83   0.0673
# VIC   model1     1.79   0.0729
# QLD   model1     1.79   0.0727
# SA    model1     1.28   0.201 
# WA    model2     0.697  0.486 
# TAS   model1     0.754  0.451 
# NT    model1     0.502  0.615 
# ACT   model1     0.267  0.790 
# 
# All states except WA had positive evidence for a changepoint, however the 
# evidence in each case did not meet conventional levels of confidence (p < .05).
# The p.values for NSW, VIC & QLD provided marginal confidence (p < .1), while 
# the p.values for the other states were positive but inconclusive (ps > 0.2). 


# Select winning models:
winning_models <- bind_rows(
  k6_state_nested |> 
    mutate(winner = map_chr(model_compare, ~dimnames(.x)[[1]][1])) |> 
    filter(winner == "model1") |> 
    select(State, fit),
  
  k6_state_nested |> 
    mutate(winner = map_chr(model_compare, ~dimnames(.x)[[1]][1])) |> 
    filter(winner == "model2") |> 
    select(State, fit = null)
) |> 
  mutate(
    plots = map(fit, ~plot(.x) + 
                  scale_x_continuous(labels = ~.x + 2018) +
                  coord_cartesian(ylim = c(0.15, 0.35)) +
                  labs(title = State, y = ""))
  )

wrap_plots(winning_models$plots, ncol = 3)


# Gender specific ####
k6_gender <- ys |> 
  mutate(High = K6_class == 3) |> 
  filter(!is.na(High)) |> 
  percent(Gender, Year, High, wt = weight_final_1) |> 
  mutate(
    Year = Year - 2018,
    N = round(sum(n)),
    n = round(n)) |> 
  filter(High) |> 
  ungroup() |> 
  filter(Gender %in% c("Male", "Female")) |> 
  select(Gender, Year, y = n, N, proportion)


k6_gender_nested <- k6_gender |> 
  group_by(Gender) |> 
  nest() |> 
  mutate(
    fit = map(data, ~mcp(
      model = list(
        y | trials(N) ~ 1 + Year,
        ~ 0 + Year
      ),
      data = .x,
      family = binomial(),
      iter = 5000,
      prior = list(cp_1 = "dnorm(2, 3) T(-3, 5)")
      # prior = list(cp_1 = "dunif(-3, 6)")
    )) |> 
      set_names(Gender)
  )


# Null model
k6_gender_nested <- k6_gender_nested |> 
  mutate(
    null = map(data, ~mcp(
      model = list(y | trials(N) ~ 1 + Year),
      data = .x,
      family = binomial(),
      iter = 5000
    )) |> 
      set_names(Gender)
  )

# Model comparison
k6_gender_nested <- k6_gender_nested |> 
  mutate(
    model_compare = map2(.x = fit, .y = null, ~{
      loo::loo_compare(loo(.x), loo(.y))
    })
  )

# Results
k6_gender_nested |> 
  transmute(
    winner = map_chr(model_compare, ~dimnames(.x)[[1]][1]),
    win.ratio = map_dbl(model_compare, 
                        ~{abs(.x[2, 1] / .x[2, 2])}),
    p.value = 2*pnorm(win.ratio, lower.tail=F)
  )

map_df(k6_gender_nested$fit, fixef, .id = "Gender")

mutate(k6_gender_nested,
  plots = map(fit, ~plot(.x) + 
                scale_x_continuous(labels = ~.x + 2018) +
                # coord_cartesian(ylim = c(0.15, 0.35)) +
                labs(title = Gender, y = ""))
) |> 
  pull(plots) |> 
  wrap_plots(ncol = 1)



# 2025 ####
# 2025 survey results:
prop_ci(3328, 17155, rowwise=T)
# 19.4% 95% CI [18.8%, 20.0%]

df <- k6_percents |> 
  add_row(
    Year = 7, y = round(0.194*17155), N = 17155, proportion = 0.194
  )


fit <- mcp(
  model = list(
    y | trials(N) ~ 1 + Year,
    ~ 0 + Year
  ),
  data = df,
  family = binomial(),
  iter = 5000,
  # prior = list(cp_1 = "dnorm(2, 3) T(-3, 5)") # normal on 2020
  prior = list(cp_1 = "dunif(0, 6)") # uniform between 2018 and 2024
)


summary(fit)

null <- mcp(
  model = list(
    y | trials(N) ~ 1 + Year),
  data = df,
  iter = 5000,
  family = binomial()
)

loo::loo_compare(loo(fit), loo(null))

elpd_diff = 543.8
se_diff = 212.2

elpd_diff/se_diff
# 2.562677

2*pnorm(elpd_diff/se_diff, lower.tail=F)
# 0.0104



# Not run ####
cbind(fit$prior)
#        [,1]                         
# cp_1   "dunif(MINX, MAXX)"          
# Year_1 "dnorm(0, 3 / (MAXX - MINX))"
# Year_2 "dnorm(0, 3 / (MAXX - MINX))"




