# Reversal of Fortunes ####
# RW Morris Sept 15th 2025
# 
# This script functions like a primitive run-file to import, preprocess and 
# analyse data. It will render a report (word.docx) when complete.  
# 
# Setup ####
library(tidyverse)
library(haven)
library(labelled)
library(flextable)
library(gtsummary)
source("R/helpers.R")

# Import (not run) ####
# Import the raw-data from Mission Australia, perform some preprocessing (e.g.,
# select relevant columns), and then saves the result in data\ys_preprocessed.rds
# for further analysis below. 
# source("R/import.R") # requires raw-data (not provided)


# Table 1. Demographics ####
source("R/table_1.R")
print(table_1)

# Figure 1. Psych distress trends ####
# 
# Add theme and adjust plot margins:
# default: margin(t = 30, r = 30, b = 30, l = 30)
theme_set(
  hrbrthemes::theme_ipsum(
    plot_margin = margin(t = 10, r = 30, b = 0, l = 0),
    # plot_margin = margin(t = 5, r = 10, b = 0, l = 0),
    grid = "Y") + 
    theme( 
      axis.text.y = element_text(
        margin = margin(r = -22), vjust = -0.5),
      panel.spacing = unit(.5, "lines")
    ))

source("R/figure_1.R")
print(fig_1)


# Reversal of fortune ####
rmarkdown::render("R/Reversal of fortune.Rmd")