# Define vector of package names
packages <- c(
  "dplyr", "tidyverse", "stringr", "lubridate", "forcats", "fastDummies", "janitor", # data wrangling
  "naniar", # missing data exploration
  "psych", "lavaan", "RNOmni", # statistical analysis
  "ggplot2", "RColorBrewer", "patchwork", "scales", # visualisation
  "summarytools", "htmlTable", "officer", "gtsummary", "flextable" # descriptive stats
  )

# Load packages
for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}

rm(packages, package)

