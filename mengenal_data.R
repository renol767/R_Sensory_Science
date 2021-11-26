library(readr)
library(skimr)
library(dplyr)
chocolates <- 
  read_csv(
    "chocolates.csv",
    col_types = cols(
      panelist = col_factor(),
      session = col_factor(),
      rank = col_factor(),
      product = col_factor(levels = paste0("choc", 1:6)),
      .default = col_integer()
    )
  )
chocolates

skim(chocolates)

chocolates %>%
  summarise(
    sample = toString(levels(product)),
    n_sample = n_distinct(product),
    n_panelist = n_distinct(panelist)
  )

ncol(chocolates) - 4
atribut_sensoris <- colnames(chocolates[-c(1, 2, 3, 4)])
atribut_sensoris

chocolates %>%
  select(atribut_sensoris) %>%
  skim_without_charts()

batas_bawah <- 0
batas_atas <- 10