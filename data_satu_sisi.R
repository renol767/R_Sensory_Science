library(readr)
library(skimr)
library(dplyr)
library(FactoMineR)
library(agricolae)
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


# DATA SATU SISI
model_bitterness <- aov(bitterness ~ product + panelist + session + panelist:product + panelist:session + product:session + rank, data = chocolates)

anova(model_bitterness)

summary.lm(model_bitterness)
-1.74 # dua digit di belakang koma/titik

model_bitterness


res_bitterness <- AovSum(model_bitterness)

res_bitterness$Ftest

res_bitterness$Ttest[1:7, 1:2]
c("choc1", "choc4", "choc2", "choc5", "choc6", "choc3")

posthoc_bitterness <- HSD.test(model_bitterness, trt = "product")
posthoc_bitterness$groups

plot.group(posthoc_bitterness, variation = "SE")