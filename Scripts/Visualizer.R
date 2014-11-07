rm(list=ls())
load("Results/Names.rdata")
source("Scripts/Functions/simulate.R")
source("Scripts/Functions/figurize.R")
source("Scripts/Functions/table.R")

## ---- Graphique ----

plot.Predict.occurrence.stand()

plot.Predict.functions()

plot.LifePredict.Ndmh.stand()

plot.LifePredict.PNdmh_DBH.tree()

## ---- Table ----
