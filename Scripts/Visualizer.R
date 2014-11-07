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
View(readRDS(PredLoss.criteria(), 
             file=paste( "Results/PredLoss_", 
                         Names$Project,
                         ".rds",
                         sep="")))
     
 View(readRDS(synth.MCMC(), 
             file=paste( "Results/SynthMCMC_", 
                         Names$Project,
                         ".rds",
                         sep="")))