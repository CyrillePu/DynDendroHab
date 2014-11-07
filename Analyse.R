rm(list=ls())
library(plyr)
load("/Results/Names.rdata")
source("/Scripts/fonctionsMod.R")
source("/Scripts/agregator.R")
source("/Scripts/figure.R")
source("/Scripts/table.R")

saveRDS(agregate.Predic.occurrence.stand(),
        file=paste( "../Results/agregate_", 
                    paste(Names$Project,
                          Names$Type[2],
                          Names$Simulation[2],
                          sep="_"),
                    ".rds",
                    sep=""))

saveRDS(agregate.Predic.functions(),
        file=paste( "../Results/agregate_", 
                    paste(Names$Project,
                          Names$Type[2],
                          Names$Simulation[1],
                          sep="_"),
                    ".rds",
                    sep=""))

saveRDS(agregate.LifePredic.stand(), 
        file=paste( "../Results/agregate_", 
                    paste(Names$Project,
                          Names$Type[1],
                          Names$Simulation[2],
                          sep="_"),
                    ".rds",
                    sep=""))

saveRDS(agregate.LifePredic.tree(), 
        file=paste( "../Results/agregate_", 
                    paste(Names$Project,
                          Names$Type[1],
                          Names$Simulation[1],
                          sep="_"),
                    ".rds",
                    sep=""))

saveRDS(PredLoss.criteria(), 
        file=paste( "../Results/PredLoss_", 
                    Names$Project,
                    ".rds",
                    sep=""))

saveRDS(synth.MCMC(), 
        file=paste( "../Results/SynthMCMC_", 
                    Names$Project,
                    ".rds",
                    sep=""))

## ---- Graphique ----
rm(list=ls())
load("/Results/Names.rdata")
source("/Scripts/fonctionsMod.R")
source("/Scripts/agregator.R")
source("/Scripts/figure.R")

plot.Predict.occurrence.stand()

plot.Predict.functions()

plot.LifePredict.Ndmh.stand()

plot.LifePredict.PNdmh_DBH.tree()