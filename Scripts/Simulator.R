#!/usr/bin/env Rscript --verbose

#Scripts permettant de réaliser les imulations nécessaires à l'analyse des modèles.
#1 h on 2.26 Ghz intel core 2 duo with 8 Go of memory 
rm(list=ls())
load("Results/Names.rdata")
source("Scripts/Functions/simulate.R")

#-----Réalisation des simulations de vérification du modèle-----
for(i in 1: length(Names$Model)){
  #Simulation sur un vecteur
  Y <- simulation(PostDist.file =paste( "Results/", 
                                        paste(Names$Project,
                                              Names$Model[i],
                                              sep="_"),
                                        ".rds",
                                        sep="") )
  saveRDS(Y, file=paste( "Results/", 
                         paste(Names$Project,
                               Names$Model[i],
                               Names$Type[2],
                               Names$Simulation[1],
                               sep="_"),
                         ".rds",
                         sep="") )
}

for(i in 1:length(Names$Model)){
  #Simulation sur les observations
  Z <- simulation(PostDist.file = paste( "Results/", 
                                         paste(Names$Project,
                                               Names$Model[i],
                                               sep="_"),
                                         ".rds",
                                         sep=""), 
                  Obs.file = Names$Data,
                  n.repetitions=1000)
  
  saveRDS(Z, file=paste( "Results/", 
                         paste(Names$Project,
                               Names$Model[i],
                               Names$Type[2],
                               Names$Simulation[2],
                               sep="_"),
                         ".rds",
                         sep=""))
}

#-----Réalisation des simulations dynamique-----

for(i in 1:length(Names$Model)){
  #Simulation sur les observations
  Z <- simulation.Dyn(PostDist.file = paste( "Results/", 
                                             paste(Names$Project,
                                                   Names$Model[i],
                                                   sep="_"),
                                             ".rds",
                                             sep=""), 
                      Obs.file = Names$Data,
                      n.repetitions=1000,
                      Acc.DBH=0.5)
  
  saveRDS(Z, file=paste( "Results/", 
                         paste(Names$Project,
                               Names$Model[i],
                               Names$Type[1],
                               Names$Simulation[2],
                               sep="_"),
                         ".rds",
                         sep=""))
}

for(i in 1:length(Names$Model)){
  #Simulation sur un vecteur
  Z <- simulation.Dyn(PostDist.file = paste( "Results/", 
                                             paste(Names$Project,
                                                   Names$Model[i],
                                                   sep="_"),
                                             ".rds",
                                             sep=""), 
                      DBH=rep(130, times=1000),
                      n.repetitions=1000,
                      Acc.DBH=0.5,
                      summary=TRUE)
  
  saveRDS(Z, file=paste( "Results/", 
                         paste(Names$Project,
                               Names$Model[i],
                               Names$Type[1],
                               Names$Simulation[1],
                               sep="_"),
                         ".rds",
                         sep=""))
}

