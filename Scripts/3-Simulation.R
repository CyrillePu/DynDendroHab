#Scripts permettant de réaliser les imulations nécessaires à l'analyse des modèles.
rm(list=ls())
#Remplir le chemin vers le dossier "Default"
setwd("Z:/Private/R/DynDendroHab/v1/Default") #remplir le chemin
load("../Results/Names.rdata")

#-----Réalisation des simulations de vérification du modèle-----
rm(list=ls()[ls()!="Names"])
source("../Scripts/fonctionsMod.R")

for(i in 1: length(Names$Model)){
  #Simulation sur un vecteur
  Y <- simulation(PostDist.file =paste( "../Results/", 
                                        paste(Names$Project,
                                              Names$Model[i],
                                              sep="_"),
                                        ".rds",
                                        sep="") )
  saveRDS(Y, file=paste( "../Results/", 
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
  Z <- simulation(PostDist.file = paste( "../Results/", 
                                         paste(Names$Project,
                                               Names$Model[i],
                                               sep="_"),
                                         ".rds",
                                         sep=""), 
                  Obs.file = Names$Data,
                  n.repetitions=100)
  
  saveRDS(Z, file=paste( "../Results/", 
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
  Z <- simulation.Dyn(PostDist.file = paste( "../Results/", 
                                             paste(Names$Project,
                                                   Names$Model[i],
                                                   sep="_"),
                                             ".rds",
                                             sep=""), 
                      Obs.file = Names$Data,
                      n.repetitions=100,
                      Acc.DBH=0.5)
  
  saveRDS(Z, file=paste( "../Results/", 
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
  Z <- simulation.Dyn(PostDist.file = paste( "../Results/", 
                                             paste(Names$Project,
                                                   Names$Model[i],
                                                   sep="_"),
                                             ".rds",
                                             sep=""), 
                      DBH=rep(130, times=100),
                      n.repetitions=100,
                      Acc.DBH=0.5)
  
  saveRDS(Z, file=paste( "../Results/", 
                         paste(Names$Project,
                               Names$Model[i],
                               Names$Type[1],
                               Names$Simulation[1],
                               sep="_"),
                         ".rds",
                         sep=""))
}

