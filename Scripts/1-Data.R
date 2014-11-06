#Ce script permet de mettre en forme la base de données utilisée dans la suite de l'analyse.
#Il faut tout d'abord compléter le chemin vers l'espace de travail.
#Ensuite redéfinir un nom de projet (ici "Cavity").
#Enfin réaliser les regroupement souhaités, puis completer la bdd.

rm(list=ls())
#setwd("Z:\Private\R\DynDendroHab\v1\Default") #remplir le chemin vers le fichier Default

#Avant de commencer à executer le script il est nécessaire de définir la liste des codes
#qui permettra de créer les noms de fichier en combinant les différentes parties. Cette
#liste est appelée Names. Pour plus d'information, lire ../Results/README

# Names <-list(Project = "DMHW", #modifier éventuellement le nom du projet
#              Model = c("W", "R", "E"),
#              Type = c("D", "M"),
#              Simulation = c("SV", "SO"))

Names <-list(Project = "DMHW", #modifier éventuellement le nom du projet
             Model = c("W"),
             Type = c("D", "M"),
             Simulation = c("SV", "SO"))
Names$Data <- paste("../Data/", Names$Project, ".rds", sep="")

save(Names, file="../Results/Names.rdata")

#---transformation des données----
load("../Data/dmh.Rdata") #rentrer le nom du fichier de base

#Ici nous regroupons les cavités de pied et de tronc.
#data$Ndmh <- data$Cavité_pied + data$Cavité_tronc
data$Pdmh <- ifelse(data$Ndmh>0, 1, 0)

DMH.data <- data.frame(Foret    = data$Parcelle,
                       Placette = data$Placette,
                       IdArbre  = data$idArbre,
                       TypeEss  = data$EssType,
                       DBH      = data$DBH,
                       Ndmh     = data$Ndmh,
                       Pdmh     = data$Pdmh)

#Ne pas modifier
saveRDS(DMH.data, Names$Data)
