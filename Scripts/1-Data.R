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

Names <-list(Project = "DMH", #modifier éventuellement le nom du projet
             Model = c("W", "R", "E"),
             Type = c("D", "M"),
             Simulation = c("SV", "SO"))
Names$Data <- paste("../Data/", Names$Project, ".rds", sep="")

save(Names, file="../Results/Names.rdata")

#---transformation des données----
library(XLConnect)

data <- readWorksheetFromFile(file = "../Data/LLarrieu.xlsx", sheet="Feuil1")
data <- data[, - which(colnames(data)=="CouleeS")]

data$Ndmh <- rowSums(data[,7:15])
saveRDS(data, file="../Data/raw_data.rds")

data <- readRDS("../Data/raw_data.rds") #rentrer le nom du fichier de base

#Ici nous regroupons les cavités de pied et de tronc.
#data$Ndmh <- data$Cavité_pied + data$Cavité_tronc

#sélection sapin et hêtre
data<- data[which(data$Code_espece %in% c("AA", "FS")),]

data$Pdmh <- ifelse(data$Ndmh>0, 1, 0)

DMH.data <- data.frame(Foret    = data$Parcelle,
                       Placette = data$Placette,
                       IdArbre  = 1:dim(data)[1],
                       TypeEss  = data$Code_espece,
                       DBH      = data$C.1.3m..cm./pi,
                       Ndmh     = data$Ndmh,
                       Pdmh     = data$Pdmh)

#Ne pas modifier
saveRDS(DMH.data, Names$Data)
