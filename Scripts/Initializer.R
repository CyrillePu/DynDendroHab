#Ce script permet de mettre en forme la base de données utilisée dans la suite de l'analyse.
#Il faut tout d'abord compléter le chemin vers l'espace de travail.
#Ensuite redéfinir un nom de projet (ici "Cavity").
#Enfin réaliser les regroupement souhaités, puis completer la bdd.

rm(list=ls())
library(rstan)
library(doBy)
library(ggplot2)
#setwd("Z:\Private\R\DynDendroHab\v1\Default") #remplir le chemin vers le fichier Default

#Avant de commencer à executer le script il est nécessaire de définir la liste des codes
#qui permettra de créer les noms de fichier en combinant les différentes parties. Cette
#liste est appelée Names. Pour plus d'information, lire ../Results/README

# Names <-list(Project = "DMHW", #modifier éventuellement le nom du projet
#              Model = c("W", "R", "E"),
#              Type = c("D", "M"),
#              Simulation = c("SV", "SO"))

Names <-list(Project = "DMH", #modifier éventuellement le nom du projet
             Model = c("W", "R", "E"), #be careful to the name in the Stan_model/Model.R
             Type = c("D", "M"),
             Simulation = c("SV", "SO"))
Names$Data <- paste("Data/", Names$Project, ".rds", sep="")

save(Names, file="Results/Names.rdata")

#---transformation des données----
library(XLConnect)
data <- readWorksheetFromFile(file = "Data/LLarrieu.xlsx", sheet="Feuil1")
data <- data[, - which(colnames(data)=="CouleeS")]

data$Ndmh <- rowSums(data[,7:15])

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
#First graph
#----Distribution of DMH bearing tree-------
DMH.data$Classe <-trunc(((DMH.data$DBH)+5/2)/5)*5
Pdmh.Ess.Classe <- summaryBy(Pdmh ~ TypeEss + Classe , 
                             data = DMH.data, 
                             FUN = function (x) {sum(x)/length(x)},
                             var.names = "Pdmh", 
                             fun.names = "")

TitreEssence <- levels(DMH.data$TypeEss)
color <- c("darkgreen", "darkblue")

ggplot(Pdmh.Ess.Classe, aes(x = Classe, y = Pdmh., fill = TypeEss)) + 
  geom_bar(stat = "identity", position= position_dodge()) + 
  theme_bw() +
  xlab("Diameter class") + ylab("Frequency of \n TMH bearing trees.") +
  scale_fill_manual(name="Species", labels=TitreEssence, 
                    values=color) +
  theme(legend.position="bottom", legend.direction = "horizontal") +
  guides(fill=guide_legend(title.position="top"))

#Ne pas modifier
saveRDS(DMH.data, Names$Data)
