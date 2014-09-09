#Calcul des critéres de comparaison des modèles 
#(Modèle simple VS modèle à plusieurs Var Expl)
#Selon la méthode Predictive Loss (Clark 2007)
#Nous utilisons une méthode basée sur l'individu
##########################################################################
rm(list=ls())
source("../Script/fonctionsMod.R")

#Chargement des données observées
data <- readRDS("../Data/DMH.rds")

#Matrice de traitement
Traitement <- matrix(data = c( "Exponentiel", "Rayleigh", "Weibull",
                               "Sim_Obs_E.rds", 
                               "Sim_Obs_R.rds", 
                               "Sim_Obs_Wkl.rds"),
                     nrow=3, ncol=2)
VectLettre <- c("E", "R", "Wkl")

RepRes <- "../Results/"
TabF <- list()
indicelist <-1

CritPred <-data.frame(matrix(nrow=3, ncol=0))

#boucle sur les modèles
for (i in 1:dim(Traitement)[1]){
  #choix des données sorties MCMC
  SimulObs<-readRDS(file = paste(RepRes, 
                                 Traitement[i, 2], 
                                 sep=""))
  Nom <- Traitement[i, 1]
  
  Results           <- summaryBy( PdmhSim+PdmhObs ~ idArbre, data=SimulObs, FUN = c(mean, var))
  Results$Gm        <- (Results$PdmhSim.mean - Results$PdmhObs.mean)^2 
  
  #Critère de prédiction
  Gm <-   sum(Results$Gm)/length(Results$Gm)
  Pm <-   sum(Results$PdmhSim.var)/length(Results$Gm)
  Dm <-   Gm + Pm
  
  CritPredTemp  <- c(Gm, Pm, Dm)
  
  CritPred <- cbind(CritPred, CritPredTemp)
}
colnames(CritPred) <-Traitement[, 1]
rownames(CritPred)<-c("Gm", "Pm", "Dm")


