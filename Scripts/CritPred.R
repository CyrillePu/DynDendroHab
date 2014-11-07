#Calcul des critéres de comparaison des modèles 
#(Modèle simple VS modèle à plusieurs Var Expl)
#Selon la méthode Predictive Loss (Clark 2007)
#Nous utilisons une méthode basée sur l'individu
##########################################################################
rm(list=ls())
source("../Scripts/fonctionsMod.R")
load("../Results/Names.rdata")

PredLoss.criteria()

PredLoss.criteria <- function (Names.files = Names){
  data <- readRDS(file = Names.files$Data)
  CritPred <-data.frame(matrix(nrow=3, ncol=0))
  
  #boucle sur les modèles
  for (i in 1:length(Names.files$Model)){
    #choix des données sorties MCMC
    in.sim <- readRDS(file=paste( "../Results/", 
                                  paste(Names.files$Project,
                                        Names.files$Model[i],
                                        Names.files$Type[2],
                                        Names.files$Simulation[2],
                                        sep="_"),
                                  ".rds",
                                  sep=""))
    
    Results           <- summaryBy( PdmhSim+PdmhObs ~ idArbre, data=in.sim, FUN = c(mean, var))
    Results$Gm        <- (Results$PdmhSim.mean - Results$PdmhObs.mean)^2 
    
    #Critère de prédiction
    Gm <-   sum(Results$Gm)/length(Results$Gm)
    Pm <-   sum(Results$PdmhSim.var)/length(Results$Gm)
    Dm <-   Gm + Pm
    
    CritPredTemp  <- c(Gm, Pm, Dm)
    CritPred <- cbind(CritPred, CritPredTemp)
  }
  colnames(CritPred) <-Names.files$Model
  rownames(CritPred)<-c("Gm", "Pm", "Dm")
  
  return(CritPred)
}