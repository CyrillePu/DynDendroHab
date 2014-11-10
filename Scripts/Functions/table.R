library(doBy)

PredLoss.criteria <- function (Names.files = Names){
  data <- readRDS(file = Names.files$Data)
  CritPred <-data.frame(matrix(nrow=0, ncol=5))
  #colnames(CritPred) <- c("Models", "Species", "Gm", "Pm", "Dm")
  
  #boucle sur les modèles
  for (i in 1:length(Names.files$Model)){
    #choix des données sorties MCMC
    in.sim <- readRDS(file=paste( "Results/", 
                                  paste(Names.files$Project,
                                        Names.files$Model[i],
                                        Names.files$Type[2],
                                        Names.files$Simulation[2],
                                        sep="_"),
                                  ".rds",
                                  sep=""))
    
    Results           <- summaryBy(PdmhSim+PdmhObs ~ idArbre, data=in.sim, FUN = c(mean, var))
    Results$gm        <- (Results$PdmhSim.mean - Results$PdmhObs.mean)^2 
    Results <- merge(Results, in.sim[,c("idArbre", "TypeEss")])
    
    #Critère de prédiction
    crit <- function(x) {sum(x)/length(x)}
    CritPredEss <- summaryBy(data=Results, 
                             gm + PdmhSim.var~ TypeEss, 
                             FUN = crit, fun.names = "")
    CritPredTot <- data.frame(Species="All", 
                     Gm = crit(Results$gm),
                     Pm = crit(Results$PdmhSim.var))
    colnames(CritPredEss) <- c("Species", "Gm", "Pm")
    CritPredTemp <- rbind(CritPredEss, CritPredTot)
    CritPredTemp$Dm <-   CritPredTemp$Gm + CritPredTemp$Pm
    CritPredTemp$Models <- Names$Model[i]
    CritPred <- rbind(CritPred, CritPredTemp)
  }
  
  return(CritPred[, c("Models", "Species", "Gm", "Pm", "Dm")])
}
###################################################

synth.MCMC <- function(Names.files=Names, param = c("lambda", "k")) {
  param <- c("lambda", "k")
 out.mod <- data.frame(matrix(nrow = 0, ncol = 9))
 for (m in 1:length(Names.files$Model)){
   in.MCMC <- readRDS(file=paste( "Results/", 
                                  paste(Names.files$Project,
                                        Names.files$Model[m],
                                        sep="_"),
                                  ".rds",
                                  sep=""))
   #extract values
   Species <- in.MCMC$Ess
   
   out.spec <- data.frame(matrix(nrow=0, ncol=7))
   for(i in 1:length(param)){
     out.param <- cbind(param[i],
                        t(apply(X=in.MCMC[[param[i]]], MARGIN = 2, FUN = q5pc)),
                        (apply(X=in.MCMC[[param[i]]], MARGIN = 2, FUN = mean)),
                        (apply(X=in.MCMC[[param[i]]], MARGIN = 2, FUN = var)))
     colnames(out.param) <- c("Param", "q2.5pc", "q50pc", "q97.5pc", "mean", "var")
     if(dim(out.param)[1]<length(param)) {out.param <- rbind(out.param, out.param)} ###/!\ bad
     out.param <- cbind(out.param, Species)
     out.spec <- rbind(out.spec, out.param)
   }
   
   out.spec$cov <- rep(diag(cov(in.MCMC[[param[1]]], in.MCMC[[param[2]]])),2)
   out.spec$Model <- Names$Model[m]
   out.mod <- rbind(out.mod, out.spec)
   out.mod <- out.mod[,c("Model", "Param", "Species", "q2.5pc", "q50pc", "q97.5pc", "mean", "var", "cov")]
 }
 return(out.mod)
}
