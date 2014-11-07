PredLoss.criteria <- function (Names.files = Names){
  data <- readRDS(file = Names.files$Data)
  CritPred <-data.frame(matrix(nrow=3, ncol=0))
  
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
 }
 return(out.mod)
}
