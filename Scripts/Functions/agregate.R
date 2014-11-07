library(plyr)

agregate.LifePredic.tree <- function(Names.files = Names) {
  
  out.agreg <- data.frame(matrix(nrow = 0, ncol = 7))
  for(t in 1:length(Names.files$Model)){
    out.sim <- readRDS( file=paste( "Results/", 
                                    paste(Names.files$Project,
                                          Names.files$Model[t],
                                          Names.files$Type[1],
                                          Names.files$Simulation[1],
                                          sep="_"),
                                    ".rds",
                                    sep=""))
    
    NarbreNdmhList<- list() 
    for(i in 1: length(out.sim[[1]])) NarbreNdmhList[[i]] <- lapply(out.sim, "[[", i)
    
    NarbreNdmhTemps <- lapply(NarbreNdmhList, FUN=function(x){
      do.call("rbind", x)
    })
    
    NarbreNdmh<-ldply(.data=NarbreNdmhTemps, 
                      .fun=function(x){
                        summaryBy(dmh.length ~ DBH + TypeEss + dmh, data=x, FUN=q5pc)
                      })
    NarbreNdmh[,4:6] <- NarbreNdmh[,4:6]/NarbreNdmh[1,4]
    colnames(NarbreNdmh) <- c("DBH","Species" ,"Ndmh", "low", "median", "high")
    NarbreNdmh$Ndmh <- as.character(NarbreNdmh$Ndmh)
    
    NarbreNdmh$Model <- Names.files$Model[t]
    out.agreg <- rbind(out.agreg, NarbreNdmh)
  }
  return(out.agreg)
} 

################################################################################################

agregate.LifePredic.stand <- function (Names.files = Names) {
  data <- readRDS(file=Names$Data)
  
  #Synthése sur les données observées
  N.treepNdmhspec <-summaryBy(DBH ~ TypeEss + Ndmh, data=data, 
                              FUN=length)
  N.treepspec <- summaryBy(formula = DBH ~ TypeEss, data = data,
                           FUN=length)
  synth.data <- merge(x = N.treepNdmhspec, y = N.treepspec, by = "TypeEss")
  synth.data$Occurrence <- synth.data$DBH.length.x/synth.data$DBH.length.y
  synth.data <- synth.data[,-c(3, 4)]
  colnames(synth.data)<- c("Species", "Ndmh", "Occurrence")
  
  synth.mod <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (m in 1:length(Names$Model)){
    out.sim <- readRDS(file=paste( "Results/", 
                                   paste(Names$Project,
                                         Names$Model[m],
                                         Names$Type[1],
                                         Names$Simulation[2],
                                         sep="_"),
                                   ".rds",
                                   sep=""))
    
    synth.spec <- data.frame(matrix(nrow = 0, ncol = 5))
    
    for (i in 1:length(levels(data$TypeEss))) {
      ind    <- data[which(data$TypeEss==levels(data$TypeEss)[i]),
                     "IdArbre"]
      out.sim.ess  <- lapply(out.sim, FUN=function(x) x[ind,])
      #Calcul du nombre d'arbres portant N dmhs dans chaque simulation 
      NarbreByNdmh <-lapply(out.sim.ess, 
                            FUN= function(x) summaryBy(dmh ~ dmh, data=x, FUN=length))
      
      #Vecteur du nombre de dmh par arbre
      Ndmh <- data.frame(Ndmh=seq(from=0, 
                                  to=max(sapply(NarbreByNdmh, 
                                                FUN=function(x, colonne) max(x[,colonne]), 
                                                colonne=1))))
      
      #Calcul de la proportion des arbres ayant Ndmh
      Synthese <- lapply(NarbreByNdmh, FUN= function(x) {
        merge(x=Ndmh, y= x, by.x="Ndmh", by.y="dmh", all.x=TRUE)
      })
      Synthese <- do.call("rbind", Synthese)
      Synthese[is.na(Synthese)] <- 0
      Synthese$Proportion <- Synthese[,2]/length(ind)
      
      #Calcul des quantiles
      Synthese <- summaryBy(data=Synthese, Proportion ~ Ndmh, FUN=q5pc)
      colnames(Synthese) <- c("Ndmh", "low", "median", "high")
      Synthese$Species <- levels(data$TypeEss)[i]
      synth.spec <- rbind(synth.spec, Synthese)
    }
    synth.spec$Model <- Names$Model[m]
    synth.mod <- rbind(synth.mod, synth.spec)
  }
  
  synth.end <- merge(synth.mod, synth.data, 
                     by =c("Species", "Ndmh"), all=TRUE)
  
  return(synth.end)
}
#################################################################

agregate.Predic.functions <- function( Names.files = Names ) {
  
  out.mod <- data.frame(matrix(nrow=0, ncol=7))
  for (m in 1:length(Names.files$Model)){
    in.sim <- readRDS(file=paste( "Results/", 
                                  paste(Names.files$Project,
                                        Names.files$Model[m],
                                        Names.files$Type[2],
                                        Names.files$Simulation[1],
                                        sep="_"),
                                  ".rds",
                                  sep="") )
    in.MCMC <- readRDS(file=paste( "Results/", 
                                   paste(Names.files$Project,
                                         Names.files$Model[m],
                                         sep="_"),
                                   ".rds",
                                   sep=""))
    out.spec <- data.frame(matrix(nrow=0, ncol=6))
    Species <- in.MCMC$Ess
    
    #boucle sur les Speciess
    for (j in seq(along=Species)){
      #Préparation des tableaux d'analyse
      out.simlbar <- summaryBy(lbar~ DBH, 
                               data=subset(in.sim, TypeEss==Species[j]), 
                               FUN = q5pc, 
                               var.names="")
      out.simlbar$func <- "F(D)"
      out.simh    <- summaryBy(h~ DBH, 
                               data=subset(in.sim, TypeEss==Species[j]), 
                               FUN = q5pc, 
                               var.names="")
      out.simh$func <- "h(D)"
      out.simf    <- summaryBy(f~ DBH, 
                               data=subset(in.sim, TypeEss==Species[j]), 
                               FUN = q5pc, 
                               var.names="")
      out.simf$func <- "f(D)"
      out.func <- rbind(out.simlbar, out.simf, out.simh)
      colnames(out.func) <- c("DBH", "low", "median", "high", "Functions")
      out.func$Species <- Species[j]
      out.spec <- rbind(out.spec, out.func)
    }
    out.spec$Model <- Names.files$Model[m]
    out.mod <- rbind(out.mod, out.spec)
  }
  return(out.mod)
}

###########################################################

agregate.Predic.occurrence.stand <- function( Names.files = Names, interv = 10) {
  
  data <- readRDS(Names.files$Data)
  #Calcul des classes
  data$Classe   <- trunc(((data$DBH)+interv/2)/interv)*interv
  #tableau d'analyses
  out.data             <- summaryBy(Pdmh ~ TypeEss + Classe, data=data, FUN=c(mean))
  colnames(out.data)    <- c("Species", "Class", "Occurrence")
  #ResultsD              <- summaryBy(Pdmh ~ TypeEss + Classe, data=data, FUN=c(mean, var, length))
  #colnames(ResultsD)    <- c("Species", "Class", "Fdmh", "Fdmhvar", "Narbres")
  #ResultsD$Interv       <- 1.96*sqrt(ResultsD$Fdmh * (1-ResultsD$Fdmh)/ResultsD$Narbres)
  
  out.mod <- data.frame(matrix(nrow=0, ncol=5))
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
    in.MCMC <- readRDS(file=paste( "Results/", 
                                   paste(Names.files$Project,
                                         Names.files$Model[i],
                                         sep="_"),
                                   ".rds",
                                   sep=""))
    Nom <- Names.files$Model[i]
    Species <- in.MCMC$Ess
    
    in.sim$Classe   <- trunc(((in.sim$DBH)+interv/2)/interv)*interv
    
    #préparation des données resimulée
    FdmhSimulations <- summaryBy(PdmhSim ~ idSim + TypeEss + Classe, 
                                 data=in.sim, 
                                 FUN = c(mean))
    colnames(FdmhSimulations) <- c("idSim", "Species", "Class", "Fdmh")
    
    FdmhResults <- summaryBy(Fdmh ~ Species + Class, 
                             data=FdmhSimulations, 
                             FUN = c(q5pc))
    colnames(FdmhResults) <- c("Species", "Class", "low", "median", "high")
    FdmhResults$Model <- Names.files$Model[i]
    out.mod <- rbind(out.mod, FdmhResults)
  }
  out.mod <- merge(out.mod, out.data,
                   by=c("Species", "Class"), all=TRUE)
  return(out.mod)
}




