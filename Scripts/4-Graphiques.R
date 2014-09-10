#Ce script permet de réaliser les graphs aprés la réalisation de simulations
rm(list=ls())
library(plyr)
setwd("C:/Users/.../Default") #remplir le chemin

TitreEssence <- c("Feuillus", "Résineux")
color <- c("darkgreen", "darkblue")

load("../Results/Names.rdata")
source("../Scripts/fonctionsMod.R")


## ---- DensiteLambda ----

par(mfcol=c(1, 1),  mar=c(4, 4, 0.5, 4), oma=c(0, 0, 0, 0), 
    ps=11, lwd=2, cex=1, cex.lab=1.2)
out <- readRDS(file=paste( "../Results/", 
                           paste(Names$Project,
                                 Names$Model[2],
                                 sep="_"),
                           ".rds",
                           sep=""))
plot(density(out$lambda[,1]),
     xlim=c(min(out$lambda)*0.9,
            max(out$lambda)*1.1),
     ylim=c(0,max(density(out$lambda[,1])$y, 
                  density(out$lambda[,2])$y)*1.1),
     col=color[1],
     main="",
     ylab="Densité",
     xlab="$\\lambda$")
lines(density(out$lambda[,2]),
      col=color[2])
abline(v=out$summary[c(1, 2),c("2.5%", "97.5%")], lty=2)

legend(120, 0.1,
       col=color,
       text.col=color,
       legend= TitreEssence, 
       lwd=2,
       lty=1,
       title="",
       bty = "n")

#-----Surface de crédibilité pour Wkl DBH+Essence-----
out <- readRDS(file=paste( "../Results/", 
                           paste(Names$Project,
                                 Names$Model[1],
                                 sep="_"),
                           ".rds",
                           sep=""))

ylim <- c(min(quantile(out$lambda[,1], probs=0.025),
              quantile(out$lambda[,2], probs=0.025)),
          max(quantile(out$lambda[,1], probs=0.95),
              quantile(out$lambda[,2], probs=0.95)))

xlim <- c(min(quantile(out$k[,1], probs=0.025),
              quantile(out$k[,2], probs=0.025)),
          max(quantile(out$k[,1], probs=0.975),
              quantile(out$k[,2], probs=0.975)))

#Calcul de la matrice de densité
densité.Feui  <- kde2d (x=out$k[, 1], y=out$lambda[, 1], n=100)
densité.Res   <- kde2d (x=out$k[, 2], y=out$lambda[, 2], n=100)

par(mfcol=c(1,1), mar=c(4, 4, 0.5, 4), oma=c(0, 0, 0, 0),
    ps=11, cex=1, cex.lab=1.2,
    lwd=2)

contour(x=densité.Feui$x, z=densité.Feui$z, y=densité.Feui$y,
        nlevels= 5, levels= c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5),
        ylim = ylim, xlim = xlim,
        xlab="k", ylab="$\\lambda$", labcex=1, 
        col=color[1])

par(new=TRUE)
contour(x=densité.Res$x, z=densité.Res$z, y=densité.Res$y,
        nlevels= 5, levels= c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5),
        ylim = ylim, xlim = xlim,
        xlab="", ylab="", col=color[2])

legend(1.5, 150,
       col=color,
       text.col=color,
       legend= TitreEssence , 
       lwd=1.5,
       lty=1,
       title="",
       bty = "n")

## ---- IPCompModVsData ----
#paramètres graphiques
par(mfcol=c(2, 3),  mar=c(1, 0.5, 0, 0), oma=c(2, 3, 2, 3), 
    lwd=2.5, cex.main=1, cex.axis=1, cex=1,
    ps=11)

interv <-10
#Préparation des données terrain

data <- readRDS(Names$Data)
#Calcul des classes
data$Classe   <- trunc(((data$DBH)+interv/2)/interv)*interv
#tableau d'analyses
ResultsD              <- summaryBy(Pdmh ~ TypeEss + Classe, data=data, FUN=c(mean, var, length))
colnames(ResultsD)    <- c("Ess", "Classe", "Fdmh", "Fdmhvar", "Narbres")
ResultsD$Interv       <- 1.96*sqrt(ResultsD$Fdmh * (1-ResultsD$Fdmh)/ResultsD$Narbres)


#boucle sur les modèles
for (i in 1:length(Names$Model)){
  #choix des données sorties MCMC
  SimulObs <- readRDS(file=paste( "../Results/", 
                                  paste(Names$Project,
                                        Names$Model[i],
                                        Names$Type[2],
                                        Names$Simulation[2],
                                        sep="_"),
                                  ".rds",
                                  sep=""))
  out <- readRDS(file=paste( "../Results/", 
                             paste(Names$Project,
                                   Names$Model[i],
                                   sep="_"),
                             ".rds",
                             sep=""))
  Nom <- Names$Model[i]
  Essence <- out$Ess
  
  SimulObs$Classe   <- trunc(((SimulObs$DBH)+interv/2)/interv)*interv
  
  
  #préparation des données resimulée
  FdmhSimulations <- summaryBy(PdmhSim ~ idSim + TypeEss + Classe, 
                               data=SimulObs, 
                               FUN = c(mean))
  colnames(FdmhSimulations) <- c("idSim", "TypeEss", "Classe", "Fdmh")
  
  FdmhResults <- summaryBy(Fdmh ~ TypeEss + Classe, 
                           data=FdmhSimulations, 
                           FUN = c(q5pc))
  colnames(FdmhResults) <- c("Ess", "Classe", "Fdmh2.5pc", "Fdmh50pc", "Fdmh97.5pc")
  
  #boucle sur les essences
  for (j in seq(along=Essence)){
    FdmhResultssub <- subset(FdmhResults, subset = (Ess==Essence[j]))
    ResultsDsub <- subset(ResultsD, subset = (Ess==Essence[j]))
    
    #Sortie graphiques
    plot(ResultsDsub$Classe, ResultsDsub$Fdmh, 
         pch=6, 
         cex=1,
         main="",
         xlab="",
         ylab="", 
         yaxt="n",
         xaxt="n")
    lines(FdmhResultssub$Classe-interv/2, FdmhResultssub$Fdmh50pc, type="s", col=color[j])
    lines(FdmhResultssub$Classe-interv/2, FdmhResultssub$Fdmh2.5pc, type="s", col=color[j], lty=2 )
    lines(FdmhResultssub$Classe-interv/2, FdmhResultssub$Fdmh97.5pc, type="s", col=color[j], lty=2 )
    
    if(j==1) {
      mtext(Nom, 
            cex=1,
            line=0.5,
            ps=11)
    }
    if(i==1) {
      mtext(text=TitreEssence[j],
            outer=TRUE, 
            line=1.2,
            adj= 1-((2*j)-1)/(2*length(Essence)),
            side=2,
            cex=1,
            font=1)
      axis(side=2)
    }
    if(j==2) {
      axis(side=1)
    }
  }
}

title(xlab="Diamètre",
      line=0.7,
      outer=TRUE)


## ---- CompModIP Wkl ----
#vecteur des diamètres.
diam <- seq(from=0, 150, by=1)

#liste des limites de graphiques en y pour chaque fonction
limites = list(c(0,1), c(0, 0.07), c(0, 0.02))

Simulations <- readRDS(file=paste( "../Results/", 
                                   paste(Names$Project,
                                         Names$Model[1],
                                         Names$Type[2],
                                         Names$Simulation[1],
                                         sep="_"),
                                   ".rds",
                                   sep="") )
out <- readRDS(file=paste( "../Results/", 
                           paste(Names$Project,
                                 Names$Model[1],
                                 sep="_"),
                           ".rds",
                           sep=""))

#Matrice de soustraitement
SubTraitement <- matrix(data=c("Simlbar", "Simh", "Simf",
                               "F(D)", "h(D)", "f(D)"),
                        nrow=3, ncol=2)
Fonctions <- list()

par(mfcol=c(3, 2),  mar=c(1, 0.5, 0, 0), oma=c(3, 3, 3, 3), 
    ps=11, cex=1, lwd=2, cex.axis=0.65)

Nom <- "Weibull"
Essence <- out$Ess

#boucle sur les essences
for (j in seq(along=Essence)){
  #Préparation des tableaux d'analyse
  Fonctions$Simlbar <- summaryBy(lbar~ DBH, 
                                 data=subset(Simulations, TypeEss==Essence[j]), 
                                 FUN = q5pc, 
                                 var.names="", 
                                 fun.names=c("q2.5", "q50", "q97.5" ))
  Fonctions$Simh    <- summaryBy(h~ DBH, 
                                 data=subset(Simulations, TypeEss==Essence[j]), 
                                 FUN = q5pc, 
                                 var.names="", 
                                 fun.names=c("q2.5", "q50", "q97.5" ))
  Fonctions$Simf    <- summaryBy(f~ DBH, 
                                 data=subset(Simulations, TypeEss==Essence[j]), 
                                 FUN = q5pc, 
                                 var.names="", 
                                 fun.names=c("q2.5", "q50", "q97.5" ))
  
  #Sortie graphique
  #boucle sur les fonctions
  for (l in seq(along=SubTraitement[,1])){
    x     <-unlist(Fonctions[[SubTraitement[l]]]["DBH"])
    y97.5 <-unlist(Fonctions[[SubTraitement[l]]][".q97.5"])
    y50   <-unlist(Fonctions[[SubTraitement[l]]][".q50"])
    y2.5  <-unlist(Fonctions[[SubTraitement[l]]][".q2.5"])
    
    plot(c(8:150,8:150), c(y97.5[8:150], y2.5[8:150]),
         type="l",
         xlab = "", 
         ylab = "",
         lty=0,
         xaxt=ifelse(l==3, yes="s", no="n"),
         yaxt=ifelse(j==1, yes="s", no="n"),
         ylim=limites[[l]],
         mgp=c(3, 1,0))
    lines(x, y97.5, col=color[j], lty=2)
    lines(x, y50, col=color[j])
    lines(x, y2.5, col=color[j], lty=2)
  }
}

mtext(TitreEssence, 
      outer=T,
      line=0.5,
      side=c(3, 3),
      adj=c(0.20, 0.80),
      col=color,
      cex=0.9)
mtext(c("Diamètre", "F(D)", "h(D)", "f(D)"), 
      line=1.5,
      side=c(1, 2, 2, 2),
      adj=c(0.5, 0.85, 0.5, 0.15) ,
      outer=T,
      cex=0.9)

#----Comp_observations_Dynamique----
rm(list=ls()[ls()!="Names"])
source("../Scripts/fonctionsMod.R")

Resultats <- readRDS(file=paste( "../Results/", 
                                 paste(Names$Project,
                                       Names$Model[1],
                                       Names$Type[1],
                                       Names$Simulation[2],
                                       sep="_"),
                                 ".rds",
                                 sep=""))
data <- readRDS(file=Names$Data)

ylim <- c(0, 1)

#Decomposition par essence
indRes    <- data[which(data$TypeEss=="Res"),"IdArbre"]
indFeui   <- data[which(data$TypeEss=="Feui"),"IdArbre"]
listRes   <- lapply(Resultats, FUN=function(x) x[which(x$IdArbre%in%indRes),])
listFeui  <- lapply(Resultats, FUN=function(x) x[which(x$IdArbre%in%indFeui),])
dataRes   <- data[indRes,]
dataFeui  <- data[indFeui,]

#Matrice des traitements
choix <- matrix(data=c("Resultats", "listRes", "listFeui",
                       "data", "dataRes", "dataFeui"),
                nrow=3, ncol=2)
Nom <- c("Toutes essences",
         "Résineux",
         "Feuillus")
#Préambule Tikz

par(mfcol=c(1, 3),  mar=c(1, 2, 1.5, 0), oma=c(2, 1.5, 0.5, 1), cex=0.85,lwd=2)

for (i in 1:dim(choix)[1]) {
  #Calcul du nombre d'arbres portant N dmhs dans chaque simulation 
  NarbreByNdmh <-lapply(get(choix[i,1]), 
                        FUN= function(x) summaryBy(dmh ~ dmh, data=x, FUN=length))
  
  #Vecteur du nombre de dmh par arbre
  Ndmh <- data.frame(Ndmh=seq(from=0, 
                              to=max(sapply(NarbreByNdmh, 
                                            FUN=function(x, colonne) max(x[,colonne]), 
                                            colonne=1))))
  
  #Calcul de la proportion des arbres ayant Ndmh
  Synthèse <- lapply(NarbreByNdmh, FUN= function(x) {
    merge(x=Ndmh, y= x, by.x="Ndmh", by.y="dmh", all.x=TRUE)
  })
  Synthèse <- do.call("rbind", Synthèse)
  Synthèse[is.na(Synthèse)] <- 0
  Synthèse$Proportion <- Synthèse[,2]/dim(get(choix[i,2]))[1]
  
  #Calcul des quantiles
  Synthèse <- summaryBy(data=Synthèse, Proportion ~ Ndmh, FUN=q5pc)
  colnames(Synthèse) <- c("Ndmh", "q2.5pc", "q50pc", "q97.5pc")
  
  #Synthése sur les données observées
  OutData <-summaryBy(DBH ~ Ndmh, data=get(choix[i,2]), FUN=function (x){length(x)/dim(get(choix[i,2]))[1]})
  colnames(OutData)<- c("Ndmh", "Prev")
  
  #Sortie graphique
  #Simulations
  plot(Synthèse[,1]-0.5, Synthèse[,4], type="s", col="red", lty=2, 
       ylab="", 
       xlab="",
       xlim=c(0, max(Synthèse$Ndmh)*0.6),
       ylim=ylim)
  lines(Synthèse[,1]-0.5, Synthèse[,3], type="s", col="red" )
  lines(Synthèse[,1]-0.5, Synthèse[,2], type="s", col="red", lty=2 )
  #Données
  lines(OutData[,1]-0.5, OutData[,2], type="s" )
  
  title(main=Nom[i], cex.main=0.9, font=1, line=0.5)
}
mtext("Nombre de DMHs par arbre", side=1, line=1, outer=TRUE, cex=0.85)
mtext("Fréquence", side=2, outer=TRUE, cex=0.85)


#-----Simulation cohorte----
rm(list=ls()[ls()!="Names"])
source("../Scripts/fonctionsMod.R")

#Nombre maxi de DMH pris en compte
limNdmh <-5

CohorteNdmh <-list()
indice <-1

#Boucle sur les traitements
for(t in 1:length(Names$Model)){
  Resultats <- readRDS( file=paste( "../Results/", 
                                    paste(Names$Project,
                                          Names$Model[t],
                                          Names$Type[1],
                                          Names$Simulation[1],
                                          sep="_"),
                                    ".rds",
                                    sep=""))
  
  out <- readRDS(file=paste( "../Results/", 
                             paste(Names$Project,
                                   Names$Model[t],
                                   sep="_"),
                             ".rds",
                             sep=""))
  
  #fonctions extraction
  lextract <- function(x) { #dans cette fonction value doit être définies en dehors
    lapply(x, FUN= subset, subset= TypeEss == value)
  }
  
  
  #sélection du groupe d'essence
  for(e in 1:length(out$Ess)){
    value <- out$Ess[e]
    listArbre <- lapply(Resultats, FUN = lextract)
    
    #Calcul du nombre d'arbre à N dmhs à chaque pas pour chaque simu
    lsummary <- function(x) {
      lapply(x, FUN=function(x) summaryBy(dmh ~ dmh, data=x, FUN=length))
    }
    NarbreNdmh <- lapply(listArbre, FUN= lsummary)
    
    rm(listArbre)#Permet d'augmenter la vitesse de calcul
    
    #transformation de la structure de la liste :
    # NarbreNdmh[[simulation]][[temps]] -> NarbreNdmhP[[temps]][[simulation]]
    NarbreNdmhP<- list() 
    for(i in 1: length(NarbreNdmh[[1]])) NarbreNdmhP[[i]] <- lapply(NarbreNdmh, "[[", i)
    
    #creation d'une liste [[temps]] ayant pour chaque pas de temps le vecteur Ndmh max
    ListNdmh <- lapply(NarbreNdmhP, FUN= function(x){
      Ndmh <- data.frame(Ndmh=seq(from=0, 
                                  to=max(sapply(x, 
                                                FUN=function(x, colonne) max(x[,colonne]), 
                                                colonne="dmh"))))
      return(Ndmh)
    })
    
    NarbreNdmhPm <- list()
    #harmonisation des ndmhs par liste
    for(i in 1:length(ListNdmh)){
      NarbreNdmhPm[[i]] <-lapply(NarbreNdmhP[[i]], FUN=function(z) {
        merge(x= ListNdmh[[i]], y= z, by.x="Ndmh", by.y="dmh", all.x=TRUE)
      }
      )
    }
    rm(NarbreNdmhP)
    
    #fusion des listes par pas de temps
    NarbreNdmhTemps <-lapply(NarbreNdmhPm, FUN=function(x){
      do.call("rbind", x)
    })
    rm(NarbreNdmhPm)
    
    
    #Création du tableau Age / Ndmhs / Fréquence_Arbre
    names(NarbreNdmhTemps)<- as.character(1:length(NarbreNdmhTemps))
    NarbreNdmh <- ldply(.data=NarbreNdmhTemps, 
                        .fun=function(x){
                          summaryBy(dmh.length~Ndmh, data=x, FUN=q5pc)
                        },
                        .id = "age")
    rm(NarbreNdmhTemps)
    NarbreNdmh[,3:5] <- NarbreNdmh[,3:5]/NarbreNdmh[1, 4]
    NarbreNdmh[,1] <- as.numeric(NarbreNdmh[,1])
    colnames(NarbreNdmh) <- c("Age", "Ndmh", "q2.5pc", "q50pc", "q97.5pc")
    
    CohorteNdmh[[indice]] <- NarbreNdmh
    rm(NarbreNdmh)
    names(CohorteNdmh)[indice] <- paste(Names$Model[t], out$Ess[e], sep="_")
    indice<-indice+1
  }
  rm(Resultats)
}


#Sorties graphiques
for (m in 1:length(CohorteNdmh)){
  NarbreNdmh <- CohorteNdmh[[m]]
  par(mfcol=c(1, 1), mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0), lwd=2)
  par(cex.lab=1.5, cex.main=1.5, cex.axis=1.4)
  
  EvolutionT <- subset(NarbreNdmh, Ndmh==0, select = c( Age, q2.5pc, q50pc, q97.5pc))
  
  #sorties graphiques
  plot(EvolutionT[,1], EvolutionT[,"q50pc"], 
       type="l",
       ylim=c(0, 1),
       xlim=c(0, 300),
       xlab="",
       ylab="")
  lines(EvolutionT[,1], EvolutionT[,"q2.5pc"], lty=2)
  lines(EvolutionT[,1], EvolutionT[,"q97.5pc"], lty=2)
  
  for (i in 1:limNdmh) {
    EvolutionT <- subset(NarbreNdmh, Ndmh==i, select = c( Age, q2.5pc, q50pc, q97.5pc))
    lines(EvolutionT[,1], EvolutionT[,"q50pc"], 
          type="l", 
          col=i+1,
          ylim=c(0, 1),
          xlim=c(0, 300),
          xlab="",
          ylab="")
    lines(EvolutionT[,1], EvolutionT[,"q2.5pc"], lty=2, col=i+1)
    lines(EvolutionT[,1], EvolutionT[,"q97.5pc"], lty=2, col=i+1)
  }
  
  legend(225, 1,
         col=seq(1, limNdmh+1, 1), 
         legend=seq(0, limNdmh, 1), 
         lty=rep(1, limNdmh+1),
         title="Ndmh",
         cex=1.5,
         bty = "n")
  title(main= paste(names(CohorteNdmh)[m], "arbres porteurs de N DMHs", sep="\n"))
  title(xlab= "Temps", line=2)
  title(ylab= "Fréquence", line=2)
}
