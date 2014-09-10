library(doBy)
##########################################################################
###Fonctions du modèle de distribution####
lbar  <- function(k, lambda,D) {1-exp(-(D/lambda)^k)}
h     <- function(k, lambda, D) {(k/lambda)*(D/lambda)^(k-1)}
f     <- function(k, lambda, D) {h(k, lambda, D)*(1-lbar(k, lambda, D))}

##########################################################################
q5pc <- function(x, ...){
  c(quantile(x, probs=0.025, names=F, na.rm = T), 
    quantile(x, probs=0.50, names=F, na.rm = T), 
    quantile(x, probs=0.975, names=F, na.rm = T))
}
##########################################################################
####Simulation à partir des résultats de MCMC####
#Cette fonction utilise les résultats d'une Analyse par MCMC afin de simuler sur un vecteur de diamètre,
#ou sur des données observées, les divers fonctions caractèristiques, et de simuler la présence de DMHs

simulation <- function(PostDist.file, 
                       n.repetitions=1000, 
                       DBH=seq(from=0, to=150, by=1), 
                       Obs.file="") {
  #chargement des sitributions a posteriori
  out <- readRDS(PostDist.file)
  #chargement des données observées ou du vect diamètre
  if(Obs.file != "") {
    data <- readRDS(Obs.file) 
  } else {
    data<-data.frame( DBH = rep(DBH, length(out$Ess)), 
                      TypeEss = sort(rep(out$Ess, length(DBH))),
                      Pdmh = NA)
  }
  
  Simulation  <- data.frame(matrix(nrow=0, ncol=8)) #matrice des simulations
  for (j in 1: n.repetitions) {
    #Tirage des paramètres
    ind     <- sample(x=1:dim(out[[1]])[1], size=1) #indice aléatoire de tirage
    Lambda    <-out$lambda[ind,]
    k         <-out$k[ind,]
    size      <- max(length(Lambda), length(k))           #nombre de combinaison de paramètre
    
    #Tableau des paramètres par essence
    VectLambda    <- rep(Lambda, ifelse(length(Lambda)==size, 1, size))                
    Vectk         <- rep(k, ifelse(length(k)==size, 1, size))                
    MatParam  <- data.frame(TypeEss=out$Ess, 
                            Lambda=as.numeric(VectLambda), 
                            k=as.numeric(Vectk))                     
    
    #Calcul des fonctions charactèristiques, simulation de Pdmh et rangement ds un tableau
    Peuplement          <- data.frame(TypeEss=data$TypeEss, DBH=data$DBH, PdmhObs=data$Pdmh)
    Peuplement$idArbre  <- seq(1, length(data$DBH))
    Peuplement          <- merge(Peuplement, MatParam)
    Peuplement$lbar     <- lbar(Peuplement$k, Peuplement$Lambda, Peuplement$DBH)
    Peuplement$PdmhSim  <- rbinom(n=length(data$DBH), size=1, prob=Peuplement$lbar)
    Peuplement$h        <- h(Peuplement$k, Peuplement$Lambda, Peuplement$DBH)
    Peuplement$f        <- f(Peuplement$k, Peuplement$Lambda, Peuplement$DBH) 
    Peuplement$idSim    <- j
    
    Simulation <- rbind(Simulation, Peuplement)
  }
  return(Simulation)
}

##########################################################################
####Simulation dynamique à partir des résultats de MCMC####
#Cette fonction utilise les résultats d'une Analyse par MCMC afin de simuler sur un vecteur de diamètre,
#ou sur des données observées, la dynamique de création dendromicrohabitats en considérant un 
#accroissement courant constant (fixé par défaut à 0.5cm/an).
#Si des données observées sont renseignées (via Obs.file) cette fonction simule
#le nombre d'arbres porteurs de Ndmh actuellement ce qui permet ainsi d'évaluer le modèle considéré.
#Si un vecteur de diamètre est donné (par défaut 100 arbres de 130 cm), la fonction rend
#en sortie l'évolution du nombre d'arbre portant Ndmh au cours du temps (data.out.type <- "temporal").

simulation.Dyn <- function(PostDist.file, 
                           Obs.file="", 
                           DBH=rep(130, times=100),
                           Acc.DBH=0.5,
                           n.repetitions=100) {
  #chargement des sitributions a posteriori
  out <- readRDS(PostDist.file)
  #chargement des données observées ou du vect diamètre
  if(Obs.file != "") {
    data <- readRDS(Obs.file) 
    data.out.type <- "final"
  } else {
    data<-data.frame( DBH = rep(DBH, length(out$Ess)), 
                      TypeEss = sort(rep(out$Ess, length(DBH))),
                      Pdmh = NA)
    data.out.type <- "temporal"
  }
  
  Resultats   <- list() #liste des résultats de toutes les simulations
  
  
  #Calcul de la Duree de simulation sachant l'accroissement
  Duree       <- floor((max(data$DBH)-7.5)/Acc.DBH) 
  
  #boucle sur le nombre de simulation
  for(j in 1:n.repetitions){
    #mise à 7.5 du DBH des arbres
    DBH<- data$DBH - Duree * Acc.DBH
    Ind <- rep(j, length(DBH))
    
    dmh   <- rep(0, length(DBH)) #vecteur indiquant l'apparition ou non d'un dmh à l'année i 
    Ndmh  <- list(data.frame(DBH=rep(7.5, length(DBH)), dmh=dmh))
    
    #boucle sur la Duree de simulation
    for (i in 2:(Duree+1)) {
      #croissance du peuplement
      DBH     <- DBH+Acc.DBH 
      #Sélection des arbres précomptables (i.e. ayant un DBH >7.5)
      DBHc                <- DBH
      DBHc[DBHc<=7.5]     <- 7.5    #diametre à l'année i pour les arbres non existant
      
      #Tirage des paramètres pour chaque essence
      #Tirage des paramètres
      ind     <- sample(x=1:dim(out[[1]])[1], size=1) #indice aléatoire de tirage
      Lambda    <-out$lambda[ind,]
      k         <-out$k[ind,]
      size      <- max(length(Lambda), length(k))           #nombre de combinaison de paramètre
      VectLambda    <- rep(Lambda, ifelse(length(Lambda)==size, 1, size))                
      Vectk         <- rep(k, ifelse(length(k)==size, 1, size))  
      MatParam  <- data.frame(TypeEss=out$Ess, 
                              Lambda=as.numeric(Lambda), 
                              k=as.numeric(k)) 
      
      #Création du peuplement avec Essence et DBH
      Peuplement          <- data.frame(IdArbre  = seq(1, length(data$DBH)),
                                        DBH      = DBHc,
                                        TypeEss  = as.vector(data$TypeEss))
      Peuplement          <- merge(Peuplement, MatParam, by="TypeEss")
      Peuplement$Proba    <- (Peuplement$DBH/Peuplement$Lambda )^Peuplement$k - ( Ndmh[[i-1]] ["DBH"]/Peuplement$Lambda )^Peuplement$k 
      dmh                 <- dmh + rbinom(n=length(DBH), 
                                          size=1, 
                                          prob=Peuplement$Proba)
      
      #enregistrement des dmhs créés l'année i
      Ndmh[[i]]      <- cbind(Peuplement[,c("IdArbre", "DBH", "TypeEss")], dmh=dmh)
    }
    if (data.out.type == "temporal") {
      Resultats[[j]] <- Ndmh[2:Duree+1]
    }else{
      Resultats[[j]] <- Ndmh[[Duree+1]][,c(1,4)]
    }
  }
  return(Resultats)
}

##########################################################################

