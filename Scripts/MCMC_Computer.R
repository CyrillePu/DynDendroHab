#Ce script permet de déterminer les distributions a posteriori des paramètres
#Il utilise le logiciel stan : http://mc-stan.org/

#Les modèles sont écrit dans un fichier R à part
#Nous prenons en compte ici le modèle de Weibull, le modèle de rayleigh, 
#le modèle exponentiel

#Avant de réaliser les sauvegardes veillez à vérifier la convergence de l'algorithme.

rm(list=ls())
library(rstan)
library(doBy)
library(ggplot2)
load("Results/Names.rdata")

#-----MCMC sampling-----
DMH.data <- readRDS(Names$Data)
#Chemin vers le fichier contenant les fonctions <<model>>
source("Stan_model/Model.R")

#Préparation des variables d'entrée
#Calcul des paramètres des priors
M.lambda <- 100       #valeur moyenne de lambda
V.lambda <- 500       #variance de lambda
r.l <- (M.lambda^2)/V.lambda
mu.l <-M.lambda/V.lambda
M.k <- 2
V.k <- 4
r.k <- (M.k^2)/V.k
mu.k <- M.k/V.k 

Ess <- as.factor(DMH.data$TypeEss)

input_data <-list(nobs = dim(DMH.data)[1],
                  nEss = length(levels(Ess)),
                  Pdmh = DMH.data$Pdmh,
                  DBH  = DMH.data$DBH,                  
                  ess  = as.integer(Ess),                  
                  r_l  = r.l,
                  mu_l = mu.l,
                  r_k  = r.k,
                  mu_k = mu.k)

#modèle de Weibull, code W
for (m in 1 : length(Names$Model)){
  MCMC_out <- stan(model_code = Names$Model[m],    
                   model_name= Names$Model[m],
                   data = input_data,
                   pars=names( get( paste( "init", Names$Model[m], sep="_"))),     
                   chains=3,
                   iter = 10000,
                   warmup=100,
                   init=rep( list( get( paste( "init", Names$Model[m], sep="_"))),
                             times=3))
  
  #Extraction des distributions a posteriori
  PostDistr     <- extract(MCMC_out)
  PostDistr$Ess <- levels(Ess)
  PostDistr$Raw <- MCMC_out
  diff.param <- parameters[!( parameters %in% names( get( paste( "init", 
                                                                 Names$Model[m], 
                                                                 sep="_"))))]
  if(length(diff.param) != 0){
    PostDistr[[diff.param]] <- matrix( rep( get( paste(diff.param, 
                                               Names$Model[m],
                                               sep="_")),
                                    dim(PostDistr[[1]])[1]))
  }
  
  
  
  #Sauvegarde des distribution a posteriori
  saveRDS(PostDistr, file=paste( "Results/", 
                                 paste(Names$Project,
                                       Names$Model[m],
                                       sep="_"),
                                 ".rds",
                                 sep="")) 
}
