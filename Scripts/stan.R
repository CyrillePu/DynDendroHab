#Ce script permet de déterminer les distributions a posteriori des paramètres
#Il utilise le logiciel stan : http://mc-stan.org/

#Les modèles sont écrit dans un fichier R à part
#Nous prenons en compte ici le modèle de Weibull, le modèle de rayleigh, 
#le modèle exponentiel

rm(list=ls())
library(rstan)
setwd("C:/Users/cyrille.pupin/Dropbox/R github/Default")
#Choisir les données :
DMH.data <- readRDS(file="../Data/DMH.rds")

#Chemin vers le fichier contenant les fonctions <<model>>
source("../Stan_model/Model.R")

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

#modèle de Weibull
MCMC_out <- stan(model_code = model_Wkl,    
                 model_name="Wkl_DBH+Ess",
                 data = input_data,
                 pars=c("lambda", "k"),     
                 chains=3,
                 iter = 10000,
                 warmup=100,
                 init=rep(list(init_Wkl),
                          times=3))

#Sauvegarde du fichier brut
saveRDS(MCMC_out, file="../Results/MCMC_Wkl.rds") 

#Verification de la convergence
plot(MCMC_out)
print(MCMC_out)

#Extraction des distributions a posteriori
PostDistr     <- extract(MCMC_out)
PostDistr$Ess <- levels(Ess)

#Sauvegarde des distribution a posteriori
saveRDS(PostDistr, file="../Results/Post_Wkl.rds")



#modèle de Rayleigh
MCMC_out <- stan(model_code = model_R,    
                 model_name="R_DBH+Ess",
                 data = input_data,
                 pars=c("lambda"),     
                 chains=3,
                 iter = 10000,
                 warmup=100,
                 init=rep(list(init_R),
                               times=3))

#Sauvegarde du fichier brut
saveRDS(MCMC_out, file="../Results/MCMC_R.rds") 

#Verification de la convergence
plot(MCMC_out)
print(MCMC_out)

#Extraction des distributions a posteriori
PostDistr     <- extract(MCMC_out)
PostDistr$k   <- matrix(rep(2, times=dim(PostDistr$lambda)[1]))
PostDistr$Ess <- levels(Ess)

#Sauvegarde des distribution a posteriori
saveRDS(PostDistr, file="../Results/Post_R.rds")


#modèle Exponentiel
MCMC_out <- stan(model_code = model_E,    
                 model_name="E_DBH+Ess",
                 data = input_data,
                 pars=c("lambda"),     
                 chains=3,
                 iter = 10000,
                 warmup=100,
                 init=rep(list(init_E),
                               times=3))

#Sauvegarde du fichier brut
saveRDS(MCMC_out, file="../Results/MCMC_E.rds") 

#Verification de la convergence
plot(MCMC_out)
print(MCMC_out)

#Extraction des distributions a posteriori
PostDistr     <- extract(MCMC_out)
PostDistr$k   <- matrix(rep(1, times=dim(PostDistr$lambda)[1]))
PostDistr$Ess <- levels(Ess)

#Sauvegarde des distribution a posteriori
saveRDS(PostDistr, file="../Results/Post_E.rds")
