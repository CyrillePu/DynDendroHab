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
#setwd("Z:/Private/R/DynDendroHab/v1/Default") #remplir le chemin
load("../Results/Names.rdata")

#Premier graphique sur la distribution des DMHs dans nos données
#----Distribution of DMH bearing tree-------
rm(list=ls()[ls()!="Names"])
data <- readRDS(Names$Data)
data$Classe <-trunc(((data$DBH)+5/2)/5)*5
Pdmh.Ess.Classe <- summaryBy(Pdmh ~ TypeEss + Classe , 
                             data = data, 
                             FUN = function (x) {sum(x)/length(x)},
                             var.names = "Pdmh", 
                             fun.names = "")

TitreEssence <- c("Feuillus", "Résineux")
color <- c("darkgreen", "darkblue")

ggplot(Pdmh.Ess.Classe, aes(x = Classe, y = Pdmh., fill = TypeEss)) + 
  geom_bar(stat = "identity", position= position_dodge()) + 
  theme_bw() +
  xlab("Classe de diamètre") + ylab("Fréquence des arbres \n porteurs de DMHs") +
  scale_fill_manual(name="Essences", labels=TitreEssence, 
                    values=color) +
  theme(legend.position="bottom", legend.direction = "horizontal") +
  guides(fill=guide_legend(title.position="top"))

#-----MCMC sampling-----
rm(list=ls()[ls()!="Names"])
DMH.data <- readRDS(Names$Data)
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
  
#modèle de Weibull, code W
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
saveRDS(MCMC_out, file= paste( "../Results/", 
                               paste(Names$Project,
                                     Names$Model[1],
                                     sep="_"),
                               "MCMC.rds",
                               sep="")) 

#Verification de la convergence
plot(MCMC_out)
print(MCMC_out)

#Extraction des distributions a posteriori
PostDistr     <- extract(MCMC_out)
PostDistr$Ess <- levels(Ess)
PostDistr$summary <- print(MCMC_out)

#Sauvegarde des distribution a posteriori
saveRDS(PostDistr, file=paste( "../Results/", 
                               paste(Names$Project,
                                     Names$Model[1],
                                     sep="_"),
                               ".rds",
                               sep="")) 


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
saveRDS(MCMC_out, file=paste( "../Results/", 
                              paste(Names$Project,
                                    Names$Model[2],
                                    sep="_"),
                              "MCMC.rds",
                              sep="") )

#Verification de la convergence
plot(MCMC_out)
print(MCMC_out)

#Extraction des distributions a posteriori
PostDistr     <- extract(MCMC_out)
PostDistr$k   <- matrix(rep(2, times=dim(PostDistr$lambda)[1]))
PostDistr$Ess <- levels(Ess)
PostDistr$summary <- print(MCMC_out)

#Sauvegarde des distribution a posteriori
saveRDS(PostDistr, file=paste( "../Results/", 
                               paste(Names$Project,
                                     Names$Model[2],
                                     sep="_"),
                               ".rds",
                               sep="") )


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
saveRDS(MCMC_out, file=paste( "../Results/", 
                              paste(Names$Project,
                                    Names$Model[3],
                                    sep="_"),
                              "MCMC.rds",
                              sep="") )

#Verification de la convergence
plot(MCMC_out)
print(MCMC_out)

#Extraction des distributions a posteriori
PostDistr     <- extract(MCMC_out)
PostDistr$k   <- matrix(rep(1, times=dim(PostDistr$lambda)[1]))
PostDistr$Ess <- levels(Ess)
PostDistr$summary <- print(MCMC_out)

#Sauvegarde des distribution a posteriori
saveRDS(PostDistr, file=paste( "../Results/", 
                               paste(Names$Project,
                                     Names$Model[3],
                                     sep="_"),
                               ".rds",
                               sep="") )