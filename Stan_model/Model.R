#définitions des modèles pour Stan

#Weibull k et lambda
model_Wkl <- "
//Définition des données entrées
data {
int<lower=1> nobs;
int<lower=1> nEss;
int<lower=0, upper=1> Pdmh[nobs];
real DBH[nobs];
int<lower=1,upper=nEss> ess[nobs];
real r_l;
real mu_l;
real r_k;
real mu_k;
}

//Définition des paramètres
parameters {
real<lower=0> lambda[nEss];
real<lower=0> k[nEss];
}

//définition du modèle
model {
// Priors sur les paramètres
for (i in 1:nEss) {
lambda[i] ~ gamma(r_l, mu_l);
k[i] ~ gamma(r_k, mu_k);
}  
// vraissemblance, p[i] correspond à 1-li=F
for (i in 1:nobs)
Pdmh[i] ~ bernoulli( 1 - exp(-pow( DBH[i] / lambda[ess[i]] , k[ess[i]])) );
}"


#Rayleigh
model_R <- "
//Définition des données entrées
  data {
    int<lower=1> nobs;
    int<lower=1> nEss;
    int<lower=0, upper=1> Pdmh[nobs];
    real DBH[nobs];
    int<lower=1,upper=nEss> ess[nobs];
    real r_l;
    real mu_l;
  }

  transformed data {
    real<lower=0> k[nEss];
    for(i in 1:nEss)
      k[i] <- 2;
  } 

//Définition des paramètres
  parameters {
    real<lower=0> lambda[nEss];
  }

//définition du modèle
  model {
    // Priors sur les paramètres
    for (i in 1:nEss) 
      lambda[i] ~ gamma(r_l, mu_l);
      
    // vraissemblance, p[i] correspond à 1-li=F
    for (i in 1:nobs)
      Pdmh[i] ~ bernoulli( 1 - exp(-pow( DBH[i] / lambda[ess[i]] , k[ess[i]])) );
  }"


#Exponentiel
model_E <- "
//Définition des données entrées
  data {
    int<lower=1> nobs;
    int<lower=1> nEss;
    int<lower=0, upper=1> Pdmh[nobs];
    real DBH[nobs];
    int<lower=1,upper=nEss> ess[nobs];
    real r_l;
    real mu_l;
  }
  
  transformed data {
  real<lower=0> k[nEss];
  for(i in 1:nEss)
    k[i] <- 1;
  } 

//Définition des paramètres
  parameters {
    real<lower=0> lambda[nEss];
  }

//définition du modèle
model {
  // Priors sur les paramètres
  for (i in 1:nEss){
    lambda[i] ~ gamma(r_l, mu_l);
   }
  // vraissemblance, p[i] correspond à 1-li=F
  for (i in 1:nobs)
    Pdmh[i] ~ bernoulli( 1 - exp(-pow( DBH[i] / lambda[ess[i]] , k[ess[i]])) );
  }"


#Valeurs d'initialisation des chaines
init_Wkl <- list(lambda=c(100, 100), k=c(2, 2))
init_R <- list(lambda=c(100, 100))
init_E <- list(lambda=c(100, 100))


