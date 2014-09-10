
Dans les scripts, la liste Names contrôle le nom des fichiers de résultats.
La définition de cette liste est

Names <-list(Project = "Cavity",
             Model = c("W", "R", "E"),
             Type = c("D", "M"),
             Simulation = c("SV", "SO"))

L'élément [Project] est le nom du projet, il est conseillé de choisir [DMH] si l'on regroupe
plusieurs types de DMHs. Si l'on s'intéresse à un type en particulier on peut indiquer le nom
de ce type : [cavity] ou [crack] ou [fungus] ...

L'élément [Model] correspond au code des modèles considérés. Nous proposons :
  [W] pour le modèle de Weibull
  [R] pour le modèle de Rayleigh
  [E] pour le modèle Exponentiel.

L'élément [Type] correspond au type de simulation. Nous considérons :
  [M] pour des simulation utilisant le modèle standard (les fonctions h(D), F(D) et f(D))
  [D] pour des simulation utilisant le modèle dynamique et permettant d'utiliser la variable 
      temps

L'élément [Simulation] correspond aux données utilisées pour réaliser la simulation : 
  [SV] correspond à une simulation sur un vecteur de diamètre
  [SO] correspond à une simulation sur des arbres observés.

Ainsi le fichier : "DMH_W_M_SV.rds" correspond aux données simulées à partir d'un vecteur et de
la distribution a posteriori des paramètres du modèle de Weibull en utilisant les fonctions du
modèle standard dans le projet DMH.

NB : Les fichiers du type "Project_Model_[MCMC].rds" correspondent aux résultats bruts de la fonction stan.