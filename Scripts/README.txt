L'analyse des modèle est basée sur des simulations. 
Ceci nécessite de manipuler un grand nombre de données.

Le fichier [MCMC.r] détermine les distributions a posteriori des paramètres à partir
des données et des modèles renseignés dans [../Stan_model/Model.R].
Le fichier [Analyse.r] fabrique les données qui sont ensuite utilisées dans 
le script [Graphiques.r] à partir des distributions a posteriori et des données.

Les fichiers de données générés sont encopmbrants, c'est pourquoi il sont stockés sous 
forme de [.rds]. L'objet [Names.rdata] est la matrice de fabrication des noms des fichiers. Un fichier explicant son fonctionnement est fournit.

Pour analyser un jeu de données :
- Tout d'abord disposer la base à analyser dans le dossier [Data]
- Ensuite utiliser le script [Data.R] pour généréer un fichier [.rds] au format
  utilisable : 
    data.frame( Foret    = , 
                Placette = ,
                IdArbre  = ,
                TypeEss  = ,
                DBH      = ,
                Ndmh     = ,
                Pdmh     = )
- Si nécessaire, dans le même script, changer le nom du projet dans [Names].
- Lancer le script.

- Ouvrir le script [MCMC], définir l'espace de travail (vers le dossier Default).
- Lancer petit à petit le script sans effectuer aucune modification, bien vérifier la 
  convergence de l'algorithme avant de sauvegarder les distributions a posteriori.

- Ouvrir le script [Analyse], vérifier le chemin de l'espace de travail.
- Lancer le script, normalement aucune modification n'est nécessaire.

- Ouvrir le script [Graphique], vérifier le chemin de l'espace de travail.
- Lancer le script sans effectuer de modifications.
