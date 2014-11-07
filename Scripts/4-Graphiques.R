#Ce script permet de réaliser les graphs aprés la réalisation de simulations
rm(list=ls())
library(plyr)
#setwd("C:/Users/.../Default") #remplir le chemin
setwd("../Default")

TitreEssence <- c("Hêtre", "Sapin")
color <- c("darkgreen", "darkblue")

load("../Results/Names.rdata")
source("../Scripts/fonctionsMod.R")


## ---- DensiteLambda ----

par(mfcol=c(1, 1),  mar=c(4, 4, 0.5, 4), oma=c(0, 0, 0, 0), 
    ps=11, lwd=2, cex=1, cex.lab=1.2)
out <- readRDS(file=paste( "../Results/", 
                           paste(Names$Project,
                                 Names$Model["R"],
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
