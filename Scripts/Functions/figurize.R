plot.LifePredict.PNdmh_DBH.tree <- function(Names.files = Names, limNdmh = 5){
  
  out.agreg <- readRDS(file=paste( "Results/agregate_", 
                                   paste(Names.files$Project,
                                         Names.files$Type[1],
                                         Names.files$Simulation[1],
                                         sep="_"),
                                   ".rds",
                                   sep=""))
  
  print(ggplot(out.agreg[out.agreg$Ndmh %in% 0:limNdmh ,], 
               aes(x = DBH, y=median, group= Ndmh, colour = Ndmh)) + 
          facet_grid( Species ~ Model) +
          geom_line(size=1) + 
          geom_ribbon(aes(ymin=low, ymax=high, fill = Ndmh), alpha=0.5) +
          theme_bw() +
          theme(text = element_text(size=25)) +
          xlab("Diameter") + ylab("Preasence probability"))
}

#######################################################################
plot.LifePredict.Ndmh.stand <- function(Names.files = Names){
  out.agreg <- readRDS(file=paste( "Results/agregate_", 
                                   paste(Names.files$Project,
                                         Names.files$Type[1],
                                         Names.files$Simulation[2],
                                         sep="_"),
                                   ".rds",
                                   sep=""))
  
  print(ggplot(out.agreg, aes(x = Ndmh-0.5, y=median, colour = Species)) + 
          facet_grid( Species ~ Model) +
          geom_step(size=1) + 
          geom_step(data=out.agreg, aes(x = Ndmh-0.5, y=low, colour = Species)) +
          geom_step(data=out.agreg, aes(x = Ndmh-0.5, y=high, colour = Species)) +
          geom_point(data = out.agreg, aes(x=Ndmh, y=Occurrence)) +
          theme_bw() +
          xlab("Diameter") + ylab("Preasence probability"))
}

##################################################################

plot.Predict.functions <- function(Names.files = Names){
  out.agreg <- readRDS(file=paste( "Results/agregate_", 
                                   paste(Names.files$Project,
                                         Names.files$Type[2],
                                         Names.files$Simulation[1],
                                         sep="_"),
                                   ".rds",
                                   sep=""))

  print(ggplot(out.agreg[ which(x = out.agreg$DBH>7),], 
               aes(x = DBH, y=median, colour = Species)) + 
          facet_grid( facets = Functions ~  Model + Species, scales="free_y") +
          geom_line(size=1) + 
          geom_ribbon(aes(ymin=low, ymax=high, fill = Species), alpha=0.5) +
          theme_bw() +
          #theme(text = element_text(size=25)) +
          xlab("Diameter") + ylab("Probability"))
}

#####################################################

plot.Predict.occurrence.stand <- function(Names.files = Names){
  out.agreg <- readRDS(file=paste( "Results/agregate_", 
                                   paste(Names.files$Project,
                                         Names.files$Type[2],
                                         Names.files$Simulation[2],
                                         sep="_"),
                                   ".rds",
                                   sep=""))
  
  print(ggplot(out.agreg, aes(x = Class-5, y=median, colour = Species)) + 
          facet_grid( Model ~ Species) +
          geom_step(size=1) + 
          geom_step(data=out.agreg, aes(x = Class-5, y=low, colour = Species)) +
          geom_step(data=out.agreg, aes(x = Class-5, y=high, colour = Species)) +
          geom_point(data = out.agreg, aes(x=Class, y=Occurrence)) +
          theme_bw() +
          xlab("Diameter") + ylab("Preasence probability"))
}