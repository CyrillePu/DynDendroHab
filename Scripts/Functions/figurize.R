#!/usr/bin/env Rscript --verbose

plot.LifePredict.PNdmh_DBH.tree <- function(Names.files = Names, 
                                            lim.Ndmh = 5,
                                            Label.mod = cbind(c("E", 
                                                                "R",
                                                                "W"), 
                                                              c("Exponential", 
                                                                "Rayleigh",
                                                                "Weibull")),
                                            Label.sp = cbind(c("AA", 
                                                               "FS"), 
                                                             c("Abies alba", 
                                                               "Fagus sylvatica"))
                                            ){
  
  out.agreg <- readRDS(file=paste( "Results/A_", 
                                   paste(Names.files$Project,
                                         Names.files$Type[1],
                                         Names.files$Simulation[1],
                                         sep="_"),
                                   ".rds",
                                   sep=""))
  
  out.agreg <- out.agreg[out.agreg$Ndmh %in% 0:lim.Ndmh ,]
  
  out.agreg <- Label.adjust(table = out.agreg,
                            Label1 = Label.mod,
                            Label2 = Label.sp)
  
  print(ggplot(data= out.agreg, 
               aes(x = DBH, y=median, group= Ndmh)) + 
          facet_grid( Model ~ Species) +
          geom_line(aes(linetype = Ndmh)) + 
          geom_ribbon(aes(ymin=low, ymax=high), alpha=0.5) +
          theme_ecology() +
          theme(strip.text.x = element_text(face="italic"),
                legend.position = c(0.85, 0.57)) +
          guides(linetype = guide_legend(ncol = 3, byrow = TRUE,
                                         title = "Number of TMH \nper tree")) +
          xlab("Diameter (cm)") + ylab("Probability of presence"))
}

#######################################################################
plot.LifePredict.Ndmh.stand <- function(Names.files = Names, 
                                        Lim.Ndmh = 8, 
                                        Label.mod = cbind(c("E", 
                                                           "R",
                                                           "W"), 
                                                         c("Exponential", 
                                                           "Rayleigh",
                                                           "Weibull")),
                                        Label.sp = cbind(c("AA", 
                                                            "FS"), 
                                                          c("Abies alba", 
                                                            "Fagus sylvatica"))
){
  
  out.agreg <- readRDS(file=paste( "Results/A_", 
                                   paste(Names.files$Project,
                                         Names.files$Type[1],
                                         Names.files$Simulation[2],
                                         sep="_"),
                                   ".rds",
                                   sep=""))
  
  out.agreg <- out.agreg[ which(x = out.agreg$Ndmh < Lim.Ndmh),]
  
  out.agreg <- Label.adjust(table = out.agreg,
                            Label1 = Label.mod,
                            Label2 = Label.sp)
  
  print(ggplot(data = out.agreg
               , aes(x = Ndmh-0.5, y=median)) + 
          facet_grid(Model ~ Species) +
          geom_step() + 
          geom_step(data=out.agreg, aes(x = Ndmh-0.5, y=low),
                    linetype = "dashed") +
          geom_step(data=out.agreg, aes(x = Ndmh-0.5, y=high),
                    linetype = "dashed") +
          geom_point(data = out.agreg, aes(x=Ndmh, y=Occurrence)) +
          theme_ecology() + 
          theme(strip.text.x = element_text(face="italic")) +
          xlab("Diameter (cm)") + ylab("Probability of presence"))
}

##################################################################

plot.Predict.functions <- function(Names.files = Names,
                                   Label.mod = cbind(c("E", 
                                                       "R",
                                                       "W"), 
                                                     c("Exponential", 
                                                       "Rayleigh",
                                                       "Weibull")),
                                   Label.sp = cbind(c("AA", 
                                                      "FS"), 
                                                    c("Abies alba", 
                                                      "Fagus sylvatica"))){
  out.agreg <- readRDS(file=paste( "Results/A_", 
                                   paste(Names.files$Project,
                                         Names.files$Type[2],
                                         Names.files$Simulation[1],
                                         sep="_"),
                                   ".rds",
                                   sep=""))
  
  out.agreg <- Label.adjust(table = out.agreg,
                            Label1 = Label.mod,
                            Label2 = Label.sp)

  print(ggplot(out.agreg[ which(x = out.agreg$DBH>7),], 
               aes(x = DBH, y=median)) + 
          facet_grid( facets = Functions ~ Model, 
                      scales="free_y") +
          geom_line(aes(linetype = Species)) + 
          geom_ribbon(aes(ymin=low, ymax=high, fill = Species), alpha=0.5) +
          scale_fill_manual(values = c("grey70", "grey30"))+
          theme_ecology() +
          theme(legend.text = element_text(face = "italic"),
                legend.position = c(0.2, 0.2)) +
          xlab("Diameter (cm)") + ylab("Probability"))
}

#####################################################

plot.Predict.occurrence.stand <- function(Names.files = Names,
                                          Label.mod = cbind(c("E", 
                                                              "R",
                                                              "W"), 
                                                            c("Exponential", 
                                                              "Rayleigh",
                                                              "Weibull")),
                                          Label.sp = cbind(c("AA", 
                                                             "FS"), 
                                                           c("Abies alba", 
                                                             "Fagus sylvatica"))){
  out.agreg <- readRDS(file=paste( "Results/A_", 
                                   paste(Names.files$Project,
                                         Names.files$Type[2],
                                         Names.files$Simulation[2],
                                         sep="_"),
                                   ".rds",
                                   sep=""))
  
  out.agreg <- Label.adjust(table = out.agreg,
                            Label1 = Label.mod,
                            Label2 = Label.sp)
  
  print(ggplot(out.agreg, aes(x = Class-5, y=median)) + 
          facet_grid(Model ~ Species) +
          geom_step() + 
          geom_step(data=out.agreg, aes(x = Class-5, 
                                        y=low),
                    linetype = "dashed") +
          geom_step(data=out.agreg, aes(x = Class-5, 
                                        y=high),
                    linetype = "dashed") +
          geom_point(data = out.agreg, aes(x=Class, 
                                           y=Occurrence)) +
          theme_ecology() +
          theme(strip.text.x = element_text(face="italic")) +
          xlab("Diameter (cm)") + ylab("Probability of presence"))
}

#################################################

theme_ecology <- function() {
  theme(panel.grid.major = element_line(size = 0.7, 
                                        color = "grey"), 
        panel.background = element_rect(fill = NA, 
                                        colour = "black",
                                        size = 1),
        strip.background = element_rect(fill = NA, 
                                        colour = NA), 
        strip.text = element_text(size = 11),
        legend.background = element_rect(fill = "white",
                                         colour = "black"),
        legend.key = element_rect(fill = NA, 
                                  colour = NA),
        text = element_text(size = 12),
        axis.text = element_text(colour = "black"),
        line = element_line(size=1.5))
}

Label.adjust <- function(table = out.agreg,
                         Label1 = Label.mod,
                         Label2 = Label.sp){
  table$Species <- as.factor(table$Species)
  table$Model <- as.factor(table$Model)
  levels(table$Species) <- Label2[,2]
  levels(table$Model) <- Label1[,2]
  return(table)
}
