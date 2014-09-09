rm(list=ls())
source("../Scripts/fonctionsMod.R")


Y <- simulation(PostDist.file = "../Results/Post_Wkl.rds")
saveRDS(Y, file="../Results/Sim_Vect_Wkl.rds")

Y <- simulation(PostDist.file = "../Results/Post_R.rds")
saveRDS(Y, file="../Results/Sim_Vect_R.rds")

Y <- simulation(PostDist.file = "../Results/Post_E.rds")
saveRDS(Y, file="../Results/Sim_Vect_E.rds")


Z <- simulation(PostDist.file = "../Results/Post_Wkl.rds", 
                Obs.file = "../Data/DMH.rds",
                n.repetitions=100)
saveRDS(Z, file="../Results/Sim_Obs_Wkl.rds")

Z <- simulation(PostDist.file = "../Results/Post_R.rds", 
                Obs.file = "../Data/DMH.rds",
                n.repetitions=100)
saveRDS(Z, file="../Results/Sim_Obs_R.rds")

Z <- simulation(PostDist.file = "../Results/Post_E.rds", 
                Obs.file = "../Data/DMH.rds",
                n.repetitions=100)
saveRDS(Z, file="../Results/Sim_Obs_E.rds")



X1<- simulation.Dyn(PostDist.file = "../Results/Post_R.rds", 
                    n.repetitions = 10,
                    Obs.file = "../Data/DMH.rds")

X2<- simulation.Dyn(PostDist.file = "../Results/Post_R.rds", 
                    n.repetitions = 10)

saveRDS(Z, file="../Results/Simul_obs.rds")

boxplot