
Results/DMH_*.rds: Scripts/MCMCcomputer.R Stan_model/Model.R Results/Names.rdata Data/Data.RDS
	Rscript $<

Results/DMH_*_**.rds: Scripts/Agregator.R Results/DMH_*.rds
	Rscript $<

Results/agregate_DMH_*.rds: Scripts/Agregator.R Scripts/Functions/table.R Results/DMH_*.rds
	Rscript $<
