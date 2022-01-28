####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# summary-statistics.R
# Created January 2022
# Last Updated January 2022

####### Import Libraries and External Files #######

library(napops)

####### Data Collection ###########################

message(paste0("Number of projects: ", napops_summary()$Projects))
message(paste0("Number of observations: ", napops_summary()$Observations))
message(paste0("Number of total samples: ", napops_summary()$Samples))
message(paste0("Number of removal samples: ", napops_summary()$Samples_Removal))
message(paste0("Number of distance samples: ", napops_summary()$Samples_Distance))
message(paste0("Number of species: ", napops_summary()$Species))
sp <- list_species()
message(paste0("Number of species for both rem and dis: ", 
               nrow(sp[which(as.numeric(sp$Removal) == 1 & as.numeric(sp$Distance) == 1), ])))
message(paste0("Range of OD Covariates: ",
               min(covariates_removal()$OD), ", ", max(covariates_removal()$OD)))
message(paste0("Median OD: ",
               median(covariates_removal()$OD)))
message(paste0("Range of TSSR Covariates: ",
               min(covariates_removal()$TSSR), ", ", max(covariates_removal()$TSSR)))
message(paste0("Median TSSR: ",
               median(covariates_removal()$TSSR)))
message(paste0("Offroad Surveys: ",
               length(covariates_distance()$Road) - sum(covariates_distance()$Road)))
message(paste0("Roadside Surveys: ",
               sum(covariates_distance()$Road)))

####### Model Selection ###########################

rem_coefs <- coef_removal(model = "best")
rem_coefs_null <- rem_coefs[which(rem_coefs$Model == 1), ]
mean(rem_coefs_null$N)

dis_coefs <- coef_distance(model = 'best')
dis_coefs_null <- dis_coefs[which(dis_coefs$Model == 1), ]
mean(dis_coefs_null$N)

####### Removal Modelling #########################

nrow(rem_coefs)
rem_sp <- sp[which(sp$Species %in% rem_coefs$Species), ]
length(unique(rem_sp$Family))

####### Distance Modelling #########################

nrow(dis_coefs)
dis_sp <- sp[which(sp$Species %in% dis_coefs$Species), ]
length(unique(dis_sp$Family))

####### American Robin ############################

nrow(covariates_removal(project = FALSE, species = "AMRO"))
