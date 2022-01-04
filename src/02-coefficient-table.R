####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 2-coefficient-table.R
# Created January 2021
# Last Updated December 2021

####### Import Libraries and External Files #######

library(napops)

source("../utilities/rm-non-sp.R")

####### Set Constants #############################

sp <- c("AMRO")

####### Read Data #################################

dis_sp <- dis_coef(species = sp)
rem_sp <- rem_coef(species = sp)

####### Order Tables by AIC and Add Delta #########

dis_sp <- dis_sp[order(dis_sp$AIC), ]
dis_sp$AIC <- dis_sp$AIC - dis_sp$AIC[1]

rem_sp <- rem_sp[order(rem_sp$AIC), ]
rem_sp$AIC <- rem_sp$AIC - rem_sp$AIC[1]

####### Output Tables #############################

write.table(x = dis_sp,
            file = paste0("output/tables/distance_",
                          sp,
                          ".csv"),
            sep = ",", row.names = FALSE)

write.table(x = rem_sp,
            file = paste0("output/tables/removal_",
                          sp,
                          ".csv"),
            sep = ",", row.names = FALSE)
