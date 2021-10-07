####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 2-coefficient-table.R
# Created January 2021
# Last Updated October 2021

####### Import Libraries and External Files #######

source("../utilities/rm-non-sp.R")

####### Set Constants #############################

sp <- c("AMRO")

####### Read Data #################################

distance <- rm_non_sp(read.csv("../results/coefficients/distance.csv"))
removal <- rm_non_sp(read.csv("../results/coefficients/removal.csv"))

####### Create Subsetted Tables ###################

dis_red <- distance[which(distance$Species %in% sp), ]
dis_red <- dis_red[order(dis_red$aic), ]
dis_red$aic <- dis_red$aic - dis_red$aic[1]

rem_red <- removal[which(removal$Species %in% sp &
                           removal$model <= 9), ]
rem_red <- rem_red[order(rem_red$aic), ]
rem_red$aic <- rem_red$aic - rem_red$aic[1]

####### Output Tables #############################

write.table(x = dis_red,
            file = paste0("output/tables/distance_",
                          sp,
                          ".csv"),
            sep = ",", row.names = FALSE)

write.table(x = rem_red,
            file = paste0("output/tables/removal_",
                          sp,
                          ".csv"),
            sep = ",", row.names = FALSE)
