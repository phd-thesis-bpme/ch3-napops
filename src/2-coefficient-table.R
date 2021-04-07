####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 2-coefficient-table.R
# Created January 2021
# Last Updated April 2021

####### Import Libraries and External Files #######

####### Set Constants #############################

sp_order <- c("AMRO", "YRWA", "SOSP", "REVI",
              "GHOW", "PIWO", "OSFL", "VESP",
              "LARB", "VATH", "CARW", "SABS",
              "BAIS", "CASP", "GWWA", "BLPH")

####### Read Data #################################

distance <- read.csv("output/tables/distance_best.csv")
removal <- read.csv("output/tables/removal_best.csv")

####### Create Subsetted Tables ###################

dis_red <- distance[which(distance$Species %in% sp_order), ]
dis_red <- dis_red[match(sp_order, dis_red$Species), ]

rem_red <- removal[which(removal$Species %in% sp_order), ]
rem_red <- rem_red[match(sp_order, rem_red$Species), ]

####### Output Tables #############################

write.table(x = dis_red,
            file = "output/tables/distance_best_reduced.csv",
            sep = ",", row.names = FALSE)

write.table(x = rem_red,
            file = "output/tables/removal_best_reduced.csv",
            sep = ",", row.names = FALSE)