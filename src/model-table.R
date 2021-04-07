####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# model-table.R
# Created January 2021
# Last Updated April 2021

####### Import Libraries and External Files #######

source("../utilities/rm-non-sp.R")

####### Read Data #################################

distance <- rm_non_sp(read.csv("results/coefficients/distance.csv"))
removal <- rm_non_sp(read.csv("results/coefficients/removal.csv"))

####### Create Reduced DF of Best Model Only ######

dis_best <- as.data.frame(matrix(data = NA,
                                 nrow = length(unique(distance$Species)),
                                 ncol = ncol(distance)))
names(dis_best) <- names(distance)
dis_best$Species <- unique(distance$Species)

rem_best <- as.data.frame(matrix(data = NA,
                                 nrow = length(unique(removal$Species)),
                                 ncol = ncol(removal)))
names(rem_best) <- names(removal)
rem_best$Species <- unique(removal$Species)


# Distance best model
for (sp in dis_best$Species)
{
  temp <- distance[which(distance$Species == sp), ]
  min_aic <- temp[which(temp$aic == min(temp$aic)), ]
  dis_best[which(dis_best$Species == sp), ] <- min_aic
}

# Removal best model
for (sp in rem_best$Species)
{
  temp <- removal[which(removal$Species == sp), ]
  min_aic <- temp[which(temp$aic == min(temp$aic)), ]
  rem_best[which(rem_best$Species == sp), ] <- min_aic
}

####### Create Table ##############################

species <- intersect(unique(dis_best$Species), unique(rem_best$Species))
rem <- rem_best[which(rem_best$Species %in% species), ]
dis <- dis_best[which(dis_best$Species %in% species), ]

model_table <- table(rem$model, dis$model)

####### Output Results ############################

write.table(rem_best, file = "output/tables/removal_best.csv", sep = ",", row.names = FALSE)
write.table(dis_best, file = "output/tables/distance_best.csv", sep = ",", row.names = FALSE)
write.table(model_table, file = "output/tables/model_table.csv", sep = ",", row.names = TRUE)
