####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# model-table.R
# Created January 2021
# Last Updated January 2021

####### Import Libraries and External Files #######

source("../utilities/rm-non-sp.R")

####### Read Data #################################

distance <- rm_non_sp(read.csv("../results/coefficients/distance.csv"))
removal <- rm_non_sp(read.csv("../results/coefficients/removal.csv"))

####### Create Table ##############################

species <- intersect(unique(distance$Species), unique(removal$Species))
removal <- removal[which(removal$Species %in% species), ]
distance <- distance[which(distance$Species %in% species), ]

model_table <- table(removal$best_model, distance$best_model)

write.table(model_table, file = "tables/model_table.csv", sep = ",", row.names = TRUE)
