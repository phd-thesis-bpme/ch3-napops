####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# coefficient-table.R
# Created January 2021
# Last Updated January 2021

####### Read Data #################################

ibp_sp <- read.csv("../utilities/IBP-Alpha-Codes20.csv")$SPEC
distance <- read.csv("../results/coefficients/distance.csv")
removal <- read.csv("../results/coefficients/removal.csv")

####### Create Tables #############################

# Order both tables taxonomically
distance_tax <- distance[match(ibp_sp, distance$Species), ]
distance_tax <- distance_tax[!is.na(distance_tax$Species), ]

removal_tax <- removal[match(ibp_sp, removal$Species), ]
removal_tax <- removal_tax[!is.na(removal_tax$Species), ]

# For the results, we'll just output the "best" model for each species
write.table(x = distance_tax[, c("Species", "n", "best_model",
                                 "best_bic", "int_best", "road_best", 
                                 "forest_best", "roadforest_best")],
            file = "tables/coefficients_best_distance_taxonomic.csv",
            sep = ",", row.names = FALSE)

write.table(x = removal_tax[, c("Species", "n", "best_model",
                                "best_bic", "int_best", "tssr_best", 
                                "tssr2_best", "jd_best", "jd2_best")],
            file = "tables/coefficients_best_removal_taxonomic.csv",
            sep = ",", row.names = FALSE)