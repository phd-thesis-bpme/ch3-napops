####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# coefficient-table.R
# Created January 2021
# Last Updated January 2021

####### Import Libraries and External Files #######

source("../utilities/order-taxo.R")
source("../utilities/rm-non-sp.R")

####### Read Data #################################

ibp_sp <- read.csv("../utilities/IBP-Alpha-Codes20.csv")$SPEC
distance <- rm_non_sp(order_taxo(read.csv("data/distance.csv")))
removal <- rm_non_sp(order_taxo(read.csv("data/removal.csv")))

####### Create Full Tables ########################

# For the results, we'll just output the "best" model for each species
write.table(x = distance[, c("Species", "n", "best_model",
                                 "best_bic", "int_best", "road_best", 
                                 "forest_best", "roadforest_best")],
            file = "tables/coefficients_best_distance_taxonomic_all.csv",
            sep = ",", row.names = FALSE)

write.table(x = removal[, c("Species", "n", "best_model",
                                "best_bic", "int_best", "tssr_best", 
                                "tssr2_best", "jd_best", "jd2_best")],
            file = "tables/coefficients_best_removal_taxonomic_all.csv",
            sep = ",", row.names = FALSE)

####### Create Reduced Tables #####################

d_indices <- seq(from = 41, to = 325, by = 41)
d_red <- distance[d_indices,]

r_indices <- seq(from = 39, to = 309, by = 39)
r_red <- removal[r_indices,]

write.table(x = d_red[, c("Species", "n", "best_model",
                          "best_bic", "int_best", "road_best", 
                          "forest_best", "roadforest_best")],
            file = "tables/coefficients_best_distance_taxonomic_red.csv",
            sep = ",", row.names = FALSE)

write.table(x = r_red[, c("Species", "n", "best_model",
                          "best_bic", "int_best", "tssr_best", 
                          "tssr2_best", "jd_best", "jd2_best")],
            file = "tables/coefficients_best_removal_taxonomic_red.csv",
            sep = ",", row.names = FALSE)