####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# species-table.R
# Created January 2021
# Last Updated January 2021

####### Import Libraries and External Files #######

source("../utilities/order-taxo.R")
source("../utilities/rm-non-sp.R")

####### Read Data #################################

ibp <- read.csv("../utilities/IBP-Alpha-Codes20.csv")
dis <- rm_non_sp(order_taxo(read.csv("data/distance.csv")))
rem <- rm_non_sp(order_taxo(read.csv("data/removal.csv")))

####### Create Table of Species ###################

codes <- union(rem$Species, dis$Species)

common_name <- ibp[which(ibp$SPEC %in% codes), "COMMONNAME"]

sci_name <- ibp[which(ibp$SPEC %in% codes), "SCINAME"]

in_removal <- ifelse(codes %in% rem$Species, 1, 0)

in_distance <- ifelse(codes %in% dis$Species, 1, 0)

species_table <- data.frame(Code = codes,
                            Common_Name = common_name,
                            Scientific_Name = sci_name,
                            removal = in_removal,
                            distance = in_distance)

write.table(x = species_table, file = "tables/species_table.csv",
            sep = ",", row.names = FALSE)