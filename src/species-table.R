####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# species-table.R
# Created January 2021
# Last Updated January 2021

####### Read Data #################################

dis <- read.csv("../results/coefficients/distance.csv")
rem <- read.csv("../results/coefficients/removal.csv")
ibp <- read.csv("../utilities/IBP-Alpha-Codes20.csv")

####### Create Table of Species ###################

all_species <- union(rem$Species, dis$Species)

# Taxonomic list of banding codes
codes <- all_species[match(ibp$SPEC, all_species)]
codes <- codes[!is.na(codes)]

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