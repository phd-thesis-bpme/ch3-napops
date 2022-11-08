####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# table-01-02-amro-coefficients.R
# Created January 2022
# Last Updated November 2022

####### Import Libraries and External Files #######

library(napops)

####### Read Data #################################

rem_coefs <- coef_removal(species = "AMRO")
dis_coefs <- coef_distance(species = "AMRO")

####### Sort & Output #############################

rem_coefs <- rem_coefs[order(rem_coefs$AIC), ]
rem_coefs$Delta_AIC <- rem_coefs$AIC - rem_coefs$AIC[1]
rem_coefs[, c(5:10)] <- round(rem_coefs[, c(5:10)], digits = 2)

dis_coefs <- dis_coefs[order(dis_coefs$AIC), ]
dis_coefs$Delta_AIC <- dis_coefs$AIC - dis_coefs$AIC[1]
dis_coefs[, c(5:9)] <- round(dis_coefs[, c(5:9)], digits = 2)

write.table(rem_coefs[, c("Model", "Delta_AIC", "Intercept", "TSSR",
                          "TSSR2", "OD", "OD2"), ],
            file = "output/tables/table1-amro_rem_coefs.csv",
            sep = ",",
            row.names = FALSE)

write.table(dis_coefs[, c("Model", "Delta_AIC", "Intercept", "Road",
                          "Forest", "RoadForest"),],
            file = "output/tables/table2-amro_dis_coefs.csv",
            sep = ",",
            row.names = FALSE)
