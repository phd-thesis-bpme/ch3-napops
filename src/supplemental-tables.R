####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# supplemental-tables.R
# Created January 2022
# Last Updated January 2022

####### Import Libraries and External Files #######

library(napops)

####### List of Projects ##########################

projects <- list_projects()
write.table(projects, file = "output/tables/S1-projects.csv", sep = ",", row.names = FALSE)

####### List of Species ##########################

species <- list_species()
sp_output <- species[, c("Species", "Common_Name", "Scientific_Name", "Removal", "Distance")]
sp_output$Notes <- ""
for (s in 1:nrow(sp_output))
{
  if (sp_output$Removal[s] == 1 & sp_output$Distance[s] == 0)
  {
    sp_output$Notes[s] <- "Removal modelling only"
  }else if (sp_output$Removal[s] == 0 & sp_output$Distance[s] == 1)
  {
    sp_output$Notes[s] <- "Distance modelling only"
  }
}

names(sp_output)[1] <- "Code"

write.table(sp_output[, c("Code", "Common_Name", "Scientific_Name", "Notes")],
            file = "output/tables/S2-species.csv", sep = ",", row.names = FALSE)

####### Removal Coefficients ######################

rem_coef <- coef_removal(model = "best")
write.table(rem_coef[, -4],
            file = "output/tables/S3-removal.csv", sep = ",", row.names = FALSE)

####### Distance Coefficients ######################

dis_coef <- coef_distance(model = "best")
write.table(dis_coef[, -4],
            file = "output/tables/S4-distance.csv", sep = ",", row.names = FALSE)
