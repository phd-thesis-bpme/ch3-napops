####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 1-model-selection.R
# Created January 2021
# Last Updated April 2021

####### Import Libraries and External Files #######

library(ggplot2)
library(ggpubr)
theme_set(theme_pubclean())

source("../utilities/rm-non-sp.R")

####### Read Data #################################

distance <- rm_non_sp(read.csv("results/coefficients/distance.csv"))
removal <- rm_non_sp(read.csv("results/coefficients/removal.csv"))

####### Distance Model Selection ##################

dis_best <- as.data.frame(matrix(data = NA,
                                 nrow = length(unique(distance$Species)),
                                 ncol = ncol(distance)))
names(dis_best) <- names(distance)
dis_best$Species <- unique(distance$Species)

# Distance best model
for (sp in dis_best$Species)
{
  temp <- distance[which(distance$Species == sp), ]
  min_aic <- temp[which(temp$aic == min(temp$aic)), ]
  dis_best[which(dis_best$Species == sp), ] <- min_aic
}

####### Removal Model Selection ###################

rem_best <- as.data.frame(matrix(data = NA,
                                 nrow = length(unique(removal$Species)),
                                 ncol = ncol(removal)))
names(rem_best) <- names(removal)
rem_best$Species <- unique(removal$Species)

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

####### Boxplot Distance Models by Sample Size ####

dis_models <- dis_best[, c("Species", "n", "model")]
dis_models$Complexity <- ifelse(dis_models$model == 1, "Low", "Medium")
dis_models[which(dis_models$model >= 4),
           "Complexity"] <- "High"
dis_models$Complexity <- factor(dis_models$Complexity,
                                levels = c("Low", "Medium", "High"))

dis_box <- ggplot(data = dis_models) + 
  geom_boxplot(aes(x = Complexity, y = n)) +
  xlab("Model Complexity") +
  ylab("Sample Size") +
  NULL

####### Boxplot Removal Models by Sample Size #####

rem_models <- rem_best[, c("Species", "n", "model")]
rem_models$Complexity <- ifelse(rem_models$model < 9, "Medium", "High")
rem_models[which(rem_models$model <= 3),
           "Complexity"] <- "Low"
rem_models$Complexity <- factor(rem_models$Complexity,
                                levels = c("Low", "Medium", "High"))

rem_box <- ggplot(data = rem_models) + 
  geom_boxplot(aes(x = Complexity, y = n)) +
  xlab("Model Complexity") +
  ylab("Sample Size") +
  NULL
####### Output Results ############################

write.table(rem_best, file = "output/tables/removal_best.csv", sep = ",", row.names = FALSE)
write.table(dis_best, file = "output/tables/distance_best.csv", sep = ",", row.names = FALSE)
write.table(model_table, file = "output/tables/model_table.csv", sep = ",", row.names = TRUE)

png(filename = "output/plots/dis_model_box.png", 
    width = 6, height = 6, units = "in", res = 300)
print(dis_box)
dev.off()

png(filename = "output/plots/rem_model_box.png", 
    width = 6, height = 6, units = "in", res = 300)
print(rem_box)
dev.off()
