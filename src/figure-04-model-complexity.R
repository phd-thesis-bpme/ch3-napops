####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# figure-04-model-complexity.R
# Created January 2022
# Last Updated February 2022

####### Import Libraries and External Files #######

library(ggplot2)
library(ggpubr)
library(viridis)
library(napops)
theme_set(theme_pubclean())

source("src/rm-non-sp.R")

####### Read Data #################################

dis_best <- coef_distance(model = "best")
rem_best <- coef_removal(model = "best")
ibp_codes <- read.csv("data/IBP-Alpha-Codes20.csv")
families <- read.csv("data/NACC_list_species.csv")

####### Boxplot Distance Models by Sample Size ####

dis_models <- dis_best[, c("Species", "N", "Model")]
dis_models$Complexity <- ifelse(dis_models$Model == 1, "Null Model", "1 Covariate")
dis_models[which(dis_models$Model >= 4),
           "Complexity"] <- "2+ Covariates"
dis_models$Complexity <- factor(dis_models$Complexity,
                                levels = c("Null Model", "1 Covariate", "2+ Covariates"))

dis_box <- ggplot(data = dis_models) + 
  geom_boxplot(aes(x = Complexity, y = N)) +
  xlab("Model Complexity") +
  ylab("Sample Size") +
  NULL

####### Boxplot Removal Models by Sample Size #####

rem_models <- rem_best[, c("Species", "N", "Model")]
rem_models$Complexity <- ifelse(rem_models$Model == 1, "Null Model", "1 Covariate")
rem_models[which(rem_models$Model %in% c(3,5:9)),
           "Complexity"] <- "2+ Covariates"
rem_models$Complexity <- factor(rem_models$Complexity,
                                levels = c("Null Model", "1 Covariate", "2+ Covariates"))

rem_box <- ggplot(data = rem_models) + 
  geom_boxplot(aes(x = Complexity, y = N)) +
  xlab("Model Complexity") +
  ylab("Sample Size") +
  NULL

####### Boxplot Side-by-Side by Sample Size #######

combined <- data.frame(Model = rep("Removal", nrow(rem_models)),
                       n = rem_models$N,
                       Complexity = rem_models$Complexity)
combined <- rbind(combined,
                  data.frame(Model = rep("Distance", nrow(dis_models)),
                             n = dis_models$N,
                             Complexity = dis_models$Complexity))

combined$Model <- factor(combined$Model, levels = c("Removal", "Distance"))

combined_box <- ggplot(data = combined) + 
  geom_boxplot(aes(x = Complexity, y = sqrt(n), fill = Model)) +
  xlab("Model Complexity") +
  ylab("SQRT(Sample Size)") +
  scale_fill_viridis(discrete=TRUE) +
  NULL

####### Output Combined Boxplot ###################
png(filename = "output/plots/Fig4-model-complexity.png",
    width = 3.5, height = 3.5, units = "in", res = 600)
print(combined_box)
dev.off()