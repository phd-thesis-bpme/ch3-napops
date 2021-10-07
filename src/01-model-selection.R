####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 1-model-selection.R
# Created January 2021
# Last Updated April 2021

####### Import Libraries and External Files #######

library(ggplot2)
library(ggpubr)
library(viridis)
theme_set(theme_pubclean())

source("../utilities/rm-non-sp.R")

####### Read Data #################################

distance <- rm_non_sp(read.csv("../results/coefficients/distance.csv"))
removal <- rm_non_sp(read.csv("../results/coefficients/removal.csv"))

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

####### (Distance) Reliability of Models ##########

dis_best$review <- FALSE
dis_best$notes <- NA

# Load files related to distance sampling models
load("../results/quant-summary/dis_species_summary.rda")
load("../results/aic/dis_aic.rda")
tau_files <- list.files(path = "../results/simulations/tau")
for (f in tau_files)
{
  load(paste0("../results/simulations/tau/", f))
}

for (sp in dis_best$Species)
{
  model <- dis_best[which(dis_best$Species == sp), "model"]
  sp_summary <- dis_species_summary[[sp]]
  sp_note <- ""
  
  # Decision tree for flagging species
  
  # #1 is deciding which model
  if (model == 1) # 1a
  {
    sp_note <- paste0(sp_note, "{null model}")
    dis_best[which(dis_best$Species == sp), "review"] <- TRUE
    dis_best[which(dis_best$Species == sp), "notes"] <- sp_note
    next()
  }else if (model == 2) # 1b, proceed to #2
  {
    # #2
    tau_list <- eval(parse(text = paste0("tau_", model)))
    n_roadside <- sum(sp_summary$roadside)
    n_offroad <- nrow(sp_summary) - n_roadside
    
    if (max(n_roadside, n_offroad) > (2 * min(n_roadside, n_offroad)))
    {
      # 2a
      sp_note <- paste0(sp_note, "{roadside obs imbalance} ")
    }
    
    # #3
    tau_roadside <- tau_list[which(tau_list$Species == sp & 
                                     tau_list$Roadside == 1), "tau"][1]
    tau_offroad <- tau_list[which(tau_list$Species == sp & 
                                     tau_list$Roadside == 0), "tau"][1]
    
    if (tau_offroad > tau_roadside)
    {
      dis_best[which(dis_best$Species == sp), "review"] <- TRUE
      sp_note <- paste0(sp_note, "{offroad edr > roadside edr}")
    }
    dis_best[which(dis_best$Species == sp), "notes"] <- sp_note
  }else if (model == 3) #1c, proceed to #4
  {
    # #4
    tau_list <- eval(parse(text = paste0("tau_", model)))
    n_forest <- nrow(sp_summary[which(sp_summary$ForestOnly_5x5 >= 0.5), ])
    n_nonforest <- nrow(sp_summary[which(sp_summary$ForestOnly_5x5 < 0.5), ])
    
    if (max(n_forest, n_nonforest) > (2 * min(n_forest, n_nonforest)))
    {
      # 4a
      sp_note <- paste0(sp_note, "{forest obs imbalance} ")
    }
    
    # #5
    tau_forest <- tau_list[which(tau_list$Species == sp & 
                                     tau_list$Forest == 1), "tau"][1]
    tau_nonforest <- tau_list[which(tau_list$Species == sp & 
                                    tau_list$Forest == 0), "tau"][1]
    
    if (tau_forest > tau_nonforest)
    {
      dis_best[which(dis_best$Species == sp), "review"] <- TRUE
      sp_note <- paste0(sp_note, "{forest edr > nonforest edr}")
    }
    dis_best[which(dis_best$Species == sp), "notes"] <- sp_note
  }else if (model == 4 || model == 5) # 1d, proceed to 6
  {
    # #6
    tau_list <- eval(parse(text = paste0("tau_", model)))
    n_roadside <- sum(sp_summary$roadside)
    n_offroad <- nrow(sp_summary) - n_roadside
    flag_potential <- FALSE
    
    if (max(n_roadside, n_offroad) > (2 * min(n_roadside, n_offroad)))
    {
      # 6a
      sp_note <- paste0(sp_note, "{roadside obs imbalance} ")
    }
    
    # #7
    n_forest <- nrow(sp_summary[which(sp_summary$ForestOnly_5x5 >= 0.5), ])
    n_nonforest <- nrow(sp_summary[which(sp_summary$ForestOnly_5x5 < 0.5), ])
    
    if (max(n_forest, n_nonforest) > (2 * min(n_forest, n_nonforest)))
    {
      # 7a
      sp_note <- paste0(sp_note, "{forest obs imbalance} ")
    }
    
    # #8
    tau_roadside_f <- tau_list[which(tau_list$Species == sp & 
                                     tau_list$Roadside == 1 &
                                     tau_list$Forest == 1), "tau"][1]
    tau_roadside_nf <- tau_list[which(tau_list$Species == sp & 
                                       tau_list$Roadside == 1 &
                                       tau_list$Forest == 0), "tau"][1]
    tau_offroad_f <- tau_list[which(tau_list$Species == sp & 
                                    tau_list$Roadside == 0 &
                                    tau_list$Forest == 1), "tau"][1]
    tau_offroad_nf <- tau_list[which(tau_list$Species == sp & 
                                      tau_list$Roadside == 0 &
                                      tau_list$Forest == 0), "tau"][1]
    
    if (tau_offroad_f > tau_roadside_f)
    {
      sp_note <- paste0(sp_note, "{offroad_f edr > roadside_f edr}")
      flag_potential <- TRUE
      if (tau_offroad_nf > tau_roadside_nf)
      {
        sp_note <- paste0(sp_note, "{offroad_nf edr > roadside_nf edr}")
      }
    }else if (tau_offroad_nf > tau_roadside_nf)
    {
      sp_note <- paste0(sp_note, "{offroad_nf edr > roadside_nf edr}")
      flag_potential <- TRUE
    }
    
    # #9/10
    tau_forest_rs <- tau_list[which(tau_list$Species == sp & 
                                   tau_list$Forest == 1 &
                                     tau_list$Roadside == 1), "tau"][1]
    tau_nonforest_rs <- tau_list[which(tau_list$Species == sp & 
                                      tau_list$Forest == 0 &
                                        tau_list$Roadside == 1), "tau"][1]
    tau_forest_or <- tau_list[which(tau_list$Species == sp & 
                                      tau_list$Forest == 1 &
                                      tau_list$Roadside == 0), "tau"][1]
    tau_nonforest_or <- tau_list[which(tau_list$Species == sp & 
                                         tau_list$Forest == 0 &
                                         tau_list$Roadside == 0), "tau"][1]
    
    if (tau_forest_rs > tau_nonforest_rs)
    {
      sp_note <- paste0(sp_note, "{forest_rs edr > nonforest_rs edr}")
      if (isTRUE(flag_potential))
      {
        dis_best[which(dis_best$Species == sp), "review"] <- TRUE
      }
      
      if (tau_forest_or > tau_nonforest_or)
      {
        sp_note <- paste0(sp_note, "{forest_or edr > nonforest_or edr}")
      }
      #sp_note <- paste0(sp_note, "{forest edr > nonforest edr}")
    }else if (tau_forest_or > tau_nonforest_or)
    {
      sp_note <- paste0(sp_note, "{forest_or edr > nonforest_or edr}")
      if (isTRUE(flag_potential))
      {
        dis_best[which(dis_best$Species == sp), "review"] <- TRUE
      }
    }
    
    dis_best[which(dis_best$Species == sp), "notes"] <- sp_note
    
    
    
    
  }
  
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
  temp <- removal[which(removal$Species == sp & removal$model <= 9), ]
  min_aic <- temp[which(temp$aic == min(temp$aic, na.rm = TRUE)), ]
  rem_best[which(rem_best$Species == sp), ] <- min_aic
}

#' For all species that had a JD-BCR interaction, calculate what
#' the best model WOULD have been without the interaction models
#' being an option
jd_bcr_sp <- rem_best[which(rem_best$model > 9), "Species"]

rem_best_without_bcr <- as.data.frame(matrix(data = NA,
                                             nrow = length(jd_bcr_sp),
                                             ncol = ncol(removal)))
names(rem_best_without_bcr) <- names(removal)
rem_best_without_bcr$Species <- jd_bcr_sp
for (sp in jd_bcr_sp)
{
  temp <- removal[which(removal$Species == sp), ]
  min_aic <- temp[which(temp$aic[1:9] == min(temp$aic[1:9], na.rm = TRUE)), ]
  rem_best_without_bcr[which(rem_best_without_bcr$Species == sp), ] <- min_aic
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
rem_models$Complexity <- ifelse(rem_models$model < 7, "Medium", "High")
rem_models[which(rem_models$model <= 3),
           "Complexity"] <- "Low"
rem_models$Complexity <- factor(rem_models$Complexity,
                                levels = c("Low", "Medium", "High"))

rem_box <- ggplot(data = rem_models) + 
  geom_boxplot(aes(x = Complexity, y = n)) +
  xlab("Model Complexity") +
  ylab("Sample Size") +
  NULL

####### Boxplot Side-by-Side by Sample Size #######

combined <- data.frame(Model = rep("Removal", nrow(rem_models)),
                       n = rem_models$n,
                       Complexity = rem_models$Complexity)
combined <- rbind(combined,
                  data.frame(Model = rep("Distance", nrow(dis_models)),
                             n = dis_models$n,
                             Complexity = dis_models$Complexity))

combined$Model <- factor(combined$Model, levels = c("Removal", "Distance"))

combined_box <- ggplot(data = combined) + 
  geom_boxplot(aes(x = Complexity, y = n, fill = Model)) +
  xlab("Model Complexity") +
  ylab("Sample Size") +
  scale_fill_viridis(discrete=TRUE) +
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

png(filename = "output/plots/combined_model_box.png",
    width = 6, height = 6, units = "in", res = 300)
print(combined_box)
dev.off()
