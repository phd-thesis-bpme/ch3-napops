####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 1-model-selection.R
# Created January 2021
# Last Updated December 2021

####### Import Libraries and External Files #######

library(ggplot2)
library(ggpubr)
library(ggfittext)
library(ggExtra)
library(viridis)
library(GGally)
library(magrittr)
library(napops)
theme_set(theme_pubclean())

source("../utilities/rm-non-sp.R")

####### Read Data #################################

dis_best <- dis_coef(model = "best")
rem_best <- rem_coef(model = "best")
ibp_codes <- read.csv("../utilities/IBP-Alpha-Codes20.csv")
families <- read.csv("../utilities/NACC_list_species.csv")

####### (Distance) Reliability of Models ##########

# dis_best$review <- FALSE
# dis_best$notes <- NA
# 
# # Load files related to distance sampling models
# load("../results/quant-summary/dis_species_summary.rda")
# load("../results/aic/dis_aic.rda")
# tau_files <- list.files(path = "../results/simulations/tau")
# for (f in tau_files)
# {
#   load(paste0("../results/simulations/tau/", f))
# }
# 
# for (sp in dis_best$Species)
# {
#   model <- dis_best[which(dis_best$Species == sp), "model"]
#   sp_summary <- dis_species_summary[[sp]]
#   sp_note <- ""
#   
#   # Decision tree for flagging species
#   
#   # #1 is deciding which model
#   if (model == 1) # 1a
#   {
#     sp_note <- paste0(sp_note, "{null model}")
#     dis_best[which(dis_best$Species == sp), "review"] <- TRUE
#     dis_best[which(dis_best$Species == sp), "notes"] <- sp_note
#     next()
#   }else if (model == 2) # 1b, proceed to #2
#   {
#     # #2
#     tau_list <- eval(parse(text = paste0("tau_", model)))
#     n_roadside <- sum(sp_summary$roadside)
#     n_offroad <- nrow(sp_summary) - n_roadside
#     
#     if (max(n_roadside, n_offroad) > (2 * min(n_roadside, n_offroad)))
#     {
#       # 2a
#       sp_note <- paste0(sp_note, "{roadside obs imbalance} ")
#     }
#     
#     # #3
#     tau_roadside <- tau_list[which(tau_list$Species == sp & 
#                                      tau_list$Roadside == 1), "tau"][1]
#     tau_offroad <- tau_list[which(tau_list$Species == sp & 
#                                      tau_list$Roadside == 0), "tau"][1]
#     
#     if (tau_offroad > tau_roadside)
#     {
#       dis_best[which(dis_best$Species == sp), "review"] <- TRUE
#       sp_note <- paste0(sp_note, "{offroad edr > roadside edr}")
#     }
#     dis_best[which(dis_best$Species == sp), "notes"] <- sp_note
#   }else if (model == 3) #1c, proceed to #4
#   {
#     # #4
#     tau_list <- eval(parse(text = paste0("tau_", model)))
#     n_forest <- nrow(sp_summary[which(sp_summary$ForestOnly_5x5 >= 0.5), ])
#     n_nonforest <- nrow(sp_summary[which(sp_summary$ForestOnly_5x5 < 0.5), ])
#     
#     if (max(n_forest, n_nonforest) > (2 * min(n_forest, n_nonforest)))
#     {
#       # 4a
#       sp_note <- paste0(sp_note, "{forest obs imbalance} ")
#     }
#     
#     # #5
#     tau_forest <- tau_list[which(tau_list$Species == sp & 
#                                      tau_list$Forest == 1), "tau"][1]
#     tau_nonforest <- tau_list[which(tau_list$Species == sp & 
#                                     tau_list$Forest == 0), "tau"][1]
#     
#     if (tau_forest > tau_nonforest)
#     {
#       dis_best[which(dis_best$Species == sp), "review"] <- TRUE
#       sp_note <- paste0(sp_note, "{forest edr > nonforest edr}")
#     }
#     dis_best[which(dis_best$Species == sp), "notes"] <- sp_note
#   }else if (model == 4 || model == 5) # 1d, proceed to 6
#   {
#     # #6
#     tau_list <- eval(parse(text = paste0("tau_", model)))
#     n_roadside <- sum(sp_summary$roadside)
#     n_offroad <- nrow(sp_summary) - n_roadside
#     flag_potential <- FALSE
#     
#     if (max(n_roadside, n_offroad) > (2 * min(n_roadside, n_offroad)))
#     {
#       # 6a
#       sp_note <- paste0(sp_note, "{roadside obs imbalance} ")
#     }
#     
#     # #7
#     n_forest <- nrow(sp_summary[which(sp_summary$ForestOnly_5x5 >= 0.5), ])
#     n_nonforest <- nrow(sp_summary[which(sp_summary$ForestOnly_5x5 < 0.5), ])
#     
#     if (max(n_forest, n_nonforest) > (2 * min(n_forest, n_nonforest)))
#     {
#       # 7a
#       sp_note <- paste0(sp_note, "{forest obs imbalance} ")
#     }
#     
#     # #8
#     tau_roadside_f <- tau_list[which(tau_list$Species == sp & 
#                                      tau_list$Roadside == 1 &
#                                      tau_list$Forest == 1), "tau"][1]
#     tau_roadside_nf <- tau_list[which(tau_list$Species == sp & 
#                                        tau_list$Roadside == 1 &
#                                        tau_list$Forest == 0), "tau"][1]
#     tau_offroad_f <- tau_list[which(tau_list$Species == sp & 
#                                     tau_list$Roadside == 0 &
#                                     tau_list$Forest == 1), "tau"][1]
#     tau_offroad_nf <- tau_list[which(tau_list$Species == sp & 
#                                       tau_list$Roadside == 0 &
#                                       tau_list$Forest == 0), "tau"][1]
#     
#     if (tau_offroad_f > tau_roadside_f)
#     {
#       sp_note <- paste0(sp_note, "{offroad_f edr > roadside_f edr}")
#       flag_potential <- TRUE
#       if (tau_offroad_nf > tau_roadside_nf)
#       {
#         sp_note <- paste0(sp_note, "{offroad_nf edr > roadside_nf edr}")
#       }
#     }else if (tau_offroad_nf > tau_roadside_nf)
#     {
#       sp_note <- paste0(sp_note, "{offroad_nf edr > roadside_nf edr}")
#       flag_potential <- TRUE
#     }
#     
#     # #9/10
#     tau_forest_rs <- tau_list[which(tau_list$Species == sp & 
#                                    tau_list$Forest == 1 &
#                                      tau_list$Roadside == 1), "tau"][1]
#     tau_nonforest_rs <- tau_list[which(tau_list$Species == sp & 
#                                       tau_list$Forest == 0 &
#                                         tau_list$Roadside == 1), "tau"][1]
#     tau_forest_or <- tau_list[which(tau_list$Species == sp & 
#                                       tau_list$Forest == 1 &
#                                       tau_list$Roadside == 0), "tau"][1]
#     tau_nonforest_or <- tau_list[which(tau_list$Species == sp & 
#                                          tau_list$Forest == 0 &
#                                          tau_list$Roadside == 0), "tau"][1]
#     
#     if (tau_forest_rs > tau_nonforest_rs)
#     {
#       sp_note <- paste0(sp_note, "{forest_rs edr > nonforest_rs edr}")
#       if (isTRUE(flag_potential))
#       {
#         dis_best[which(dis_best$Species == sp), "review"] <- TRUE
#       }
#       
#       if (tau_forest_or > tau_nonforest_or)
#       {
#         sp_note <- paste0(sp_note, "{forest_or edr > nonforest_or edr}")
#       }
#       #sp_note <- paste0(sp_note, "{forest edr > nonforest edr}")
#     }else if (tau_forest_or > tau_nonforest_or)
#     {
#       sp_note <- paste0(sp_note, "{forest_or edr > nonforest_or edr}")
#       if (isTRUE(flag_potential))
#       {
#         dis_best[which(dis_best$Species == sp), "review"] <- TRUE
#       }
#     }
#     
#     dis_best[which(dis_best$Species == sp), "notes"] <- sp_note
#     
#     
#     
#     
#   }
#   
# }

####### Create Table and Heatmaps (ALL) ###########

species <- intersect(unique(dis_best$Species), unique(rem_best$Species))
rem <- rem_best[which(rem_best$Species %in% species), ]
dis <- dis_best[which(dis_best$Species %in% species), ]

model_table <- table(rem$Model, dis$Model)
model_table_long <- as.data.frame(model_table)
names(model_table_long) <- c("Removal", "Distance", "Frequency")

rem_model_strings <- c("(1) Null",
                       "(2) OD",
                       "(3) OD + OD2",
                       "(4) TSSR",
                       "(5) TSSR + TSSR2",
                       "(6) TSSR + OD",
                       "(7) TSSR + OD + OD2",
                       "(8) TSSR + TSSR2 + OD",
                       "(9) TSSR + TSSR2 + OD + OD2")
dis_model_strings <- c("(1) Null",
                       "(2) Roadside",
                       "(3) Forest Coverage",
                       "(4) Roadside + Forest Coverage",
                       "(5) Roadside X Forest Coverage")

model_table_long$Removal_String <- rem_model_strings[model_table_long$Removal] %>% 
  factor(levels = rem_model_strings)
model_table_long$Distance_String <- dis_model_strings[model_table_long$Distance] %>%
  factor(levels = dis_model_strings)


# Heatmap of model selections
heatmap_all <- ggplot(data = model_table_long,
                      aes(x = as.numeric(Removal),
                          y = as.numeric(Distance),
                          fill = Frequency,
                          label = Frequency)) +
  geom_tile(aes()) +
  scale_fill_viridis_c(option = "B", direction = -1) +
  geom_fit_text(contrast = TRUE) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab("Distance Model") +
  xlab("Removal Model") +
  scale_x_continuous(breaks = seq(1,9), labels = rem_model_strings) +
  scale_y_continuous(breaks = seq(1,5), labels = dis_model_strings) +
  coord_fixed() +
  annotate("text", x = 1:9, y = 6, label = as.character(aggregate(model_table_long$Frequency, 
                                                                  by = list(Removal = model_table_long$Removal), 
                                                                  FUN = sum)$x)) +
  annotate("text", x = 10, y = 1:5, label = as.character(aggregate(model_table_long$Frequency, 
                                                                  by = list(Distance = model_table_long$Distance), 
                                                                  FUN = sum)$x)) +
  NULL 

####### Create Table and Heatmaps (Families) ######

species <- intersect(unique(dis_best$Species), unique(rem_best$Species))
species_names <- merge(data.frame(Species = species), ibp_codes[, c("SPEC", "COMMONNAME")],
                       by.x = "Species", by.y = "SPEC")
species_taxo <- merge(species_names, families[, c("common_name", "order", "family")],
                      by.x = "COMMONNAME", by.y = "common_name")

model_combos <- paste0(model_table_long$Removal,
                       "-",
                       model_table_long$Distance)
plot_list <- vector(mode = "list", length = length(unique(species_taxo$family)))
names(plot_list) <- unique(species_taxo$family)

for (f in unique(species_taxo$family))
{
  species <- species_taxo[which(species_taxo$family == f), "Species"]
  rem <- rem_best[which(rem_best$Species %in% species), ]
  dis <- dis_best[which(dis_best$Species %in% species), ]
  
  model_table <- table(rem$Model, dis$Model)
  model_table_temp <- as.data.frame(model_table)
  names(model_table_temp) <- c("Removal", "Distance", "Frequency")
  model_table_temp$Combo <- paste0(model_table_temp$Removal,
                                   "-",
                                   model_table_temp$Distance)
  
  combo_matrix <- matrix(unlist(strsplit(model_combos, split = "-")), ncol = 2, byrow = TRUE)
  model_table_full <- data.frame(Combo = model_combos,
                                 Removal = combo_matrix[,1],
                                 Distance = combo_matrix[,2])
  
  model_table_full <- merge(model_table_temp[, c("Combo", "Frequency")],
                            model_table_full,
                            by = "Combo", all = TRUE)
  model_table_full[is.na(model_table_full$Frequency), "Frequency"] <- 0

  
  # Heatmap of model selections
  plot_list[[f]] <- ggplot(model_table_full, aes(y = Removal, x = Distance, fill = Frequency, label = Frequency)) +
    geom_tile() +
    scale_fill_viridis_c(limits = c(0,7), option = "B", direction = -1) +
    geom_fit_text(contrast = TRUE) +
    theme(legend.position = "none") +
    #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    xlab("Distance Model") +
    ylab("Removal Model") +
    ggtitle(paste0(f,", n = ",
                   length(species)))+ 
    annotate("text", x = 1:9, y = 6, label = as.character(aggregate(model_table_long$Frequency, 
                                                                    by = list(Removal = model_table_long$Removal), 
                                                                    FUN = sum)$x)) +
    annotate("text", x = 10, y = 1:5, label = as.character(aggregate(model_table_long$Frequency, 
                                                                     by = list(Distance = model_table_long$Distance), 
                                                                     FUN = sum)$x)) +
    NULL
}

# Get the top 12 populated families
families_to_plot <- c("Parulidae", "Passerellidae", "Tyrannidae",
                      "Picidae", "Icteridae", "Corvidae")

####### Boxplot Distance Models by Sample Size ####

dis_models <- dis_best[, c("Species", "N", "Model")]
dis_models$Complexity <- ifelse(dis_models$Model == 1, "Low", "Medium")
dis_models[which(dis_models$Model >= 4),
           "Complexity"] <- "High"
dis_models$Complexity <- factor(dis_models$Complexity,
                                levels = c("Low", "Medium", "High"))

dis_box <- ggplot(data = dis_models) + 
  geom_boxplot(aes(x = Complexity, y = N)) +
  xlab("Model Complexity") +
  ylab("Sample Size") +
  NULL

####### Boxplot Removal Models by Sample Size #####

rem_models <- rem_best[, c("Species", "N", "Model")]
rem_models$Complexity <- ifelse(rem_models$Model < 7, "Medium", "High")
rem_models[which(rem_models$Model <= 3),
           "Complexity"] <- "Low"
rem_models$Complexity <- factor(rem_models$Complexity,
                                levels = c("Low", "Medium", "High"))

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
  geom_boxplot(aes(x = Complexity, y = n, fill = Model)) +
  xlab("Model Complexity") +
  ylab("Sample Size") +
  scale_fill_viridis(discrete=TRUE) +
  NULL
####### Output Results ############################

write.table(rem_best, file = "output/tables/removal_best.csv", sep = ",", row.names = FALSE)
write.table(dis_best, file = "output/tables/distance_best.csv", sep = ",", row.names = FALSE)
write.table(model_table, file = "output/tables/model_table.csv", sep = ",", row.names = TRUE)

png(filename = "output/plots/heatmap_all.png",
    width = 7, height = 5, units = "in", res = 600)
print(heatmap_all)
dev.off()

pdf(file = "output/plots/model_heatmap_family.pdf")
print(plot_list)
dev.off()

png(filename = "output/plots/heatmap_family_matrix.png",
    width = 7, height = 6, units = "in", res = 600)
print(family_model_matrix)
dev.off()

png(filename = "output/plots/dis_model_box.png", 
    width = 6, height = 6, units = "in", res = 300)
print(dis_box)
dev.off()

png(filename = "output/plots/rem_model_box.png", 
    width = 6, height = 6, units = "in", res = 300)
print(rem_box)
dev.off()

png(filename = "output/plots/combined_model_box.png",
    width = 6, height = 6, units = "in", res = 600)
print(combined_box)
dev.off()
