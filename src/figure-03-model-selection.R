####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# figure-03-model-selection.R
# Created January 2022
# Last Updated January 2022

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

source("src/rm-non-sp.R")

####### Read Data #################################

dis_best <- coef_distance(model = "best")
rem_best <- coef_removal(model = "best")
ibp_codes <- read.csv("data/IBP-Alpha-Codes20.csv")
families <- read.csv("data/NACC_list_species.csv")

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
  plot_list[[f]] <- ggplot(model_table_full,
                           aes(x = as.numeric(Removal),
                               y = as.numeric(Distance), 
                               fill = as.integer(Frequency), 
                               label = as.integer(Frequency))) +
    geom_tile(aes()) +
    scale_fill_viridis_c(option = "B", direction = -1) +
    geom_fit_text(contrast = TRUE) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    ylab("Distance Model") +
    xlab("Removal Model") +
    scale_x_continuous(breaks = seq(1,9), labels = rem_model_strings) +
    scale_y_continuous(breaks = seq(1,5), labels = dis_model_strings) +
    ggtitle(paste0(f,", n = ",
                   length(species)))+ 
    coord_fixed() +
    annotate("text", x = 1:9, y = 6, label = as.character(aggregate(as.integer(model_table_full$Frequency), 
                                                                    by = list(Removal = model_table_full$Removal), 
                                                                    FUN = sum)$x)) +
    annotate("text", x = 10, y = 1:5, label = as.character(aggregate(as.integer(model_table_full$Frequency), 
                                                                     by = list(Distance = model_table_full$Distance), 
                                                                     FUN = sum)$x)) +
    NULL
}


####### Output Results ############################

write.table(rem_best, file = "output/tables/removal_best.csv", sep = ",", row.names = FALSE)
write.table(dis_best, file = "output/tables/distance_best.csv", sep = ",", row.names = FALSE)

png(filename = "output/plots/Fig3-model-selection.png",
    width = 7, height = 5, units = "in", res = 600)
print(heatmap_all)
dev.off()

pdf(file = "output/plots/model_heatmap_family.pdf")
print(plot_list)
dev.off()
