####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 9-plot-removal-geofacet.R
# Created April 2021
# Last Updated April 2021

####### Import Libraries and External Files #######

library(ggplot2)
library(geofacet)
library(ggpubr)
theme_set(theme_pubclean())

####### Set Constants #############################
bcr_grid <- data.frame(
  code = c("BCR3", "BCR1", "BCR7", "BCR2", "BCR4", "BCR8", "BCR6", "BCR12", "BCR11", "BCR5", "BCR10", "BCR17", "BCR13", "BCR14", "BCR23", "BCR16", "BCR9", "BCR32", "BCR19", "BCR18", "BCR28", "BCR22", "BCR24", "BCR30", "BCR33", "BCR15", "BCR39", "BCR20", "BCR21", "BCR25", "BCR26", "BCR29", "BCR36", "BCR37", "BCR34", "BCR35", "BCR27", "BCR40", "BCR31", "BCR38"),
  name = c("BCR3", "BCR1", "BCR7", "BCR2", "BCR4", "BCR8", "BCR6", "BCR12", "BCR11", "BCR5", "BCR10", "BCR17", "BCR13", "BCR14", "BCR23", "BCR16", "BCR9", "BCR32", "BCR19", "BCR18", "BCR28", "BCR22", "BCR24", "BCR30", "BCR33", "BCR15", "BCR39", "BCR20", "BCR21", "BCR25", "BCR26", "BCR29", "BCR36", "BCR37", "BCR34", "BCR35", "BCR27", "BCR40", "BCR31", "BCR38"),
  row = c(1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8),
  col = c(5, 1, 7, 1, 2, 7, 3, 7, 4, 1, 2, 4, 8, 9, 7, 3, 2, 1, 5, 4, 8, 6, 7, 9, 3, 2, 1, 4, 5, 6, 7, 8, 5, 6, 3, 4, 8, 2, 8, 3),
  stringsAsFactors = FALSE
)

####### Read Data #################################

load("output/tables/phi_sim_bcr_list.rda")

####### Plot Geofacet #############################

for (sp in names(sim_data_list))
{
  sim_data <- sim_data_list[[sp]]
  
  # Simulate p 
  sim_data$Time <- 5
  sim_data$p <- 1 - exp(-(sim_data$Time * sim_data$phi))
  sim_data$p_2.5 <- 1 - exp(-(sim_data$Time * sim_data$phi_2.5))
  sim_data$p_97.5 <- 1 - exp(-(sim_data$Time * sim_data$phi_97.5))
  
  # Hold TSSR (we'll call it sr here) constant at 1
  sr <- 1
  
  sim_data$BCR_name <- paste0("BCR", sim_data$BCR)

  p <- ggplot(data = sim_data[which(sim_data$TSSR == sr), ]) +
    geom_line(aes(x = JD, y = p)) +
    geom_ribbon(aes(x = JD, ymin = p_2.5, ymax = p_97.5),
                alpha = 0.25) +
    ylim(0, 1) +
    theme(legend.position = "none") +
    facet_geo(~ BCR_name, grid = bcr_grid)
  
  png(filename = paste0("output/plots/removal/",
                        sp,
                        "_geo.png"),
      width = 16, height = 12, res = 300, units = "in")
  print(p)
  dev.off()
}

