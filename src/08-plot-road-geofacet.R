####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 8-plot-road-geofacet.R
# Created October 2021
# Last Updated October 2021

####### Import Libraries and External Files #######

library(ggplot2)
library(geofacet)
library(ggpubr)
library(sf)
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

load("../results/quant-summary/dis_species_summary.rda")
bcr <- read_sf("../utilities/shp/bcr",
               layer = "BBS_BCR_strata")

####### Plot Roadside Geofacet ####################

pdf(file = "output/plots/distance/roadside_geofacet.pdf",
    width = 14, height = 8.5)

for (sp in names(dis_species_summary))
{
  df <- dis_species_summary[[sp]]
  
  coords <- matrix(c(df$Longitude, df$Latitude),
                   nrow = nrow(df))
  
  # Get BCR
  pts <- st_as_sf(data.frame(coords), coords = 1:2, crs = 4326)
  bcr <- bcr %>% st_transform(st_crs(pts))
  bcr_names <- bcr$ST_12
  intersections <- as.integer(st_nearest_feature(pts, bcr))
  df$BCR <- bcr_names[intersections]
  
  p <- ggplot(data = df) +
    geom_bar(aes(x = ifelse(roadside == 1, "On", "Off"))) +
    theme(legend.position = "none") +
    facet_geo(~ BCR, grid = bcr_grid) +
    labs(title = sp) +
    xlab("Roadside Status") +
    ylab("Sampling Events") +
    NULL
  print(p)
}

dev.off()