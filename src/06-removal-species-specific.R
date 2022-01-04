####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 6-removal-species-overview.R
# Created April 2021
# Last Updated January 2022

####### Import Libraries and External Files #######

library(ggplot2)
library(sf)
library(GGally)
library(ggpubr)
library(viridis)
library(napops)
theme_set(theme_pubclean())

####### Set Constants #############################

species <- c("AMRO")

####### Generate Species Overview #################

for (sp in species)
{
  bcr_coverage <- get_spatial_coverage(model = "rem",
                                       species = sp)
  laea = st_crs("+proj=laea +lat_0=45 +lon_0=-95") 
  
  bcr_coverage[which(bcr_coverage$ncounts == 0), "ncounts"] <- NA
  
  bcr_coverage <- st_transform(bcr_coverage, crs = laea)
  mp <- ggplot()+
    geom_sf(data = bcr_coverage,fill = viridis::cividis(1,begin = 1),colour = grey(0.75))+
    geom_sf(data = bcr_coverage,aes(fill = ncounts),colour = NA)+
    scale_color_viridis_c(aesthetics = "fill",direction = -1, na.value = "grey")+
    theme(legend.position = "bottom") +
    labs(fill = "Samples") +
    NULL
  
  to_plot <- get_removal_covariates(project = FALSE,
                                    species = sp)
  
  rem_plot <- ggplot(to_plot, aes(x = OD, y = TSSR) ) +
    geom_hex() +
    scale_fill_continuous(type = "viridis", name = "Count") +
    theme(legend.position="bottom") +
    NULL
  
  png(filename = paste0("output/plots/removal/",
                        sp,
                        "_overview.png"),
      width = 8, height = 4, units = "in", res = 300)
  ggarrange(mp, rem_plot, nrow = 1, ncol = 2, labels = c("a)", "b)"))

  dev.off()
}

