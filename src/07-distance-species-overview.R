####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 7-distance-species-overview.R
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
  bcr_coverage <- get_spatial_coverage(model = "dis",
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
  
  dis_covars <- get_distance_covariates(project = FALSE,
                                        species = sp)
  
  dis_covars$Roadside <- ifelse(dis_covars$Road == 1, "On-Road", "Off-Road")
  
  dis_plot <- ggplot(data = dis_covars, aes(x = Forest, fill = Roadside)) +
    geom_histogram(position = "identity", alpha = 0.6, bins = 25) +
    scale_fill_viridis(discrete=TRUE, name = "") +
    scale_color_viridis(discrete=TRUE) +
    ylab("Count") +
    xlab("Forest Coverage") +
    theme(legend.position="bottom") +
    NULL
  
  png(filename = paste0("output/plots/distance/",
                        sp,
                        "_overview.png"),
      width = 8, height = 4, units = "in", res = 300)
  ggarrange(mp, dis_plot, ncol = 2, nrow = 1, labels = c("a)", "b)"))

  dev.off()
}

