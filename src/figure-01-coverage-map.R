####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# figure-01-coverage-map.R
# Created January 2021
# Last Updated November 2022

####### Import Libraries and External Files #######

library(ggplot2)
library(viridis)
library(ggpubr)
library(sf)
library(napops)
theme_set(theme_pubclean())

####### Read Data #################################

bcr_coverage <- spatial_coverage(model = "all")
proj_coverage <- spatial_coverage(model = "nproj")

####### Generate Maps #############################

laea = st_crs("+proj=laea +lat_0=45 +lon_0=-95") 

bcr_coverage[which(bcr_coverage$ncounts == 0), "ncounts"] <- NA

bcr_coverage <- st_transform(bcr_coverage, crs = laea)
counts_map = ggplot()+
  geom_sf(data = bcr_coverage,fill = viridis::cividis(1,begin = 1))+
  geom_sf(data = bcr_coverage,aes(fill = ncounts),colour = NA)+
  scale_color_viridis_c(aesthetics = "fill",direction = -1, na.value = "grey")+
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(fill = "Samples") +
  NULL

proj_coverage[which(proj_coverage$ncounts == 0), "ncounts"] <- NA

proj_coverage <- st_transform(proj_coverage, crs = laea)
proj_map = ggplot()+
  geom_sf(data = proj_coverage,fill = viridis::cividis(1,begin = 1))+
  geom_sf(data = proj_coverage,aes(fill = ncounts),colour = NA)+
  scale_color_viridis_c(aesthetics = "fill",direction = -1, na.value = "grey")+
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(fill = "Projects") +
  NULL


####### Output Map ################################


png("output/plots/Fig1-coverage-map.png",
    width = 6, height = 4, units = "in", res = 600)
ggarrange(counts_map, proj_map, labels = c("A", "B"))
dev.off()
