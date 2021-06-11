####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 3-coverage-map.R
# Created January 2021
# Last Updated April 2021

####### Import Libraries and External Files #######

library(ggplot2)
library(ggpubr)
library(sf)
theme_set(theme_pubclean())

####### Read Data #################################

# Load BCR map
project_list <- load("../results/spatial-summary/project_coverage_bcr.rda")

####### Generate Map ##############################

laea = st_crs("+proj=laea +lat_0=45 +lon_0=-95") 

bcr_coverage[which(bcr_coverage$ncounts == 0), "ncounts"] <- NA

bcr_coverage <- st_transform(bcr_coverage, crs = laea)
png("output/plots/coverage-map.png", width = 8, height = 6, res = 300, units = "in")
mp = ggplot()+
  geom_sf(data = bcr_coverage,fill = viridis::cividis(1,begin = 1))+
  geom_sf(data = bcr_coverage,aes(fill = ncounts),colour = NA)+
  scale_color_viridis_c(aesthetics = "fill",direction = -1, na.value = "grey")+
  theme(legend.position = c(1, 0.2)) +
  labs(fill = "Samples") +
  NULL
print(mp)
dev.off()
