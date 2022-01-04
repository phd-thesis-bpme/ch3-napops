####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 01-fig-coverage-map-covariates.R
# Created January 2021
# Last Updated January 2022

####### Import Libraries and External Files #######

library(ggplot2)
library(ggridges)
library(viridis)
library(ggpubr)
library(sf)
library(napops)
theme_set(theme_pubclean())

####### Read Data #################################

bcr_coverage <- get_spatial_coverage(model = "all")
rem_covars <- get_removal_covariates()
dis_covars <- get_distance_covariates()

####### Generate Map ##############################

laea = st_crs("+proj=laea +lat_0=45 +lon_0=-95") 

bcr_coverage[which(bcr_coverage$ncounts == 0), "ncounts"] <- NA

bcr_coverage <- st_transform(bcr_coverage, crs = laea)
#png("output/plots/coverage-map.png", width = 8, height = 6, res = 300, units = "in")
mp = ggplot()+
  geom_sf(data = bcr_coverage,fill = viridis::cividis(1,begin = 1))+
  geom_sf(data = bcr_coverage,aes(fill = ncounts),colour = NA)+
  scale_color_viridis_c(aesthetics = "fill",direction = -1, na.value = "grey")+
  theme(legend.position = c(1, 0.2)) +
  labs(fill = "Samples") +
  NULL
#print(mp)
#dev.off()

####### Generate Removal Covariate Space ##########

rem_plot <- ggplot(rem_covars, aes(x = OD, y = TSSR) ) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", name = "Count")

####### Generate Distance Covariate Space ##########

dis_covars$Roadside <- ifelse(dis_covars$Road == 1, "On-Road", "Off-Road")

dis_plot <- ggplot(data = dis_covars, aes(x = Forest, fill = Roadside)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 25) +
  scale_fill_viridis(discrete=TRUE, name = "Roadside Status") +
  scale_color_viridis(discrete=TRUE) +
  ylab("Count") +
  xlab("Forest Coverage") +
  NULL

# dis_plot_onroad <- ggplot(data = dis_covars[which(dis_covars$Roadside == "On-Road"), ]) +
#   geom_histogram(aes(x = Forest), bins = 25) +
#   ylab("Count") +
#   xlab("") +
#   NULL
# 
# dis_plot_offroad <- ggplot(data = dis_covars[which(dis_covars$Roadside == "Off-Road"), ]) +
#   geom_histogram(aes(x = Forest), bins = 25) +
#   xlab("Count") +
#   ylab("Forest Coverage") +
#   NULL

####### Output Composite Figure ###################

png("output/plots/coverage_covariates.png",
    width = 8, height = 8, units = "in", res = 300)
ggarrange(mp, 
          ggarrange(rem_plot, dis_plot, ncol = 2, labels = c("b)", "c)")),
          nrow = 2,
          labels = "a)")
dev.off()
