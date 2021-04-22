####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 11-distance-species-overview.R
# Created April 2021
# Last Updated April 2021

####### Import Libraries and External Files #######

library(ggplot2)
library(GGally)
library(ggpubr)
library(viridis)
theme_set(theme_pubclean())

####### Set Constants #############################

species <- c("CARW")

####### Read Data #################################

load("results-master/quant-summary/dis_species_summary.rda")
load("results-master/spatial-summary/dis_coverage_bcr.rda")

####### Generate Species Overview #################

for (sp in species)
{
  bcr_coverage <- bcr_dis_coverage[[sp]]
  mp <- ggplot()+
    geom_sf(data = bcr_coverage,fill = viridis::cividis(1,begin = 1),colour = grey(0.75))+
    geom_sf(data = bcr_coverage,aes(fill = ncounts),colour = NA)+
    scale_color_viridis_c(aesthetics = "fill",direction = -1)+
    theme(legend.position = c(1, 0.2)) +
    labs(fill = "Samples") +
    NULL
  
  fc_hist <- ggplot(data = dis_species_summary[[sp]]) +
    geom_histogram(aes(x = ForestOnly_5x5)) +
    xlab("Forest Coverage") +
    ylab("Sampling Events") +
    NULL
  
  road_hist <- ggplot(data = dis_species_summary[[sp]]) +
    geom_bar(aes(x = ifelse(roadside == 1, "On-Road", "Off-Road"))) +
    xlab("Roadside Status") +
    ylab("Sampling Events") +
    NULL
  
  png(filename = paste0("output/plots/distance/",
                        sp,
                        "_overview.png"),
      width = 8, height = 8, units = "in", res = 300)
  ggarrange(mp, 
            ggarrange(fc_hist, road_hist, ncol = 2, labels = c("b)", "c)")),
            nrow = 2,
            labels = "a)")
  dev.off()
}

