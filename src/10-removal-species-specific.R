####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 10-removal-species-overview.R
# Created April 2021
# Last Updated April 2021

####### Import Libraries and External Files #######

library(ggplot2)
library(GGally)
library(ggpubr)
library(viridis)
theme_set(theme_pubclean())

####### Set Constants #############################

species <- c("SOSP", "GHOW")

####### Read Data #################################

load("results-master/quant-summary/rem_species_summary.rda")
load("results-master/spatial-summary/rem_coverage_bcr.rda")

####### Generate Species Overview #################

for (sp in species)
{
  bcr_coverage <- bcr_rem_coverage[[sp]]
  mp <- ggplot()+
    geom_sf(data = bcr_coverage,fill = viridis::cividis(1,begin = 1),colour = grey(0.75))+
    geom_sf(data = bcr_coverage,aes(fill = ncounts),colour = NA)+
    scale_color_viridis_c(aesthetics = "fill",direction = -1)+
    theme(legend.position = c(1, 0.2)) +
    labs(fill = "Samples") +
    NULL
  
  jd_hist <- ggplot(data = rem_species_summary[[sp]]) +
    geom_histogram(aes(x = (JD*365))) +
    xlab("Julian Day") +
    ylab("Sampling Events") +
    NULL
  
  tssr_hist <- ggplot(data = rem_species_summary[[sp]]) +
    geom_histogram(aes(x = (TSSR*24))) +
    xlab("Time Since Local Sunrise") +
    ylab("Sampling Events") +
    NULL
  
  png(filename = paste0("output/plots/removal/",
                        sp,
                        "_overview.png"),
      width = 8, height = 8, units = "in", res = 300)
  ggarrange(mp, 
            ggarrange(jd_hist, tssr_hist, ncol = 2, labels = c("b)", "c)")),
            nrow = 2,
            labels = "a)")
  dev.off()
}

