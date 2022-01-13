####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# figure-02-covariate-space.R
# Created January 2022
# Last Updated January 2022

####### Import Libraries and External Files #######

library(ggplot2)
library(ggridges)
library(viridis)
library(ggpubr)
library(napops)
theme_set(theme_pubclean())

####### Read Data #################################

rem_covars <- covariates_removal()
dis_covars <- covariates_distance()

####### Generate Removal Covariate Space ##########

rem_plot <- ggplot(rem_covars, aes(x = OD, y = TSSR) ) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", name = "Count", direction = -1) +
  theme(legend.position="bottom") +
  NULL

####### Generate Distance Covariate Space ##########

dis_covars$Roadside <- ifelse(dis_covars$Road == 1, "On-Road", "Off-Road")

dis_plot <- ggplot(data = dis_covars, aes(x = Forest, fill = Roadside)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 25) +
  scale_fill_viridis(discrete=TRUE, name = "") +
  scale_color_viridis(discrete=TRUE) +
  ylab("Count") +
  xlab("Forest Coverage") +
  theme(legend.position="bottom") +
  NULL

####### Output Composite Figure ###################

png("output/plots/coverage_covariates.png",
    width = 6, height = 8, units = "in", res = 300)
ggarrange(mp, 
          ggarrange(rem_plot, dis_plot, ncol = 2, labels = c("b)", "c)")),
          nrow = 2,
          labels = "a)")
dev.off()
