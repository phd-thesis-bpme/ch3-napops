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
library(magrittr)
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

####### Generate Distance Max Radius Bins #########
dis_covars$Max_Distance <- max_survey_radius(code = dis_covars$Survey_Method) %>%
  factor(levels = c("30", "75", "100", "150", "400", "Inf"))

radius_bins <- ggplot(data = dis_covars, aes(x = Max_Distance)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
  xlab("Maximum Survey Radius (m)") +
  ylab("Count") +
  ylim(c(0,481000)) +
  NULL

####### Generate Removal Max Time Bins ############
rem_covars$Max_Time <- max_survey_time(code = rem_covars$Survey_Method) %>%
  factor(levels = c("3", "5", "6", "8", "10"))

time_bins <- ggplot(data = rem_covars, aes(x = Max_Time)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
  xlab("Maximum Survey Time (min)") +
  ylab("Count") +
  ylim(c(0,198000)) +
  NULL

####### Output Composite Figure ###################

png("output/plots/Fig2-covariates.png",
    width = 7.5, height = 7.5, units = "in", res = 1200)
ggarrange(rem_plot, dis_plot, 
          time_bins, radius_bins,
          ncol = 2, nrow = 2,
          labels = c("A", "B", "C", "D"))
dev.off()
