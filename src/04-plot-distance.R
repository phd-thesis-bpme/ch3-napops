####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 4-plot-distance.R
# Created January 2021
# Last Updated October 2021

####### Import Libraries and External Files #######

library(ggplot2)
library(GGally)
library(ggpubr)
library(viridis)
theme_set(theme_pubclean())

####### Set Constants #############################

sp_figure <- c("AMRO")

####### Read Data #################################

best_mod <- read.csv(paste0("output/tables/distance_",
                            sp_figure,
                            ".csv"))[1,"model"]
load(paste0("../results/simulations/tau/tau_", best_mod, ".rda"))

####### Plot by Species ###########################
forest_level <- c(1.0, 0.0)

sp <- sp_figure

plot_list <- vector(mode = "list", length = length(forest_level))
i <- 1

sim_data <- eval(parse(text = paste0("tau_", best_mod)))
sim_data <- sim_data[which(sim_data$Species == sp),]
sim_data$Roadside_Status <- ifelse(sim_data$Roadside == 1, "On-Road", "Off-road")

for (fc in c(1.0, 0.0))
{
  plot_list[[i]] <- 
    ggplot(data = sim_data[which(sim_data$Forest == fc &
                                   sim_data$Species == sp),]) +
    geom_line(aes(x = Radius, y = q, color = as.factor(Roadside_Status))) +
    geom_ribbon(aes(x = Radius, ymin = q_2.5, ymax = q_97.5, color = as.factor(Roadside_Status)),
                alpha = 0.25, linetype = 0) +
    ylim(0, 1) +
    scale_color_viridis(name = "Roadside Status", discrete=TRUE) +
    NULL
  i <- i + 1  
}

figure <- ggmatrix(
  plot_list,
  ncol = length(forest_level),
  nrow = 1,
  xAxisLabels = c("Forest", "Non-forest"),
  legend = 2
)

figure$title <- NULL
figure$xlab <- "Survey Distance"
figure$ylab <- "Perceptibility (q)"

png(filename = paste0("output/plots/distance/",
                      sp,
                      "_figure.png"),
    width = 6, height = 4, units = "in", res = 300)
print(figure)
dev.off()
