####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 5-plot-removal.R
# Created January 2021
# Last Updated December 2021

####### Import Libraries and External Files #######

library(ggplot2)
library(GGally)
library(ggpubr)
library(viridis)
theme_set(theme_pubclean())

####### Set Constants #############################

sp_figure <- c("AMRO")

####### Read Data #################################

best_mod <- read.csv(paste0("output/tables/removal_",
                            sp_figure,
                            ".csv"))[1,"model"]
load(paste0("../results/simulations/phi/phi_", best_mod, ".rda"))

####### Plot by Species (OD) ######################

# Hold TSSR (we'll call it sr here) constant at 1
sr <- 1
time_values <- c(1,3,5,10)

# sm_list <- vector(mode = "list", length = length(unique(sim_data$Species)))
# names(sm_list) <- unique(sim_data$Species)

sp <- sp_figure

# Empty plot list
plot_list <- vector(mode = "list", length = length(time_values))

i <- 1

sim_data <- eval(parse(text = paste0("phi_", best_mod)))

for (tv in time_values)
{
  plot_list[[i]] <- 
    ggplot(data = sim_data[which(sim_data$TSSR == sr & 
                                   sim_data$Time == tv &
                                   sim_data$Species == sp),]) +
    geom_line(aes(x = OD, y = p, color = as.factor(1))) +
    geom_ribbon(aes(x = OD, ymin = p_2.5, ymax = p_97.5, color = NA),
                alpha = 0.25) +
    ylim(0, 1) +
    theme(legend.position = "none") +
    scale_color_viridis(discrete=TRUE) +
    NULL
  i <- i + 1
}

od_figure <- ggmatrix(
  plot_list,
  ncol = length(time_values),
  nrow = 1,
  xAxisLabels = c("1 min", "3 min", "5 min", "10 min")
)

#sm_list[[sp]] <- plot_matrix

od_figure$title <- NULL
od_figure$xlab <- "Ordinal Day"
od_figure$ylab <- "Availability (p)"

png(filename = paste0("output/plots/removal/",
                      sp,
                      "_od.png"),
    width = 6, height = 3, units = "in", res = 300)
print(od_figure)
dev.off()

####### Plot by Species (TSSR) ####################

# Hold OD (we'll call it day here) constant at 160 (i.e. June 9)
day <- 160
  
# Empty plot list
plot_list <- vector(mode = "list", length = length(time_values))

i <- 1

for (tv in time_values)
{
  plot_list[[i]] <- 
    ggplot(data = sim_data[which(sim_data$OD == day & 
                                   sim_data$Time == tv &
                                   sim_data$Species == sp),]) +
    geom_line(aes(x = TSSR, y = p, color = as.factor(1))) +
    geom_ribbon(aes(x = TSSR, ymin = p_2.5, ymax = p_97.5, color = NA),
                alpha = 0.25) +
   # stat_summary(aes(x = TSSR, y = p), fun = mean, geom = "smooth", size = 1.25) +
    ylim(0, 1) +
    theme(legend.position = "none") +
    scale_color_viridis(discrete=TRUE) +
    NULL
  i <- i + 1
}

tssr_figure <- ggmatrix(
  plot_list,
  ncol = length(time_values),
  nrow = 1,
  xAxisLabels = c("1 min", "3 min", "5 min", "10 min")
)

#sm_list[[sp]] <- plot_matrix

tssr_figure$title <- NULL
tssr_figure$xlab <- "Time Since Local Sunrise"
tssr_figure$ylab <- "Availability (p)"

png(filename = paste0("output/plots/removal/",
                      sp,
                      "_tssr.png"),
    width = 6, height = 3, units = "in", res = 300)
print(tssr_figure)
dev.off()

par(mfrow = c(2,1))
od_figure
tssr_figure

