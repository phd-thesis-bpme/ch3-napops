####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# plot-distance.R
# Created January 2021
# Last Updated January 2021

####### Import Libraries and External Files #######

library(ggplot2)
library(GGally)
library(ggpubr)
theme_set(theme_pubclean())

####### Read Data #################################

# "simulate-tau.R" MUST BE RUN before trying to read in tau_sim.csv!!
sim_data <- read.csv("tables/tau_sim.csv")
family <- read.csv("../utilities/NACC_list_species.csv")[, c("common_name",
                                                             "family")]
ibp <- read.csv("../utilities/IBP-Alpha-Codes20.csv")[, c("SPEC",
                                                          "COMMONNAME")]

####### Plot All Species with Mean ################

forest_level <- c(1.0, 0.0)

# Create a variable where you can separate roadside status but species
sim_data$SpeciesRoad <- paste0(sim_data$Species, sim_data$Roadside)

# Empty plot list
plot_list <- vector(mode = "list", length = length(forest_level))
i <- 1

for (fc in c(1.0, 0.0))
{
  plot_list[[i]] <- 
    ggplot(data = sim_data[which(sim_data$Forest == fc),]) +
    geom_line(aes(x = Radius, y = q, group = SpeciesRoad, color = as.factor(Roadside)), alpha = 0.05) +
    stat_summary(aes(x = Radius, y = q, group = as.factor(Roadside), color = as.factor(Roadside)), fun = mean, geom = "smooth", size = 1.25) +
    ylim(0, 1) +
    #theme(legend.position = "none") +
    NULL
  i <- i + 1  
}

plot_matrix <- ggmatrix(
  plot_list,
  ncol = length(forest_level),
  nrow = 1,
  xAxisLabels = c("Forest", "Non-forest")
)
png(filename = "plots/distance/distance_all_species.png",
    width = 7, height = 4, units = "in", res = 300)
print(plot_matrix)
dev.off()

####### Plot by Species ###########################

sm_list <- vector(mode = "list", length = length(unique(sim_data$Species)))
names(sm_list) <- unique(sim_data$Species)

for (sp in unique(sim_data$Species))
{
  # Empty plot list
  plot_list <- vector(mode = "list", length = length(forest_level))
  i <- 1
  
  for (fc in c(1.0, 0.0))
  {
    plot_list[[i]] <- 
      ggplot(data = sim_data[which(sim_data$Forest == fc &
                                     sim_data$Species == sp),]) +
      geom_line(aes(x = Radius, y = q, color = as.factor(Roadside))) +
      geom_ribbon(aes(x = Radius, ymin = q_2.5, ymax = q_97.5, color = as.factor(Roadside)),
                  alpha = 0.25) +
      #stat_summary(aes(x = Radius, y = q, group = as.factor(Roadside), color = as.factor(Roadside)), fun = mean, geom = "smooth", size = 1.25) +
      ylim(0, 1) +
      #theme(legend.position = "none") +
      NULL
    i <- i + 1  
  }
  
  plot_matrix <- ggmatrix(
    plot_list,
    ncol = length(forest_level),
    nrow = 1,
    xAxisLabels = c("Forest", "Non-forest"),
    title = paste0("Species: ",
                   sp)
  )
  
  sm_list[[sp]] <- plot_matrix
}

save(sm_list, file = "plots/distance/distance_plot_list.rda")

pdf(file = paste0("plots/distance/distance_species.pdf"))
print(sm_list)
dev.off() 
