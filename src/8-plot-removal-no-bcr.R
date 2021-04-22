####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 8-plot-removal-no-bcr.R
# Created January 2021
# Last Updated April 2021

####### Import Libraries and External Files #######

library(ggplot2)
library(GGally)
library(ggpubr)
library(viridis)
theme_set(theme_pubclean())

####### Set Constants #############################

sp_figure <- "GHOW"

####### Read Data #################################

sim_data <- read.csv("output/tables/phi_sim_no_bcr.csv")
load("output/tables/phi_sim_bcr_list.rda")

####### Simulate p ################################

time_values <- c(1, 3, 5, 10)
time <- rep(time_values, times = nrow(sim_data))

sim_data <- sim_data[rep(seq_len(nrow(sim_data)),
                         each = length(time_values)), ]
sim_data$Time <- time
sim_data$p <- 1 - exp(-(sim_data$Time * sim_data$phi))
sim_data$p_2.5 <- 1 - exp(-(sim_data$Time * sim_data$phi_2.5))
sim_data$p_97.5 <- 1 - exp(-(sim_data$Time * sim_data$phi_97.5))

####### Plot by Species (JD) ######################

# Hold TSSR (we'll call it sr here) constant at 1
sr <- 1

sm_list <- vector(mode = "list", length = length(unique(sim_data$Species)))
names(sm_list) <- unique(sim_data$Species)

for (sp in unique(sim_data$Species))
{
  # Empty plot list
  plot_list <- vector(mode = "list", length = length(time_values))
  
  i <- 1
  
  for (tv in time_values)
  {
    plot_list[[i]] <- 
      ggplot(data = sim_data[which(sim_data$TSSR == sr & 
                                     sim_data$Time == tv &
                                     sim_data$Species == sp),]) +
      geom_line(aes(x = JD, y = p)) +
      geom_ribbon(aes(x = JD, ymin = p_2.5, ymax = p_97.5),
                  alpha = 0.25) +
      ylim(0, 1) +
      theme(legend.position = "none") +
      scale_color_viridis(discrete=TRUE) +
      NULL
    i <- i + 1
  }
  
  plot_matrix <- ggmatrix(
    plot_list,
    ncol = length(time_values),
    nrow = 1,
    xAxisLabels = c("1 min", "3 min", "5 min", "10 min"),
    title = paste0("Species ",
                   sp)
  )
  
  sm_list[[sp]] <- plot_matrix
}

jd_figure <- sm_list[[sp_figure]]

save(sm_list, file = "output/plots/removal/removal_jd_plot_list.rda")

pdf(file = paste0("output/plots/removal/removal_jd_species.pdf"), width = 7, height = 3)
print(sm_list)
dev.off() 

####### Plot by Species (TSSR) ####################

# Hold JD (we'll call it day here) constant at 152 (i.e. June 1)
day <- 152

sm_list <- vector(mode = "list", length = length(unique(sim_data$Species)))
names(sm_list) <- unique(sim_data$Species)

for (sp in unique(sim_data$Species))
{
  # Empty plot list
  plot_list <- vector(mode = "list", length = length(time_values))
  
  i <- 1
  
  for (tv in time_values)
  {
    plot_list[[i]] <- 
      ggplot(data = sim_data[which(sim_data$JD == day & 
                                     sim_data$Time == tv &
                                     sim_data$Species == sp),]) +
      geom_line(aes(x = TSSR, y = p)) +
      geom_ribbon(aes(x = TSSR, ymin = p_2.5, ymax = p_97.5),
                  alpha = 0.25) +
     # stat_summary(aes(x = TSSR, y = p), fun = mean, geom = "smooth", size = 1.25) +
      ylim(0, 1) +
      theme(legend.position = "none") +
      scale_color_viridis(discrete=TRUE) +
      NULL
    i <- i + 1
  }
  
  plot_matrix <- ggmatrix(
    plot_list,
    ncol = length(time_values),
    nrow = 1,
    xAxisLabels = c("1 min", "3 min", "5 min", "10 min"),
    title = paste0("Species ",
                   sp)
  )
  
  sm_list[[sp]] <- plot_matrix
}

tssr_figure <- sm_list[[sp_figure]]

save(sm_list, file = "output/plots/removal/removal_tssr_plot_list.rda")

pdf(file = paste0("output/plots/removal/removal_tssr_species.pdf"), width = 7, height = 3)
print(sm_list)
dev.off() 

####### Paper Figure ##############################

jd_figure$title <- NULL
jd_figure$xlab <- "Julian Day"
jd_figure$ylab <- "p"

png(filename = paste0("output/plots/removal/",
                      sp_figure,
                      "_jd.png"),
    width = 6, height = 3, units = "in", res = 300)
print(jd_figure)
dev.off()

tssr_figure$title <- NULL
tssr_figure$xlab <- "Time Since Local Sunrise"
tssr_figure$ylab <- "p"

png(filename = paste0("output/plots/removal/",
                      sp_figure,
                      "_tssr.png"),
    width = 6, height = 3, units = "in", res = 300)
print(tssr_figure)
dev.off()
