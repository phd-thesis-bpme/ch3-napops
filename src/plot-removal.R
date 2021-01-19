####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# plot-removal.R
# Created January 2021
# Last Updated January 2021

####### Import Libraries and External Files #######

library(ggplot2)
library(GGally)
library(ggpubr)
theme_set(theme_pubclean())

####### Read Data #################################

sim_data <- read.csv("tables/phi_sim.csv")
family <- read.csv("../utilities/NACC_list_species.csv")[, c("common_name",
                                                             "family")]
ibp <- read.csv("../utilities/IBP-Alpha-Codes20.csv")[, c("SPEC",
                                                          "COMMONNAME")]
load("data/var-covar/rem_vcv_best.rda")

####### Plot All Species with Mean (JD) ###########

# Hold TSSR (we'll call it sr here) constant at 1
sr <- 1

# Empty plot list
plot_list <- vector(mode = "list", length = length(time_values))

i <- 1

for (tv in time_values)
{
  plot_list[[i]] <- 
    ggplot(data = sim_data[which(sim_data$TSSR == sr & 
                                   sim_data$Time == tv),]) +
    geom_line(aes(x = JD, y = p, group = Species), alpha = 0.05) +
    stat_summary(aes(x = JD, y = p), fun = mean, geom = "smooth", size = 1.25) +
    ylim(0, 1) +
    #theme(legend.position = "none") +
    NULL
  i <- i + 1
}

plot_matrix <- ggmatrix(
  plot_list,
  ncol = length(time_values),
  nrow = 1,
  xAxisLabels = c("1 min", "3 min", "5 min", "10 min")
)

png(filename = "plots/removal/removal_jd_all_species.png",
    width = 7, height = 3, units = "in", res = 300)
print(plot_matrix)
dev.off()

####### Plot All Species with Mean (TSSR) ###########

# Hold JD (we'll call it day here) constant at 152 (i.e. June 1)
day <- 152

# Empty plot list
plot_list <- vector(mode = "list", length = length(time_values))

i <- 1

for (tv in time_values)
{
  plot_list[[i]] <- 
    ggplot(data = sim_data[which(sim_data$JD == day & 
                                   sim_data$Time == tv),]) +
    geom_line(aes(x = TSSR, y = p, group = Species), alpha = 0.05) +
    stat_summary(aes(x = TSSR, y = p), fun = mean, geom = "smooth", size = 1.25) +
    ylim(0, 1) +
    #theme(legend.position = "none") +
    NULL
  i <- i + 1
}

plot_matrix <- ggmatrix(
  plot_list,
  ncol = length(time_values),
  nrow = 1,
  xAxisLabels = c("1 min", "3 min", "5 min", "10 min")
)

png(filename = "plots/removal/removal_tssr_all_species.png",
    width = 7, height = 3, units = "in", res = 300)
print(plot_matrix)
dev.off()

####### Plot by Species (JD) ######################

sm_list <- vector(mode = "list", length = length(unique(sim_data$Species)))
sm <- 1

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
      #stat_summary(aes(x = JD, y = p), fun = mean, geom = "smooth", size = 1.25) +
      ylim(0, 1) +
      theme(legend.position = "none")
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
  
  sm_list[[sm]] <- plot_matrix
  sm <- sm + 1
}

pdf(file = paste0("plots/removal/removal_jd_species.pdf"), width = 7, height = 3)
print(sm_list)
dev.off() 

####### Plot by Family (TSSR) #####################

sm_list <- vector(mode = "list", length = length(unique(sim_data$Species)))
sm <- 1

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
      theme(legend.position = "none")
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
  
  sm_list[[sm]] <- plot_matrix
  sm <- sm + 1
}

pdf(file = paste0("plots/removal/removal_tssr_species.pdf"), width = 7, height = 3)
print(sm_list)
dev.off() 

####### Plot by Family (JD) #######################

# Add Family grouping
family <- merge(x = family, y = ibp,
                by.x = "common_name", by.y = "COMMONNAME")
sim_data <- merge(x = sim_data, y = family[, c("SPEC", "family")],
                  by.x = "Species", by.y = "SPEC")

pm_list <- vector(mode = "list", length = length(unique(sim_data$family)))
pm <- 1

for (f in sort(unique(sim_data$family)))
{
  # Empty plot list
  plot_list <- vector(mode = "list", length = length(time_values))
  
  i <- 1
  n_sp <- length(unique(sim_data[which(sim_data$family == f), "Species"]))
  
  for (tv in time_values)
  {
    plot_list[[i]] <- 
      ggplot(data = sim_data[which(sim_data$TSSR == sr & 
                                     sim_data$Time == tv &
                                     sim_data$family == f),]) +
      geom_line(aes(x = JD, y = p, group = Species), alpha = 0.2) +
      stat_summary(aes(x = JD, y = p), fun = mean, geom = "smooth", size = 1.25) +
      ylim(0, 1) +
      theme(legend.position = "none")
    i <- i + 1
  }
  
  plot_matrix <- ggmatrix(
    plot_list,
    ncol = length(time_values),
    nrow = 1,
    xAxisLabels = c("1 min", "3 min", "5 min", "10 min"),
    title = paste0("Family ",
                   f,
                   " (n = ",
                   n_sp,
                   ")")
  )
  
  pm_list[[pm]] <- plot_matrix
  pm <- pm + 1
}

pdf(file = paste0("plots/removal/removal_jd_families.pdf"), width = 7, height = 3)
print(pm_list)
dev.off() 

####### Plot by Family (TSSR) #####################

pm_list <- vector(mode = "list", length = length(unique(sim_data$family)))
pm <- 1

for (f in sort(unique(sim_data$family)))
{
  # Empty plot list
  plot_list <- vector(mode = "list", length = length(time_values))
  
  i <- 1
  n_sp <- length(unique(sim_data[which(sim_data$family == f), "Species"]))
  
  for (tv in time_values)
  {
    plot_list[[i]] <- 
      ggplot(data = sim_data[which(sim_data$JD == day & 
                                     sim_data$Time == tv &
                                     sim_data$family == f),]) +
      geom_line(aes(x = TSSR, y = p, group = Species), alpha = 0.2) +
      stat_summary(aes(x = TSSR, y = p), fun = mean, geom = "smooth", size = 1.25) +
      ylim(0, 1) +
      theme(legend.position = "none")
    i <- i + 1
  }
  
  plot_matrix <- ggmatrix(
    plot_list,
    ncol = length(time_values),
    nrow = 1,
    xAxisLabels = c("1 min", "3 min", "5 min", "10 min"),
    title = paste0("Family ",
                   f,
                   " (n = ",
                   n_sp,
                   ")")
  )
  
  pm_list[[pm]] <- plot_matrix
  pm <- pm + 1
}

pdf(file = paste0("plots/removal/removal_tssr_families.pdf"), width = 7, height = 3)
print(pm_list)
dev.off() 
