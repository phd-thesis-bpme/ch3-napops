####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 10-p-vs-q.R
# Created November 2021
# Last Updated November 2021

####### Import Libraries and External Files #######

library(ggplot2)
library(GGally)
library(ggpubr)
library(viridis)
#library(magrittr)
#library(napops)
theme_set(theme_pubclean())

####### Set Constants #############################

tau_files <- list.files(path = "../results/simulations/tau")
phi_files <- list.files(path = "../results/simulations/phi")
od <- 152
tssr <- 1
time <- 5
radius <- 100
cbPalette <- rev(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
families_to_plot <- c("Parulidae", "Passerellidae", "Tyrannidae",
                      "Picidae", "Icteridae", "Corvidae")

####### Read Data #################################

rem <- read.csv("output/tables/removal_best.csv")
dis <- read.csv("output/tables/distance_best.csv")
ibp_codes <- read.csv("../utilities/IBP-Alpha-Codes20.csv")
families <- read.csv("../utilities/NACC_list_species.csv")

for (f in tau_files)
{
  load(paste0("../results/simulations/tau/", f))
}

for (f in phi_files)
{
  load(paste0("../results/simulations/phi/", f))
}

####### Data Wrangling ############################

# Get family and orders for each species
species <- intersect(dis$Species, rem$Species)
species_names <- merge(data.frame(Species = species), ibp_codes[, c("SPEC", "COMMONNAME")],
                       by.x = "Species", by.y = "SPEC")
species_taxo <- merge(species_names, families[, c("common_name", "order", "family")],
                      by.x = "COMMONNAME", by.y = "common_name")

# Get the best model for each
species_taxo <- merge(species_taxo, rem[, c("Species", "model")],
                      by = "Species")
names(species_taxo)[ncol(species_taxo)] <- "Removal_Model"
species_taxo <- merge(species_taxo, dis[, c("Species", "model")],
                      by = "Species")
names(species_taxo)[ncol(species_taxo)] <- "Distance_Model"

# Drop species that we are not interested in plotting
species_taxo <- species_taxo[which(species_taxo$family %in% families_to_plot), ]

####### Null Model ################################

# Create empty columns in data frame
detect_df <- data.frame(Species = species_taxo$Species,
                        p = NA,
                        p25 = NA,
                        p975 = NA,
                        q = NA,
                        q25 = NA,
                        q975 = NA)

detect_df <- merge(species_taxo, detect_df, by = "Species")

phi <- phi_1[which(phi_1$Time == time &
                     phi_1$Species %in% detect_df$Species), ]
phi <- phi[!duplicated(phi$Species),]
phi <- phi[match(detect_df$Species, phi$Species),]

tau <- tau_1[which(tau_1$Radius == radius &
                     tau_1$Species %in% detect_df$Species), ]
tau <- tau[!duplicated(tau$Species), ]
tau <- tau[match(detect_df$Species, tau$Species), ]

detect_df$p <- phi$p
detect_df$p25 <- phi$p_2.5
detect_df$p975 <- phi$p_97.5
detect_df$q <- tau$q
detect_df$q25 <- tau$q_2.5
detect_df$q975 <- tau$q_97.5

detect_df$family <- factor(detect_df$family,
                           families_to_plot)

null_plot <- ggplot(data = detect_df[which(detect_df$family != "Other"),]) +
  geom_point(aes(x = p, y = q, colour = family), size = 2) +
  geom_errorbar(aes(x = p, ymin = q25, ymax = q975), alpha = 0.3) +
  geom_errorbarh(aes(y = q, xmin = p25, xmax = p975), alpha = 0.3) +
  scale_color_manual(values = cbPalette, position = "right") +
  xlab("Availability (p)") +
  ylab("Perceptibility (q)") +
  NULL

####### Forest x Road Grid ########################

plot_list <- vector(mode = "list", length = 4)

i <- 1
for (road in c(1, 0))
{
  for (forest in c(0.0, 1.0))
  {
    # Create empty columns in data frame
    detect_df <- data.frame(Species = species_taxo$Species,
                            p = NA,
                            p25 = NA,
                            p975 = NA,
                            q = NA,
                            q25 = NA,
                            q975 = NA)
    detect_df <- merge(species_taxo, detect_df, by = "Species")
    
    for (sp in detect_df$Species)
    {
      temp <- detect_df[which(detect_df$Species == sp),]
      phi <- eval(parse(text = paste0("phi_",
                                      temp$Removal_Model)))
      phi <- phi[which(phi$Time == time & 
                         phi$Species == sp &
                         phi$TSSR == tssr &
                         phi$JD == od), ]
      
      tau <- eval(parse(text = paste0("tau_",
                                      temp$Distance_Model)))
      tau <- tau[which(tau$Radius == radius & 
                         tau$Roadside == road &
                         tau$Forest == forest &
                         tau$Species == sp), ]
      
      detect_df[which(detect_df$Species == sp), ]$p <- phi$p
      detect_df[which(detect_df$Species == sp), ]$p25 <- phi$p_2.5
      detect_df[which(detect_df$Species == sp), ]$p975 <- phi$p_97.5
      detect_df[which(detect_df$Species == sp), ]$q <- tau$q
      detect_df[which(detect_df$Species == sp), ]$q25 <- tau$q_2.5
      detect_df[which(detect_df$Species == sp), ]$q975 <- tau$q_97.5
    }
    detect_df$family <- factor(detect_df$family,
                               families_to_plot)
    
    plot_list[[i]] <- ggplot(data = detect_df[which(detect_df$Removal_Model != 5),]) +
      geom_point(aes(x = p, y = q, colour = family), size = 2) +
      geom_errorbar(aes(x = p, ymin = q25, ymax = q975), alpha = 0.3) +
      geom_errorbarh(aes(y = q, xmin = p25, xmax = p975), alpha = 0.3) +
      scale_color_manual(values = cbPalette, position = "right") +
      xlab("Availability (p)") +
      ylab("Perceptibility (q)") +
      NULL
    
    i <- i + 1
  }
}

plot_matrix <- ggmatrix(
  plot_list,
  ncol = 2,
  nrow = 2,
  xAxisLabels = c("Non-forest", "Forest"),
  yAxisLabels = c("Roadside", "Offroad"),
  legend = 2)

####### Output Results ############################

png(filename = "output/plots/family_detectability_null.png", 
    width = 7, height = 7, units = "in", res = 600)
null_plot
dev.off()

png(filename = "output/plots/family_detectability_matrix.png",
    width = 7, height = 7, units = "in", res = 600)
plot_matrix
dev.off()
