####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# figure-08-removal-species.R
# Created April 2021
# Last Updated November 2022

####### Import Libraries and External Files #######

library(ggplot2)
library(sf)
library(GGally)
library(ggpubr)
library(viridis)
library(napops)
theme_set(theme_pubclean())

####### Set Constants #############################

sp <- c("AMRO")
mod <- coef_removal(species = sp, model = "best")$Model

####### Generate Map ##############################
bcr_coverage <- spatial_coverage(model = "rem",
                                     species = sp)
laea = st_crs("+proj=laea +lat_0=45 +lon_0=-95") 

bcr_coverage[which(bcr_coverage$ncounts == 0), "ncounts"] <- NA

bcr_coverage <- st_transform(bcr_coverage, crs = laea)
mp <- ggplot()+
  geom_sf(data = bcr_coverage,fill = viridis::cividis(1,begin = 1),colour = grey(0.75))+
  geom_sf(data = bcr_coverage,aes(fill = ncounts),colour = NA)+
  scale_color_viridis_c(aesthetics = "fill",direction = -1, na.value = "grey")+
  theme(legend.position = "bottom") +
  labs(fill = "Samples") +
  NULL
  
####### Generate Covariate Space ##################
to_plot <- covariates_removal(species = sp)

cov_plot <- ggplot(to_plot, aes(x = OD, y = TSSR) ) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", name = "Count", direction = -1) +
  theme(legend.position="bottom") +
  xlab("OD") +
  ylab("TSSR (Hours)") +
  NULL

####### Generate p vs OD Plot #####################

time_values <- c(1,3,5,10)

i <- 1
sim_data <- vector(mode = "list", length = length(time_values))
for (tv in time_values)
{
  sim_data[[i]] <- avail(species = sp,
                   model = mod,
                   tssr = 1.6,
                   od = c(91:212),
                   time = tv,
                   quantiles = c(0.025, 0.975))
  sim_data[[i]]$Time <- paste0(tv, " min")
  
  i <- i + 1
}

to_plot <- do.call(rbind, sim_data)
to_plot$Time <- factor(to_plot$Time,
                       levels = c("1 min",
                                  "3 min",
                                  "5 min",
                                  "10 min"))

plot_od <- 
  ggplot(data = to_plot) +
  geom_line(aes(x = OD, y = p_est, color = as.factor(Time))) +
  geom_ribbon(aes(x = OD, ymin = p_2.5, ymax = p_97.5, color = as.factor(Time)),
              alpha = 0.25) +
  ylim(0, 1) +
  theme(legend.position = "right") +
  scale_color_manual(values = c("#CC79A7", "#D55E00", "#0072B2", "#009E73"), name = "Survey Duration") +
  xlab("Ordinal Day") +
  ylab("Availability (p)") +
  NULL

####### Generate p vs TSSR Plot ###################

time_values <- c(1,3,5,10)

i <- 1
sim_data <- vector(mode = "list", length = length(time_values))
for (tv in time_values)
{
  sim_data[[i]] <- avail(species = sp,
                         model = mod,
                         tssr = seq(-2,6, by = 0.1),
                         od = 160,
                         time = tv,
                         quantiles = c(0.025, 0.975))
  sim_data[[i]]$Time <- paste0(tv, " min")
  
  i <- i + 1
}

to_plot <- do.call(rbind, sim_data)
to_plot$Time <- factor(to_plot$Time,
                       levels = c("1 min",
                                  "3 min",
                                  "5 min",
                                  "10 min"))

plot_tssr <- 
  ggplot(data = to_plot) +
  geom_line(aes(x = TSSR, y = p_est, color = as.factor(Time))) +
  geom_ribbon(aes(x = TSSR, ymin = p_2.5, ymax = p_97.5, color = as.factor(Time)),
              alpha = 0.25) +
  ylim(0, 1) +
  theme(legend.position = "right") +
  scale_color_manual(values = c("#CC79A7", "#D55E00", "#0072B2", "#009E73"), name = "Survey Duration") +
  xlab("Time Since Sunrise (Hours)") +
  ylab("Availability (p)") +
  NULL

####### Output Plot ###############################

png(filename = "output/plots/Fig8-removal-species.png",
    width = 7, height = 7, units = "in", res = 600)
ggarrange(ggarrange(mp, cov_plot, nrow = 2, labels = c("A", "B")),
          ggarrange(plot_od, plot_tssr, 
                    nrow = 2, legend = "bottom", common.legend = TRUE,
                    labels = c("C", "D")),
          ncol = 2,
          widths = c(1,1.5))
dev.off()


