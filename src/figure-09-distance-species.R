####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# figure-09-distance-species.R
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
mod <- coef_distance(species = sp, model = "best")$Model

####### Generate Map ##############################
bcr_coverage <- spatial_coverage(model = "dis",
                                 species = sp)
laea = st_crs("+proj=laea +lat_0=45 +lon_0=-95") 

bcr_coverage[which(bcr_coverage$ncounts == 0), "ncounts"] <- NA

bcr_coverage <- st_transform(bcr_coverage, crs = laea)
mp <- ggplot()+
  geom_sf(data = bcr_coverage,fill = viridis::cividis(1,begin = 1),colour = grey(0.75))+
  geom_sf(data = bcr_coverage,aes(fill = ncounts),colour = NA)+
  scale_color_viridis_c(aesthetics = "fill",direction = -1, na.value = "grey")+
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(fill = "Samples") +
  NULL

####### Generate Covariate Space ##################
to_plot <- covariates_distance(species = sp)
to_plot$Roadside_Status <- ifelse(to_plot$Road == 1, "On-Road", "Off-road")

dis_plot <- ggplot(data = to_plot, aes(x = Forest, fill = as.factor(Roadside_Status))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 25) +
  scale_fill_viridis(discrete=TRUE, name = "") +
  scale_color_viridis(discrete=TRUE) +
  ylab("Count") +
  xlab("Forest Coverage") +
  theme(legend.position="bottom") +
  NULL

####### Generate Distance Plot #####################

dis_values <- seq(25,400, by = 25)

i <- 1
sim_data <- vector(mode = "list", length = length(dis_values))
for (dis in dis_values)
{
  sim_data[[i]] <- percept(species = sp,
                           model = mod,
                           road = c(TRUE, FALSE),
                           forest = c(0,1),
                           distance = dis,
                           quantiles = c(0.025, 0.975))
  sim_data[[i]]$Radius <- dis
  
  i <- i + 1
}

sim_data <- do.call(rbind, sim_data)

forest_level <- c(1.0, 0.0)

plot_list <- vector(mode = "list", length = length(forest_level))
i <- 1
sim_data$Roadside_Status <- ifelse(sim_data$Road == 1, "On-Road", "Off-road")

for (fc in c(1.0, 0.0))
{
  plot_list[[i]] <- 
    ggplot(data = sim_data[which(sim_data$Forest == fc),]) +
    geom_line(aes(x = Radius, y = q_est, color = as.factor(Roadside_Status)), size = 1.5) +
    geom_ribbon(aes(x = Radius, ymin = q_2.5, ymax = q_97.5, color = as.factor(Roadside_Status)),
                alpha = 0.25, linetype = 0) +
    ylim(0, 1) +
    xlab("Distance from Observer") +
    ylab("Perceptibility (q)") +
    scale_color_viridis(name = "Roadside Status", discrete=TRUE) +
    NULL
  i <- i + 1  
}

####### Output Plot ###############################

png(filename = "output/plots/Fig9-distance-species.png",
    width = 7, height = 7, units = "in", res = 600)
ggarrange(ggarrange(mp, dis_plot, nrow = 2, labels = c("A", "B")),
          ggarrange(plotlist = plot_list, 
                    nrow = 2, legend = "bottom", common.legend = TRUE,
                    labels = c("C", "D")),
          ncol = 2,
          widths = c(1,1.5))
dev.off()