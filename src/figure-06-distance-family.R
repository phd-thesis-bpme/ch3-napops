####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# figure-06-distance-family.R
# Created January 2022
# Last Updated January 2022

####### Import Libraries and External Files #######

library(napops)
library(ggplot2)
library(ggpubr)
library(magrittr)
theme_set(theme_pubclean())

####### Read Data #################################

dis_sp <- coef_distance(model = "best")
ibp_codes <- read.csv("data/IBP-Alpha-Codes20.csv")
families <- read.csv("data/NACC_list_species.csv")

####### Generate Data #############################

df <- data.frame(Species = dis_sp$Species, Model = dis_sp$Model) %>%
  merge(ibp_codes[, c("SPEC", "COMMONNAME")],
        by.x = "Species", by.y = "SPEC") %>%
  merge(families[, c("common_name", "family")],
        by.x = "COMMONNAME", by.y = "common_name")

names(df) <- c("English", "Species", "Model", "Family")

# Create empty dataframe for on-road

forest <- seq(0,1, by = 0.05)

onroad_df <- data.frame(Species = rep(df$Species, each = length(forest)),
                        Model = rep(df$Model, each = length(forest)),
                        Family = rep(df$Family, each = length(forest)),
                        Road = rep(TRUE, times = nrow(df)))
onroad_df$q <- NA

# Create empty dataframe for p vs TSSR, keeping OD constant
forest <- seq(0,1, by = 0.05)

offroad_df <- data.frame(Species = rep(df$Species, each = length(forest)),
                        Model = rep(df$Model, each = length(forest)),
                        Family = rep(df$Family, each = length(forest)),
                        Road = rep(FALSE, times = nrow(df)))
offroad_df$q <- NA

for (sp in unique(onroad_df$Species))
{
  print(sp)
  
  mod <- onroad_df[which(onroad_df$Species == sp), "Model"][1]
  q_sp <- edr(species = sp,
                  model = mod,
                  road = TRUE,
                  forest = forest)
  onroad_df$Forest <- q_sp$Forest
  onroad_df[which(onroad_df$Species == sp), "q"] <- q_sp$EDR_est
  
  mod <- offroad_df[which(offroad_df$Species == sp), "Model"][1]
  q_sp <- edr(species = sp,
                  model = mod,
                  road = FALSE,
                  forest = forest)
  offroad_df$Forest <- q_sp$Forest
  offroad_df[which(offroad_df$Species == sp), "q"] <- q_sp$EDR_est
}

####### Generate Plot #############################

families_to_plot <- c("Parulidae", "Passerellidae",
                      "Tyrannidae", "Picidae")#, "Icteridae")
families_english <- c("(New World Warblers)", "(New World Sparrows)",
                      "(Tyrant Flycatchers)", "(Woodpeckers)")#, "(Blackbirds)")
n_family <- unname(table(df$Family)[families_to_plot])

colours <- c("#CC79A7", "#D55E00", "#0072B2", "#009E73")#, "#E69F00")

# Generate family plots for OD
plot_list_onroad <- vector(mode = "list", length = length(families_to_plot) + 1)
names(plot_list_onroad) <- c("All", families_to_plot)

# Generate family plots for TSSR
plot_list_offroad <- vector(mode = "list", length = length(families_to_plot) + 1)
names(plot_list_offroad) <- c("All", families_to_plot)

# Generate family "plots" for labels
plot_list_labels <- vector(mode = "list", length = length(families_to_plot) + 1)
names(plot_list_labels) <- c("All", families_to_plot)

# Generate plots of all

label_road <- "Effective Detection Radius (EDR)\nvs. Forest Coverage\nRoadside Survey"
plot_list_onroad[["All"]] <- ggplot() + 
  annotate("text", x = 4, y = 25, size=5, label = label_road) + 
  xlim(c(0,8)) +
  theme_void()

label_offroad <- "Effective Detection Radius (EDR)\nvs. Forest Coverage\nOffroad Survey"
plot_list_offroad[["All"]] <- ggplot() + 
  annotate("text", x = 4, y = 25, size=5, label = label_offroad) + 
  xlim(c(0,8)) +
  theme_void()

plot_list_labels[["All"]] <- ggplot() +
  theme_void()



c <- 1

for (f in families_to_plot)
{
  ## On Road Plots
  onroad_df_plot <- onroad_df
  onroad_df_plot$FamilyActive <- ifelse(onroad_df_plot$Family == f,
                                    f,
                                    "Other")
  onroad_df_plot$FamilyActive <- ifelse((onroad_df_plot$Model %in% c(2,3,4,5) &
                                       onroad_df_plot$FamilyActive == f),
                                    f,
                                    "Other")
  
  onroad_df_plot <- onroad_df_plot[-which(onroad_df_plot$Species == "GIWO"),]
  
  plot_list_onroad[[f]] <- ggplot() +
    geom_line(data = onroad_df_plot[which(onroad_df_plot$Family == f & 
                                        onroad_df_plot$FamilyActive == "Other"), ],
              aes(x = Forest, y = q, group = Species), colour = "#bebebe", alpha = 1) +
    geom_line(data = onroad_df_plot[which(onroad_df_plot$Family == f), ],
              aes(x = Forest, y = q, group = Species), color = "#bebebe", alpha = 1) +
    stat_summary(data = onroad_df_plot[which(onroad_df_plot$Family == f), ],
                 fun.y=mean,
                 aes(x = Forest, y = q, group=1), geom="line", colour="black", size = 1.5) +
    theme(legend.position = "none") +
    xlab("Forest Coverage") +
    ylab("EDR (m)") +
    ylim(c(0,225)) +
    NULL
  
  ## Off Road Plots
  offroad_df_plot <- offroad_df
  offroad_df_plot$FamilyActive <- ifelse(offroad_df_plot$Family == f,
                                        f,
                                        "Other")
  offroad_df_plot$FamilyActive <- ifelse((offroad_df_plot$Model %in% c(2,3,4,5) &
                                           offroad_df_plot$FamilyActive == f),
                                        f,
                                        "Other")
  
  offroad_df_plot <- offroad_df_plot[-which(offroad_df_plot$Species == "GIWO"),]
  
  plot_list_offroad[[f]] <- ggplot() +
    geom_line(data = offroad_df_plot[which(offroad_df_plot$Family == f & 
                                            offroad_df_plot$FamilyActive == "Other"), ],
              aes(x = Forest, y = q, group = Species), colour = "#bebebe", alpha = 1) +
    geom_line(data = offroad_df_plot[which(offroad_df_plot$Family == f), ],
              aes(x = Forest, y = q, group = Species), color = "#bebebe", alpha = 1) +
    stat_summary(data = offroad_df_plot[which(offroad_df_plot$Family == f), ],
                 fun.y=mean,
                 aes(x = Forest, y = q, group=1), geom="line", colour="black", size = 1.5) +
    theme(legend.position = "none") +
    xlab("Forest Coverage") +
    ylab("EDR (m)") +
    ylim(c(0,225)) +
    NULL
  
  ## Label Plots
  label <- paste0(f, "\n n = ", n_family[c])
  plot_list_labels[[f]] <- ggplot() + 
    annotate("text", x = 4, y = 25, size=5, label = label) + 
    xlim(c(0,8)) +
    theme_void()
  
  
  c <- c + 1
}

png(filename = "output/plots/Fig6-distance-family.png",
    height = 8, width = 7.5, res = 1200, units = "in")
ggarrange(ggarrange(plotlist = plot_list_onroad, nrow = length(plot_list_onroad)),
          ggarrange(plotlist = plot_list_labels, nrow = length(plot_list_labels)),
          ggarrange(plotlist = plot_list_offroad, nrow = length(plot_list_offroad)),
          ncol = 3,
          #labels = c("A", " ", "B"),
          widths = c(2.5,1,2.5))
dev.off()
