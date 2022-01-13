####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# figure-05-removal-family.R
# Created January 2022
# Last Updated January 2022

####### Import Libraries and External Files #######

library(napops)
library(ggplot2)
library(ggpubr)
library(magrittr)
theme_set(theme_pubclean())

####### Read Data #################################

rem_sp <- coef_removal(model = "best")
ibp_codes <- read.csv("../utilities/IBP-Alpha-Codes20.csv")
families <- read.csv("../utilities/NACC_list_species.csv")

####### Generate Data #############################

df <- data.frame(Species = rem_sp$Species, Model = rem_sp$Model) %>%
  merge(ibp_codes[, c("SPEC", "COMMONNAME")],
        by.x = "Species", by.y = "SPEC") %>%
  merge(families[, c("common_name", "family")],
        by.x = "COMMONNAME", by.y = "common_name")

names(df) <- c("English", "Species", "Model", "Family")

median_od <- median(covariates_removal()$OD)
median_tssr <- round(median(covariates_removal()$TSSR), 1)

# Create empty dataframe for p vs OD, keeping TSSR constant

od <- seq(91, 212, by = 11)
od_df <- data.frame(Species = rep(df$Species, each = length(od)),
                    Model = rep(df$Model, each = length(od)),
                    Family = rep(df$Family, each = length(od)),
                    OD = rep(od, times = nrow(df)))
od_df$p <- NA

# Create empty dataframe for p vs TSSR, keeping OD constant
tssr <- seq(-2, 6)
tssr_df <- data.frame(Species = rep(df$Species, each = length(tssr)),
                      Model = rep(df$Model, each = length(tssr)),
                      Family = rep(df$Family, each = length(tssr)),
                      TSSR = rep(tssr, times = nrow(df)))
tssr_df$p <- NA

for (sp in unique(od_df$Species))
{
  print(sp)
  
  mod <- od_df[which(od_df$Species == sp), "Model"][1]
  p_sp <- avail(species = sp,
                model = mod,
                od = od,
                tssr = median_tssr,
                time = 5)
  od_df[which(od_df$Species == sp), "p"] <- p_sp$p
  
  mod <- tssr_df[which(tssr_df$Species == sp), "Model"][1]
  p_sp <- avail(species = sp,
                model = mod,
                od = median_od,
                tssr = tssr,
                time = 5)
  tssr_df[which(tssr_df$Species == sp), "p"] <- p_sp$p
}

####### Generate Plot #############################

families_to_plot <- c("Parulidae", "Passerellidae",
                      "Tyrannidae", "Picidae")#, "Icteridae")
families_english <- c("(New World Warblers)", "(New World Sparrows)",
                      "(Tyrant Flycatchers)", "(Woodpeckers)")#, "(Blackbirds)")
n_family <- unname(table(df$Family)[families_to_plot])

colours <- c("#CC79A7", "#D55E00", "#0072B2", "#009E73")#, "#E69F00")

# Generate family plots for OD
plot_list_od <- vector(mode = "list", length = length(families_to_plot) + 1)
names(plot_list_od) <- c("All", families_to_plot)

# Generate family plots for TSSR
plot_list_tssr <- vector(mode = "list", length = length(families_to_plot) + 1)
names(plot_list_tssr) <- c("All", families_to_plot)

# Generate family "plots" for labels
plot_list_labels <- vector(mode = "list", length = length(families_to_plot) + 1)
names(plot_list_labels) <- c("All", families_to_plot)

# Generate plots of all

label_od <- "Availability (p) vs. Ordinal Day\nTime Since Sunrise = 1.6\nSurvey Duration = 5 mins"
plot_list_od[["All"]] <- ggplot() + 
  annotate("text", x = 4, y = 25, size=5, label = label_od) + 
  xlim(c(0,8)) +
  theme_void()

label_tssr <- "Availability (p) vs.\nTime Since Sunrise\nOrdinal Day = 160\nSurvey Duration = 5 mins"
plot_list_tssr[["All"]] <- ggplot() + 
  annotate("text", x = 4, y = 25, size=5, label = label_tssr) + 
  xlim(c(0,8)) +
  theme_void()

plot_list_labels[["All"]] <- ggplot() +
  theme_void()



c <- 1

for (f in families_to_plot)
{
  ## OD Plots
  od_df_plot <- od_df
  od_df_plot$FamilyActive <- ifelse(od_df_plot$Family == f,
                                    f,
                                    "Other")
  
  od_df_plot$FamilyActive <- ifelse((od_df_plot$Model %in% c(2,3,6,7,8,9) &
                                       od_df_plot$FamilyActive == f),
                                    f,
                                    "Other")
  
  plot_list_od[[f]] <- ggplot() +
    geom_line(data = od_df_plot[which(od_df_plot$Family == f & 
                                        od_df_plot$FamilyActive == "Other"), ],
              aes(x = OD, y = p, group = Species), colour = "#20A387FF", alpha = 0.75) +
    geom_line(data = od_df_plot[which(od_df_plot$FamilyActive == f), ],
              aes(x = OD, y = p, group = Species), color = "#151515", alpha = 1) +
    #scale_color_manual(values = c("#BEBEBE", "#482677FF")) +
    theme(legend.position = "none") +
    xlab("Ordinal Day") +
    ylab("Availability (p)") +
    NULL
  
  ## TSSR Plots
  tssr_df_plot <- tssr_df
  tssr_df_plot$FamilyActive <- ifelse(tssr_df_plot$Family == f,
                                      f,
                                      "Other")
  
  tssr_df_plot$FamilyActive <- ifelse((tssr_df_plot$Model %in% c(4,5,6,7,8,9) &
                                         tssr_df_plot$FamilyActive == f),
                                      f,
                                      "Other")
  
  plot_list_tssr[[f]] <- ggplot() +
    geom_line(data = tssr_df_plot[which(tssr_df_plot$Family == f & 
                                          tssr_df_plot$FamilyActive == "Other"), ],
              aes(x = TSSR, y = p, group = Species), colour = "#20A387FF", alpha = 0.75) +
    geom_line(data = tssr_df_plot[which(tssr_df_plot$FamilyActive == f), ],
              aes(x = TSSR, y = p, group = Species), color = "#151515", alpha = 1) +
    #scale_color_manual(values = c("#BEBEBE", "#482677FF")) +
    theme(legend.position = "none") +
    xlab("Time Since Sunrise") +
    ylab("Availability (p)") +
    NULL
  
  ## Label Plots
  label <- paste0(f, "\n n = ", n_family[c])
  plot_list_labels[[f]] <- ggplot() + 
    annotate("text", x = 4, y = 25, size=5, label = label) + 
    xlim(c(0,8)) +
    theme_void()
  
  
  c <- c + 1
}

png(filename = "output/plots/Fig5-removal-family.png",
    height = 8, width = 7.5, res = 1200, units = "in")
ggarrange(ggarrange(plotlist = plot_list_od, nrow = length(plot_list_od)),
          ggarrange(plotlist = plot_list_labels, nrow = length(plot_list_labels)),
          ggarrange(plotlist = plot_list_tssr, nrow = length(plot_list_tssr)),
          ncol = 3,
          #labels = c("A", " ", "B"),
          widths = c(2.5,1,2.5))
dev.off()
