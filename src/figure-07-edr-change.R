####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# figure-07-edr-change.R
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

onroad_df <- onroad_df[-which(onroad_df$Species == "GIWO"),]
offroad_df <- offroad_df[-which(offroad_df$Species == "GIWO"),]

####### Generate Plot #############################

families_to_plot <- c("Parulidae", "Passerellidae",
                      "Tyrannidae", "Picidae")#, "Icteridae")
families_english <- c("(New World Warblers)", "(New World Sparrows)",
                      "(Tyrant Flycatchers)", "(Woodpeckers)")#, "(Blackbirds)")
n_family <- unname(table(df$Family)[families_to_plot])

plot_list <- vector(mode = "list", length = length(families_to_plot))
names(plot_list) <- families_to_plot

# Generate family "plots" for labels
plot_list_labels <- vector(mode = "list", length = length(families_to_plot))
names(plot_list_labels) <- families_to_plot

c <- 1

for (f in families_to_plot)
{
  # Main plot
  onroad_fam <- onroad_df[onroad_df$Family == f, ]
  offroad_fam <- offroad_df[offroad_df$Family == f, ]
  
  to_plot <- data.frame(Species = onroad_fam$Species,
                        Forest = onroad_fam$Forest,
                        Delta_EDR = onroad_fam$q - offroad_fam$q)
  
  plot_list[[f]] <- ggplot(data = to_plot, aes(x = Forest, y = Delta_EDR, group = Species)) +
    geom_line(color = "#bebebe", alpha = 1) +
    stat_summary(fun.y=mean,
                 aes(group=1), geom="line", colour="black", size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    xlab("Forest Coverage") +
    ylab("Change in EDR (m)") +
    NULL
  
  ## Label Plots
  label <- paste0(f, "\n n = ", n_family[c])
  plot_list_labels[[f]] <- ggplot() + 
    annotate("text", x = 4, y = 25, size=5, label = label) + 
    xlim(c(0,8)) +
    theme_void()
  
  c <- c + 1
}

png(filename = "output/plots/Fig7-edr-change.png",
    height = 8, width = 7.5, res = 1200, units = "in")
ggarrange(ggarrange(plotlist = plot_list, nrow = length(plot_list)),
          ggarrange(plotlist = plot_list_labels, nrow = length(plot_list_labels)),
          ncol = 2,
          #labels = c("A", " ", "B"),
          widths = c(2,1))
dev.off()