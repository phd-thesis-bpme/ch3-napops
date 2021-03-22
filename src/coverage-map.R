####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# coverage-map.R
# Created January 2021
# Last Updated January 2021
# Code adapted from https://github.com/AdamCSmithCWS/NAPOPS_coverage

####### Import Libraries and External Files #######

library(bbsBayes)
library(ggplot2)
library(sf)
library(tidyverse)
library(ggpubr)
theme_set(theme_pubclean())
#library(rnaturalearth)
#library(rnaturalearthdata)

source("../utilities/get-data.R")

####### Read Data #################################

# Get project names, and remove BAM (deal with that later in script)
project_list <- read.table(here::here("../utilities/proj-list"))[,1]
remove <- c("bam-can_m", "bam-mn_m")
project_list <- setdiff(as.vector(project_list), remove)
n_proj <- length(project_list)

# Create combined sample data frame
project_samples <- vector('list', n_proj)
for (i in 1:n_proj)
{
  p <- project_list[i]
  
  # Get samples
  data_dir <- paste0("../project-",
                     p,
                     "/output/",
                     p,
                     "_samples.rda")
  project_samples[[i]] <- data.frame(get_data(data_dir))
}
project_samples <- do.call(rbind, project_samples)

####### Wrangle Non-BAM Data ######################

coords <- project_samples[, c("Longitude", "Latitude")]
coords <- coords[which(!is.na(coords$Latitude)), ]
coords <- coords[which(!is.na(coords$Longitude)), ]
pc = st_as_sf(coords,coords = c("Longitude","Latitude"), crs = 4326)

bcrf <- read_sf(dsn = "shp/bcr",
                layer = 'bcrfinalg0_Project2_Dissolve')
bcr <- bcrf %>% st_transform(st_crs(pc))

locat = system.file("maps",
                    package="bbsBayes")
map.file = "BBS_usgs_strata"

strat = sf::read_sf(dsn = locat,
                    layer = map.file)

strat1 <- strat %>% st_transform(st_crs(pc))

tt <- st_intersects(strat1,pc)
tt_df <- data.frame(ST_12 = strat1$ST_12,
                    ncounts = lengths(tt),
                    sqrt_ncounts = sqrt(lengths(tt)),
                    stringsAsFactors = FALSE)

####### Wrangle Abstracted BAM Canada Data ########

#' Deal with abstracted BAM counts by using BCR x Prov combination
#' per sampling point

project <- "bam-can_m"
load("../project-bam-can_m/rawdata/bam-can_m_raw.rda")
load("../project-bam-can_m/output/bam-can_m_samples.rda")

# Recreate Sample ID in original data set
# Format time and date
time_formatted <- sprintf("%s:%s:%s", 
                          DT$HOUR, 
                          DT$MIN, 
                          "00")

date_formatted <- sprintf("%s-%s-%s",
                          DT$YEAR,
                          DT$MONTH,
                          DT$DAY)

DT$utc <- paste0(date_formatted,
                 "T",
                 time_formatted,
                 "Z")

DT$Sample_ID <- paste0(project, ":",
                       DT$SS, ":",
                       DT$utc)

#' Match up Sample IDs in data set with sampling DF, and extract
#' the BCR and Jurisdiction fields
bam_sample <- merge(x = sampling_df,
                    y = DT[, c("Sample_ID", "JURS", "BCR")],
                    by = "Sample_ID")
bam_sample <- bam_sample[!duplicated(bam_sample$Sample_ID),]
bam_sample$bcr_prov <- paste0(bam_sample$JURS, "-", bam_sample$BCR)
bam_canada <- bam_sample

####### Wrangle Abstracted BAM MN Data ############

#' Deal with abstracted BAM counts by using BCR x Prov combination
#' per sampling point

project <- "bam-mn_m"
load("../project-bam-mn_m/rawdata/bam-mn_m_raw.rda")
load("../project-bam-mn_m/output/bam-mn_m_samples.rda")

# Recreate Sample ID in original data set
# Format time and date
time_formatted <- sprintf("%s:%s:%s", 
                          MN$HOUR, 
                          MN$MIN, 
                          "00")

date_formatted <- sprintf("%s-%s-%s",
                          MN$YEAR,
                          MN$MONTH,
                          MN$DAY)

MN$utc <- paste0(date_formatted,
                 "T",
                 time_formatted,
                 "Z")

MN$Sample_ID <- paste0(project, ":",
                       MN$SS, ":",
                       MN$utc)

#' Match up Sample IDs in data set with sampling DF, and extract
#' the BCR and Jurisdiction fields
bam_sample <- merge(x = sampling_df,
                    y = MN[, c("Sample_ID", "JURS", "BCR")],
                    by = "Sample_ID")
bam_sample <- bam_sample[!duplicated(bam_sample$Sample_ID),]
bam_sample$bcr_prov <- paste0(bam_sample$JURS, "-", bam_sample$BCR)
bam_mn <- bam_sample

####### Combine Wrangled Data #####################

bam_counts <- rbind(bam_canada, bam_mn)
bam <- as.data.frame(table(bam_counts$bcr_prov))
names(bam) <- c("bcr_prov", "count")

bam$bcr_prov <- as.character(bam$bcr_prov)
bam$ST_12 = paste0(c(rep("CA-",13),rep("US-",4),
                     rep("CA-",20),rep("US-",1)),bam$bcr_prov)
bam = bam[-which(bam$bcr_prov %in% c("MN-0","ON-0")),]
for(sst in as.character(bam$ST_12))
{
  ww = which(tt_df$ST_12 == sst)
  wwb = which(bam$ST_12 == sst)
  if(length(ww) > 0)
  {
    tt_df[ww,"ncounts"] <- tt_df[ww,"ncounts"]+bam[wwb,"count"]
  }
}

tt_df$nc_cat <- cut(tt_df$ncounts,breaks = c(-1,0,(c(100,200,500,1000,15000))))
tt_df$sqrt_ncounts <- sqrt(tt_df$ncounts)
strat2 <- left_join(strat1,tt_df)

# Save this to use for NA-POPS dashboard
save(strat2, file = "tables/project_coverage.rda")

strat3 <- filter(strat2,ncounts < 100)

####### Generate Map ##############################

png("plots/coverage-map.png", width = 6, height = 6, res = 300, units = "in")
mp = ggplot()+
  geom_sf(data = bcr,fill = viridis::cividis(1,begin = 1),colour = grey(0.75))+
  geom_sf(data = strat2,aes(fill = sqrt_ncounts),colour = NA)+
  #geom_sf(data = strat3,colour = grey(0.75),fill = "white",alpha = 0.01)+
  scale_color_viridis_c(aesthetics = "fill",direction = -1)+
  theme(legend.position = "bottom")
print(mp)
dev.off()
