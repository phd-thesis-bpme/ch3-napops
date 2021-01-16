####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# napops-summary-statistics.R
# Created January 2021
# Last Updated January 2021

####### Import Libraries and External Files #######

library(sf)
library(bbsBayes)

source("../utilities/get-data.R")

####### Create Empty Summary Data List ############

summary_stats <- list()

####### Read Data #################################

project_list <- read.table(here::here("../utilities/proj-list"))[,1]
n_proj <- length(project_list)

# Create combined sample data frame
project_samples <- vector('list', n_proj)
project_counts <- vector('list', n_proj)
for (i in 1:n_proj)
{
  p <- project_list[i]
  
  # Get counts
  data_dir <- paste0("../project-",
                     p,
                     "/output/",
                     p,
                     "_counts.rda")
  project_counts[[i]] <- data.frame(get_data(data_dir))
  
  # Get samples
  data_dir <- paste0("../project-",
                     p,
                     "/output/",
                     p,
                     "_samples.rda")
  project_samples[[i]] <- data.frame(get_data(data_dir))
}
names(project_samples) <- names(project_counts) <- project_list

####### Summary of Samples and Counts DFs #########

summary_stats[["n_observations"]] <- nrow(do.call(rbind, project_counts))
summary_stats[["n_samples"]] <- nrow(do.call(rbind, project_samples))

utc <- do.call(rbind, project_samples)$UTC
year <- unique(strptime(utc, 
                        format = "%Y-%m-%dT%H:%M:%SZ", 
                        tz = "UTC")$year + 1900)
summary_stats[["years"]] <- sort(year[!is.na(year)])

####### Number of Projects ########################

# We will create a list of all projects used here, and a count of them
all_projects <- vector('list', n_proj)

# First, the list of projects. Start with metaprojects first
metaprojects <- sort(grep("_m", project_list, value = TRUE))
n_meta <- length(metaprojects)

for (i in 1:n_meta)
{
  sampling_df <- project_samples[[metaprojects[i]]]
  p_split <- strsplit(sampling_df$Project, split = ":")
  p <- unique(sapply(p_split, "[[", 2))
  mp <- unique(sapply(p_split, "[[", 1))
  
  all_projects[[i]] <- data.frame(Metaproject = rep(mp, times = length(p)),
                                  Project = p)
}

# Now the standalone projects
standalone_projects <- sort(setdiff(project_list, metaprojects))
n_stand <- length(standalone_projects)

for (i in 1:n_stand)
{
  mp <- "Standalone projects not part of a larger metaproject"
  p <- standalone_projects[i]
  
  all_projects[[i + n_meta]] <- data.frame(Metaproject = mp,
                                           Project = p)
}

all_projects <- do.call(rbind, all_projects)

summary_stats[["total_projects"]] <- nrow(all_projects)

####### Number of Species #########################

rem_sp <- read.csv("../results/coefficients/removal.csv")$Species
dis_sp <- read.csv("../results/coefficients/distance.csv")$Species

sp <- union(rem_sp, dis_sp)
summary_stats[["n_species"]] <- length(sp)

####### Number of States/Provinces ################

coords <- do.call(rbind, project_samples)[, c("Latitude", "Longitude")]
coords <- coords[!is.na(coords$Latitude), ]
coords <- coords[!is.na(coords$Longitude), ]

coords <- st_as_sf(coords,coords = c("Longitude","Latitude"), crs = 4326)
state = sf::read_sf(dsn = system.file("maps",
                                      package="bbsBayes"),
                    layer = "BBS_ProvState_strata")
state <- state %>% st_transform(st_crs(coords))

counts_state <- st_intersects(state, coords)
cs_df <- data.frame(State = state$ST_12,
                    Counts = lengths(counts_state))

summary_stats[["n_states"]] <- nrow(cs_df[which(cs_df$Counts > 0), ])

####### Output Summary Statistics and Tables ######

write.table(x = all_projects, file = "tables/project_list.csv",
            sep = ",", row.names = FALSE)

write.table(cs_df, file = "tables/counts_by_state.csv", sep = ",", row.names = FALSE)

save(summary_stats, file = "summary_statistics.rda")