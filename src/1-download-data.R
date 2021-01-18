####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 1-download-data.R
# Created January 2021
# Last Updated January 2021

####### Import Libraries and External Files #######

library(curl)

####### Download Coefficients from Github #########

#' NOTE: this is mostly for demonstrative purposes as the coefficients
#' already live on my local computer. However, the point of NA-POPS is
#' to have an open-source database of these coefficients, so this
#' demonstrates the ease of access

# Download distance coefficients
download.file("https://raw.githubusercontent.com/na-pops/results/master/coefficients/distance.csv",
              destfile = "data/distance.csv",
              method = "curl")

# Download distance coefficients
download.file("https://raw.githubusercontent.com/na-pops/results/master/coefficients/removal.csv",
              destfile = "data/removal.csv",
              method = "curl")

####### Download Var-Covar from Github ############

#' Here, we will download all of them and just save them as a list
#' to output as 4 .rda objects (removal/distance best/full)

# Download all distance var-covars
dis_species <- read.csv("data/distance.csv")$Species
dis_vcv_best <- vector(mode = "list", length = length(dis_species))
dis_vcv_full <- vector(mode = "list", length = length(dis_species))
names(dis_vcv_best) <- dis_species
names(dis_vcv_full) <- dis_species

for (sp in dis_species)
{
  best_file <- paste0("data/var-covar/raw/distance/",
                      sp,
                      "_best.csv")
  full_file <- paste0("data/var-covar/raw/distance/",
                      sp,
                      "_full.csv")
  
  download.file(paste0("https://raw.githubusercontent.com/na-pops/results/master/var-covar/distance/",
                       sp,
                       "_best.csv"),
                destfile = best_file,
                method = "curl")
  dis_vcv_best[[sp]] <- read.csv(best_file)
  
  download.file(paste0("https://raw.githubusercontent.com/na-pops/results/master/var-covar/distance/",
                       sp,
                       "_full.csv"),
                destfile = full_file,
                method = "curl")
  dis_vcv_full[[sp]] <- read.csv(full_file)
}

save(dis_vcv_best, file = "data/var-covar/dis_vcv_best.rda")
save(dis_vcv_full, file = "data/var-covar/dis_vcv_full.rda")

# Download all removal var-covars
rem_species <- read.csv("data/removal.csv")$Species
rem_vcv_best <- vector(mode = "list", length = length(rem_species))
rem_vcv_full <- vector(mode = "list", length = length(rem_species))
names(rem_vcv_best) <- rem_species
names(rem_vcv_full) <- rem_species

for (sp in rem_species)
{
  best_file <- paste0("data/var-covar/raw/removal",
                      sp,
                      "_best.csv")
  full_file <- paste0("data/var-covar/raw/removal/",
                      sp,
                      "_full.csv")
  
  download.file(paste0("https://raw.githubusercontent.com/na-pops/results/master/var-covar/removal/",
                       sp,
                       "_best.csv"),
                destfile = best_file,
                method = "curl")
  rem_vcv_best[[sp]] <- read.csv(best_file)
  
  download.file(paste0("https://raw.githubusercontent.com/na-pops/results/master/var-covar/removal/",
                       sp,
                       "_full.csv"),
                destfile = full_file,
                method = "curl")
  rem_vcv_full[[sp]] <- read.csv(full_file)
}

save(rem_vcv_best, file = "data/var-covar/rem_vcv_best.rda")
save(rem_vcv_full, file = "data/var-covar/rem_vcv_full.rda")