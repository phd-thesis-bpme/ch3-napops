####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: NA-POPS-paper-2021
# 1-download-coefficients.R
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