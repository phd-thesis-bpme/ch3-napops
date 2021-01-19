# NA-POPS-paper-2021

This repo serves as the main post-hoc analysis to be used for the results
section of the NA-POPS manuscript. Below are brief explanations of each of
the scripts in this repository.

## 1-download-data.R
MUST BE RUN before doing any other script. This script will download all
coefficients from the NA-POPS results repository, as well as all variance-covariance
matrices. The coefficients will simply be saved as .csv files in the /data subdirectory.
The var-covar matrices will be saved as .rda objects, which will be a list of all var-covar
matrices for the "best" model and "full" models, for both distance and removal sampling.

## coefficient-table.R
Generate a taxonomically-ordered table of coefficients of the best model.

## coverage-map.R
Generate a map of all sampling events, summarized by BCR x Province/State intersection.

## model-table.R
Generate a count table of which models were chosen by BIC.

## napops-summary-statistics.R
Generate some quick summary statistics of NA-POPS as a whole (how many species, how many 
data points, etc.)

## plot-distance.R
Plot values of q against various maximum survey distances, separated by levels of forest
coverage. NOTE: simulate-tau.R MUST BE RUN prior to this.

## plot-removal.R
Plot values of p against various values of JD and TSSR, separated by various maximum survey
times. NOTE: simulate-phi.R MUST BE RUN prior to this.

## simulate-phi.R
Simulate values of phi given a range of TSSR and JD values. Also will generate 95%
confidence bands around phi for each TSSR x JD combination.

## simulate-tau.R
Simulate values of tau given a range of Roadside status and forest coverage values. Also
will generate 95% confidence bands around tau for each Roadside x Forest combination

## species-table.R
Generate a taxonomically-ordered list of all species modelled here, with banding code,
common name, and scientific name. Also binary values for if it was modelled in removal
and distance.