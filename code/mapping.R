library(data.table)
library(ggplot2)
library(maptools)
library(SpatialEpi)
library(sp)
library(spatial)
library(rgdal)

home_dir <- "C:/Users/twh42/Documents/UW_Class/CSSS_554/final_project"
setwd(home_dir)

shp_path <- "data/NGA/shape_files/2013/shps/"

#' Set parameters
recall   <- 3
length   <- 3
age_bins <- 5

#' Read in data
asfr_path <- sprintf("data/prepped/asfr_recall_%d_length_%d_age_%d.csv", recall, length, age_bins)
tfr_path  <- sprintf("data/prepped/tfr_recall_%d_length_%d_age_%d.csv", recall, length, age_bins)
asfr      <- fread(asfr_path); asfr[, inverse_variance :=  1/se^2]
tfr       <- fread(tfr_path)

tfr2013 <- tfr[SurveyId == "NG2013DHS"]

#' Shapefiles 
map <- maptools::readShapePoly(fn = paste0(shp_path, "sdr_subnational_boundaries2"), repair = T)
pdf("figures/nigeria_2013_outline.pdf", width = 18, height = 13)
plot(map, main = "Nigeria")
dev.off()

#' 