library(data.table)
library(ggplot2)
library(maptools)
library(SpatialEpi)
library(sp)
library(spatial)
library(rgdal)

home_dir <- "C:/Users/twh42/Documents/UW_Class/CSSS_554/final_project"
setwd(home_dir)

shp_path <- "data/NGA/shape_files/2010/shps/"

#' Set parameters
recall   <- 3
length   <- 3
age_bins <- 5

#' Read in data
asfr_path <- sprintf("data/prepped/asfr_recall_%d_length_%d_age_%d.csv", recall, length, age_bins)
tfr_path  <- sprintf("data/prepped/tfr_recall_%d_length_%d_age_%d.csv", recall, length, age_bins)
asfr      <- fread(asfr_path); asfr[, inverse_variance :=  1/se^2]
tfr       <- fread(tfr_path)

tfr2010 <- tfr[SurveyId == "NG2010MIS"]

#' Shapefiles 
map <- rgdal::readOGR(dsn = paste0(shp_path, "sdr_subnational_boundaries2.shp"))
pdf("figures/nigeria_2010_outline.pdf", width = 18, height = 13)
plot(map, main = "Nigeria")
dev.off()

map@data$SORT <- 1:37
map@data <- merge(map@data, tfr2010[,.(ADM1NAME, tfr)], by.x = "REGNAME", by.y = "ADM1NAME", all.x = T)
map@data <- map@data[order(map@data$SORT),]

pdf("figures/nigeria_2010_tfr.pdf", width = 18, height = 13)
sp::spplot(map, zcol = "tfr", main = "Nigeria Total Fertility Rate 2007-2010")
dev.off()

 