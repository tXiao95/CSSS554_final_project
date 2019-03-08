library(data.table)
library(ggplot2)
library(broom)
library(rgeos)
library(maptools)
library(SpatialEpi)
library(sp)
library(sf)
library(spatial)
library(rgdal)
library(RColorBrewer)
library(gridExtra)

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
asfr      <- fread(asfr_path)#; asfr[, inverse_variance :=  1/se^2]
tfr       <- fread(tfr_path)

map <- function(sid, age, min = NULL, max = NULL){
  #' @param sid : The Survey ID [NG2008DHS, NG2013DHS, NG2010MIS, NG2003DHS, NG1990DHS]
  #' @param age : The starting age of the 5-yr age group to plot
  #' @param facet : Whether to facet, so have all surveys together on one plot, or to have them on separate plots
  #' @param fixed_scale : If there are multiple plots, fix the scale to just one. 
  #' @param min : Minimum Value to graph
  #' @param max : Maximum Value to graph
  
  if(!(age %in% c(seq(15, 45, 5), "tfr"))){stop("Age must be 5-yr age group or TFR")}
  if(!(sid %in% c("NG2003DHS", "NG2008DHS", "NG2010MIS", "NG2013DHS", "NG2015MIS"))){stop("Invalid Survey ID")}
  
  map    <- rgdal::readOGR(dsn = paste0(shp_path, "sdr_subnational_boundaries2.shp"))
  map_df <- data.table(broom::tidy(map, region = "REGNAME"))
  
  if(age == "tfr"){
    tfr <- fread(tfr_path)[SurveyId == sid]
    if(is.null(min)) min <- min(tfr$tfr)
    if(is.null(max)) max <- max(tfr$tfr)
    map_df <- merge(map_df, tfr[,.(STATE, tfr)], by.x = "id", by.y = "STATE", allow.cartesian = T)
    gg <- ggplot() + 
      geom_polygon(data = map_df, aes(x = long, y = lat, group = group, fill = tfr), color = 'black', size = .2) + 
      theme_void() + 
      ggtitle(paste0(sid, " ", age)) + 
      coord_quickmap() + 
      #facet_wrap(~age_start) + 
      scale_fill_gradient(low = "blue", high = "yellow", limits = c(min, max))
  } else{
    asfr <- fread(asfr_path)[SurveyId == sid & age_start == age]
    if(is.null(min)) min <- min(asfr$asfr)
    if(is.null(max)) max <- max(asfr$asfr)
    map_df <- merge(map_df, asfr[,.(STATE, age_start, asfr)], by.x = "id", by.y = "STATE", allow.cartesian = T)
    gg <- ggplot() + 
      geom_polygon(data = map_df, aes(x = long, y = lat, group = group, fill = asfr), color = 'black', size = .2) + 
      theme_void() + 
      ggtitle(paste0(sid, " ASFR ", age)) + 
      coord_quickmap() + 
      facet_wrap(~age_start) + 
      scale_fill_gradient(low = "blue", high = "yellow", limits = c(min, max))
  }
  gg
}

#' Nigeria Map of TFR
m1 <- map("NG2008DHS", "tfr")
m2 <- map("NG2013DHS", "tfr")
pdf("figures/nigera_tfr.pdf", width = 15, height = 10)
m1
m2
dev.off()

#' Nigeria Map of ASFR DHS
sid <- "NG2013DHS"
min <- NULL
max <- NULL
pdf(paste0("figures/", sid, "_asfr.pdf"), width = 15, height = 10)
m15 <- map(sid, 15, min = min, max = max); m15
m20 <- map(sid, 20, min = min, max = max); m20
m25 <- map(sid, 25, min = min, max = max); m25
m30 <- map(sid, 30, min = min, max = max); m30
m35 <- map(sid, 35, min = min, max = max); m35
m40 <- map(sid, 40, min = min, max = max); m40
m45 <- map(sid, 45, min = min, max = max); m45
dev.off()