library(data.table)
library(ggplot2)
library(broom)
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
shp_path <- "data/NGA/shape_files/2013/shps/"

#' Set parameters
recall   <- 3
length   <- 3
age_bins <- 5

sid  <- "NG2008DHS"
age <- 15

#' Read in data
asfr_path <- sprintf("data/prepped/asfr_recall_%d_length_%d_age_%d.csv", recall, length, age_bins)
tfr_path  <- sprintf("data/prepped/tfr_recall_%d_length_%d_age_%d.csv", recall, length, age_bins)
asfr      <- fread(asfr_path)#; asfr[, inverse_variance :=  1/se^2]
tfr       <- fread(tfr_path)

map <- function(sid, age, facet = F, fixed_scale = F){
  #' @param sid : The Survey ID [NG2008DHS, NG2013DHS, NG2010MIS, NG2003DHS, NG1990DHS]
  #' @param age : The starting age of the 5-yr age group to plot
  #' @param facet : Whether to facet, so have all surveys together on one plot, or to have them on separate plots
  #' @param fixed_scale : If there are multiple plots, fix the scale to just one. 
  
  if(!(age %in% c(seq(15, 45, 5), "tfr"))){stop("Age must be 5-yr age group or TFR")}
  if(!(sid %in% c("NG2003DHS", "NG2008DHS", "NG2010MIS", "NG2013DHS"))){stop("Invalid Survey ID")}
  
  if(age == "tfr"){
    tfr <- fread(tfr_path)[SurveyId == sid]
  } else{
    asfr <- fread(asfr_path)[SurveyId == sid]# & age_start == age]
  }
  
  map <- rgdal::readOGR(dsn = paste0(shp_path, "sdr_subnational_boundaries2.shp"))
  # map_df <- ggplot2::fortify(map)
  map_df <- data.table(broom::tidy(map, region = "REGNAME"))
  map_df <- merge(map_df, asfr[,.(STATE, asfr, age_start)], by.x = "id", by.y = "STATE", allow.cartesian = T)
  gg <- ggplot() + 
    geom_polygon(data = map_df, aes(x = long, y = lat, group = group, fill = asfr), color = 'black', size = .2) + 
    theme_void() + 
    ggtitle(paste0(sid, " ", age)) + 
    coord_quickmap() + 
    facet_wrap(~age_start)
  gg
}


#' Only 2003, 2008, and 2013 Surveys have each subnational (37 states)
tfr1990 <- tfr[SurveyId == "NG1990DHS"] %>% setnames("tfr", "tfr1990")
tfr2010 <- tfr[SurveyId == "NG2010MIS"] %>% setnames("tfr", "tfr2010")
tfr2003 <- tfr[SurveyId == "NG2003DHS"] %>% setnames("tfr", "tfr2003")
tfr2008 <- tfr[SurveyId == "NG2008DHS"] %>% setnames("tfr", "tfr2008")
tfr2013 <- tfr[SurveyId == "NG2013DHS"] %>% setnames("tfr", "tfr2013")


#' Shapefiles 
map <- rgdal::readOGR(dsn = paste0(shp_path, "sdr_subnational_boundaries2.shp"))

#map <- sf::st_read(dsn = paste0(shp_path, "sdr_subnational_boundaries2.shp"))
# pdf("figures/nigeria_2010_outline.pdf", width = 18, height = 13)
# plot(map, main = "Nigeria")
# dev.off()

map@data$SORT <- 1:37
map@data <- merge(map@data, tfr2003[,.(STATE, tfr2003)], by.x = "REGNAME", by.y = "STATE", all.x = T) 
map@data <- merge(map@data, tfr2008[,.(STATE, tfr2008)], by.x = "REGNAME", by.y = "STATE", all.x = T)
map@data <- merge(map@data, tfr2013[,.(STATE, tfr2013)], by.x = "REGNAME", by.y = "STATE", all.x = T)
map@data <- map@data[order(map@data$SORT),]

pdf("figures/nigeria_all_years_tfr.pdf", width = 18, height = 13)
a <- sp::spplot(map, zcol = "tfr2003", main = "Nigeria Total Fertility Rate 2000-2003", 
           #col.regions = colorRampPalette(brewer.pal(8, "Greys"))(50), 
           colorkey=list(at=seq(3, 9, length.out = 15)))
b <- sp::spplot(map, zcol = "tfr2008", main = "Nigeria Total Fertility Rate 2005-2008", 
           #col.regions = colorRampPalette(brewer.pal(8, "Greys"))(50), 
           colorkey=list(at=seq(3, 9, length.out = 15)))
c <- sp::spplot(map, zcol = "tfr2013", main = "Nigeria Total Fertility Rate 2010-2013", 
           #col.regions = colorRampPalette(brewer.pal(8, "Greys"))(50), 
           colorkey=list(at=seq(3, 9, length.out = 15)))
grid.arrange(b, c, ncol=3)
dev.off()

 