library(data.table)
library(ggplot2)
library(maptools)
library(SpatialEpi)
library(sp)
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

#' Only 2003, 2008, and 2013 Surveys have each subnational (37 states)
tfr2003 <- tfr[SurveyId == "NG2003DHS"] %>% setnames("tfr", "tfr2003")
tfr2008 <- tfr[SurveyId == "NG2008DHS"] %>% setnames("tfr", "tfr2008")
tfr2013 <- tfr[SurveyId == "NG2013DHS"] %>% setnames("tfr", "tfr2013")

#' Shapefiles 
map <- rgdal::readOGR(dsn = paste0(shp_path, "sdr_subnational_boundaries2.shp"))
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
grid.arrange(b, c, ncol=2)
dev.off()

 