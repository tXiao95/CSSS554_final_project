#' Create graph file for INLA
library(data.table)
library(INLA)
library(spdep)
library(rgdal)

yr <- 2008

home_dir <- "C:/Users/twh42/Documents/UW_Class/CSSS_554/final_project"
setwd(home_dir)
shp_path <- sprintf("data/NGA/shape_files/%d/shps/", yr)
#' Shapefiles 
map <- rgdal::readOGR(dsn = paste0(shp_path, "sdr_subnational_boundaries2.shp"))
map$STATE <- map$REGNAME
graph <- spdep::poly2nb(map)
nb2INLA(nb = graph, file = paste0(shp_path, "nga.graph"))
