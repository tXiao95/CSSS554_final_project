#' Create graph file for INLA
library(data.table)
library(INLA)
library(spdep)
library(rgdal)
library(readr)

yr <- 2013

home_dir <- "C:/Users/twh42/Documents/UW_Class/CSSS_554/final_project"
setwd(home_dir)
shp_path <- sprintf("data/NGA/shape_files/%d/shps/", yr)
#' Shapefiles 
map <- rgdal::readOGR(dsn = paste0(shp_path, "sdr_subnational_boundaries2.shp"))

index_map <- data.table(STATE = map$REGNAME, graph_index = 1:length(map$REGNAME))

#map$STATE <- map$REGNAME
graph <- spdep::poly2nb(map, row.names = 1:37)
nb2INLA(nb = graph, file = paste0(shp_path, "nga.graph"))

readr::write_csv(index_map, paste0(shp_path, "index_map.csv"))

# coords <- coordinates(map)
# plot(coords)
# 
# plot(map)
# plot(graph, coords, add = T)

