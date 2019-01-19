library(data.table)
library(rdhs)
library(magrittr)
library(ggplot2)

setwd("C:/Users/twh42/Documents/UW_Class/CSSS_554/final_project/")

email    <- "twh42@uw.edu"
project  <- "Spatial Regression of Total Fertility Rate in Nigeria"
cluster_num_var <- "cluster number"

rdhs::set_rdhs_config(email = email,
                      project = project,
                      config_path = "rdhs.json",
                      cache_path = "data/NGA",
                      data_frame = "data.table::data.table", 
                      global = FALSE)

# Steps to identifying DHS data sets you want 
# 1. Check survey charactersitics 
# 2. Identify relevant countries
# 3. Find all surveys that have both survey characteristics and countries
# 4. Find the actual dataset linked to those identified surveys 
sc    <- rdhs::dhs_survey_characteristics()
sid   <- sc[grepl("GPS", SurveyCharacteristicName), SurveyCharacteristicID]
locs  <- rdhs::dhs_countries()
survs <- rdhs::dhs_surveys(surveyCharacteristicIds = sid, 
                     countryIds = "NG",
                     surveyYearStart = 1950)

#' Pull in datasets 
datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "flat")

datasets15 <- datasets[SurveyYear == 2015,  .(SurveyYear, FileType, FileName)]

#' Download datasets into directory
downloads <- rdhs::get_datasets(datasets$FileName)

gps   <- readRDS(file = downloads$NGGE71FL)
unique(gps$DHSCLUST)
indiv <- readRDS(file = downloads$NGIR71FL)

lbls <- data.table(get_variable_labels(indiv))[, description := tolower(description)]
d <- lbls[grepl("cluster", description), variable]
lbls[description == cluster_num_var, variable]

