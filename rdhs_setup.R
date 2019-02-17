library(data.table)
library(rdhs)
library(haven)
library(ggplot2)
library(argparse)

home_dir <- "C:/Users/twh42/Documents/UW_Class/CSSS_554/final_project"
setwd(home_dir)

email    <- "twh42@uw.edu"
project  <- "Spatial Regression of Total Fertility Rate in Nigeria"

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

#' Pull in dataset metadata
datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "FL")
#' Download actual survey datasets into directory
downloads <- rdhs::get_datasets(datasets$FileName)
readr::write_csv(datasets, "datasets.csv")
saveRDS(downloads, file = "downloads.rds")


