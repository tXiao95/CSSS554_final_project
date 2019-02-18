#' Name: Thomas Hsiao
#' Purpose: Check if the collapsed DHS microdata calculation of TFR matches up with that 
#' reported on StatCompiler

library(rdhs)
library(data.table)
library(readr)

tags  <- rdhs::dhs_tags()
tagid <- tags[TagName == "Fertility Rates", TagID]

ind    <- rdhs::dhs_indicators()
ind_id <- ind[1:9,.(IndicatorId, Definition)]$IndicatorId

dhs_subnat <- dhs_data(tagIds = tagid, 
               countryIds = "NG", 
               surveyYearStart = 1970, 
               breakdown = "subnational", 
               indicatorIds = ind_id)[IndicatorId %in% ind_id] %>% data.table

dhs <- dhs_data(tagIds = tagid, 
                countryIds = "NG", 
                surveyYearStart = 1970, 
                indicatorIds = ind_id)[IndicatorId %in% ind_id] %>% data.table

#' Subnational Prep
dhs_subnat <- dhs_subnat[,.(SurveyId, CharacteristicLabel, Indicator, Value)]
dhs_subnat[, CharacteristicLabel := gsub("\\.\\.", "", CharacteristicLabel)]
dhs_subnat[, CharacteristicLabel := tolower(CharacteristicLabel)]
dhs_subnat[, Indicator := gsub("Age specific fertility rate: ", "", Indicator)]
dhs_subnat[, Indicator := gsub("Total fertility rate 15-49", "TFR-TFR", Indicator)]
dhs_subnat[, c("age_start", "age_end") := tstrsplit(Indicator, "-", fixed = T)]
dhs_subnat[, Indicator := NULL]
setnames(dhs_subnat, "CharacteristicLabel", "location")
dhs_subnat[, period := 0]
dhs_subnat[, length_of_period_yr := 3]
dhs_subnat[, recall_yr := 3]

readr::write_csv(dhs_subnat, "data/prepped/dhs_subnational.csv")

#' National Prep
dhs <- dhs[,.(SurveyId, Indicator, Value)]
dhs[, Indicator := gsub("Age specific fertility rate: ", "", Indicator)]
dhs[, Indicator := gsub("Total fertility rate 15-49", "TFR-TFR", Indicator)]
dhs[, c("age_start", "age_end") := tstrsplit(Indicator, "-", fixed = T)]
dhs[, Indicator := NULL]
dhs[, location := "Nigeria"]
dhs[, period := 0]
dhs[, length_of_period_yr := 3]
dhs[, recall_yr := 3]

readr::write_csv(dhs_subnat, "data/prepped/dhs_national.csv")
