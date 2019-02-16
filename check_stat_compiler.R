#' Name: Thomas Hsiao
#' Purpose: Check if the collapsed DHS microdata calculation of TFR matches up with that 
#' reported on StatCompiler

library(rdhs)
library(data.table)

tags  <- rdhs::dhs_tags()
tagid <- tags[TagName == "Fertility Rates", TagID]

ind    <- rdhs::dhs_indicators()
ind_id <- ind[9,.(IndicatorId, Definition)]$IndicatorId

df <- dhs_data(tagIds = tagid, 
               countryIds = "NG", 
               surveyYearStart = 1970, 
               breakdown = "subnational", 
               indicatorIds = ind_id)[IndicatorId == ind_id]
