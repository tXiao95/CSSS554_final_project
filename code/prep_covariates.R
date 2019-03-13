#' Prep covariates for use in spatial models 
#' 1. Education
#' 2. Contraception
#' 3. Income 
#' 4. Religion
#' 5. Child labor
#' 6. Cost of raising Children 
#' 7. Female labor force
#' 8. Marriage

library(rdhs)
library(data.table)
library(readr)

tags  <- rdhs::dhs_tags()
tagid <- tags[TagName == "Fertility Rates", TagID]

ind    <- rdhs::dhs_indicators()


edu_id <- ind[grepl("education", Definition)][10]$IndicatorId
metneed_id <- ind[grepl("unmet", Definition)][3]$IndicatorId


ind_id <- ind[1:9,.(IndicatorId, Definition)]$IndicatorId

dhs_subnat <- dhs_data(tagIds = tagid, 
                       countryIds = "NG", 
                       surveyYearStart = 1970, 
                       breakdown = "subnational", 
                       indicatorIds = ind_id)[IndicatorId %in% ind_id] %>% data.table

dhs_subnat <- dhs_data( countryIds = "NG", 
                       surveyYearStart = 1970, 
                       breakdown = "subnational", 
                       indicatorIds = edu_id)[IndicatorId %in% edu_id] %>% data.table
metneed <- dhs_data( countryIds = "NG", 
                        surveyYearStart = 1970, 
                        breakdown = "subnational", 
                        indicatorIds = metneed_id)#[IndicatorId %in% metneed_id] %>% data.table


