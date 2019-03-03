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


ind[grepl("education", Definition)]
ind[grepl("contraception", Definition)]
ind[grepl("religion", Definition)]
ind[grepl("labor", Definition)]


ind_id <- ind[1:9,.(IndicatorId, Definition)]$IndicatorId

dhs_subnat <- dhs_data(tagIds = tagid, 
                       countryIds = "NG", 
                       surveyYearStart = 1970, 
                       breakdown = "subnational", 
                       indicatorIds = ind_id)[IndicatorId %in% ind_id] %>% data.table