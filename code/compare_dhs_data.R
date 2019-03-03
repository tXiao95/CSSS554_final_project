library(rdhs)
library(data.table)
library(readr)

home_dir <- "C:/Users/twh42/Documents/UW_Class/CSSS_554/final_project"
setwd(home_dir)

dhsnat <- fread("data/prepped/dhs_national.csv")
dhssub <- fread("data/prepped/dhs_subnational.csv")
tfr    <- fread("data/prepped/tfr_recall_3_length_3_age_5.csv")
asfr    <- fread("data/prepped/asfr_recall_3_length_3_age_5.csv")

#' 1. Compare National TFR
dhsnattfr <- dhsnat[age_start == "TFR"]
comp1 <- merge(dhsnattfr[, .(SurveyId, Value)], 
               tfr[STATE == "national",.(SurveyId, tfr)], by = "SurveyId")
ggplot(comp1, aes(Value, tfr)) + geom_text(aes(label = SurveyId)) + geom_abline()

#' 2. Compare Subnational TFR
dhssubtfr <- dhssub[age_start == "TFR"]
comp2 <- merge(dhssubtfr[,.(SurveyId, location, Value)], 
               tfr[, .(SurveyId, STATE, tfr)], by.x = c("SurveyId", "location"), by.y = c("SurveyId", "STATE"))
ggplot(comp2, aes(Value, tfr)) + 
  geom_text(aes(label = location)) + 
  geom_abline() + 
  xlab("DHS") + 
  ylab("Manual Calculation") + 
  facet_wrap(~SurveyId) + 
  theme_classic()

#' 3. Compare National ASFR
dhsnatasfr <- fsetdiff(dhsnat, dhsnattfr)
dhsnatasfr[, `:=`(age_start = as.numeric(age_start), age_end = as.numeric(age_end))]
comp3 <- merge(dhsnatasfr[,.(SurveyId, Value, age_start, age_end)], 
      asfr[STATE =="national",.(SurveyId, asfr, age_start, age_end)], by = c("SurveyId", "age_start", "age_end"))
comp3[, asfr := asfr * 1000]
ggplot(comp3, aes(Value, asfr)) + 
  geom_text(aes(label = age_start)) + 
  geom_abline() + 
  xlab("DHS") + 
  ylab("Manual Calculation") + 
  facet_wrap(~SurveyId) 

#' 4. Compare Subnational ASFR
dhssubasfr <- fsetdiff(dhssub, dhssubtfr)
dhssubasfr[, `:=`(age_start = as.numeric(age_start), age_end = as.numeric(age_end))]
comp4 <- merge(dhssubasfr[,.(SurveyId, location, Value, age_start, age_end)], 
               asfr[STATE !="national",.(SurveyId, STATE, asfr, age_start, age_end)], 
               by.x = c("SurveyId", "location", "age_start", "age_end"), by.y = c("SurveyId", "STATE", "age_start", "age_end"))
comp4[, asfr := asfr * 1000]
ggplot(comp4, aes(Value, asfr)) + 
  #geom_text(aes(label = location)) + 
  geom_point(alpha=0.5) + 
  geom_abline() + 
  xlab("DHS") + 
  ylab("Manual Calculation") + 
  facet_wrap(~age_start) 
