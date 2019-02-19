library(data.table)
library(ggplot2)
library(rdhs)
library(argparse)
library(magrittr)
library(readr)

home_dir <- "C:/Users/twh42/Documents/UW_Class/CSSS_554/final_project"
setwd(home_dir)

collapse_asfr <- function(level = "all", br, ir, recall_yr = 3, length_of_period_yr = 3, age_bins = 5){
  #' @description Uses the BR and IR microdata to collapse into 15-49 ASFR 
  #' @param br : A data.table with BR module
  #' @param ir : A data.table with IR module
  #' @param recall_yr : How many years before the interview to count births. 
  #' @param length_period_yr : The number of years to calculate ASFR over within the Recall period. For example, 
  #' a recall of 15 years and a length of period of 3 years would result in 5 calculations of ASFR 
  #' @param age_bins: The age intervals to collapse ASFR into - either single year or 5-yr
  #' @param level : The level of aggregation to perform the collapse on. Can be either
  
  birth <- copy(br)
  indiv <- copy(ir)
  message(paste0("Age Interval: ", age_bins, " Years"))
  message(paste0("Recall Length: ", recall_yr, " Years"))
  message(paste0("Estimation Period length: ", length_of_period_yr, " Years"))
  message(paste0("Collapsing to level: ", level))
  age_bin_months          <- 12 * age_bins
  recall_months           <- 12 * recall_yr
  length_of_period_months <- 12 * length_of_period_yr
  
  #' Checks on arguments
  if (!(class(ir)[1] == "data.table" & class(br)[1] == "data.table")){stop("BR and IR datasets must be data.table")}
  if (!(age_bins %in% c(1, 5))){stop("Only 5-yr or 1-yr intervals allowed")}
  if (!(level %in% c("all", "admin1"))){stop("Only 'all' or 'admin1' aggregations allowed")}
  
  if(level == "all"){
    id.vars <- c("age", "period", "SurveyId")
  } else if(level == "admin1"){
    id.vars <- c("age", "period", "SurveyId", "ADM1NAME")
  }
  
  ####### NUMERATOR ##################
  #' Begin by calculating the dataset holding the numberator. Exclude 1 month before interview due to censoring
  birth[, period_birth_months := cmc_interview - cmc_birth]
  #' -1 from months so that X months before interview is included in the preceding period
  birth[, period := (period_birth_months - 1) %/% (length_of_period_months)] 
  # Include all births from end of recall period to 1 months before the interview (since month of interview is censored)
  birth <- birth[between(period_birth_months, 1, recall_months, incbounds = T)] 
  # Calculate age group of women at time of birth - number of months divided by months in age interval
  birth[, age := age_bins * floor((cmc_birth - cmc_mom_dob) / age_bin_months)]
  collapse_birth <- birth[, .(nbirths = sum(pweight / 1e6)), by = id.vars]
  
  ####### DENOMINATOR ###############
  #' Now calculate the denominator - need to count the number of person years each woman 
  #' 1. Calculate number of estimation periods allowed for within the recall period defined
  indiv[, num_periods := recall_months / length_of_period_months]
  #' Expand the women's dataset so that each women is counted the number of estimation periods she lived through
  indiv <- indiv[rep(1:.N, num_periods)][, period := 0:(.N-1), by = .(id, SurveyId)]
  #' 2. Calculate the beginning and end age group the woman contributed exposure to within each estimation period. Subtract 1 is for counting final 
  #' age at the 1 month preceding the month of the interview
  indiv[, age_end_period := age_bins * floor((cmc_interview - 1 - period * length_of_period_months - cmc_mom_dob) / age_bin_months)]
  indiv[, age_begin_period := age_bins * floor((cmc_interview - 1 - (period + 1) * length_of_period_months - cmc_mom_dob) / age_bin_months)]
  
  #' Only include respondents that at least start in the very beginning of any period at least 10-14
  indiv <- indiv[age_begin_period >= 10]
  
  #' 3. Calculate the number of age groups that woman contributed to within the estimation period
  indiv[, num_age_groups := 1 + (age_end_period - age_begin_period) / age_bins]
  #' 4. Expand the data drame so each woman is counted the number of age groups she lived through within each period
  indiv <- indiv[rep(1:.N, num_age_groups)][, age_step := 0:(.N-1), by = .(id, SurveyId, period)]
  #' 5. Calculate the age group that the given row corresponds to. Now have period-age specific rows
  indiv[, age := age_begin_period + age_step * age_bins]
  #' 6. Compute the number of months the woman contributed to an age group for the given "period-age"
  #' First, calculate the age in months of each woman at the beginning and end of the estimation period - across all the ages lived
  indiv[, cmc_exp_end := cmc_interview - (period * length_of_period_months) - 1]
  indiv[, cmc_period := cmc_exp_end - (length_of_period_months * 0.5)]
  indiv[, age_months_end_period := (cmc_interview - 1) - (period * length_of_period_months) - cmc_mom_dob]
  indiv[, age_months_begin_period := (cmc_interview - 1) - ((period + 1) * length_of_period_months) - cmc_mom_dob]
  
  indiv <- indiv[between(age, 15, 45)]
  
  #' If the point at which the woman reached the beginning of the age group is before the point at which the period starts, 
  #' then replace the value of start with the point at which ther period began
  indiv[, age_group_month_start := ifelse(12 * age < age_months_begin_period, age_months_begin_period, 12 * age)]
  #' If the point at which the woman reached the end of the age group is after the point at which the period ends, 
  #' then replace the value of end with the point at which the period ends
  indiv[, age_group_month_end := ifelse(12 * (age + 5) > age_months_end_period, age_months_end_period, 12 * (age + 5))]
  indiv[, months_exposure := age_group_month_end - age_group_month_start]
  
  #' Collapse to person years per age group and weighted CMC for the whole group
  collapse_indiv <- indiv[,.(py = sum(pweight/1e6 * months_exposure) / 12, 
                             cmc = weighted.mean(cmc_period, pweight/1e6)), id.vars]
  
  #' Merge the births data with the women's PY 
  final_df <- merge(collapse_birth, collapse_indiv, by = id.vars)
  final_df[,asfr := nbirths / py]
  final_df[,year := floor((cmc - 1) / 12) + 1900]
  
  #' Data variance calculation: Assume the counts of fertility have Poisson distributions...though fertility counts 
  #' are usually much more frequent than usual disease outcomes
  final_df[, se := sqrt(nbirths) / py]
  final_df[, lower := 0.5 * qchisq(0.05 / 2, 2 * nbirths) / py]
  final_df[, upper := 0.5 * qchisq(1 - 0.05 / 2, 2 * (nbirths + 1)) / py]
  
  if(level == "admin1"){
    final_df <- final_df[!is.na(ADM1NAME)]
  }
  
  #' Create age interval columns
  setnames(final_df, "age", "age_start")
  final_df[, age_end := age_start + (age_bins - 1)]
  final_df[, length_of_period_yr := length_of_period_yr]
  final_df[, recall_yr := recall_yr]
  final_df[, age_bins := age_bins]
  final_df[, period_year := as.numeric(gsub("NG|DHS|MIS", "", SurveyId)) - 0.5 * (period + 1) * length_of_period_yr]
  
  final_df
}

##### MAIN SCRIPT ###########
br <- fread("data/prepped/birth_recode.csv")
ir <- fread("data/prepped/women_recode.csv")

#' DHS uses recall of 3, length of period of 3, and age intervals of 5 yrs
recall_yr           <- 15
length_of_period_yr <- 3
age_bins            <- 5
filename_asfr       <- sprintf("asfr_recall_%d_length_%d_age_%d.csv", 
                               recall_yr, length_of_period_yr, age_bins)
filename_tfr        <- sprintf("tfr_recall_%d_length_%d_age_%d.csv", 
                               recall_yr, length_of_period_yr, age_bins)

df <- rbindlist(lapply(c("all", "admin1"), collapse_asfr, br, ir, recall_yr, length_of_period_yr, age_bins), fill = T)
df[is.na(ADM1NAME), ADM1NAME := "national"]

tfr <- df[,.(tfr = 5 * sum(asfr), 
             age_start = min(age_start), 
             age_end = max(age_end), 
             num_age = .N), by = .(SurveyId, period, period_year, ADM1NAME, length_of_period_yr, recall_yr, age_bins)]

#' Only take TFR data that has all 7 age groups within 15-49, or TFR over 15-44
tfr <- tfr[num_age == 7 | (age_start == 15 & age_end == 44 & num_age == 6)]

readr::write_csv(df, paste0("data/prepped/", filename_asfr))
readr::write_csv(tfr, paste0("data/prepped/", filename_tfr))
