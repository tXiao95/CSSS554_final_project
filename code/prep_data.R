library(data.table)
library(rdhs)
library(haven)
library(ggplot2)
library(argparse)

#' Necessary variables
#' b3: CMC DOB of child
#' v011: CMC DOB of respondent (mother)
#' v008: CMC date of interview
#' v005: Woman's individual sample weight
#' v020: Ever married indicator
#' awfactt: All woman factor (Only needed with ever-married samples)
#' v021: primary sampling unit
#' v022: Strata

to_cmc  <- function(y){12 * (y - 1900) + 1}
inv_cmc <- function(cmc){(cmc - 1) / 12 + 1900}

#' Set up and read in variables
downloads <- readRDS("downloads.rds")
datasets  <- fread("datasets.csv")

#' Identify variables to pull from each dataset
nec_var <- c("caseid", "b3", "v011", "v008", "v005", "v020", "awfactt", "v021","v022", "v023", "v024", "v025")
rename  <- c("id", "cmc_birth", "cmc_mom_dob", "cmc_interview", "pweight", "ever_married", "awfactt", 
             "psu", "strata","self_weight", "region_residence", "urban_rural")
var_map <- data.table(short = nec_var, full = rename)

#' Perform operations on both the Birth and Individual Recode
br_dfs  <- datasets[FileType == "Births Recode"]
ir_dfs  <- datasets[FileType == "Individual Recode"]
gps_dfs <- datasets[FileType == "Geographic Data"]

br <- rdhs::search_variables(br_dfs$FileName, variables = nec_var, reformat = T) %>%
      rdhs::extract_dhs(add_geo = T) %>%
      data.table::rbindlist(fill = T) 

ir <- rdhs::search_variables(ir_dfs$FileName, variables = nec_var, reformat = T) %>%
      rdhs::extract_dhs(add_geo = T) %>%
      data.table::rbindlist(fill = T)

gps <- (lapply(gps_dfs$FileName, function(x){
  x <- gsub("ZIP|zip", "rds", x)
  df <- readRDS(paste0("data/NGA/datasets/", x))
  df
}))

br[, ADM1NAME := ifelse(ADM1NAME == "NULL", NA, ADM1NAME)]
br[, ADM1NAME := toslower(ADM1NAME)]
ir[, ADM1NAME := ifelse(ADM1NAME == "NULL", NA, ADM1NAME)]
ir[, ADM1NAME := tolower(ADM1NAME)]
names(br)[names(br) %in% var_map$short] <- var_map[short %in% names(br),full]
names(ir)[names(ir) %in% var_map$short] <- var_map[short %in% names(ir),full]

collapse_asfr <- function(br, ir, recall_yr = 3, length_of_period_yr = 3, age_bins = 5, level = "all"){
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
  age_bin_months          <- 12 * age_bins
  recall_months           <- 12 * recall_yr
  length_of_period_months <- 12 * length_of_period_yr
  
  if (!(age_bins %in% c(1, 5))){stop("Only 5-yr or 1-yr intervals allowed")}
  if (!(level %in% c("all", "admin1"))){stop("Only 'all' or 'admin1' aggregations allowed")}
  
  ####### NUMERATOR ##################
  #' Begin by calculating the dataset holding the numberator. Exclude 1 month before interview due to censoring
  birth[, period_birth_months := cmc_interview - cmc_birth]
  #' -1 from months so that X months before interview is included in the preceding period
  birth[, period := (period_birth_months - 1) %/% (length_of_period_months)] 
  # Include all births from end of recall period to 1 months before the interview (since month of interview is censored)
  birth <- birth[between(period_birth_months, 1, recall_months, incbounds = T)] 
  # Calculate age group of women at time of birth - number of months divided by months in age interval
  birth[, age := age_bins * floor((cmc_birth - cmc_mom_dob) / age_bin_months)]
  collapse_birth <- birth[, .(nbirths = sum(pweight / 1e6)), 
                              by = .(period, age, SurveyId)]
  
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
  
  indiv <- indiv[between(age, 10, 45)]
  
  #' If the point at which the woman reached the beginning of the age group is before the point at which the period starts, 
  #' then replace the value of start with the point at which ther period began
  indiv[, age_group_month_start := ifelse(12 * age < age_months_begin_period, age_months_begin_period, 12 * age)]
  #' If the point at which the woman reached the end of the age group is after the point at which the period ends, 
  #' then replace the value of end with the point at which the period ends
  indiv[, age_group_month_end := ifelse(12 * (age + 5) > age_months_end_period, age_months_end_period, 12 * (age + 5))]
  indiv[, months_exposure := age_group_month_end - age_group_month_start]
  
  #' Collapse to person years per age group and weighted CMC for the whole group
  collapse_indiv <- indiv[,.(py = sum(pweight/1e6 * months_exposure) / 12, 
                             cmc = weighted.mean(cmc_period, pweight/1e6)), .(age, period, SurveyId)]
  
  #' Merge the births data with the women's PY 
  final_df <- merge(collapse_birth, collapse_indiv, by = c("age", "period", "SurveyId"))
  final_df[,asfr := nbirths / py]
  final_df[,year := floor((cmc - 1) / 12) + 1900]
  
  #' Assume the counts of fertility have Poisson distributions...though fertility counts 
  #' are usually much more frequent than usual disease outcomes
  final_df[, se := sqrt(nbirths) / py]
  final_df[, lower := 0.5 * qchisq(0.05 / 2, 2 * nbirths) / py]
  final_df[, upper := 0.5 * qchisq(1 - 0.05 / 2, 2 * (nbirths + 1)) / py]
  
  
  final_df
}

final_df <- collapse_asfr(br, ir, recall_yr = 15, length_of_period_yr = 3, age_bins = 5, level = "all")

readr::write_csv(final_df, paste0("data/prepped/data.csv"))
