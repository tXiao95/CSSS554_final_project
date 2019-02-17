library(data.table)
library(rdhs)
library(haven)
library(ggplot2)
library(argparse)

#' Take surveys I want and save to a more accessible format: convert form .rds to .csv
#' Necessary variables
#' b3: CMC DOB of child
#' v011: CMC DOB of respondent (mother)
#' v008: CMC date of interview
#' v005: Woman's individual sample weight
#' awfactt: All woman factor (Only needed with ever-married samples)

to_cmc  <- function(y){12 * (y - 1900) + 12}
inv_cmc <- function(cmc){(cmc - 12) / 12 + 1900}

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

ir <- search_variables(ir_dfs$FileName, variables = nec_var, reformat = T) %>%
      extract_dhs(add_geo = T) %>%
      rbindlist(fill = T)

gps <- (lapply(gps_dfs$FileName, function(x){
  x <- gsub("ZIP|zip", "rds", x)
  df <- readRDS(paste0("data/NGA/datasets/", x))
  df
}))

br[, ADM1NAME := ifelse(ADM1NAME == "NULL", NA, ADM1NAME)]
ir[, ADM1NAME := ifelse(ADM1NAME == "NULL", NA, ADM1NAME)]
br[, ADM1NAME := tolower(ADM1NAME)]
ir[, ADM1NAME := tolower(ADM1NAME)]
names(br)[names(br) %in% var_map$short] <- var_map[short %in% names(br),full]
names(ir)[names(ir) %in% var_map$short] <- var_map[short %in% names(ir),full]

collapse_asfr <- function(br, ir, recall_yr = 3, length_of_period_yr = 3, age_bins = 5){
  #' @description Uses the BR and IR microdata to collapse into 15-49 ASFR 
  #' @param br : A data.table with BR module
  #' @param ir : A data.table with IR module
  #' @param recall : How many months before the interview to count births. 
  #' @param length_period : The number of years to calculate ASFR 
  #' @param age_bins: Either 5 or 1 in years. The age intervals to collapse ASFR into - either single year or 5-yr
  
  birth <- copy(br)
  indiv <- copy(ir)
  message(paste0("Age Interval: ", age_bins, " Years"))
  message(paste0("Recall Length: ", recall_yr, " Years"))
  message(paste0("Estimation Period length: ", length_of_period_yr, " Years"))
  age_bin_months          <- 12 * age_bins
  recall_months           <- 12 * recall_yr
  length_of_period_months <- 12 * length_of_period_yr
  
  if (!(age_bins %in% c(1, 5))){stop("Only 5-yr or 1-yr intervals allowed")}
  
  
  ###### NUMERATOR ##################
  #' Begin by calculating the dataset holding the numberator. Exclude 1 month before interview due to censoring
  birth[, period_birth_months := cmc_interview - cmc_birth]
  #' -1 from months so that X months before interview is included in the preceding period
  birth[, period_birth := (period_birth_months - 1) %/% (length_of_period_months)] 
  # Include all births from end of recall period to 1 months before the interview (since month of interview is censored)
  birth <- birth[period_birth_months <= recall_months & period_birth_months >= 1] 
  # Calculate age group of women at time of birth - number of months divided by months in age interval
  birth[, age := age_bins * floor((cmc_birth - cmc_mom_dob) / age_bin_months)]
  collapse_birth <- birth[, .(nbirths = sum(pweight / 1e6)), 
                              by = .(period_birth, agegroup, CLUSTER, SurveyId)]
  
  ####### DENOMINATOR ###############
  #' Now calculate the denominator - need to count the number of person years each woman 
  #' 1. Calculate number of estimation periods allowed for within the recall period defined
  indiv[, num_periods := length(recall_months / length_of_period_months)]
  #' Expand the women's dataset so that each women is counted the number of periods she lived through
  indiv <- indiv[rep(1:.N, num_periods)][, period := 0:(.N-1), by = .(id, SurveyId)]
  #' 2. Calculate the beginning and end age group the woman contributed exposure to within each estimation period. Subtract 1 is for counting age 
  #' and periods starting from the 1 month preceding the month of the interview
  indiv[, age_end_period := age_bins * floor((cmc_interview - period * length_of_period_months - 1 - cmc_mom_dob) / age_bin_months)]
  indiv[, age_begin_period := age_bins * floor((cmc_interview - (period + 1) * length_of_period_months - 1 - cmc_mom_dob) / age_bin_months)]
  #' 3. Calculate the number of age groups that woman contributed to within the estimation period
  indiv[, num_age_groups := 1 + (age_end_period - age_begin_period) / age_bins]
  #' 4. Expand the data drame so each woman is counted the number of age groups she lived through within each period
  indiv <- indiv[rep(1:.N, num_age_groups)][, age_step := 0:(.N-1), by = .(id, SurveyId)]
  #' 5. Calculate the age group that the given row corresponds to
  indiv[, age := age_begin_period + age_step * age_bins]
  #' 6. Compute the number of months the woman contributed to for the given "period-age"
  
  
}
