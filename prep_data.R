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

#' Set up and read in variables
downloads <- readRDS(here("downloads.rds"))
datasets  <- fread(here("datasets.csv"))

#' Identify variables to pull from each dataset
nec_var <- c("b3", "v011", "v008", "v005") #, "awfactt")
rename  <- c("cmc_birth", "cmc_mom_dob", "cmc_interview", "pweight") #, "awfactt")
var_map <- data.table(short = nec_var, full = rename)

#' Perform operations on both the Birth and Individual Recode
br_dfs <- datasets[FileType == "Births Recode"]
ir_dfs <- datasets[FileType == "Individual Recode"]

br <- rdhs::search_variables(br_dfs$FileName, variables = nec_var) %>%
      rdhs::extract_dhs(add_geo = T) %>%
      data.table::rbindlist(fill = T)

ir <- search_variables(ir_dfs$FileName, variables = nec_var) %>%
      extract_dhs(add_geo = T) %>%
      rbindlist(fill = T)

names(br)[names(br) %in% var_map$short] <- var_map[short %in% names(br),full]
names(ir)[names(ir) %in% var_map$short] <- var_map[short %in% names(ir),full]

#' Calculate how many months before the interview the birth occurred
br[, birth_period := cmc_interview - cmc_birth]
br[, agegroup := as.integer((cmc_birth - cmc_mom_dob) / 60)] # age at time of birth
br <- br[birth_period <= recall]

#' Maximum number of months can spend in a single 5-yr age group: 5 * 12 = 60

ir[, lowagegroup := as.integer((cmc_interview - cmc_mom_dob))]


#' We use the BR module to calculate the numerator in ASFR, and use the 
#' IR module to calculate the denominator. 