library(data.table)
library(rdhs)
library(ggplot2)
library(argparse)
library(magrittr)

#' Necessary variables
#' b3: CMC DOB of child
#' v011: CMC DOB of respondent (mother)
#' v008: CMC date of interview
#' v005: Woman's individual sample weight
#' v020: Ever married indicator
#' awfactt: All woman factor (Only needed with ever-married samples)
#' v021: primary sampling unit
#' v022: Strata

home_dir <- "C:/Users/twh42/Documents/UW_Class/CSSS_554/final_project"
setwd(home_dir)
outdir <- "data/prepped/"

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
br[, ADM1NAME := tolower(ADM1NAME)]
br[ADM1NAME == "abuja", ADM1NAME := "fct-abuja"]
ir[, ADM1NAME := ifelse(ADM1NAME == "NULL", NA, ADM1NAME)]
ir[, ADM1NAME := tolower(ADM1NAME)]
ir[ADM1NAME == "abuja", ADM1NAME := "fct-abuja"]
ir[ADM1NAME == "fct abuja", ADM1NAME := "fct-abuja"]
ir[ADM1NAME == "borno - urban", ADM1NAME := "borno"]
names(br)[names(br) %in% var_map$short] <- var_map[short %in% names(br),full]
names(ir)[names(ir) %in% var_map$short] <- var_map[short %in% names(ir),full]

readr::write_csv(br, paste0(outdir, "birth_recode.csv"))
readr::write_csv(ir, paste0(outdir, "women_recode.csv"))
saveRDS(gps, paste0(outdir, "gps.rds"))


