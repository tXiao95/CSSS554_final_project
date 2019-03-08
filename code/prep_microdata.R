library(data.table)
library(rdhs)
library(ggplot2)
library(argparse)
library(magrittr)

#' Necessary variables
#' b3: CMC DOB of child
#' v005: Woman's individual sample weight
#' v006: Month
#' v007: Year 
#' v008: CMC Date of Interview
#' v011: CMC DOB of respondent (mother, woman)
#' v020: Ever married indicator
#' v021: Primary Sampling United
#' v022: Strata
#' v024: Regions
#' v025: Urban/Rural - Type of Place of residence
#' awfactt: All woman factor for total population(Only needed with ever-married samples)
#' awfactu: All woman factor for place of residence (urban/rural)
#' awfactr: All woman factor for region (according to country regions)
#' awfacte: All woman factor for education (none, primary, secondary, higher)
#' awfactw: All woman factor for wealth index (lowest, second, middle, fourth, highest)

home_dir <- "C:/Users/twh42/Documents/UW_Class/CSSS_554/final_project"
setwd(home_dir)
outdir <- "data/prepped/"

#' Set up and read in variables
downloads <- readRDS("downloads.rds")
datasets  <- fread("datasets.csv")

#' Identify variables to pull from each dataset - all births are in the women's dataset
b <- sapply(1:20, function(x){ifelse(nchar(x)==1, paste0("0",x),x)}) %>% paste0("b3_", .)
nec_var <- c("caseid", "cmc_birth", "v011", "v008", "v005", "v020", "awfactt",
             "v021","v022", "v023", "v024", "v025", b)
rename  <- c("id", "cmc_birth", "cmc_mom_dob", "cmc_interview", "pweight", "ever_married", "awfactt", 
             "psu", "strata","self_weight", "region_residence", "urban_rural", b)
var_map <- data.table(short = nec_var, full = rename)

#' Perform operations on both the Birth and Individual Recode
#br_dfs  <- datasets[FileType == "Births Recode"]
ir_dfs  <- datasets[FileType == "Individual Recode"]
#gps_dfs <- datasets[FileType == "Geographic Data"]

# br <- rdhs::search_variables(br_dfs$FileName, variables = nec_var, reformat = T) %>%
#       rdhs::extract_dhs(add_geo = T) %>%
#       data.table::rbindlist(fill = T) 

ir <- rdhs::search_variables(ir_dfs$FileName, variables = nec_var, reformat = T) %>%
      rdhs::extract_dhs(add_geo = T) %>%
      data.table::rbindlist(fill = T)

gps <- (lapply(gps_dfs$FileName, function(x){
  x <- gsub("ZIP|zip", "rds", x)
  df <- readRDS(paste0("data/NGA/datasets/", x))
  df
}))

#' Standardize Admin1 State names
# br[, ADM1NAME := ifelse(ADM1NAME == "NULL", NA, ADM1NAME)]
# br[, ADM1NAME := tolower(ADM1NAME)]
# br[ADM1NAME == "abuja", ADM1NAME := "fct-abuja"]
ir[, ADM1NAME := ifelse(ADM1NAME == "NULL", NA, ADM1NAME)]
ir[, ADM1NAME := tolower(ADM1NAME)]
ir[ADM1NAME == "abuja", ADM1NAME := "fct-abuja"]
ir[ADM1NAME == "fct abuja", ADM1NAME := "fct-abuja"]
ir[ADM1NAME == "borno - urban", ADM1NAME := "borno"]
# names(br)[names(br) %in% var_map$short] <- var_map[short %in% names(br),full]
names(ir)[names(ir) %in% var_map$short] <- var_map[short %in% names(ir),full]
id.vars <- names(ir) %>% setdiff(b)

#' Create births data from the IR data
br <- melt.data.table(ir, id.vars = id.vars, measure.vars = b, variable.name = "kidnum", value.name = "cmc_birth", na.rm=T)

#' Write Data 
readr::write_csv(br, paste0(outdir, "birth_recode.csv"))
readr::write_csv(ir, paste0(outdir, "women_recode.csv"))
saveRDS(gps, paste0(outdir, "gps.rds"))

# fert.rates <- fert(ir[SurveyId == "NG2015MIS"], Indicator = "asfr", 
#                    Strata = "strata", Cluster = "CLUSTER", Weight = "pweight", 
#                    Date_of_interview = "cmc_interview", 
#                    Woman_DOB = "cmc_mom_dob", Class = "ADM1NAME")
