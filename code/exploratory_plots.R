library(data.table)
library(ggplot2)

home_dir <- "C:/Users/twh42/Documents/UW_Class/CSSS_554/final_project"
setwd(home_dir)

#' Set parameters
recall   <- 15
length   <- 3
age_bins <- 5

#' Read in data
asfr_path <- sprintf("data/prepped/asfr_recall_%d_length_%d_age_%d.csv", recall, length, age_bins)
tfr_path  <- sprintf("data/prepped/tfr_recall_%d_length_%d_age_%d.csv", recall, length, age_bins)
asfr      <- fread(asfr_path)
tfr       <- fread(tfr_path)

age_pattern_path <- sprintf("figures/age_pattern_subnat_recall_%d_length_%d_age_%d.pdf", recall, length, age_bins)
time_series_path <- sprintf("figures/time_series_subnat_recall_%d_length_%d_age_%d.pdf", recall, length, age_bins)

#' Age pattern Plots for national and subnational
pdf(file = age_pattern_path, height = 13, width = 18)
for(loc in unique(asfr$STATE)){
  message(loc)
  gg <- ggplot(asfr[STATE == loc], aes((age_start + age_end) / 2, asfr)) + 
    geom_point(aes(col = SurveyId), alpha = 0.5, size = 4) + 
    #geom_line(aes(col = SurveyId)) + 
    geom_errorbarh(aes(xmin = age_start, xmax = age_end, col = SurveyId), height = 0, size = 1) + 
    geom_errorbar(aes(ymin = lower, ymax = upper, col = SurveyId), width = 0, size = 1) + 
    facet_wrap(~period_year) + 
    theme_bw(base_size = 25) + 
    xlab("Age") + 
    ylab("ASFR") + 
    ggtitle(loc) + 
    guides(size = F) + 
    coord_cartesian(ylim = c(0, 0.75)) 
  print(gg)
}
dev.off()

#' Time Series plot per Age group for national and subnational
pdf(file = time_series_path, height = 13, width = 18)
for(loc in unique(asfr$STATE)){
  message(loc)
  gg <- ggplot(asfr[STATE == loc], aes(period_year, asfr)) + 
    geom_point(aes(col = SurveyId), alpha = 0.5, size = 4) +
    geom_errorbar(aes(ymin = lower, ymax = upper, col = SurveyId), width = 0, size = 1, alpha = 0.7) + 
    facet_wrap(~age_start) + 
    theme_bw(base_size = 30) + 
    ggtitle(loc) + 
    coord_cartesian(ylim = c(0, 0.75)) + 
    xlab("Year") + 
    ylab("ASFR") + 
    guides(size = F)
  print(gg)
}
dev.off()
