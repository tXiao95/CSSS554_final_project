library(data.table)
library(ggplot2)

asfr <- fread("data/prepped/asfr_recall_3_length_3_age_5.csv")
tfr <- fread("data/prepped/tfr_recall_3_length_3_age_5.csv")

figures_path <- "figures/"


pdf(file = paste0(figures_path, "age_pattern_subnat.pdf"), height = 13, width = 18)
for(loc in unique(asfr$ADM1NAME)){
  message(loc)
  gg <- ggplot(asfr[ADM1NAME == loc], aes((age_start + age_end) / 2, asfr)) + 
    geom_point(aes(col = SurveyId), alpha = 0.5, size = 3) + 
    geom_line(aes(col = SurveyId)) + 
    #geom_errorbarh(aes(xmin = age_start, xmax = age_end, col = SurveyId), height = 0) + 
    #geom_errorbar(aes(ymin = lower, ymax = upper, col = SurveyId), width = 0) + 
    facet_wrap(~period) + 
    theme_bw(base_size = 25) + 
    xlab("Age") + 
    ylab("ASFR") + 
    ggtitle(loc)
  print(gg)
}
dev.off()

pdf(file = paste0(figures_path, "timeseries_subnat.pdf"), height = 13, width = 18)
for(loc in unique(asfr$ADM1NAME)){
  message(loc)
  gg <- ggplot(asfr[ADM1NAME == loc], aes(year, asfr)) + 
    geom_point(aes(col = SurveyId), size = 3, alpha = 0.5) +
    geom_errorbar(aes(ymin = lower, ymax = upper, col = SurveyId), width = 0) + 
    facet_wrap(~age_start) + 
    theme_bw(base_size = 30) + 
    ggtitle(loc)
  print(gg)
}
dev.off()

setnames(df, "age", "age_start")
df[, age_end := age_start ]
# Time Series for each age group
pdf(file = paste0(figures_path, "time_series.pdf"), height = 13, width = 18)
gg1 <- ggplot(final_df, aes(year, asfr)) + 
  geom_point(aes(col = SurveyId), size = 3, alpha = 0.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper, col = SurveyId), width = 0) + 
  facet_wrap(~age) + 
  theme_bw(base_size = 30)
print(gg1)
dev.off()

pdf(file = paste0(figures_path, "asfr_se_scatter.pdf"), height = 13, width = 18)
gg3 <- ggplot(df, aes(asfr, se)) + 
  geom_point(size = 5) + 
  ggtitle("Rate vs. SE Scatter") + 
  theme_bw(base_size = 25)
print(gg3)
dev.off()
