library(data.table)
library(ggplot2)

df <- fread("data/prepped/data.csv")

figures_path <- "figures/"

setnames(df, "age", "age_start")
df[, age_end := age_start + 4]
# Time Series for each age group
pdf(file = paste0(figures_path, "time_series.pdf"), height = 13, width = 18)
gg1 <- ggplot(final_df, aes(year, asfr)) + 
  geom_point(aes(col = SurveyId), size = 3, alpha = 0.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper, col = SurveyId), width = 0) + 
  facet_wrap(~age) + 
  theme_bw(base_size = 30)
print(gg1)
dev.off()

# Age Pattern for each year
pdf(file = paste0(figures_path, "age_pattern.pdf"), height = 13, width = 18)
gg2 <- ggplot(df, aes((age_start + age_end) / 2, asfr)) + 
  geom_point(aes(col = SurveyId), alpha = 0.5, size = 3) +
  geom_errorbarh(aes(xmin = age_start, xmax = age_end, col = SurveyId), height = 0) + 
  geom_errorbar(aes(ymin = lower, ymax = upper, col = SurveyId), width = 0) + 
  facet_wrap(~year) + 
  theme_bw(base_size = 25) + 
  xlab("Age") + 
  ylab("ASFR")
print(gg2)
dev.off()

pdf(file = paste0(figures_path, "asfr_se_scatter.pdf"), height = 13, width = 18)
gg3 <- ggplot(df, aes(asfr, se)) + 
  geom_point() + 
  ggtitle("Rate vs. SE Scatter") + 
  theme_bw(base_size = 25)
print(gg3)
dev.off()
