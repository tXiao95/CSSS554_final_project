library(data.table)
library(ggplot2)
library(INLA)
library(RColorBrewer)
library(gridExtra)

home_dir <- "C:/Users/twh42/Documents/UW_Class/CSSS_554/final_project"
setwd(home_dir)
shp_path <- "data/NGA/shape_files/2010/shps/"

#' Set parameters
recall   <- 3
length   <- 3
age_bins <- 5

#' Read in data
asfr_path <- sprintf("data/prepped/asfr_recall_%d_length_%d_age_%d.csv", recall, length, age_bins)
tfr_path  <- sprintf("data/prepped/tfr_recall_%d_length_%d_age_%d.csv", recall, length, age_bins)
asfr      <- fread(asfr_path)[SurveyId == "NG2013DHS" & STATE != "national"]#; asfr[, inverse_variance :=  1/se^2]
tfr       <- fread(tfr_path)

#' Data: ASFR vs. Standard Error 
ggplot(asfr, aes(asfr, se)) + geom_point(alpha=0.5, aes(color = py), size = 2)

#' Model 1: Quasi-Poisson Model
df <- asfr[]
model <- glm(nbirths ~ 1 + as.factor(age_start), family = "quasipoisson", data = df, offset = log(py))

df[, pred := predict(model, type = "response")]
ggplot(df, aes(nbirths/py, pred/py)) + geom_point(alpha=0.5) + geom_abline()

#' Model 2: Poisson-Gamma Model 


#' Model 3: Poisson-LogNormal Model 
inla.fit <- inla(nbirths ~ 1 + as.factor(age_start) + f(STATE, model = "iid"), E = py, data = df, family = "poisson", control.predictor = list(compute = T))
df[, inlaest := inla.fit$summary.fitted.values$`0.5quant`]

ggplot(df, aes(inlaest, asfr)) + geom_point() + geom_abline() + facet_wrap(~age_start)

#' Model 4: Poisson-LogNormal-Spatial Model 

#' Model 5: Empirical Bayes Model 