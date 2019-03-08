library(data.table)
library(ggplot2)
library(INLA)
library(RColorBrewer)
library(gridExtra)
library(spdep)

home_dir <- "C:/Users/twh42/Documents/UW_Class/CSSS_554/final_project"
setwd(home_dir)
shp_path <- "data/NGA/shape_files/2010/shps/"

#' Set parameters
recall   <- 3
length   <- 3
age_bins <- 5
sid      <- "NG2008DHS"

#' Read in data
asfr_path <- sprintf("data/prepped/asfr_recall_%d_length_%d_age_%d.csv", recall, length, age_bins)
tfr_path  <- sprintf("data/prepped/tfr_recall_%d_length_%d_age_%d.csv", recall, length, age_bins)
asfr      <- fread(asfr_path)[SurveyId == sid & STATE != "national"]
tfr       <- fread(tfr_path)

#' Data: ASFR vs. Standard Error 
ggplot(asfr, aes(log(py), se)) + geom_point(alpha=0.5, aes(color = py), size = 2) + xlab("Log of Person-years") + ylab("Standard Error")

#' Model 1: Quasi-Poisson Model
df <- asfr[]
model <- glm(nbirths ~ 1 + as.factor(age_start), family = "quasipoisson", data = df, offset = log(py))
df[, pred := predict(model, df, type = "response")]
df[, theta := mean(pred / py), by = age_start]
df <- df[order(age_start, asfr), ]
df[, index := 1:.N, age_start]
ggplot(df, aes(index, nbirths/py)) + geom_point(alpha=0.5) + 
  facet_wrap(~age_start) + geom_hline(aes(yintercept = theta), linetype = "dashed", color = "red", size = 1) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) + 
  theme_bw() + xlab("Index of State") + 
  ylab("ASFR")

#' Model 3: Poisson-LogNormal Model - Age fixed effect 
df[,STATEID:=.GRP, by = STATE]
inla.fit <- inla(nbirths ~ 1 + as.factor(age_start) + f(STATEID, model = "iid"), E = py, data = df[], family = "poisson", control.predictor = list(compute = T))
df[, inlaest := inla.fit$summary.fitted.values$`0.5quant`]
df[, inlalower := inla.fit$summary.fitted.values$`0.025quant`]
df[, inlaupper := inla.fit$summary.fitted.values$`0.975quant`]
ggplot(df, aes(index, inlaest)) + geom_point(alpha=0.6, color = "blue") + facet_wrap(~age_start) + 
  geom_hline(aes(yintercept = theta), linetype = "dashed", color = "black", size = 1) + 
  geom_point(aes(index, nbirths/py), color = "red", alpha=0.6) + 
  #geom_errorbar(aes(ymin = lower, ymax = upper), color = "red", width = 0, alpha = 0.6) + 
  geom_errorbar(aes(ymin = inlalower, ymax = inlaupper), color = "blue", width = 0, alpha = 0.6) + 
  theme_bw() + xlab("Index of State") + ylab("ASFR Estimate")

#' Model 3: Poisson-LogNormal Model - Separate Age Groups
df[,STATEID:=.GRP, by = STATE]
dt <- df[age_start == 25]
inla.fit <- inla(nbirths ~ 1 + f(STATEID, model = "iid"), E = py, data = dt, family = "poisson", control.predictor = list(compute = T))
dt[, inlaest := inla.fit$summary.fitted.values$`0.5quant`]
dt[, inlalower := inla.fit$summary.fitted.values$`0.025quant`]
dt[, inlaupper := inla.fit$summary.fitted.values$`0.975quant`]
ggplot(dt, aes(index, inlaest)) + geom_point(alpha=0.6, color = "blue") + facet_wrap(~age_start) + 
  geom_hline(aes(yintercept = theta), linetype = "dashed", color = "black", size = 1) + 
  geom_point(aes(index, nbirths/py), color = "red", alpha=0.6) + 
  #geom_errorbar(aes(ymin = lower, ymax = upper), color = "red", width = 0, alpha = 0.6) + 
  geom_errorbar(aes(ymin = inlalower, ymax = inlaupper), color = "blue", width = 0, alpha = 0.6) + 
  theme_bw() + xlab("Index of State") + ylab("ASFR Estimate")

ggplot(df, aes(asfr, inlaest)) + geom_point(alpha=0.7) + facet_wrap(~age_start) + geom_abline() + theme_bw() + xlab("ASFR") + ylab("INLA")

#' Model 4: Poisson-LogNormal-Spatial Model 
formula <- nbirths ~ 1 + as.factor(age_start) + f(STATEID, model = "bym2", graph = paste0(shp_path, "nga.graph"),
                       scale.model = T, constr = T,
                       hyper = list(phi=list(prior="pc", param=c(0.5, 0.5), initial = 1),
                                    prec = list(prior="pc.prec", param = c(0.3, 0.01), initial = 5)))

spatial.fit <- inla(formula, data = df, family = "poisson",
                    E = py, verbose = T, control.predictor = list(compute=T))
df[, spatialest := spatial.fit$summary.fitted.values$`0.5quant`]
df[, spatiallower := spatial.fit$summary.fitted.values$`0.025quant`]
df[, spatialupper := spatial.fit$summary.fitted.values$`0.975quant`]
ggplot(df, aes(asfr, spatialest)) + geom_point() + geom_abline() + facet_wrap(~age_start)
ggplot(df, aes(inlaest, spatialest)) + geom_point() + geom_abline() + facet_wrap(~age_start) 