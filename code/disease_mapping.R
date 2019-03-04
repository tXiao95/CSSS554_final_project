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
ggplot(df, aes(nbirths/py, pred/py)) + geom_point(alpha=0.5) + geom_abline() + facet_wrap(~age_start)

#' Model 2: Poisson-Gamma Model 


#' Model 3: Poisson-LogNormal Model 
df[,STATEID:=.GRP, by = STATE]
inla.fit <- inla(nbirths ~ 1 + as.factor(age_start) + f(STATEID, model = "iid"), E = py, data = df[], family = "poisson", control.predictor = list(compute = T))
df[, inlaest := inla.fit$summary.fitted.values$`0.5quant`]
ggplot(df, aes(asfr, inlaest)) + geom_point() + geom_abline() + facet_wrap(~age_start)

#' Model 4: Poisson-LogNormal-Spatial Model 
formula <- nbirths ~ 1 + as.factor(age_start) + f(STATEID, model = "bym2", graph = paste0(shp_path, "nga.graph"),
                       scale.model = T, constr = T,
                       hyper = list(phi=list(prior="pc", param=c(0.5, 0.5), initial = 1),
                                    prec = list(prior="pc.prec", param = c(0.3, 0.01), initial = 5)))
spatial.fit <- inla(formula, data = df, family = "poisson",
                    E = py, verbose = T, control.predictor = list(compute=T))
df[, spatialest := spatial.fit$summary.fitted.values$`0.5quant`]
ggplot(df, aes(asfr, spatialest)) + geom_point() + geom_abline() + facet_wrap(~age_start)

#' Model 5: Empirical Bayes Model 