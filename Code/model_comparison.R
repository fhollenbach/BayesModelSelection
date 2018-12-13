library(bayesplot)
library(brms)
library(rethinking)
library(tidyverse)
library(tidybayes)
library(xtable)

source("~/Documents/GitHub/BayesModelSelection/code/info_functions.R")

load("~/Dropbox/BayesChapter/Model_Results/model_HM_imp.rda")
load("~/Dropbox/BayesChapter/Model_Results/model_AR_imp.rda")
load("~/Dropbox/BayesChapter/Model_Results/model_lag_imp.rda")
load("~/Dropbox/BayesChapter/Model_Results/model_ineq_imp.rda")



##model HM
vars <- get_variables(model_HM)[c(1:12, 30)]

model.HM.95 <- model_HM %>% spread_draws(b_Intercept, b_L_Polity_s_interp, b_L_Fiscal_Rel_interp, b_D_Fiscal_Rel_Interp, b_L_D_Fiscal_Rel_Interp, b_L_logGDPPERCAP, b_L_CivilWar, b_L_REGION_DEM_DIFFUSE, b_L_WORLD_DEM_DIFFUSE, b_D_GDPPERCAP, b_D_RegionalDiffusion, b_D_WORLD_DEM_DIFFUSE, sigma) %>% mutate(longRun_fiscal =  b_L_Fiscal_Rel_interp /abs(b_L_Polity_s_interp)) %>% median_qi(.width = c(0.95))
vars  <- c(vars[1:12], "longRun_fiscal", vars[13])

est  <-round( model.HM.95[vars], 3)
CI.low <- matrix(as.numeric(model.HM.95[paste(vars, "lower", sep = ".")]), ncol = 1)
row.names(CI.low) <- vars
CI.high <- matrix(as.numeric(model.HM.95[paste(vars, "upper", sep = ".")]), ncol = 1)
row.names(CI.high) <- vars
CI <- data.frame(cbind(CI.low,CI.high))
CI$interval <- paste("(", round(CI$X1,3), ",", round(CI$X2,3), ")", sep = "")

col.HM <- unlist(sapply(seq_along(est), function(i) append(est[i], CI$interval[i], i)))
col.HM <- c(col.HM[1:24], rep(NA, 16), col.HM[25:28]) ### add 6 rows for AR, 8 for ineq & 2 for lag model




##model AR
vars <- get_variables(model_AR)[c(1:15, 33)]

model.AR.95 <- model_AR %>% spread_draws(b_Intercept, b_L_Polity_s_interp, b_L_Fiscal_Rel_interp, b_D_Fiscal_Rel_Interp, b_L_D_Fiscal_Rel_Interp, b_L_logGDPPERCAP, b_L_CivilWar, b_L_REGION_DEM_DIFFUSE, b_L_WORLD_DEM_DIFFUSE, b_D_GDPPERCAP, b_D_RegionalDiffusion, b_D_WORLD_DEM_DIFFUSE, b_post1980, b_post1980_L_Fiscal_Rel_interp, b_post1980_D_Fiscal_Rel_Interp, sigma) %>% mutate(longRun_fiscal =  b_L_Fiscal_Rel_interp /abs(b_L_Polity_s_interp)) %>% mutate(longRun_fiscal_post1980 =  b_post1980_L_Fiscal_Rel_interp/abs(b_L_Polity_s_interp)) %>% median_qi(.width = c(0.95))
vars  <- c(vars[1:15], "longRun_fiscal", vars[16])

est  <- round(model.AR.95[vars], 3)
CI.low <- matrix(as.numeric(model.AR.95[paste(vars, "lower", sep = ".")]), ncol = 1)
row.names(CI.low) <- vars
CI.high <- matrix(as.numeric(model.AR.95[paste(vars, "upper", sep = ".")]), ncol = 1)
row.names(CI.high) <- vars
CI <- data.frame(cbind(CI.low,CI.high))
CI$interval <- paste("(", round(CI$X1,3), ",", round(CI$X2,3), ")", sep = "")

col.AR <- unlist(sapply(seq_along(est), function(i) append(est[i], CI$interval[i], i)))
col.AR <- c(col.AR[1:30], rep(NA, 10), col.AR[31:34]) ### add 8 rows for ineq & 2 for lag model



##model ineq
vars <- get_variables(model_ineq)[c(1:16, 34)]

model.ineq.95 <- model_ineq %>% spread_draws(b_Intercept, b_L_Polity_s_interp, b_L_Fiscal_Rel_interp, b_D_Fiscal_Rel_Interp, b_L_D_Fiscal_Rel_Interp, b_L_logGDPPERCAP, b_L_CivilWar, b_L_REGION_DEM_DIFFUSE, b_L_WORLD_DEM_DIFFUSE, b_D_GDPPERCAP, b_D_RegionalDiffusion, b_D_WORLD_DEM_DIFFUSE, b_L_very_unequal_utip, b_D_very_unequal_utip, b_unequal_L_Fiscal_Rel_interp, b_unequal_D_Fiscal_Rel_Interp, sigma) %>% mutate(longRun_fiscal =  b_L_Fiscal_Rel_interp /abs(b_L_Polity_s_interp)) %>% mutate(longRun_fiscal_ineq =  b_unequal_L_Fiscal_Rel_interp/abs(b_L_Polity_s_interp)) %>% median_qi(.width = c(0.9))
vars  <- c(vars[1:16], "longRun_fiscal", vars[17])

est  <- round(model.ineq.95[vars], 3)
CI.low <- matrix(as.numeric(model.ineq.95[paste(vars, "lower", sep = ".")]), ncol = 1)
row.names(CI.low) <- vars
CI.high <- matrix(as.numeric(model.ineq.95[paste(vars, "upper", sep = ".")]), ncol = 1)
row.names(CI.high) <- vars
CI <- data.frame(cbind(CI.low,CI.high))
CI$interval <- paste("(", round(CI$X1,3), ",", round(CI$X2,3), ")", sep = "")

col.ineq <- unlist(sapply(seq_along(est), function(i) append(est[i], CI$interval[i], i)))
col.ineq <- c(col.ineq[1:24], rep(NA, 6), col.ineq[25:32], rep(NA, 2), col.ineq[33:36] ) ### add 6 rows for AR & 2 for lag model


##model lag
vars <- get_variables(model_lag)[c(1:13, 31)]

model.lag.95 <- model_lag %>% spread_draws(b_Intercept, b_L_Polity_s_interp, b_L_Fiscal_Rel_interp, b_D_Fiscal_Rel_Interp, b_L_D_Fiscal_Rel_Interp, b_L_logGDPPERCAP, b_L_CivilWar, b_L_REGION_DEM_DIFFUSE, b_L_WORLD_DEM_DIFFUSE, b_D_GDPPERCAP, b_D_RegionalDiffusion, b_D_WORLD_DEM_DIFFUSE, b_polity_L_Fiscal_Rel_interp, sigma) %>% mutate(longRun_fiscal =  b_L_Fiscal_Rel_interp /abs(b_L_Polity_s_interp)) %>% median_qi(.width = c(0.95))

vars  <- c(vars[1:13], "longRun_fiscal", vars[14])
est  <- round(model.lag.95[vars], 3)
CI.low <- matrix(as.numeric(model.lag.95[paste(vars, "lower", sep = ".")]), ncol = 1)
row.names(CI.low) <- vars
CI.high <- matrix(as.numeric(model.lag.95[paste(vars, "upper", sep = ".")]), ncol = 1)
row.names(CI.high) <- vars
CI <- data.frame(cbind(CI.low,CI.high))
CI$interval <- paste("(", round(CI$X1,3), ",", round(CI$X2,3), ")", sep = "")

col.lag <- unlist(sapply(seq_along(est), function(i) append(est[i], CI$interval[i], i)))
col.lag <- c(col.lag[1:24], rep(NA, 14),col.lag[25:26], col.lag[27:30]) ### add 6 rows for AR & 8 for Ineq model


length(col.HM)
length(col.AR)
length(col.ineq)
length(col.lag)

vars <-tibble("Intercept", "Polity t-1", "Fiscal Reliance t-1", "$Delta$ Fiscal Reliance", "$Delta$ Fiscal Reliance t-1", "log GDP pC t-1", "Civil War t-1", "Regional dem diffusion t-1", "Global dem diffusion t-1", "$Delta$ GDP pC", "$Delta$ Regional Dem Diffusion", "$Delta$ Global Dem Diffusion","post 1980", "post 1980 $times$ Fisc Rel t-1", "post 1980 $times$ $Delta$ Fisc Rel", "Inequality t-1", "$Delta$ Inequality", "Inequality t-1 $times$ Fisc Rel t-1", "$Delta$ Inequality $times$ $Delta$ Fisc Rel", "Polity t-1 $times$ Fisc Rel t-1","Long Run Fisc Rel", "Sigma")
vars_ci <- rep(NA, 22)
names <- unlist(sapply(seq_along(vars), function(i) append(vars[i], vars_ci[i], i)))

results <- tibble(Variable = names, HM = col.HM, AR = col.AR, Inequality = col.ineq, Polity = col.lag)



tab  <- xtable(results,  caption = "Model Results and 95 Credible Intervals", label = "tab:models", align = "llcccc")
print(tab, include.rownames = FALSE, booktabs = TRUE)

### deivance based on median estimate aic, dic, bic,
info_HM  <- info_crit(model_HM)
info_AR <- info_crit(model_AR)
info_ineq <- info_crit(model_ineq)
info_lag <- info_crit(model_lag)


####
### make Table with info criteria
models_criteria <- bind_rows(info_HM, info_AR, info_ineq, info_lag)
Model  <-as.tibble(c("HM", "AR", "Inequality", "Polity"))
names(Model) <- "Model"
models_criteria <- bind_cols(Model, models_criteria)

table  <- xtable(models_criteria, digits = 1, caption = "Information Critera", label = "tab:infoCrit", align = "llcccc")
print(table, include.rownames = FALSE, booktabs = TRUE)


names(models) <- "Model"

### waic
waic.hm <- waic(model_HM)$estimate[3, ]
waic.ar <- waic(model_AR)$estimate[3, ]
waic.ineq <- waic(model_ineq)$estimate[3, ]
waic.lag <- waic(model_lag)$estimate[3, ]

waic   <- bind_rows(waic.hm, waic.ar, waic.ineq, waic.lag)
Model  <-as.tibble(c("HM", "AR", "Inequality", "Polity"))
names(Model) <- "Model"
models_waic <- bind_cols(Model, waic)

table  <- xtable(models_waic, digits = 2, caption = "WAIC for all Models", label = "tab:waic", align = "llcc")
print(table, include.rownames = FALSE, booktabs = TRUE)


### kfold

kfold_cross  <- kfold(model_HM, model_AR, model_ineq, model_lag, compare = TRUE, k = 10)


kfold.hm <- kfold_cross$model_HM$estimate[3,]
kfold.ar <- kfold_cross$model_AR$estimate[3,]
kfold.ineq <- kfold_cross$model_ineq$estimate[3,]
kfold.lag <- kfold_cross$model_lag$estimate[3,]

kfold <- bind_rows(kfold.hm, kfold.ar, kfold.ineq, kfold.lag)
Model  <-as.tibble(c("HM", "AR", "Inequality", "Polity"))
names(Model) <- "Model"
models_kfold <- bind_cols(Model, kfold)

table  <- xtable(models_kfold, digits = 2, caption = "Kfold Crossvalidation for all Models", label = "tab:kfold", align = "llcc")
print(table, include.rownames = FALSE, booktabs = TRUE)



### looo

loo_models  <- loo(model_HM, model_AR, model_ineq, model_lag, compare = TRUE, reloo = TRUE)
