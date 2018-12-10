library(bayesplot)
library(brms)
library(rethinking)
library(tidyverse)
library(tidybayes)

load("~/Dropbox/BayesChapter/Model_Results/model_HM_imp.rda")
load("~/Dropbox/BayesChapter/Model_Results/model_AR_imp.rda")
load("~/Dropbox/BayesChapter/Model_Results/model_lag_imp.rda")
load("~/Dropbox/BayesChapter/Model_Results/model_ineq_imp.rda")



logLik_fun <- function(model, type){
    n  <- dim(model$data)[1]
    data <- model$data
    if(type == "median"){
        estimates <- posterior_samples(model) %>% apply(2, median)
    }
    if(type == "mean"){
        estimates <- posterior_samples(model) %>% apply(2, mean)
    }
    para <- estimates[-c(which(names(estimates) %in% c("sigma", "lp__", "temp_Intercept")))]
    intercept <- as.tibble(rep(1, n))
    names(intercept) <- "intercept"
    data  <- bind_cols(intercept, data[, -c(which(names(data) == "D_polity_s_interp"))])
    first  <- -(n / 2) * log(2 / pi)
    second  <- n * log(sqrt(estimates["sigma"]))

    third  <- (1 / (2 * estimates["sigma"])) * sum((model$data$D_polity_s_interp - (as.matrix(data)%*% as.matrix(para,nrow=1)))^2)
    logLik <-first - second - third
    return(logLik)
}


info_crit <- function(model){

    log_likelihood <- logLik_fun(model, type = "median")### log likelihood based on median para
    deviance <- -2 * log_likelihood ##deviance

    aic  <-  deviance + 2 * (dim(summary(model)$fixed)[1]+1) ## aic based on deviance
    bic  <- (log(summary(model)$nobs) * (dim(summary(model)$fixed)[1]+4)) - deviance ### bic based on deviance
    dfLL <- model %>% log_lik() %>% as_tibble() ## returns log likelihood matrix for each obs and sample of the posterior
    deviance_post  <- dfLL %>% mutate(sums     = rowSums(.), deviance = -2*sums) ### posterior of deviance, i.e., for each parameter in sample
    mean_deviance <- mean(deviance_post$deviance) ## mean of posterior deviance

    deviance_mean <- -2 * logLik_fun(model, type = "mean") ### deviance based on mean parameter
    dic  <-  mean_deviance + (deviance_mean -  mean_deviance) ## dic based on  deviance estimates
    ret  <- tibble(AIC = aic, Deviance = deviance, DIC = dic, BIC = bic)
    return(ret)
}





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
col.HM <- c(col.HM[1:24], rep(NA, 16), col.HM[25:28]) ### add 6 rows for AR, 6 for ineq & 4 for lag model




##model AR
vars <- get_variables(model_AR)[c(1:15, 33)]

model.AR.95 <- model_AR %>% spread_draws(b_Intercept, b_L_Polity_s_interp, b_L_Fiscal_Rel_interp, b_D_Fiscal_Rel_Interp, b_L_D_Fiscal_Rel_Interp, b_L_logGDPPERCAP, b_L_CivilWar, b_L_REGION_DEM_DIFFUSE, b_L_WORLD_DEM_DIFFUSE, b_D_GDPPERCAP, b_D_RegionalDiffusion, b_D_WORLD_DEM_DIFFUSE, b_post1980, b_post1980_L_Fiscal_Rel_interp, b_post1980_D_Fiscal_Rel_Interp, sigma) %>% mutate(longRun_fiscal =  b_L_Fiscal_Rel_interp /abs(b_L_Polity_s_interp)) %>% median_qi(.width = c(0.95))
vars  <- c(vars[1:15], "longRun_fiscal", vars[16])

est  <- round(model.AR.95[vars], 3)
CI.low <- matrix(as.numeric(model.AR.95[paste(vars, "lower", sep = ".")]), ncol = 1)
row.names(CI.low) <- vars
CI.high <- matrix(as.numeric(model.AR.95[paste(vars, "upper", sep = ".")]), ncol = 1)
row.names(CI.high) <- vars
CI <- data.frame(cbind(CI.low,CI.high))
CI$interval <- paste("(", round(CI$X1,3), ",", round(CI$X2,3), ")", sep = "")

col.AR <- unlist(sapply(seq_along(est), function(i) append(est[i], CI$interval[i], i)))
col.AR <- c(col.AR[1:30], rep(NA, 10), col.AR[31:34]) ### add 6 rows for ineq & 4 for lag model



##model ineq
vars <- get_variables(model_ineq)[c(1:15, 33)]

model.ineq.95 <- model_ineq %>% spread_draws(b_Intercept, b_L_Polity_s_interp, b_L_Fiscal_Rel_interp, b_D_Fiscal_Rel_Interp, b_L_D_Fiscal_Rel_Interp, b_L_logGDPPERCAP, b_L_CivilWar, b_L_REGION_DEM_DIFFUSE, b_L_WORLD_DEM_DIFFUSE, b_D_GDPPERCAP, b_D_RegionalDiffusion, b_D_WORLD_DEM_DIFFUSE, b_very_unequal_utip, b_unequal_L_Fiscal_Rel_interp, b_unequal_D_Fiscal_Rel_Interp, sigma) %>% mutate(longRun_fiscal =  b_L_Fiscal_Rel_interp /abs(b_L_Polity_s_interp)) %>% median_qi(.width = c(0.95))
vars  <- c(vars[1:15], "longRun_fiscal", vars[16])

est  <- round(model.ineq.95[vars], 3)
CI.low <- matrix(as.numeric(model.ineq.95[paste(vars, "lower", sep = ".")]), ncol = 1)
row.names(CI.low) <- vars
CI.high <- matrix(as.numeric(model.ineq.95[paste(vars, "upper", sep = ".")]), ncol = 1)
row.names(CI.high) <- vars
CI <- data.frame(cbind(CI.low,CI.high))
CI$interval <- paste("(", round(CI$X1,3), ",", round(CI$X2,3), ")", sep = "")

col.ineq <- unlist(sapply(seq_along(est), function(i) append(est[i], CI$interval[i], i)))
col.ineq <- c(col.ineq[1:24], rep(NA, 6), col.ineq[25:30], rep(NA, 4), col.ineq[31:34] ) ### add 6 rows for AR & 4 for lag model


##model lag
vars <- get_variables(model_lag)[c(1:14, 32)]

model.lag.95 <- model_lag %>% spread_draws(b_Intercept, b_L_Polity_s_interp, b_L_Fiscal_Rel_interp, b_D_Fiscal_Rel_Interp, b_L_D_Fiscal_Rel_Interp, b_L_logGDPPERCAP, b_L_CivilWar, b_L_REGION_DEM_DIFFUSE, b_L_WORLD_DEM_DIFFUSE, b_D_GDPPERCAP, b_D_RegionalDiffusion, b_D_WORLD_DEM_DIFFUSE, b_polity_L_Fiscal_Rel_interp, b_polity_D_Fiscal_Rel_Interp, sigma) %>% mutate(longRun_fiscal =  b_L_Fiscal_Rel_interp /abs(b_L_Polity_s_interp)) %>% median_qi(.width = c(0.95))

vars  <- c(vars[1:14], "longRun_fiscal", vars[15])
est  <- round(model.lag.95[vars], 3)
CI.low <- matrix(as.numeric(model.lag.95[paste(vars, "lower", sep = ".")]), ncol = 1)
row.names(CI.low) <- vars
CI.high <- matrix(as.numeric(model.lag.95[paste(vars, "upper", sep = ".")]), ncol = 1)
row.names(CI.high) <- vars
CI <- data.frame(cbind(CI.low,CI.high))
CI$interval <- paste("(", round(CI$X1,3), ",", round(CI$X2,3), ")", sep = "")

col.lag <- unlist(sapply(seq_along(est), function(i) append(est[i], CI$interval[i], i)))
col.lag <- c(col.lag[1:24], rep(NA, 12),col.lag[25:28], col.lag[29:32]) ### add 6 rows for AR & Ineq model


length(col.HM)
length(col.AR)
length(col.ineq)
length(col.lag)

vars <-tibble("Intercept", "Polity t-1", "Fiscal Reliance t-1", "$Delta$ Fiscal Reliance", "$Delta$ Fiscal Reliance t-1", "log GDP pC t-1", "Civil War t-1", "Regional dem diffusion t-1", "Global dem diffusion t-1", "$Delta$ log GDP pC", "$Delta$ Regional Dem Diffusion", "$Delta$ Global Dem Diffusion","post 1980", "post 1980 $times$ Fisc Rel t-1", "post 1980 $times$ $Delta$ Fisc Rel", "Inequality", "Inequality $times$ Fisc Rel t-1", "Inequality $times$ $Delta$ Fisc Rel", "Polity t-1 $times$ Fisc Rel t-1", "Polity t-1 $times$ $Delta$ Fisc Rel","Long Run Fisc Rel", "Sigma")
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

waic  <- models_criteria <- bind_rows(waic.hm, waic.ar, waic.ineq, waic.lag)
Model  <-as.tibble(c("HM", "AR", "Inequality", "Polity"))
names(Model) <- "Model"
models_waic <- bind_cols(Model, waic)

table  <- xtable(models_waic, digits = 2, caption = "WAIC for all Models", label = "tab:waic", align = "llcc")
print(table, include.rownames = FALSE, booktabs = TRUE)


### kfold

kfold_cross  <- kfold(model_HM, model_AR, model_ineq, model_lag, compare = TRUE, k = 10)
