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
    if(type == "median"){
        estimates <- posterior_samples(model) %>% apply(2, median)
    }
    if(type == "mean"){
        estimates <- posterior_samples(model) %>% apply(2, mean)
    }
    para <- estimates[-c(which(names(post_median) %in% c("sigma", "lp__")))]
    intercept <- as.tibble(rep(1, n))
    names(intercept) <- "intercept"
    data  <- bind_cols(intercept, model$data[, -c(which(names(model$data) == "D_polity_s_interp"))])
    first  <- -(n / 2) * log(2 / pi)
    second  <- n * log(sqrt(estimates["sigma"]))

    third  <- (1 / (2 * estimates["sigma"])) * sum((model$data$D_polity_s_interp - (as.matrix(data)%*% as.matrix(para,nrow=1)))^2)
    logLik <-first - second - third
    return(logLik)
}



vars <- get_variables(model_HM)

model.HM <- model_HM %>% spread_draws(b_Intercept, b_L_Polity_s_interp, b_L_Fiscal_Rel_interp, b_D_Fiscal_Rel_Interp, b_L_D_Fiscal_Rel_Interp, b_L_logGDPPERCAP, b_L_CivilWar, b_L_REGION_DEM_DIFFUSE, b_L_WORLD_DEM_DIFFUSE, b_D_GDPPERCAP, b_D_RegionalDiffusion, b_D_WORLD_DEM_DIFFUSE) %>% mutate(longRun_fiscal =  b_L_Fiscal_Rel_interp /abs(b_L_Polity_s_interp)) %>% median_qi(.width = c(0.5))
m
mcmc_intervals(select(model.HM, -c(.chain, .iteration, .draw)))

info_crit <- function(model){

    log_likelihood <- logLik_fun(model, type = "median")### log likelihood based on median para
    deviance <- -2 * log_likelihood ##deviance

    aic  <-  deviance + 2 * (dim(summary(model)$fixed)[1]+1) ## aic based on deviance
    bic  <- (log(summary(model)$nobs) * (dim(summary(model)$fixed)[1]+1)) - deviance ### bic based on deviance
    dfLL <- model %>% log_lik() %>% as_tibble() ## returns log likelihood matrix for each obs and sample of the posterior
    deviance_post  <- dfLL %>% mutate(sums     = rowSums(.), deviance = -2*sums) ### posterior of deviance, i.e., for each parameter in sample
    mean_deviance <- mean(deviance_post$deviance) ## mean of posterior deviance

    deviance_mean <- -2 * logLik_fun(model, type = "mean") ### deviance based on mean parameter
    dic  <-  mean_deviance + (deviance_mean -  mean_deviance) ## dic based on  deviance estimates
    ret  <- tibble(aic = aic, deviance = deviance, dic = dic, bic = bic)
    return(ret)
}


### deivance based on median estimate aic, dic, bic,
info_HM  <- info_crit(model_HM)
info_AR <- info_crit(model_AR)
info_ineq <- info_crit(model_ineq)
info_lag <- info_crit(model_lag)



### waic
waic(model_HM, model_AR, model_ineq, model_lag)

#### bayesfactor
bayes_factor(model_HM, model_AR, model_ineq, model_lag)
