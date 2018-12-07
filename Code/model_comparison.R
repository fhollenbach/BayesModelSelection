library(bayesplot)
library(brms)
library(rethinking)
library(tidyverse)
library(tidybayes)
load("~/Dropbox/BayesChapter/Model_Results/model_HM_imp.rda")
load("~/Dropbox/BayesChapter/Model_Results/model_AR_imp.rda")
load("~/Dropbox/BayesChapter/Model_Results/model_lag_imp.rda")
load("~/Dropbox/BayesChapter/Model_Results/model_ineq_imp.rda")

vars <- get_variables(model_HM)

model.HM <- model_HM %>% spread_draws(b_Intercept, b_L_Polity_s_interp, b_L_Fiscal_Rel_interp, b_D_Fiscal_Rel_Interp, b_L_D_Fiscal_Rel_Interp, b_L_logGDPPERCAP, b_L_CivilWar, b_L_REGION_DEM_DIFFUSE, b_L_WORLD_DEM_DIFFUSE, b_D_GDPPERCAP, b_D_RegionalDiffusion, b_D_WORLD_DEM_DIFFUSE) %>% mutate(longRun_fiscal =  b_L_Fiscal_Rel_interp / b_L_Polity_s_interp)



info_crit <- function(model){
    dfLL <- model %>% log_lik() %>% as_tibble() ## returns log likelihood matrix for each obs and sample of the posterior
    deviance_post  <- dfLL %>% mutate(sums     = rowSums(.), deviance = -2*sums) ### posterior of deviance, i.e., for each parameter sample
    medians <-  apply(dfLL, 2, median) ### log likelihood for each observation based on median estimate
    deviance_median <-  -2*sum(medians) ### deviance based on the median estimate log-likelihood
    deviance_mean  <- -2 * sum(apply(dfLL, 2, mean)) ### deviance based on the mean estimate log-likelihood
    aic  <-  deviance_median + 2 * (dim(summary(model)$fixed)[1]+1) ## aic based on median deviance estimate
    dic  <- mean(deviance_post$deviance) + (mean(deviance_post$deviance) - deviance_mean) ## dic based on median deviance estimate
    bic  <- (log(summary(model)$nobs) * (dim(summary(model)$fixed)[1]+1)) - (2 * sum(medians)) ### bic based on median estimate log likelihood
    ret  <- tibble(aic = aic, deviance = deviance_median, dic = dic, bic = bic)
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
