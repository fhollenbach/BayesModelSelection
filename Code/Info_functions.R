

info_crit <- function(model, file){
    m <- stan_model(file = file)
    n  <- dim(model$data)[1]
    data <- model$data
    estimates_median <- posterior_samples(model) %>% apply(2, median)
    estimates_mean <- posterior_samples(model) %>% apply(2, mean)
    
    inits_median <- list(b = estimates_median[-which(names(estimates_median)%in%c("sigma","lp__"))], sigma = estimates_median["sigma"])
    dat <- brms::standata(model)
      
    samp_median <- rstan::sampling(m , init=list(inits_median) , data= dat, pars="dev" ,chains=1 , iter=1 , cores=1 )
    deviance <- as.numeric(rstan::extract(samp_median,"dev") )
    
    log_likelihood <- deviance/-2 ###

    aic  <- deviance + 2 * (dim(summary(model)$fixed)[1]+1) ## aic based on deviance
    bic  <- deviance + (log(summary(model)$nobs) * (dim(summary(model)$fixed)[1]+1)) ### bic based on deviance
    dfLL <- model %>% log_lik() %>% as_tibble() ## returns log likelihood matrix for each obs and sample of the posterior
    deviance_post  <- dfLL %>% mutate(sums     = rowSums(.), deviance = -2*sums) ### posterior of deviance, i.e., for each parameter in sample
    mean_deviance <- mean(deviance_post$deviance) ## mean of posterior deviance
    
    inits_mean <- list(b = estimates_mean[-which(names(estimates_mean)%in%c("sigma","lp__"))], sigma = estimates_mean["sigma"])
    samp_mean <- rstan::sampling(m , init=list(inits_mean) , data= dat, pars="dev" ,chains=1 , iter=1 , cores=1 )
    deviance_mean <- as.numeric(rstan::extract(samp_mean,"dev") )
    
    dic  <-  mean_deviance + (deviance_mean -  mean_deviance) ## dic based on  deviance estimates
    ret  <- tibble(AIC = aic, Deviance = deviance, DIC = dic, BIC = bic)
    return(ret)
}


info_crit_RE <- function(model, file){
  m <- stan_model(file = file)
  n  <- dim(model$data)[1]
  data <- model$data
  estimates_median <- posterior_samples(model) %>% apply(2, median)
  estimates_mean <- posterior_samples(model) %>% apply(2, mean)
  
  inits_median <- list(b = estimates_median[1:7], sigma = estimates_median[10], sd_1 = list(estimates_median[8]), sd_2 = list(estimates_median[9]), z_1 = matrix(estimates_median[47:53],ncol =7), z_2 = matrix(estimates_median[54:81],ncol=28) )
  dat <- brms::standata(model)
  
  samp_median <- rstan::sampling(m , init=list(inits_median) , data= dat, pars="dev" ,chains=1 , iter=1 , cores=1 )
  deviance <- as.numeric(rstan::extract(samp_median,"dev") )
  
  log_likelihood <- deviance/-2 ###
  
  aic  <- deviance + 2 * (dim(summary(model)$fixed)[1]+1) ## aic based on deviance
  bic  <- deviance + (log(summary(model)$nobs) * (dim(summary(model)$fixed)[1]+1)) ### bic based on deviance
  dfLL <- model %>% log_lik() %>% as_tibble() ## returns log likelihood matrix for each obs and sample of the posterior
  deviance_post  <- dfLL %>% mutate(sums     = rowSums(.), deviance = -2*sums) ### posterior of deviance, i.e., for each parameter in sample
  mean_deviance <- mean(deviance_post$deviance) ## mean of posterior deviance
  
  inits_mean <- list(b = estimates_mean[1:7], sigma = estimates_mean[10], sd_1 = list(estimates_mean[8]), sd_2 = list(estimates_mean[9]), z_1 = matrix(estimates_mean[47:53],ncol =7), z_2 = matrix(estimates_mean[54:81],ncol=28) )
  samp_mean <- rstan::sampling(m , init=list(inits_mean) , data= dat, pars="dev" ,chains=1 , iter=1 , cores=1 )
  deviance_mean <- as.numeric(rstan::extract(samp_mean,"dev") )
  
  dic  <-  mean_deviance + (deviance_mean -  mean_deviance) ## dic based on  deviance estimates
  ret  <- tibble(AIC = aic, Deviance = deviance, DIC = dic, BIC = bic)
  return(ret)
}
