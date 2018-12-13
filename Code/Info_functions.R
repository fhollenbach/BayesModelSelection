
logLik_fun_old <- function(model, type){
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
    logLik <- first - second - third
    return(logLik)
}


info_crit <- function(model){
    m <- stan_model(file = "~/Documents/GitHub/BayesModelSelection/code/deviance_sample.stan")
    n  <- dim(model$data)[1]
    data <- model$data
    estimates_median <- posterior_samples(model) %>% apply(2, median)
    estimates_mean <- posterior_samples(model) %>% apply(2, mean)
    
    inits_median <- list(b = estimates_median[-which(names(estimates_median)%in%c("b_Intercept", "sigma", "temp_Intercept","lp__"))], temp_Intercept = estimates_median["temp_Intercept"], sigma = estimates_median["sigma"])
    dat <- list(N= n, Y = data$D_polity_s_interp, K = dim(summary(model)$fixed)[1], X = as.matrix(cbind(rep(1,dim(data)[1]),data[,-c(1)])))
    samp_median <- rstan::sampling(m , init=list(inits_median) , data= dat, pars="dev" ,chains=1 , iter=1 , cores=1 )
    deviance <- as.numeric(extract(samp_median,"dev") )
    
    log_likelihood <- deviance/-2 ###

    aic  <- deviance + 2 * (dim(summary(model)$fixed)[1]+1) ## aic based on deviance
    bic  <- deviance + (log(summary(model)$nobs) * (dim(summary(model)$fixed)[1]+1)) ### bic based on deviance
    dfLL <- model %>% log_lik() %>% as_tibble() ## returns log likelihood matrix for each obs and sample of the posterior
    deviance_post  <- dfLL %>% mutate(sums     = rowSums(.), deviance = -2*sums) ### posterior of deviance, i.e., for each parameter in sample
    mean_deviance <- mean(deviance_post$deviance) ## mean of posterior deviance
    
    inits_mean <- list(b = estimates_mean[-which(names(estimates_mean)%in%c("b_Intercept", "sigma", "temp_Intercept","lp__"))], temp_Intercept = estimates_mean["temp_Intercept"], sigma = estimates_mean["sigma"])
    dat <- list(N= n, Y = data$D_polity_s_interp, K = dim(summary(model)$fixed)[1], X = as.matrix(cbind(rep(1,dim(data)[1]),data[,-c(1)])))
    samp_mean <- rstan::sampling(m , init=list(inits_mean) , data= dat, pars="dev" ,chains=1 , iter=1 , cores=1 )
    deviance_mean <- as.numeric(extract(samp_mean,"dev") )
    
    dic  <-  mean_deviance + (deviance_mean -  mean_deviance) ## dic based on  deviance estimates
    ret  <- tibble(AIC = aic, Deviance = deviance, DIC = dic, BIC = bic)
    return(ret)
}
