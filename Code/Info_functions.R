
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
    logLik <- first - second - third
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
