

info_crit <- function(model){
    dfLL <- model %>% log_lik() %>% as_tibble()
    deviance_post  <- dfLL %>% mutate(sums     = rowSums(.), deviance = -2*sums)
    medians <-  apply(dfLL, 2, median)
    deviance_median <-  -2*sum(medians)
    deviance_mean  <- -2 * sum(apply(dfLL, 2, mean))
    aic  <-  deviance_median + 2 * (dim(summary(model)$fixed)[1]+1)
    dic  <- mean(deviance_post$deviance) + (mean(deviance_post$deviance) - deviance_mean)
    bic  <- (log(summary(model)$nobs) * (dim(summary(model)$fixed)[1]+1)) - (2 * sum(medians))
    ret  <- tibble(aic = aic, deviance = deviance_median, dic = dic, bic = bic)
    return(ret)
}
