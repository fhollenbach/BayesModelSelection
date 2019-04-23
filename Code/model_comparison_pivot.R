library(bayesplot)
library(brms)
library(rethinking)
library(tidyverse)
library(tidybayes)
library(xtable)

source("~/Documents/GitHub/BayesModelSelection/code/info_functions.R")

load("~/Dropbox/BayesChapter/Model_Results/model1.rda")
load("~/Dropbox/BayesChapter/Model_Results/model2.rda")
load("~/Dropbox/BayesChapter/Model_Results/model3.rda")
load("~/Dropbox/BayesChapter/Model_Results/model4.rda")
load("~/Dropbox/BayesChapter/Model_Results/model5.rda")
load("~/Dropbox/BayesChapter/Model_Results/model6.rda")

### add info criteria in brms 
model1 <- add_ic(model1, ic = c("loo", "waic", "kfold"))
model2 <- add_ic(model2, ic = c("loo", "waic", "kfold"))
model3 <- add_ic(model3, ic = c("loo", "waic", "kfold"))
model4 <- add_ic(model4, ic = c("loo", "waic", "kfold"))


##model 1
vars <- get_variables(model1)[c(1:3)]

model1.95 <- model1 %>% spread_draws(b_transfloormedianslocationpredict, b_transinflxmedgrid,sigma) %>% median_qi(.width = c(0.95))

est  <-round( model1.95[vars], 3)
CI.low <- matrix(as.numeric(model1.95[paste(vars, "lower", sep = ".")]), ncol = 1)
row.names(CI.low) <- vars
CI.high <- matrix(as.numeric(model1.95[paste(vars, "upper", sep = ".")]), ncol = 1)
row.names(CI.high) <- vars
CI <- data.frame(cbind(CI.low,CI.high))
CI$interval <- paste("(", round(CI$X1,3), ",", round(CI$X2,3), ")", sep = "")

col.1 <- unlist(sapply(seq_along(est), function(i) append(est[i], CI$interval[i], i)))
col.1 <- c(rep(NA, 2), col.1[1:2], rep(NA, 6), col.1[3:4], rep(NA,2), col.1[5:6]) ### add 2 rows for lag, 4 rows for other vars8 for ineq & 2 for lag model, 6 for full




##model 2
vars <- get_variables(model2)[1:3]

model2.95 <- model2 %>% spread_draws(b_transfpivotalgridlocklocationpre, b_transinflxmedgrid, sigma)  %>% median_qi(.width = c(0.95))

est  <- round(model2.95[vars], 3)
CI.low <- matrix(as.numeric(model2.95[paste(vars, "lower", sep = ".")]), ncol = 1)
row.names(CI.low) <- vars
CI.high <- matrix(as.numeric(model2.95[paste(vars, "upper", sep = ".")]), ncol = 1)
row.names(CI.high) <- vars
CI <- data.frame(cbind(CI.low,CI.high))
CI$interval <- paste("(", round(CI$X1,3), ",", round(CI$X2,3), ")", sep = "")

col.2 <- unlist(sapply(seq_along(est), function(i) append(est[i], CI$interval[i], i)))
col.2 <- c(rep(NA, 4), col.2[1:2], rep(NA, 4), col.2[3:4], rep(NA,2), col.2[5:6]) ### add 2 rows for lag, 4 rows for other vars8 for ineq & 2 for lag model, 6 for full

##model 3
vars <- get_variables(model3)[1:3]

model3.95 <- model3 %>% spread_draws(b_transfpartycartelopenrule, b_transinflxpartunitblock, sigma)  %>% median_qi(.width = c(0.95))

est  <- round(model3.95[vars], 3)
CI.low <- matrix(as.numeric(model3.95[paste(vars, "lower", sep = ".")]), ncol = 1)
row.names(CI.low) <- vars
CI.high <- matrix(as.numeric(model3.95[paste(vars, "upper", sep = ".")]), ncol = 1)
row.names(CI.high) <- vars
CI <- data.frame(cbind(CI.low,CI.high))
CI$interval <- paste("(", round(CI$X1,3), ",", round(CI$X2,3), ")", sep = "")

col.3 <- unlist(sapply(seq_along(est), function(i) append(est[i], CI$interval[i], i)))
col.3 <- c(rep(NA, 6), col.3[1:2], rep(NA, 4), col.3[3:4], col.3[5:6]) 


##model 4
vars <- get_variables(model4)[1:3]

model4.95 <- model4 %>% spread_draws(b_transfpartycartelclosedrulepred, b_transinflxpartunitblock, sigma)  %>% median_qi(.width = c(0.95))

est  <- round(model4.95[vars], 3)
CI.low <- matrix(as.numeric(model4.95[paste(vars, "lower", sep = ".")]), ncol = 1)
row.names(CI.low) <- vars
CI.high <- matrix(as.numeric(model4.95[paste(vars, "upper", sep = ".")]), ncol = 1)
row.names(CI.high) <- vars
CI <- data.frame(cbind(CI.low,CI.high))
CI$interval <- paste("(", round(CI$X1,3), ",", round(CI$X2,3), ")", sep = "")

col.4 <- unlist(sapply(seq_along(est), function(i) append(est[i], CI$interval[i], i)))
col.4 <- c(rep(NA, 8), col.4[1:2], rep(NA, 2), col.4[3:4], col.4[5:6]) 

##model 5
vars <- get_variables(model5)[1:8]

model5.95 <- model5 %>% spread_draws(b_L_sqtransform, b_transfloormedianslocationpredict, b_transfpivotalgridlocklocationpre, b_transfpartycartelopenrule, b_transfpartycartelclosedrulepred, b_transinflxmedgrid, b_transinflxpartunitblock, sigma)  %>% median_qi(.width = c(0.95))

est  <- round(model5.95[vars], 3)
CI.low <- matrix(as.numeric(model5.95[paste(vars, "lower", sep = ".")]), ncol = 1)
row.names(CI.low) <- vars
CI.high <- matrix(as.numeric(model5.95[paste(vars, "upper", sep = ".")]), ncol = 1)
row.names(CI.high) <- vars
CI <- data.frame(cbind(CI.low,CI.high))
CI$interval <- paste("(", round(CI$X1,3), ",", round(CI$X2,3), ")", sep = "")

col.5 <- unlist(sapply(seq_along(est), function(i) append(est[i], CI$interval[i], i)))


##model 6
vars <- get_variables(model6)[c(1:7,10)]

model6.95 <- model6 %>% spread_draws(b_L_sqtransform, b_transfloormedianslocationpredict, b_transfpivotalgridlocklocationpre, b_transfpartycartelopenrule, b_transfpartycartelclosedrulepred, b_transinflxmedgrid, b_transinflxpartunitblock, sigma)  %>% median_qi(.width = c(0.95))

est  <- round(model6.95[vars], 3)
CI.low <- matrix(as.numeric(model6.95[paste(vars, "lower", sep = ".")]), ncol = 1)
row.names(CI.low) <- vars
CI.high <- matrix(as.numeric(model6.95[paste(vars, "upper", sep = ".")]), ncol = 1)
row.names(CI.high) <- vars
CI <- data.frame(cbind(CI.low,CI.high))
CI$interval <- paste("(", round(CI$X1,3), ",", round(CI$X2,3), ")", sep = "")

col.6 <- unlist(sapply(seq_along(est), function(i) append(est[i], CI$interval[i], i)))


length(col.1)
length(col.2)
length(col.3)
length(col.4)
length(col.5)
length(col.6)

vars <-tibble("lag DV", "Median only", "Pivotal politics", "Party cartel open rule", "Party cartel closed rule", "Inflation (median/pivot)", "Inflation (party)", "Sigma")

vars_ci <- rep(NA, 8)
names <- unlist(sapply(seq_along(vars), function(i) append(vars[i], vars_ci[i], i)))

results <- tibble(Variable = names, Model1 = col.1, Model2 = col.2, Model3 = col.3, Model4 = col.4, ModelFull = col.5, ModelFullRE = col.6)
N <- tibble(Variable = "N", Model1 = "117", Model2 = "117", Model3 = "117", Model4 = "117", ModelFull = "117", ModelFullRE = "117")
RE <- tibble(Variable = "Random Effects", Model1 = "No", Model2 = "No", Model3 = "No", Model4 = "No", ModelFull = "No", ModelFullRE = "Yes")

results <- bind_rows(results, N, RE)


tab  <- xtable(results,  caption = "Model Results and 95 Credible Intervals", label = "tab:models", align = "llcccccc")
print(tab, include.rownames = FALSE, booktabs = TRUE)

### deivance based on median estimate aic, dic, bic,
info_1  <- info_crit(model1, file = "~/Documents/GitHub/BayesModelSelection/code/deviance_sample_normal.stan")
info_2 <- info_crit(model2, file = "~/Documents/GitHub/BayesModelSelection/code/deviance_sample_normal.stan")
info_3 <- info_crit(model3, file = "~/Documents/GitHub/BayesModelSelection/code/deviance_sample_normal.stan")
info_4 <- info_crit(model4, file = "~/Documents/GitHub/BayesModelSelection/code/deviance_sample_normal.stan")
info_5  <- info_crit(model5, file = "~/Documents/GitHub/BayesModelSelection/code/deviance_sample_normal.stan")
info_6  <- info_crit_RE(model6, file = "~/Documents/GitHub/BayesModelSelection/code/deviance_sampleRE.stan")


### Table BIC
table.bic <- tibble(Model =  c("Model 1", "Model 2", "Model 3", "Model 4"), BIC = c(info_1$BIC, info_2$BIC, info_3$BIC, info_4$BIC), Dif = c("M3/M1", "M3/M2", NA, "M3/M4"), BF = c((info_1$BIC - info_3$BIC) / 2, (info_2$BIC - info_3$BIC) / 2, NA, (info_4$BIC - info_3$BIC) / 2))
table <- t(as.matrix(table.bic))
table  <- xtable(table, caption = "BIC and Approximation to Bayes Factor", label = "tab:bic", align = "lcccc")
print(table,  include.rownames = FALSE, include.colnames = FALSE, booktabs = TRUE)




##### marginal likelihood and bayes factor from bridge sampling
ml1 <- bridge_sampler(model1, log = TRUE)
ml2 <- bridge_sampler(model2, log = TRUE)
ml3 <- bridge_sampler(model3, log = TRUE)
ml4 <- bridge_sampler(model4, log = TRUE)


## Table BF
bf31 <- bayes_factor(model3, model1, log = TRUE)
bf32 <- bayes_factor(model3, model2, log = TRUE)
bf34 <- bayes_factor(model3, model4, log = TRUE)


table.bridge <- tibble(Model =  c("Model 1", "Model 2", "Model 3", "Model 4"), LogMarginalLikelihood  = c( ml1$logml,  ml2$logml,  ml3$logml,  ml4$logml), dif = c("M3/M1", "M3/M2", NA, "M3/M4"), BF = c(bf31$bf, bf32$bf,NA, bf34$bf))

table <- t(as.matrix(table.bridge))
table  <- xtable(table, caption = "Log Marginal Likelihood and Bayes Factor using Bridgesampling", label = "tab:bridge", align = "lcccc")
print(table,  include.rownames = FALSE, include.colnames = FALSE, booktabs = TRUE)


table.bf <- as.tibble(matrix(c(bf31$bf, bf32$bf, bf34$bf),nrow= 1,ncol=3))
names(table.bf) <- c("M3/M1", "M3/M2", "M3/M4")#  <-as.tibble(matrix(c("Model 1", "Model 2", "Model 3", "Model 4"),nrow=1))

table  <- xtable(table.bf, caption = "Bayes Factor Model Comparison", label = "tab:bf", align = "lccc")
print(table,  include.rownames = FALSE, include.colnames = TRUE, booktabs = TRUE)

####
### make Table with info criteria loo, kfold, waic

table <- tibble(Model =  c("Model 1", "Model 2", "Model 3", "Model 4"), loo = c(model1$loo$estimates[3,1], model2$loo$estimates[3,1], model3$loo$estimates[3,1], model4$loo$estimates[3,1]), waic = c(model1$waic$estimates[3,1], model2$waic$estimates[3,1], model3$waic$estimates[3,1], model4$waic$estimates[3,1]), kfold = c(model1$kfold$estimates[3,1], model2$kfold$estimates[3,1], model3$kfold$estimates[3,1], model4$kfold$estimates[3,1]))

table <- t(as.matrix(table))
table  <- xtable(table, caption = "Information Criteria for Evaluating Theories of Congress", label = "tab:ics", align = "lcccc")
print(table,  include.rownames = TRUE, include.colnames = FALSE, booktabs = TRUE)


comp <- compare_ic(model1, model2, model3, model4, ic= "loo")
names(comp)

table <- tibble(Model =  c("Model 1", "Model 2", "Model 3", "Model 4", "Model 1 - Model 2", "Model 1 - Model 3", "Model 1 - Model 4", "Model 2 - Model 3", "Model 2 - Model 4", "Model 3 - Model 4"), looic = c(comp$model1$estimates[3,1], comp$model2$estimates[3,1], comp$model3$estimates[3,1], comp$model4$estimates[3,1], comp$ic_diffs__[1,1], comp$ic_diffs__[2,1], comp$ic_diffs__[3,1], comp$ic_diffs__[4,1], comp$ic_diffs__[5,1], comp$ic_diffs__[6,1]), SE = c(comp$model1$estimates[3,2], comp$model2$estimates[3,2], comp$model3$estimates[3,2], comp$model4$estimates[3,2], comp$ic_diffs__[1,2], comp$ic_diffs__[2,2], comp$ic_diffs__[3,2], comp$ic_diffs__[4,2], comp$ic_diffs__[5,2], comp$ic_diffs__[6,2]))
table  <- xtable(table, caption = "Loo Information Criteria for Evaluating Theories of Congress with Standard Errors", label = "tab:loo", align = "lccc", digits = 1)
print(table,  include.rownames = FALSE, include.colnames = TRUE, booktabs = TRUE)


loo_list <- list(model1$loo, model2$loo, model3$loo, model4$loo)
stack  <- loo_model_weights(loo_list,method = c("stacking"))


table <- tibble(Model =  c("Model 1", "Model 2", "Model 3", "Model 4"), stack = round(c(stack[[1]], stack[[2]], stack[[3]], stack[[4]]), 2))

table <- t(as.matrix(table))
table  <- xtable(table, caption = "Model Weights Based on Stacking", label = "tab:stacking", align = "lcccc")
print(table,  include.rownames = FALSE, include.colnames = FALSE, booktabs = TRUE)
