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

####
### make Table with info criteria
models_criteria <- bind_rows(info_1, info_2, info_3, info_4, info_5, info_6)
Model  <-as.tibble(c("Model 1", "Model 2", "Model 3", "Model 4", "Full Model", "Full Model RE"))
names(Model) <- "Model"
models_criteria <- bind_cols(Model, models_criteria)

table  <- xtable(models_criteria, digits = 1, caption = "Information Critera", label = "tab:infoCrit", align = "lccccc")
print(table, include.rownames = FALSE, booktabs = TRUE)


names(models) <- "Model"

### waic
waic.1 <- waic(model1)$estimate[3, ]
waic.2 <- waic(model2)$estimate[3, ]
waic.3 <- waic(model3)$estimate[3, ]
waic.4 <- waic(model4)$estimate[3, ]
waic.5 <- waic(model5)$estimate[3, ]
waic.6 <- waic(model6)$estimate[3, ]

waic   <- bind_rows(waic.1, waic.2, waic.3, waic.4, waic.5, waic.6)
Model  <-as.tibble(c("Model 1", "Model 2", "Model 3", "Model 4", "Full Model", "Full Model RE"))
names(Model) <- "Model"
models_waic <- bind_cols(Model, waic)

table  <- xtable(models_waic, digits = 2, caption = "WAIC for all Models", label = "tab:waic", align = "llcc")
print(table, include.rownames = FALSE, booktabs = TRUE)
save(models_waic, file = "~/Dropbox/BayesChapter/Model_Results/waic_res.rda")

### kfold

kfold_cross  <- kfold(model1, model2, model3, model4, model5, model6, compare = TRUE, k = 10)


kfold.1 <- kfold_cross$model1$estimate[3,]
kfold.2 <- kfold_cross$model2$estimate[3,]
kfold.3 <- kfold_cross$model3$estimate[3,]
kfold.4 <- kfold_cross$model4$estimate[3,]
kfold.5 <- kfold_cross$model5$estimate[3,]
kfold.6 <- kfold_cross$model6$estimate[3,]


kfold <- bind_rows(kfold.1, kfold.2, kfold.3, kfold.4, kfold.5, kfold.6)
Model  <-as.tibble(c("Model 1", "Model 2", "Model 3", "Model 4", "Full Model", "Full Model RE"))
names(Model) <- "Model"
models_kfold <- bind_cols(Model, kfold)

table  <- xtable(models_kfold, digits = 2, caption = "Kfold Crossvalidation for all Models", label = "tab:kfold", align = "llcc")
print(table, include.rownames = FALSE, booktabs = TRUE)



### looo

loo_models  <- loo(model1, model2, model3, model4, model5, model6, compare = TRUE, reloo = TRUE)

loo  <- bind_rows(loo_models$model1$estimate[1, ], loo_models$model2$estimate[1, ], loo_models$model3$estimate[1, ], loo_models$model4$estimate[1, ], loo_models$model5$estimate[1, ], loo_models$model6$estimate[1, ])
Model  <-as.tibble(c("Model 1", "Model 2", "Model 3", "Model 4", "Full Model", "Full Model RE"))
names(Model) <- "Model"
models_loo <- bind_cols(Model, waic)

table  <- xtable(models_loo, digits = 2, caption = "Psis-Loo ELPD for all Models", label = "tab:loo", align = "llcc")
print(table, include.rownames = FALSE, booktabs = TRUE)
save(models_loo, file = "~/Dropbox/BayesChapter/Model_Results/loo_res.rda")
