

library(brms)
library(tidyverse)
library(tictoc)
library(rethinking)
library(haven)

data <- read_dta("~/Dropbox/BayesChapter/Data/PartiesPivotsPolicies/PartiesPivotsandPolicyReplicationDataV2.dta")

summary(data)

data  <- data %>% filter(laggedsq < 1 & laggedsq > -1)
data  <- data %>% group_by(issue) %>% mutate(L_sqtransform = lag(sqtransform, order_by = congress))

m1  <- lm(sqtransform ~ transfloormedianslocationpredict + transinflxmedgrid + 0 , data = data)
summary(m1)

m2  <- lm(sqtransform ~ transfpivotalgridlocklocationpre + transinflxmedgrid + 0, data = data)
summary(m2)
m3  <- lm(sqtransform ~ transfpartycartelopenrule + transinflxpartunitblock + 0, data = data)
summary(m3)
m4  <- lm(sqtransform ~ transfpartycartelclosedrulepred + transinflxpartunitblock + 0, data = data)
summary(m4)

#### subset to those variables used later, so that we can omit all missing data
prior = c(set_prior("normal(0,5)", class = "b"))

model1 <- brm(formula = sqtransform ~ - 1 + transfloormedianslocationpredict + transinflxmedgrid, data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model1, file = "~/Dropbox/BayesChapter/Model_Results/model1.rda")


model2 <- brm(formula = sqtransform ~ - 1 + transfpivotalgridlocklocationpre + transinflxmedgrid, data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model2, file = "~/Dropbox/BayesChapter/Model_Results/model2.rda")

model3 <- brm(formula = sqtransform ~ - 1 + transfpartycartelopenrule + transinflxpartunitblock, data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model3, file = "~/Dropbox/BayesChapter/Model_Results/model3.rda")

model4 <- brm(formula = sqtransform ~ - 1 + transfpartycartelclosedrulepred + transinflxpartunitblock, data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model4, file = "~/Dropbox/BayesChapter/Model_Results/model4.rda")

model5 <- brm(formula = sqtransform ~ - 1 + L_sqtransform + transfloormedianslocationpredict +transfpivotalgridlocklocationpre + transfpartycartelopenrule +transfpartycartelclosedrulepred + transinflxmedgrid + transinflxpartunitblock, data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model5, file = "~/Dropbox/BayesChapter/Model_Results/model5.rda")

prior = c(set_prior("normal(0,5)", class = "b"))
model6 <- brm(formula = sqtransform ~ - 1 + L_sqtransform + transfloormedianslocationpredict +transfpivotalgridlocklocationpre + transfpartycartelopenrule +transfpartycartelclosedrulepred + transinflxmedgrid + transinflxpartunitblock + (1 | congress + issue), data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model6, file = "~/Dropbox/BayesChapter/Model_Results/model6.rda")