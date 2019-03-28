################################################################
################################################################
######  This file estimates the models for comparison     ######
######     in Hollenbach & Montgomery 2019                ######
################################################################
################################################################

library(conflicted)
library(here)
library(brms)
library(tidyverse)
library(haven)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("Position", "base")

data <- read_dta(here("Data", "PartiesPivotsandPolicyReplicationDataV2.dta"))

summary(data)

data  <- data %>% filter(laggedsq < 1 & laggedsq > -1)
data  <- data %>% group_by(issue) %>% mutate(L_sqtransform = lag(sqtransform, order_by = congress, default = NA))

m1  <- lm(sqtransform ~ transfloormedianslocationpredict + transinflxmedgrid + 0 , data = data)
summary(m1)

m2  <- lm(sqtransform ~ transfpivotalgridlocklocationpre + transinflxmedgrid + 0, data = data)
summary(m2)
m3  <- lm(sqtransform ~ transfpartycartelopenrule + transinflxpartunitblock + 0, data = data)
summary(m3)
m4  <- lm(sqtransform ~ transfpartycartelclosedrulepred + transinflxpartunitblock + 0, data = data)
summary(m4)

data <- data %>% select(c(sqtransform,L_sqtransform, transfloormedianslocationpredict, transfpivotalgridlocklocationpre, transfpartycartelopenrule, transfpartycartelclosedrulepred, transinflxmedgrid, transinflxpartunitblock, congress, issue))
data <- na.omit(data)

#### subset to those variables used later, so that we can omit all missing data
prior = c(set_prior("normal(0,5)", class = "b"))
model1 <- brm(formula = sqtransform ~ - 1 + transfloormedianslocationpredict + transinflxmedgrid, data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model1, file = here("Results", "model1.rda"))


model2 <- brm(formula = sqtransform ~ - 1 + transfpivotalgridlocklocationpre + transinflxmedgrid, data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model2, file = here("Results","model2.rda"))

model3 <- brm(formula = sqtransform ~ - 1 + transfpartycartelopenrule + transinflxpartunitblock, data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model3, file = here("Results", "model3.rda"))

model4 <- brm(formula = sqtransform ~ - 1 + transfpartycartelclosedrulepred + transinflxpartunitblock, data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model4, file = here("Results", "model4.rda"))


model5 <- brm(formula = sqtransform ~ - 1 + L_sqtransform + transfloormedianslocationpredict +transfpivotalgridlocklocationpre + transfpartycartelopenrule +transfpartycartelclosedrulepred + transinflxmedgrid + transinflxpartunitblock, data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model5, file = here("Results", "model5.rda"))


prior = c(set_prior("normal(0,5)", class = "b"))
model6 <- brm(formula = sqtransform ~ - 1 + L_sqtransform + transfloormedianslocationpredict +transfpivotalgridlocklocationpre + transfpartycartelopenrule +transfpartycartelclosedrulepred + transinflxmedgrid + transinflxpartunitblock + (1 | congress + issue), data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model6, file = here("Results", "model6.rda"))
