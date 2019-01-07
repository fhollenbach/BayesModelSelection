

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

data <- data %>% select(c(sqtransform,L_sqtransform, transfloormedianslocationpredict, transfpivotalgridlocklocationpre, transfpartycartelopenrule, transfpartycartelclosedrulepred, transinflxmedgrid, transinflxpartunitblock, congress, issue))
data <- na.omit(data)

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




#### mnodel 6 but with horseshoe

scaled <- as.tibble(scale(select(model.data.horse, -c(hmccode,year,post1980,gdp_verylow_firstoil_year,32:48))))

model.data.scaled <- bind_cols(select(model.data.horse, c(hmccode,year,post1980,gdp_verylow_firstoil_year)), scaled)
model.data.scaled <- bind_cols(model.data.scaled, select(model.data.horse, c(32:48)))


summary(data)
ratio <- 7/(dim(data)[2]-1)

#writeLines(readLines("~/Documents/GitHub/BayesModelSelection/Code/linear_regHorseshoe.stan"))
n <- dim(model.data.scaled)[1]
X <- as.matrix(select(model.data.scaled,-c(hmccode, year, D_polity_s_interp)))
data_stan <- list(y = model.data.scaled$D_polity_s_interp, K = dim(X)[2], X = t(X), m_exp = 10)

rstan_options(auto_write = TRUE) 
options(mc.cores = parallel::detectCores())

finnish_fit <- stan(file='~/Documents/GitHub/BayesModelSelection/Code/linear_regHorseshoe.stan', init = 0,data=data_stan, seed=12345, iter = 5000, control=list(adapt_delta=0.99, max_treedepth=15), cores = 4)




