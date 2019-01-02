


library(brms)
library(tidyverse)
library(fastDummies)
library(haven)
library(tictoc)
library(sbgcop)
library(rethinking)
data <- read_dta("~/Documents/GitHub/BayesModelSelection/Data/HaberMenaldoRepl.dta")
names(data)
#### from CPS article columns Table 2, cols 3,4
                                        #
### one haber & menaldo -
### ross model where interaction oil & after 1980
### haber menaldo model with inequality measure and interaction of oil and inequ (dunning 2008)
### interacting oil with democracy lag

### last model with only lags and some additional variable:  gdp_verylow_firstoil_year

data_sub <- data %>% select(c(hmccode, year, D_polity_s_interp, L_Polity_s_interp, L_Fiscal_Rel_interp, D_Fiscal_Rel_Interp, L_D_Fiscal_Rel_Interp, L_logGDPPERCAP, L_CivilWar, L_REGION_DEM_DIFFUSE, L_WORLD_DEM_DIFFUSE,  D_GDPPERCAP, D_RegionalDiffusion, D_WORLD_DEM_DIFFUSE, very_unequal_utip, gdp_verylow_firstoil_year, reserves_interp_area, population, coal_income_pc, natural_gas_income_pc))

data_sub <- data_sub %>% mutate(L_very_unequal_utip = lag(very_unequal_utip, order_by = year),
                                L_reserves_interp_area = lag(reserves_interp_area, order_by = year),
                                L_population = lag(population, order_by = year),
                                L_coal_income_pc = lag(coal_income_pc, order_by = year),
                                L_natural_gas_income_pc = lag(natural_gas_income_pc, order_by = year))

data_imp <- data_sub[complete.cases(select(data_sub, -c(gdp_verylow_firstoil_year,very_unequal_utip, L_very_unequal_utip, reserves_interp_area, L_reserves_interp_area, population, L_population, coal_income_pc, L_coal_income_pc, natural_gas_income_pc, L_natural_gas_income_pc))), ]


missing <- data_imp %>% select(-c(hmccode, year))
#sbgImp <- sbgcop.mcmc(missing, nsamp=2100, odens = 3, seed=6886) ##2000 samples, only every 3 saved, 700 resulting
#save(sbgImp, file ="~/Dropbox/BayesChapter/Model_Results/imputation.rda")
load("~/Dropbox/BayesChapter/Model_Results/imputation.rda")

sbgData = sbgImp$'Y.impute'[,,]
mean_imp  <- as.tibble(apply(sbgImp$'Y.impute',c(1,2),mean))
names(mean_imp) <- names(select(data_imp, -c(hmccode, year)))
model.data  <- bind_cols(select(data_imp, c(hmccode, year)), mean_imp)
all(model.data[, -c(15:25)] == data_imp[, -c(15:25)])



model.data.HM <- model.data %>% select(-c(very_unequal_utip, gdp_verylow_firstoil_year, L_very_unequal_utip, reserves_interp_area, L_reserves_interp_area, population, L_population, coal_income_pc, L_coal_income_pc, natural_gas_income_pc, L_natural_gas_income_pc))
model.data.HM <- dummy_cols(model.data.HM, select_columns = c("hmccode"), remove_first_dummy = TRUE)

### take out  "year_62"  "year_78"  "year_123" for HM
formula_HM <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.HM)[-c(1, 2, 3)],collapse="+"),sep=""))
test <- lm(formula_HM, data = model.data.HM)
drop  <- names(test$coefficients[is.na(test$coefficients) == T])
drop


model.data.HM  <- data.frame(model.data.HM %>% select(-c(drop)))

formula_HM <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.HM)[-c(1, 2, 3)],collapse="+"),sep=""))

lm(formula_HM, data = model.data.HM)

get_prior(formula_HM, data = model.data.HM, family = gaussian())
### specify priors here for all four models
prior = c(set_prior("normal(0,10)", class = "b"), set_prior("normal(0,10)", class = "Intercept"))

tic()
model_HM <- brm(formula = formula_HM, data = model.data.HM, family = gaussian(), warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, prior = prior)
toc()
save(model_HM,  file ="~/Dropbox/BayesChapter/Model_Results/model_HM_imp.rda")
brms::stancode(model_HM)




#### Andersen/Ross CPS 2015 model that includes interaction of post1980 and fiscal reliance
### Table 2 column 3
model.data.AR <- model.data %>% select(-c(very_unequal_utip, gdp_verylow_firstoil_year, L_very_unequal_utip, reserves_interp_area, L_reserves_interp_area, population, L_population, coal_income_pc, L_coal_income_pc, natural_gas_income_pc, L_natural_gas_income_pc)) %>% mutate(post1980 = case_when(year <= 1980 ~ 0,
                                                                                                                                                                                               year > 1980 ~ 1))
model.data.AR <- model.data.AR %>% mutate(post1980_L_Fiscal_Rel_interp = post1980 * L_Fiscal_Rel_interp,
                                       post1980_D_Fiscal_Rel_Interp = post1980 * D_Fiscal_Rel_Interp)

model.data.AR <- dummy_cols(model.data.AR, select_columns = c("hmccode"), remove_first_dummy = TRUE)



formula_AR <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.AR)[-c(1, 2, 3)],collapse="+"),sep=""))
test <- lm(formula_AR, data = model.data.AR)
drop  <- names(test$coefficients[is.na(test$coefficients) == T])
drop

tic()
model_AR <- brm(formula = formula_AR, data = model.data.AR, family = gaussian(), warmup = 2000, iter = 4000, chains = 4, cores = 4, control = list(max_treedepth = 15), save_all_pars =T, prior = prior, seed = 1234)
toc()
save(model_AR,  file ="~/Dropbox/BayesChapter/Model_Results/model_AR_imp.rda")

##### add inequality data and interaction with inequality
### why does this take so long
### how long does HM model take on 200 iterations? then add one by one variable

model.data.ineq <- model.data %>% select(-c(gdp_verylow_firstoil_year, reserves_interp_area, L_reserves_interp_area, population, L_population, coal_income_pc, L_coal_income_pc, natural_gas_income_pc, L_natural_gas_income_pc)) %>% mutate(
  D_very_unequal_utip = very_unequal_utip - L_very_unequal_utip,  
  unequal_L_Fiscal_Rel_interp = L_very_unequal_utip * L_Fiscal_Rel_interp,
  unequal_D_Fiscal_Rel_Interp = D_very_unequal_utip * D_Fiscal_Rel_Interp) %>% select( -c(very_unequal_utip))

dim(model.data.ineq)
model.data.ineq <- dummy_cols(model.data.ineq, select_columns = c("hmccode"), remove_first_dummy = TRUE)

formula_ineq <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.ineq)[-c(1, 2, 3)],collapse="+"),sep=""))
test <- lm(formula_ineq, data = model.data.ineq)
drop  <- names(test$coefficients[is.na(test$coefficients) == T])
drop
model.data.ineq  <- model.data.ineq %>% select(-c(drop))

formula_ineq <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.ineq)[-c(1, 2, 3)],collapse="+"),sep=""))

tic()
model_ineq <- brm(formula = formula_ineq, data = model.data.ineq, family = gaussian(),  warmup = 2000, iter = 4000, chains = 4, cores = 4, control = list(max_treedepth = 15), save_all_pars =T, prior = prior, seed = 1234)
toc()
save(model_ineq, file ="~/Dropbox/BayesChapter/Model_Results/model_ineq_imp.rda")

### now interaction with polity lag
model.data.lag <-  model.data %>% select(-c(very_unequal_utip, L_very_unequal_utip, gdp_verylow_firstoil_year, reserves_interp_area, L_reserves_interp_area, population, L_population, coal_income_pc, L_coal_income_pc, natural_gas_income_pc, L_natural_gas_income_pc))  %>% mutate(polity_L_Fiscal_Rel_interp = L_Polity_s_interp * L_Fiscal_Rel_interp)

model.data.lag <- dummy_cols(model.data.lag, select_columns = c("hmccode"), remove_first_dummy = TRUE)

formula_lag <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.lag)[-c(1, 2, 3)],collapse="+"),sep=""))
test <- lm(formula_lag, data = model.data.lag)
drop  <- names(test$coefficients[is.na(test$coefficients) == T])
drop
model.data.lag  <- model.data.lag %>% select(-c(drop))

formula_lag <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.lag)[-c(1, 2, 3)],collapse="+"),sep=""))

tic()
model_lag <- brm(formula = formula_lag, data = model.data.lag, family = gaussian(),  warmup = 2000, iter = 4000, chains = 4, cores = 4, control = list(max_treedepth = 15), save_all_pars =T, prior = prior, seed = 1234)
toc()
save(model_lag,  file ="~/Dropbox/BayesChapter/Model_Results/model_lag_imp.rda")

### model with all variables 
model.data.full <-  select(model.data, -c(reserves_interp_area, L_reserves_interp_area, population, L_population, coal_income_pc, L_coal_income_pc, natural_gas_income_pc, L_natural_gas_income_pc)) %>%  group_by(hmccode) %>% mutate(post1980 = case_when(year <= 1980 ~ 0,
                                                                                      year > 1980 ~ 1),
                                                                 D_very_unequal_utip = very_unequal_utip - L_very_unequal_utip,
                                                                 post1980_L_Fiscal_Rel_interp = post1980 * L_Fiscal_Rel_interp,
                                                                 post1980_D_Fiscal_Rel_Interp = post1980 * D_Fiscal_Rel_Interp,
                                                                 unequal_L_Fiscal_Rel_interp = L_very_unequal_utip * L_Fiscal_Rel_interp,
                                                                 unequal_D_Fiscal_Rel_Interp = D_very_unequal_utip * D_Fiscal_Rel_Interp,
                                                                 polity_L_Fiscal_Rel_interp = L_Polity_s_interp * L_Fiscal_Rel_interp,
                                                                 firstoil_L_Fiscal_Rel_interp = gdp_verylow_firstoil_year * L_Fiscal_Rel_interp,
                                                                 firstoil_D_Fiscal_Rel_Interp = gdp_verylow_firstoil_year * D_Fiscal_Rel_Interp)
                                                                 
model.data.full <- model.data.full %>% select(-c(very_unequal_utip))                                                                 
names(model.data.full)                                                                                               


model.data.full <- dummy_cols(model.data.full, select_columns = c("hmccode"), remove_first_dummy = TRUE)

formula_full <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.full)[-c(1, 2, 3)],collapse="+"),sep=""))
test <- lm(formula_full, data = model.data.full)
drop  <- names(test$coefficients[is.na(test$coefficients) == T])
drop
model.data.full  <- model.data.full %>% select(-c(drop))

formula_full <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.full)[-c(1, 2, 3)],collapse="+"),sep=""))

tic()
model_full <- brm(formula = formula_full, data = model.data.full, family = gaussian(),  warmup = 2000, iter = 4000, chains = 4, cores = 4, control = list(max_treedepth = 15), save_all_pars =T, prior = prior, seed = 1234)
toc()
save(model_full,  file ="~/Dropbox/BayesChapter/Model_Results/model_full_imp.rda")


#### model with horseshoe prior
#### scale data so that all variables (except binary) have mean = 0 and sd =1
model.data.full <-  model.data %>%  group_by(hmccode) %>% mutate(post1980 = case_when(year <= 1980 ~ 0, 
                                                                                      year > 1980 ~ 1),
                                                                                      D_very_unequal_utip = very_unequal_utip - L_very_unequal_utip,
                                                                                      post1980_D_Fiscal_Rel_Interp = post1980 * D_Fiscal_Rel_Interp,
                                                                                      unequal_D_Fiscal_Rel_Interp = D_very_unequal_utip * D_Fiscal_Rel_Interp,
                                                                                      polity_L_Fiscal_Rel_interp = L_Polity_s_interp * L_Fiscal_Rel_interp,
                                                                                      firstoil_L_Fiscal_Rel_interp = gdp_verylow_firstoil_year * L_Fiscal_Rel_interp,
                                                                                      firstoil_D_Fiscal_Rel_Interp = gdp_verylow_firstoil_year * D_Fiscal_Rel_Interp,
                                                                                      L_log_pop = log(L_population),
                                                                                      log_pop = log(population),
                                                                                      D_log_pop = log_pop - L_log_pop,
                                                                                      D_reserves_interp_area = reserves_interp_area - L_reserves_interp_area,
                                                                                      D_coal_income_pc = coal_income_pc - L_coal_income_pc, 
                                                                                      D_natural_gas_income_pc = natural_gas_income_pc - L_natural_gas_income_pc)

model.data.horse <- model.data.full %>% select(-c(very_unequal_utip, reserves_interp_area, population, L_population, log_pop, coal_income_pc, natural_gas_income_pc))                                                                 
names(model.data.horse)                                                                                               


model.data.horse <- dummy_cols(model.data.horse, select_columns = c("hmccode"), remove_first_dummy = TRUE)




scaled <- as.tibble(scale(select(model.data.horse, -c(hmccode,year,post1980,gdp_verylow_firstoil_year,32:48))))

model.data.scaled <- bind_cols(select(model.data.horse, c(hmccode,year,post1980,gdp_verylow_firstoil_year)), scaled)
model.data.scaled <- bind_cols(model.data.scaled, select(model.data.horse, c(32:48)))

formula_horse <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.scaled)[-which(names(model.data.scaled)%in%c("hmccode", "year", "D_polity_s_interp"))],collapse="+"),sep=""))
test <- lm(formula_horse, data = model.data.horse)
drop  <- names(test$coefficients[is.na(test$coefficients) == T])
drop

summary(model.data.scaled)
ratio <- 7/(dim(model.data.horse)[2]-1)

#writeLines(readLines("~/Documents/GitHub/BayesModelSelection/Code/linear_regHorseshoe.stan"))
n <- dim(model.data.scaled)[1]
X <- as.matrix(select(model.data.scaled,-c(hmccode, year, D_polity_s_interp)))
data_stan <- list(y = model.data.scaled$D_polity_s_interp, K = dim(X)[2], X = t(X), m_exp = 10)

rstan_options(auto_write = TRUE) 
options(mc.cores = parallel::detectCores())

finnish_fit <- stan(file='~/Documents/GitHub/BayesModelSelection/Code/linear_regHorseshoe.stan', init = 0,data=data_stan, seed=12345, iter = 5000, control=list(adapt_delta=0.99, max_treedepth=15), cores = 4)

check_hmc_diagnostics(finnish_fit)

util$check_n_eff(finnish_fit)


prior = c(set_prior("horseshoe(df = 3, par_ratio = 0.15, scale_slab = 4, df_slab = 4)", class = "b"), set_prior("normal(0,10)", class = "Intercept"))
formula_horse

tic()
model_full_horseshoe <- brm(formula = formula_horse, inits = 0, data = model.data.horse, family = gaussian(),  warmup = 1000, iter = 2000, chains = 4, cores = 4, control = list(adapt_delta = 0.99, max_treedepth = 15), save_all_pars =T, prior = prior, seed = 1234)
toc()
save(model_full_horseshoe,  file ="~/Dropbox/BayesChapter/Model_Results/model_full_horseshoe.rda")


