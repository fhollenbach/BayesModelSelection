


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

### last model with only lags and some additional variable: total_fuel_income_pc,total_resources_income_pc, gdp_verylow_firstoil_year, and reserves_interp_area


data_sub <- data %>% select(c(hmccode, year, D_polity_s_interp, L_Polity_s_interp, L_Fiscal_Rel_interp, D_Fiscal_Rel_Interp, L_D_Fiscal_Rel_Interp, L_logGDPPERCAP, L_CivilWar, L_REGION_DEM_DIFFUSE, L_WORLD_DEM_DIFFUSE,  D_GDPPERCAP, D_RegionalDiffusion, D_WORLD_DEM_DIFFUSE, very_unequal_utip,total_resources_income_pc, gdp_verylow_firstoil_year, reserves_interp_area))

data_sub <- data_sub %>% mutate(L_very_unequal_utip = lag(very_unequal_utip, order_by = year),
                                L_total_resources_income_pc = lag(total_resources_income_pc, order_by = year),
                                L_reserves_interp_area = lag(reserves_interp_area, order_by = year)) %>% select(-c(very_unequal_utip,total_resources_income_pc, reserves_interp_area))


data_imp <- data_sub[complete.cases(select(data_sub, -c(gdp_verylow_firstoil_year, L_very_unequal_utip, L_total_resources_income_pc, L_reserves_interp_area))), ]
#### missing: gdp_verylow_firstoil_year 150
#####    L_very_unequal_utip 353
###### L_total_fuel_income_pc 64
### L_total_resources_income_pc 64
##### L_reserves_interp_area 71
cor(as.matrix(select(data_imp, c(L_total_resources_income_pc, L_reserves_interp_area))), use = "pairwise.complete.obs")


missing <- data_imp %>% select(-c(hmccode, year))
sbgImp <- sbgcop.mcmc(missing, nsamp=2100, odens = 3, seed=6886) ##2000 samples, only every 3 saved, 700 resulting
save(sbgImp, file ="~/Dropbox/BayesChapter/Model_Results/imputation.rda")


sbgData = sbgImp$'Y.impute'[,,]
mean_imp  <- as.tibble(apply(sbgImp$'Y.impute',c(1,2),mean))
names(mean_imp) <- names(select(data_imp, -c(hmccode, year)))
model.data  <- bind_cols(select(data_imp, c(hmccode, year)), mean_imp)
all(model.data[, -c(15:19)] == data_imp[, -c(15:19)])



model.data.HM <- model.data %>% select(-c(very_unequal_utip, total_fuel_income_pc,total_resources_income_pc, gdp_verylow_firstoil_year, reserves_interp_area))
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
model.data.AR <- model.data %>% select(-c(very_unequal_utip, total_fuel_income_pc,total_resources_income_pc, gdp_verylow_firstoil_year, reserves_interp_area)) %>% mutate(post1980 = case_when(year <= 1980 ~ 0,
                                                                                                                                                                                               year > 1980 ~ 1))
model.data.AR <- model.data.AR %>% mutate(
                                       post1980_L_Fiscal_Rel_interp = post1980 * L_Fiscal_Rel_interp,
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

model.data.ineq <- model.data %>% select(-c(total_fuel_income_pc,total_resources_income_pc, gdp_verylow_firstoil_year, reserves_interp_area)) %>% mutate(
                                                                                                                                                      unequal_L_Fiscal_Rel_interp = L_very_unequal_utip * L_Fiscal_Rel_interp,
                                                                                                                                                      D_very_unequal_utip = very_unequal_utip - L_very_unequal_utip,

                                                                                                                                                      unequal_D_Fiscal_Rel_Interp = D_very_unequal_utip * D_Fiscal_Rel_Interp)

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
model.data.lag <-  model.data %>% select(-c(very_unequal_utip, total_fuel_income_pc,total_resources_income_pc, gdp_verylow_firstoil_year, reserves_interp_area))  %>% mutate(
                                                                                                                                                                          polity_L_Fiscal_Rel_interp = L_Polity_s_interp * L_Fiscal_Rel_interp,
                                                                                                                                                                          polity_D_Fiscal_Rel_Interp = L_Polity_s_interp * D_Fiscal_Rel_Interp)

model.data.lag <- dummy_cols(model.data.lag, select_columns = c("hmccode"), remove_first_dummy = TRUE)

formula_lag <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.lag)[-c(1, 2, 3)],collapse="+"),sep=""))
test <- lm(formula_lag, data = model.data.lag)
drop  <- names(test$coefficients[is.na(test$coefficients) == T])

model.data.lag  <- model.data.lag %>% select(-c(drop))

formula_lag <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.lag)[-c(1, 2, 3)],collapse="+"),sep=""))

tic()
model_lag <- brm(formula = formula_lag, data = model.data.lag, family = gaussian(),  warmup = 2000, iter = 4000, chains = 4, cores = 4, control = list(max_treedepth = 15), save_all_pars =T, prior = prior, seed = 1234)
toc()
save(model_lag,  file ="~/Dropbox/BayesChapter/Model_Results/model_lag_imp.rda")

### model with all variables but only lags
### add some more lags for total_fuel_income_pc,total_resources_income_pc, gdp_verylow_firstoil_year, and reserves_interp_area
model.data.full <-  model.data %>%  group_by(hmccode) %>% mutate(post1980 = case_when(year <= 1980 ~ 0,
                                                                                      year > 1980 ~ 1),
                                                                 unequal_L_Fiscal_Rel_interp = L_very_unequal_utip * L_Fiscal_Rel_interp,
                                                                 polity_L_Fiscal_Rel_interp = L_Polity_s_interp * L_Fiscal_Rel_interp,
                                                                 post1980_L_Fiscal_Rel_interp = post1980 * L_Fiscal_Rel_interp)

model.data.full <- model.data.full %>% select(-c(D_Fiscal_Rel_Interp, L_D_Fiscal_Rel_Interp, D_GDPPERCAP, D_RegionalDiffusion, D_WORLD_DEM_DIFFUSE))
names(model.data.full)
