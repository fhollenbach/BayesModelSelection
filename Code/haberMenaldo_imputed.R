


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



data_sub <- data %>% select(c(hmccode, year, D_polity_s_interp, L_Polity_s_interp, L_Fiscal_Rel_interp, D_Fiscal_Rel_Interp, L_D_Fiscal_Rel_Interp, L_logGDPPERCAP, L_CivilWar, L_REGION_DEM_DIFFUSE, L_WORLD_DEM_DIFFUSE,  D_GDPPERCAP, D_RegionalDiffusion, D_WORLD_DEM_DIFFUSE, very_unequal_utip))
data_imp <- data_sub[complete.cases(select(data_sub, -c(very_unequal_utip))), ]


missing <- data_imp %>% select(-c(hmccode, year))
sbgImp <- sbgcop.mcmc(missing, nsamp=2100, odens = 3, seed=6886) ##2000 samples, only every 3 saved, 700 resulting
save(sbgImp, file ="~/Dropbox/BayesChapter/Model_Results/imputation.rda")


sbgData = sbgImp$'Y.impute'[,,]
mean_imp  <- as.tibble(apply(sbgImp$'Y.impute',c(1,2),mean))
names(mean_imp) <- names(select(data_imp, -c(hmccode, year)))
model.data  <- bind_cols(select(data_imp, c(hmccode, year)), mean_imp)
all(model.data[, -15] == data_imp[, -15])



model.data.HM <- model.data %>% select(-c(very_unequal_utip))
model.data.HM <- dummy_cols(model.data.HM, select_columns = c("hmccode"), remove_first_dummy = TRUE)

### take out  "year_62"  "year_78"  "year_123" for HM
formula_HM <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.HM)[-c(1, 2, 3)],collapse="+"),sep=""))
test <- lm(formula_HM, data = model.data.HM)
drop  <- names(test$coefficients[is.na(test$coefficients) == T])
drop


model.data.HM  <- data.frame(model.data.HM %>% select(-c(drop)))

formula_HM <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.HM)[-c(1, 2, 3)],collapse="+"),sep=""))






mHM_rethinking <- map2stan(
    alist(
        D_polity_s_interp ~ dnorm( mu , sigma ) ,
        mu <- a + b_lag_polity*L_Polity_s_interp + b_lag_fiscal*L_Fiscal_Rel_interp +
            b_d_fiscal*D_Fiscal_Rel_Interp + b_l_d_fiscal*L_D_Fiscal_Rel_Interp + b_l_gdp*L_logGDPPERCAP +
            b_l_civilwar*L_CivilWar + b_l_region*L_REGION_DEM_DIFFUSE + b_l_world*L_WORLD_DEM_DIFFUSE +
            b_d_gdp*D_GDPPERCAP + b_d_region*D_RegionalDiffusion + b_d_world*D_WORLD_DEM_DIFFUSE +
            b_70*hmccode_70 + b_101*hmccode_101 + b_130*hmccode_130 + b_155*hmccode_155 + b_385*hmccode_385 +
            b_411*hmccode_411 + b_475*hmccode_475 + b_481*hmccode_481 + b_540*hmccode_540 + b_551*hmccode_551 +
            b_615*hmccode_615 + b_630* hmccode_630 + b_679*hmccode_679 + b_690*hmccode_690 + b_692*hmccode_692 +
            b_698*hmccode_698 + b_850*hmccode_850 ,
        a ~ dnorm(0,5),
        c(b_lag_polity, b_lag_fiscal, b_d_fiscal, b_l_d_fiscal, b_l_gdp, b_l_civilwar, b_l_region, b_l_world, b_d_gdp, b_d_region, b_d_world, b_70, b_101, b_130, b_155, b_385, b_411, b_475, b_481, b_540, b_551, b_615, b_630, b_679, b_690, b_692, b_698, b_850) ~ dnorm(0,5),
        sigma ~ dcauchy(0,1)
    ), data=model.data.HM, warmup = 1000, iter=4500,chains=4, WAIC=TRUE , cores=4)




get_prior(formula_HM, data = model.data.HM, family = gaussian())
### specify priors here for all four models
prior = c(set_prior("normal(0,4)", class = "b"), set_prior("normal(0,4)", class = "Intercept"))

tic()
model_HM <- brm(formula = formula_HM, data = model.data.HM, family = gaussian(), warmup = 1000, iter = 2500, chains = 4, cores = 4, control = list(max_treedepth = 15), save_all_pars =T, prior = prior, seed = 1234)
toc()
save(model_HM,  file ="~/Dropbox/BayesChapter/Model_Results/model_HM_imp.rda")




#### Andersen/Ross CPS 2015 model that includes interaction of post1980 and fiscal reliance
### Table 2 column 3
model.data.AR <- model.data %>% select(-c(very_unequal_utip)) %>% mutate(post1980 = case_when(year <= 1980 ~ 0,
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
model_AR <- brm(formula = formula_AR, data = model.data.AR, family = gaussian(), warmup = 1000, iter = 2500, chains = 4, cores = 4, control = list(max_treedepth = 15), save_all_pars =T, prior = prior, seed = 1234)
toc()
save(model_AR,  file ="~/Dropbox/BayesChapter/Model_Results/model_AR_imp.rda")

##### add inequality data and interaction with inequality
### why does this take so long
### how long does HM model take on 200 iterations? then add one by one variable

model.data.ineq <- model.data %>% mutate(unequal_L_Fiscal_Rel_interp = very_unequal_utip * L_Fiscal_Rel_interp,
                                         unequal_D_Fiscal_Rel_Interp = very_unequal_utip * D_Fiscal_Rel_Interp)

dim(model.data.ineq)
model.data.ineq <- dummy_cols(model.data.ineq, select_columns = c("hmccode"), remove_first_dummy = TRUE)

formula_ineq <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.ineq)[-c(1, 2, 3)],collapse="+"),sep=""))
test <- lm(formula_ineq, data = model.data.ineq)
drop  <- names(test$coefficients[is.na(test$coefficients) == T])
drop
model.data.ineq  <- model.data.ineq %>% select(-c(drop))

formula_ineq <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.ineq)[-c(1, 2, 3)],collapse="+"),sep=""))

tic()
model_ineq <- brm(formula = formula_ineq, data = model.data.ineq, family = gaussian(), warmup = 1000, iter = 2500, chains = 4, cores = 4, control = list(max_treedepth = 15), save_all_pars =T, prior = prior, seed = 1234)
toc()
save(model_ineq, file ="~/Dropbox/BayesChapter/Model_Results/model_ineq_imp.rda")

### now interaction with polity lag
model.data.lag <-  model.data %>% select(-c(very_unequal_utip))  %>% mutate(
                                                                         polity_L_Fiscal_Rel_interp = L_Polity_s_interp * L_Fiscal_Rel_interp,
                                                                         polity_D_Fiscal_Rel_Interp = L_Polity_s_interp * D_Fiscal_Rel_Interp)

model.data.lag <- dummy_cols(model.data.lag, select_columns = c("hmccode"), remove_first_dummy = TRUE)

formula_lag <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.lag)[-c(1, 2, 3)],collapse="+"),sep=""))
test <- lm(formula_lag, data = model.data.lag)
drop  <- names(test$coefficients[is.na(test$coefficients) == T])

model.data.lag  <- model.data.lag %>% select(-c(drop))

formula_lag <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.lag)[-c(1, 2, 3)],collapse="+"),sep=""))

tic()
model_lag <- brm(formula = formula_lag, data = model.data.lag, family = gaussian(), warmup = 1000, iter = 2500, chains = 4, cores = 4, control = list(max_treedepth = 15), save_all_pars =T, prior = prior, seed = 1234)
toc()
save(model_lag,  file ="~/Dropbox/BayesChapter/Model_Results/model_lag_imp.rda")
