


library(brms)
library(tidyverse)
library(fastDummies)
library(haven)
library(tictoc)
data <- read_dta("~/Documents/GitHub/BayesModelSelection/Data/HaberMenaldoRepl_full.dta")
names(data)
#### from CPS article columns Table 2, cols 3,4
                                        #
### one haber & menaldo -
### ross model where interaction oil & after 1980
### haber menaldo model with inequality measure and interaction of oil and inequ (dunning 2008)
### interacting oil with democracy lag



data_sub <- data %>% select(c(hmccode, year, D_polity_s_interp, L_Polity_s_interp, L_tot_oil_inc_interp, D_tot_oil_inc_interp, L_LogPerCapGDP_interp, L_CivilWar_interp, L_REGION_DEM_DIFFUSE, L_WORLD_DEM_DIFFUSE, D_LogperCapGDP_int, D_Region_Dem_Diffuse, D_World_Dem_Diffuse, very_unequal_utip))
data_sub <- na.omit(data_sub)

model.data.HM <- data_sub
model.data.HM <- data_sub %>% select(-c(very_unequal_utip))
model.data.HM <- dummy_cols(data_sub, select_columns = c("hmccode"), remove_first_dummy = TRUE)

### take out  "year_62"  "year_78"  "year_123" for HM
formula_HM <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.HM)[-c(1, 2, 3)],collapse="+"),sep=""))
test <- lm(formula_HM, data = model.data.HM)
drop  <- names(test$coefficients[is.na(test$coefficients) == T])
model.data.HM  <- model.data.HM %>% select(-c(drop))

formula_HM <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.HM)[-c(1, 2, 3)],collapse="+"),sep=""))

tic()
model_HM <- brm(formula = formula_HM, data = model.data.HM, family = gaussian(), warmup = 100, iter = 200, chains = 1, cores = 1, control = list(max_treedepth = 15))
toc()

                                        #save(model_HM,  file ="~/Dropbox/BayesChapter/Model_Results/model_HM.rda")




#### Andersen/Ross CPS 2015 model that includes interaction of post1980 and fiscal reliance
### Table 2 column 3
model.data.AR <- data_sub %>% mutate(post1980 = case_when(year <= 1980 ~ 0,
                                                          year > 1980 ~ 1))
model.data.AR <- model.data.AR %>% mutate(
                                       post1980_L_tot_oil_inc_interp = post1980 * L_tot_oil_inc_interp,
                                       post1980_D_tot_oil_inc_interp = post1980 * D_tot_oil_inc_interp)

formula_AR <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.AR)[-c(1, 2, 3)],collapse="+"),sep=""))
test <- lm(formula_AR, data = model.data.AR)
drop  <- names(test$coefficients[is.na(test$coefficients) == T])
drop

tic()
model_AR <- brm(formula = formula_AR, data = model.data.AR, family = gaussian(), warmup = 100, iter = 200, chains = 1, cores = 1, control = list(max_treedepth = 30))
toc()
                                        #save(model_AR,  file ="~/Dropbox/BayesChapter/Model_Results/model_AR.rda")

##### add inequality data and interaction with inequality
### why does this take so long
### how long does HM model take on 200 iterations? then add one by one variable

model.data.ineq <- data_sub %>% mutate(unequal_L_tot_oil_inc_interp = very_unequal_utip * L_tot_oil_inc_interp,
                                       unequal_D_tot_oil_inc_interp = very_unequal_utip * D_tot_oil_inc_interp)

dim(model.data.ineq)
model.data.ineq <- dummy_cols(model.data.ineq, select_columns = c("hmccode"), remove_first_dummy = TRUE)

formula_ineq <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.ineq)[-c(1, 2, 3)],collapse="+"),sep=""))
test <- lm(formula_ineq, data = model.data.ineq)
drop  <- names(test$coefficients[is.na(test$coefficients) == T])
model.data.ineq  <- model.data.ineq %>% select(-c(drop))

formula_ineq <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.ineq)[-c(1, 2, 3)],collapse="+"),sep=""))

tic()
model_ineq <- brm(formula = formula_ineq, data = model.data.ineq, family = gaussian(), warmup = 100, iter = 200, chains = 1, cores = 1, control = list(max_treedepth = 16))
toc()

### add inequality
                                        #save(model_ineq, file ="~/Dropbox/BayesChapter/Model_Results/model_ineq.rda")

### now interaction with polity lag
model.data.lag <-  data_sub %>% mutate(
                                    polity_L_tot_oil_inc_interp = L_Polity_s_interp * L_tot_oil_inc_interp,
                                    polity_D_tot_oil_inc_interp = L_Polity_s_interp * D_tot_oil_inc_interp)

model.data.lag <- dummy_cols(model.data.lag, select_columns = c("hmccode"), remove_first_dummy = TRUE)

formula_lag <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.lag)[-c(1, 2, 3)],collapse="+"),sep=""))
test <- lm(formula_lag, data = model.data.lag)
drop  <- names(test$coefficients[is.na(test$coefficients) == T])
model.data.lag  <- model.data.lag %>% select(-c(drop))

formula_lag <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.lag)[-c(1, 2, 3)],collapse="+"),sep=""))

tic()
model_lag <- brm(formula = formula_lag, data = model.data.lag, family = gaussian(), warmup = 100, iter = 200, chains = 1, cores = 1, control = list(max_treedepth = 15))
toc()
                                        #save(model_lag,  file ="~/Dropbox/BayesChapter/Model_Results/model_lag.rda")
