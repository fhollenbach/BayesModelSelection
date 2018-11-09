


library(brms)
library(tidyverse)
library(fastDummies)
library(haven)
library(tictoc)
data <- read_dta("~/Documents/GitHub/BayesModelSelection/Data/HaberMenaldoRepl.dta")
names(data)
#### from CPS article columns Table 2, cols 3,4
                                        #
### one haber & menaldo -
### ross model where interaction oil & after 1980
### haber menaldo model with inequality measure and interaction of oil and inequ (dunning 2008)
### interacting oil with democracy lag



data_sub <- data %>% select(c(hmccode, year, D_polity_s_interp, L_Polity_s_interp, L_Fiscal_Rel_interp, D_Fiscal_Rel_Interp, L_D_Fiscal_Rel_Interp, L_logGDPPERCAP, L_CivilWar, L_REGION_DEM_DIFFUSE, L_WORLD_DEM_DIFFUSE,  D_GDPPERCAP, D_RegionalDiffusion, D_WORLD_DEM_DIFFUSE, very_unequal_utip))
data_sub <- na.omit(data_sub)


model.data.HM <- na.omit(data_sub)
model.data.HM <- data_sub %>% select(-c(very_unequal_utip))
model.data.HM <- dummy_cols(model.data.HM, select_columns = c("hmccode"), remove_first_dummy = TRUE)

### take out  "year_62"  "year_78"  "year_123" for HM
formula_HM <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.HM)[-c(1, 2, 3)],collapse="+"),sep=""))
test <- lm(formula_HM, data = model.data.HM)
drop  <- names(test$coefficients[is.na(test$coefficients) == T])
drop


model.data.HM  <- model.data.HM %>% select(-c(drop))

formula_HM <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.HM)[-c(1, 2, 3)],collapse="+"),sep=""))

tic()
model_HM <- brm(formula = formula_HM, data = model.data.HM, family = gaussian(), warmup = 1000, iter = 2500, chains = 4, cores = 4, control = list(max_treedepth = 15))
toc()
save(model_HM,  file ="~/Dropbox/BayesChapter/Model_Results/model_HM.rda")




#### Andersen/Ross CPS 2015 model that includes interaction of post1980 and fiscal reliance
### Table 2 column 3
model.data.AR <- data_sub %>% select(-c(very_unequal_utip)) %>% mutate(post1980 = case_when(year <= 1980 ~ 0,
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
model_AR <- brm(formula = formula_AR, data = model.data.AR, family = gaussian(), warmup = 1000, iter = 2500, chains = 4, cores = 4, control = list(max_treedepth = 30))
toc()
save(model_AR,  file ="~/Dropbox/BayesChapter/Model_Results/model_AR.rda")

##### add inequality data and interaction with inequality
### why does this take so long
### how long does HM model take on 200 iterations? then add one by one variable

model.data.ineq <- data_sub %>% mutate(unequal_L_Fiscal_Rel_interp = very_unequal_utip * L_Fiscal_Rel_interp,
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
model_ineq <- brm(formula = formula_ineq, data = model.data.ineq, family = gaussian(), warmup = 1000, iter = 2500, chains = 4, cores = 4, control = list(max_treedepth = 16))
toc()
save(model_ineq, file ="~/Dropbox/BayesChapter/Model_Results/model_ineq.rda")

### now interaction with polity lag
model.data.lag <-  data_sub %>% select(-c(very_unequal_utip))  %>% mutate(
                                                                       polity_L_Fiscal_Rel_interp = L_Polity_s_interp * L_Fiscal_Rel_interp,
  polity_D_Fiscal_Rel_Interp = L_Polity_s_interp * D_Fiscal_Rel_Interp)

model.data.lag <- dummy_cols(model.data.lag, select_columns = c("hmccode"), remove_first_dummy = TRUE)

formula_lag <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.lag)[-c(1, 2, 3)],collapse="+"),sep=""))
test <- lm(formula_lag, data = model.data.lag)
drop  <- names(test$coefficients[is.na(test$coefficients) == T])

model.data.lag  <- model.data.lag %>% select(-c(drop))

formula_lag <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.lag)[-c(1, 2, 3)],collapse="+"),sep=""))

tic()
model_lag <- brm(formula = formula_lag, data = model.data.lag, family = gaussian(), warmup = 1000, iter = 2500, chains = 4, cores = 4, control = list(max_treedepth = 15))
toc()
save(model_lag,  file ="~/Dropbox/BayesChapter/Model_Results/model_lag.rda")
