


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


data_sub <- data[, c(names(data)[names(data) %in% c("hmccode", "year", "D_polity_s_interp", "L_Polity_s_interp", "L_Fiscal_Rel_interp", "D_Fiscal_Rel_Interp", "L_D_Fiscal_Rel_Interp", "L_logGDPPERCAP L_CivilWar", "L_REGION_DEM_DIFFUSE", "L_WORLD_DEM_DIFFUSE", "D_GDPPERCAP", "D_RegionalDiffusion", "D_WORLD_DEM_DIFFUSE")])]



model.data.HM <- na.omit(data_sub)

model.data.HM <- dummy_cols(data_sub, select_columns = c("hmccode", "year"), remove_first_dummy = TRUE)

### take out  "year_62"  "year_78"  "year_123" for HM
formula_HM <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.HM)[-c(1, 2, 3,97,152,168)],collapse="+"),sep=""))
lm(formula_HM, data = model.data.HM)


model_HM <- brm(formula = formula_HM, data = model.data, family = gaussian(), warmup = 1000, iter = 2500, chains = 4, cores = 4, control = list(max_treedepth = 15))
save(model_HM,  file ="~/Dropbox/BayesChapter/Model_Results/model_HM.rda")




#### Andersen/Ross CPS 2015 model that includes interaction of post1980 and fiscal reliance
### Table 2 column 3
model.data.AR <- model.data %>% mutate(post1980 = case_when(year <= 1980 ~ 0,
                                                            year > 1980 ~ 1))
model.data.AR <- model.data.AR %>% mutate(post1980_L_FiscalReliance = post1980 * L_Fiscal_Rel_interp)
model.data.AR <- model.data.AR %>% mutate(post1980_D_FiscalReliance = post1980 * D_Fiscal_Rel_Interp)

formula_AR <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.AR)[-c(1, 2, 3)],collapse="+"),sep=""))
model_AR <- brm(formula = formula_AR, data = model.data.AR, family = gaussian(), warmup = 1000, iter = 2500, chains = 4, cores = 4, control = list(max_treedepth = 30))
save(model_AR,  file ="~/Dropbox/BayesChapter/Model_Results/model_AR.rda")

##### add inequality data and interaction with inequality
### why does this take so long
### how long does HM model take on 200 iterations? then add one by one variable
model.data.ineq <- data_sub
model.data.ineq <- na.omit(model.data.ineq)
dim(model.data.ineq)
model.data.ineq <- dummy_cols(model.data.ineq, select_columns = c("hmccode", "year"), remove_first_dummy = TRUE)

formula_ineq <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.ineq)[-c(1, 2, 3)],collapse="+"),sep=""))
tic()
model_ineq <- brm(formula = formula_ineq, data = model.data.ineq, family = gaussian(), warmup = 100, iter = 200, chains = 1, cores = 1, control = list(max_treedepth = 16))
toc()

### add inequality
model.data.ineq <- model.data.HM %>% left_join(data[, c("hmccode", "year", "unequal_utip")], by = c("hmccode", "year"))
model.data.ineq <- na.omit(model.data.ineq)
dim(model.data.ineq)
model.data.ineq <- dummy_cols(model.data.ineq, select_columns = c("hmccode", "year"), remove_first_dummy = TRUE)

formula_ineq <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.ineq)[-c(1, 2, 3)],collapse="+"),sep=""))
tic()
model_ineq <- brm(formula = formula_ineq, data = model.data.ineq, family = gaussian(), warmup = 100, iter = 200, chains = 1, cores = 1, control = list(max_treedepth = 16))
toc()


model.data.ineq <- model.data.ineq %>% mutate(unequal_L_FiscalReliance = unequal_utip * L_Fiscal_Rel_interp)

formula_ineq <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.ineq)[-c(1, 2, 3)],collapse="+"),sep=""))
tic()
model_ineq <- brm(formula = formula_ineq, data = model.data.ineq, family = gaussian(), warmup = 100, iter = 200, chains = 1, cores = 1, control = list(max_treedepth = 16))
toc()

model.data.ineq <- model.data.ineq %>% mutate(unequal_D_FiscalReliance = unequal_utip * D_Fiscal_Rel_Interp)
model.data.ineq <- na.omit(model.data.ineq)
model.data.ineq <- dummy_cols(model.data.ineq, select_columns = c("hmccode", "year"), remove_first_dummy = TRUE)

dim(model.data.ineq)
formula_ineq <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.ineq)[-c(1, 2, 3)],collapse="+"),sep=""))
tic()
model_ineq <- brm(formula = formula_ineq, data = model.data.ineq, family = gaussian(), warmup = 100, iter = 200, chains = 1, cores = 1, control = list(max_treedepth = 16))
toc()



formula_ineq <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.ineq)[-c(1, 2, 3)],collapse="+"),sep=""))

model_ineq <- brm(formula = formula_ineq, data = model.data.ineq, family = gaussian(), warmup = 100, iter = 200, chains = 1, cores = 1, control = list(max_treedepth = 16))
save(model_ineq, file ="~/Dropbox/BayesChapter/Model_Results/model_ineq.rda")




### now interaction with polity lag
model.data.lag <-  model.data %>% mutate(polity_L_FiscalReliance = L_Polity_s_interp * L_Fiscal_Rel_interp)
model.data.lag <-  model.data.lag %>% mutate(polity_D_FiscalReliance = L_Polity_s_interp * D_Fiscal_Rel_Interp)


formula_lag <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.lag)[-c(1, 2, 3)],collapse="+"),sep=""))

model_lag <- brm(formula = formula_lag, data = model.data.lag, family = gaussian(), warmup = 1000, iter = 2500, chains = 4, cores = 4, control = list(max_treedepth = 15))
save(model_lag,  file ="~/Dropbox/BayesChapter/Model_Results/model_lag.rda")
