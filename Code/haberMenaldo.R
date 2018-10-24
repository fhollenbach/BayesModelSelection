


library(brms)
library(rstanarm)
library(tidyverse)
library(fastDummies)
library(haven)
data <- read_dta("~/Dropbox/BayesChapter/Data/HaberMenaldoRepl.dta")


#### from CPS article columns Table 2, cols 3,4
                                        #
### one haber & menaldo -
### ross model where interaction oil & after 1980
### haber menaldo model with inequality measure and interaction of oil and inequ (dunning 2008)
### interacting oil with democracy lag


model.data <- data[, c(names(data)[names(data) %in% c("hmccode", "year", "D_polity_s_interp", "L_Polity_s_interp", "L_Fiscal_Rel_interp", "D_Fiscal_Rel_Interp", "L_D_Fiscal_Rel_Interp", "L_logGDPPERCAP L_CivilWar", "L_REGION_DEM_DIFFUSE", "L_WORLD_DEM_DIFFUSE", "D_GDPPERCAP", "D_RegionalDiffusion", "D_WORLD_DEM_DIFFUSE", names(data)[c(58:74, 137:151, 153:196, 198:281)])])]



model.data <- na.omit(model.data)



formula_HM <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data)[-c(1, 2, 3)],collapse="+"),sep=""))
test <- lm(formula1, data = model.data)  ### pretty close

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

model.data.ineq <- model.data %>% left_join(data[, c("hmccode", "year", "very_unequal_utip")], by = c("hmccode", "year"))
model.data.ineq <- model.data.ineq %>% mutate(unequal_L_FiscalReliance = very_unequal_utip * L_Fiscal_Rel_interp)
model.data.ineq <- model.data.ineq %>% mutate(unequal_D_FiscalReliance = very_unequal_utip * D_Fiscal_Rel_Interp)


formula_ineq <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.ineq)[-c(1, 2, 3)],collapse="+"),sep=""))

model_ineq <- brm(formula = formula_ineq, data = model.data.ineq, family = gaussian(), warmup = 100, iter = 250, chains = 4, cores = 4, control = list(max_treedepth = 16))
save(model_ineq, file ="~/Dropbox/BayesChapter/Model_Results/model_ineq.rda")




### now interaction with polity lag
model.data.lag <-  model.data %>% mutate(polity_L_FiscalReliance = L_Polity_s_interp * L_Fiscal_Rel_interp)
model.data.lag <-  model.data.lag %>% mutate(polity_D_FiscalReliance = L_Polity_s_interp * D_Fiscal_Rel_Interp)


formula_lag <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.lag)[-c(1, 2, 3)],collapse="+"),sep=""))

model_lag <- brm(formula = formula_lag, data = model.data.lag, family = gaussian(), warmup = 1000, iter = 2500, chains = 4, cores = 4, control = list(max_treedepth = 15))
save(model_lag,  file ="~/Dropbox/BayesChapter/Model_Results/model_lag.rda")
