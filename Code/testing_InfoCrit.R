### check model comparison function 





library(brms)
library(tidyverse)
library(fastDummies)
library(haven)
library(tictoc)
library(sbgcop)
library(rethinking)



source("~/Documents/GitHub/BayesModelSelection/code/info_functions.R")

data <- read_dta("~/Documents/GitHub/BayesModelSelection/Data/HaberMenaldoRepl.dta")
names(data)

data_sub <- data %>% select(c(hmccode, year, D_polity_s_interp, L_Polity_s_interp, L_Fiscal_Rel_interp, D_Fiscal_Rel_Interp, L_D_Fiscal_Rel_Interp, L_logGDPPERCAP, L_CivilWar, L_REGION_DEM_DIFFUSE, L_WORLD_DEM_DIFFUSE,  D_GDPPERCAP, D_RegionalDiffusion, D_WORLD_DEM_DIFFUSE, very_unequal_utip))
data_imp <- data_sub[complete.cases(select(data_sub, -c(very_unequal_utip))), ]



model.data.HM <- data_imp %>% select(-c(very_unequal_utip))
model.data.HM <- dummy_cols(model.data.HM, select_columns = c("hmccode"), remove_first_dummy = TRUE)


formula_HM <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.HM)[-c(1, 2, 3)],collapse="+"),sep=""))
test <- lm(formula_HM, data = model.data.HM)
drop  <- names(test$coefficients[is.na(test$coefficients) == T])
drop


model.data.HM  <- model.data.HM %>% select(-c(drop))

formula_HM <- as.formula(paste("D_polity_s_interp ~",paste(names(model.data.HM)[-c(1, 2, 3)],collapse="+"),sep=""))

formula_HM

prior = c(set_prior("normal(0,20)", class = "b"), set_prior("normal(0,20)", class = "Intercept"), set_prior("cauchy(0, 2)", class = "sigma"))

model_HM <- brm(formula = formula_HM, data = model.data.HM, family = gaussian(), warmup = 1000, iter = 2000, chains = 4, cores = 4, control = list(max_treedepth = 15), save_all_pars =T, prior = prior, seed = 1234)

mHM_rethinking <- map2stan(
    alist(
        D_polity_s_interp ~ dnorm( mu , sigma ) ,
        mu <- a + b1 * L_Polity_s_interp + b2 * L_Fiscal_Rel_interp +
            b3 * D_Fiscal_Rel_Interp + b4 * L_D_Fiscal_Rel_Interp + b5 * L_logGDPPERCAP +
            b6 * L_CivilWar + b7 * L_REGION_DEM_DIFFUSE + b8 * L_WORLD_DEM_DIFFUSE +
            b9 * D_GDPPERCAP + b10 * D_RegionalDiffusion + b11 * D_WORLD_DEM_DIFFUSE +
            b12 * hmccode_70 + b13 * hmccode_101 + b14 * hmccode_130 + b15 * hmccode_155 + b16 * hmccode_385 +
            b17 * hmccode_411 + b18 * hmccode_475 + b19 * hmccode_481 + b20 * hmccode_540 + b21 * hmccode_551 +
            b22 * hmccode_615 + b23 * hmccode_630 +b24 *  hmccode_679 + b25 * hmccode_690 + b26 * hmccode_692 +
            b27 * hmccode_698 + b29 * hmccode_850 ,
        a ~ dnorm(0,4),
        c(b1, b2, b3, b4, b5, b6,b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, b22, b23, b24, b25, b26, b27) ~ dnorm(0,4),
        sigma ~ dcauchy(0,2)
    ), data=model.data.HM, iter=2000,chains=4, WAIC=TRUE , cores=4)
