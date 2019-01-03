

library(brms)
library(tidyverse)
library(tictoc)
library(rethinking)
load("~/Dropbox/BayesChapter/Data/Prio/confl_data.rda")

#### subset to those variables used later, so that we can omit all missing data
data  <- data %>% mutate(log_pop = log(pop_gpw_sum + 1))



data <- data %>% select(c(gwno, year, ucdp_event, mountains_mean, forest_gc, bdist1, capdist, prec_gpcc, temp, droughtend_spi, petroleum_y,petroleum_s, diamprim_y, diamsec_y, diamprim_s, diamsec_s, gem_y, drug_y,  gem_s, urban_gc, nlights_calib_mean, agri_gc, imr_mean, excluded, v2x_polyarchy, v2x_jucon, v2xel_locelec, v2xel_regelec, v2svstterr,  e_polity2, e_parcomp, e_parreg, e_polcomp, e_xrcomp, e_xropen, e_xrreg, log_pop, gcp_ppp))



data <- data %>% mutate(petroleum_y = as.double(petroleum_y),
                        petroleum_s = as.double(petroleum_s),
                        diamprim_y = as.double(diamprim_y),
                        diamprim_s = as.double(diamprim_s),
                        diamsec_y = as.double(diamsec_y),
                        diamsec_s = as.double(diamsec_s),
                        gem_y = as.double(gem_y),
                        gem_s = as.double(gem_s),
                        drug_y = as.double(drug_y)) %>% mutate(petroleum_y = case_when(is.na(petroleum_y) == T ~ 0,
                                                                                       TRUE ~ petroleum_y),
           petroleum_s = case_when(is.na(petroleum_s) == T ~ 0,
                                   TRUE ~ petroleum_s),
           diamprim_y = case_when(is.na(diamprim_y) == T ~ 0,
                                  TRUE ~ diamprim_y),
           diamprim_s = case_when(is.na(diamprim_s) == T ~ 0,
                                  TRUE ~ diamprim_s),
           diamsec_y = case_when(is.na(diamsec_y) == T ~ 0,
                                 TRUE ~diamsec_y),
           diamsec_s = case_when(is.na(diamsec_s) == T ~ 0,
                                 TRUE ~ diamsec_s),
           gem_s = case_when(is.na(gem_s) == T ~ 0,
                             TRUE ~ gem_s),
           gem_y = case_when(is.na(gem_y) == T ~ 0,
                             TRUE ~ diamsec_y),
           drug_y = case_when(is.na(drug_y) == T ~ 0,
                              TRUE ~ drug_y),
           e_parcomp = case_when( e_parcomp == -88 ~ NA_real_,
                                 e_parcomp == -77~ NA_real_,
                                 e_parcomp == -66~ NA_real_,
                                 TRUE ~  e_parcomp),
           e_parreg = case_when( e_parreg == -88 ~ NA_real_,
                                e_parreg == -77 ~ NA_real_,
                                e_parreg == -66 ~ NA_real_,
                                TRUE ~  e_parreg),
           e_polcomp = case_when( e_polcomp == -88 ~ NA_real_,
                                 e_polcomp == -77 ~ NA_real_,
                                 e_polcomp == -66 ~ NA_real_,
                                 TRUE ~  e_polcomp),
           e_xrcomp = case_when( e_xrcomp == -88 ~ NA_real_,
                                e_xrcomp == -77 ~ NA_real_,
                                e_xrcomp == -66 ~ NA_real_,
                                TRUE ~  e_xrcomp),
           e_xropen = case_when( e_xropen == -88 ~ NA_real_,
                                e_xropen == -77 ~ NA_real_,
                                e_xropen == -66 ~ NA_real_,
                                TRUE ~  e_xropen),
           e_xrreg = case_when( e_xrreg == -88 ~ NA_real_,
                               e_xrreg == -77 ~ NA_real_,
                               e_xrreg == -66 ~ NA_real_,
                               TRUE ~  e_xrreg))


names(data)
dim(data)
summary(data)
data  <- na.omit(data)
dim(data)

summary(data)
scaled <-  as.tibble(scale(select(data, c( mountains_mean, forest_gc, bdist1,  capdist, prec_gpcc, temp, droughtend_spi,  urban_gc, nlights_calib_mean, agri_gc,  imr_mean, excluded, v2x_polyarchy, v2x_jucon, v2xel_locelec, v2xel_regelec, v2svstterr, e_polity2, e_parcomp, e_parreg, e_polcomp, e_xrcomp, e_xropen, e_xrreg, log_pop, gcp_ppp))))

scaled_data  <- bind_cols(select(data, - c( mountains_mean, forest_gc, bdist1,  capdist, prec_gpcc, temp, droughtend_spi,  urban_gc, nlights_calib_mean, agri_gc,  imr_mean, excluded, v2x_polyarchy, v2x_jucon, v2xel_locelec, v2xel_regelec, v2svstterr, e_polity2, e_parcomp, e_parreg, e_polcomp, e_xrcomp, e_xropen, e_xrreg, log_pop, gcp_ppp)), scaled)

### all models
### log_pop, gcp_ppp

### geographic
###  mountain_mean, forest_gc, bdist1, capdist, prec_gpcc, temp,droughtend_spi,

### resources
### petroleum_y, diamprim_y, diamsec_y, gem_y, drug_y

### socio- economic
### urban_gc, nlights_calib_mean. agri_ih, imr_mean, cmr_mean, e_migdppc, e_migdpgro, e_wbgi_rle,

###political
### excluded, v2x_polyarchy, v2x_jucon, v2xel_locelec, v2xel_regelec, v2svstterr,  e_polity2, e_parcomp, e_parreg, e_polcomp, e_xrcomp, e_xropen, e_xrreg,


###first model, geographic factors
prior = c(set_prior("student_t(5,0,3)", class = "b"))

tic()
model_geo <- brm(formula = ucdp_event ~ petroleum_y + diamprim_y + diamsec_y + gem_y + drug_y + petroleum_s + diamprim_s + diamsec_s + gem_s + log_pop + gcp_ppp + (1 |gwno + year), data = scaled_data, family = bernoulli("logit"), warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, prior = prior, control = list(adapt_delta = 0.99))
toc()
save(model_geo,  file ="~/Dropbox/BayesChapter/Model_Results/model_geo.rda")

#### resource model
tic()
model_res <- brm(formula = ucdp_event ~mountains_mean + forest_gc + bdist1 + capdist + prec_gpcc + temp + droughtend_spi + log_pop + gcp_ppp  + (1 |gwno + year), data = scaled_data, family = bernoulli("logit"), warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, prior = prior, adapt_delta = 0.99)
toc()
save(model_res,  file ="~/Dropbox/BayesChapter/Model_Results/model_res.rda")

#### socio-econ w. pol model
tic()
model_soc <- brm(formula = ucdp_event ~ urban_gc + nlights_calib_mean + agri_ih + imr_mean + v2svstterr + e_polity2 + log_pop + gcp_ppp + (1 |gwno + year), data = scaled_data, family = bernoulli("logit"), warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, prior = prior, adapt_delta = 0.99)
toc()
save(model_soc,  file ="~/Dropbox/BayesChapter/Model_Results/model_soc.rda")


#### socio-econ w. pol model
tic()
model_pol <- brm(formula = ucdp_event ~ excluded + v2x_polyarchy + v2x_jucon + v2xel_locelec + v2xel_regelec + v2svstterr + e_polity2 + e_parcomp + e_parreg + e_polcomp + e_xrcomp + e_xropen + e_xrreg + log_pop + gcp_ppp + (1 |gwno + year), data = scaled_data, family = bernoulli("logit"), warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, prior = prior, adapt_delta = 0.99)
toc()
save(model_pol,  file ="~/Dropbox/BayesChapter/Model_Results/model_pol.rda")

