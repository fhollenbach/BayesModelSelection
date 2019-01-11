

library(brms)
library(tidyverse)
library(tictoc)
library(rethinking)
library(haven)
library(sjstats)
library(tidybayes)
library(bayesplot)
data <- read_dta("~/Dropbox/BayesChapter/Data/PartiesPivotsPolicies/PartiesPivotsandPolicyReplicationDataV2.dta")

summary(data)

data  <- data %>% filter(laggedsq < 1 & laggedsq > -1)
data  <- data %>% group_by(issue) %>% mutate(L_sqtransform = lag(sqtransform, order_by = congress))
data <- data %>% select(c(sqtransform,L_sqtransform, transfloormedianslocationpredict, transfpivotalgridlocklocationpre, transfpartycartelopenrule, transfpartycartelclosedrulepred, transinflxmedgrid, transinflxpartunitblock, congress, issue))
data <- na.omit(data)

mix <- mixture(gaussian, gaussian, gaussian, gaussian, order = "none")
fit2 <- brm(bf(sqtransform ~ -1, mu1 ~  - 1 + transfloormedianslocationpredict + transinflxmedgrid, mu2 ~ - 1 + transfpivotalgridlocklocationpre + transinflxmedgrid, mu3 ~ - 1 + transfpartycartelopenrule + transinflxpartunitblock, mu4 ~  - 1 + transfpartycartelclosedrulepred + transinflxpartunitblock), data = data, inits = 0, family = mix, cores =4, save_all_pars =T, control = list(adapt_delta = 0.99, max_treedepth = 15), seed = 12345, chains = 4, iter = 5000) 



