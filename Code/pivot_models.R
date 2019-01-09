

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

m1  <- lm(sqtransform ~ transfloormedianslocationpredict + transinflxmedgrid + 0 , data = data)
summary(m1)

m2  <- lm(sqtransform ~ transfpivotalgridlocklocationpre + transinflxmedgrid + 0, data = data)
summary(m2)
m3  <- lm(sqtransform ~ transfpartycartelopenrule + transinflxpartunitblock + 0, data = data)
summary(m3)
m4  <- lm(sqtransform ~ transfpartycartelclosedrulepred + transinflxpartunitblock + 0, data = data)
summary(m4)

data <- data %>% select(c(sqtransform,L_sqtransform, transfloormedianslocationpredict, transfpivotalgridlocklocationpre, transfpartycartelopenrule, transfpartycartelclosedrulepred, transinflxmedgrid, transinflxpartunitblock, congress, issue))
data <- na.omit(data)

#### subset to those variables used later, so that we can omit all missing data
prior = c(set_prior("normal(0,5)", class = "b"))

model1 <- brm(formula = sqtransform ~ - 1 + transfloormedianslocationpredict + transinflxmedgrid, data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model1, file = "~/Dropbox/BayesChapter/Model_Results/model1.rda")


model2 <- brm(formula = sqtransform ~ - 1 + transfpivotalgridlocklocationpre + transinflxmedgrid, data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model2, file = "~/Dropbox/BayesChapter/Model_Results/model2.rda")

model3 <- brm(formula = sqtransform ~ - 1 + transfpartycartelopenrule + transinflxpartunitblock, data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model3, file = "~/Dropbox/BayesChapter/Model_Results/model3.rda")

model4 <- brm(formula = sqtransform ~ - 1 + transfpartycartelclosedrulepred + transinflxpartunitblock, data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model4, file = "~/Dropbox/BayesChapter/Model_Results/model4.rda")

model5 <- brm(formula = sqtransform ~ - 1 + L_sqtransform + transfloormedianslocationpredict +transfpivotalgridlocklocationpre + transfpartycartelopenrule +transfpartycartelclosedrulepred + transinflxmedgrid + transinflxpartunitblock, data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model5, file = "~/Dropbox/BayesChapter/Model_Results/model5.rda")

prior = c(set_prior("normal(0,5)", class = "b"))
model6 <- brm(formula = sqtransform ~ - 1 + L_sqtransform + transfloormedianslocationpredict +transfpivotalgridlocklocationpre + transfpartycartelopenrule +transfpartycartelclosedrulepred + transinflxmedgrid + transinflxpartunitblock + (1 | congress + issue), data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(model6, file = "~/Dropbox/BayesChapter/Model_Results/model6.rda")




#### mnodel 6 but with horseshoe

data <- read_dta("~/Dropbox/BayesChapter/Data/PartiesPivotsPolicies/PartiesPivotsandPolicyReplicationDataV2.dta")
summary(data)

data  <- data %>% filter(laggedsq < 1 & laggedsq > -1)
data  <- data %>% group_by(issue) %>% mutate(L_sqtransform = lag(sqtransform, order_by = congress))


data <- data %>% select(c(sqtransform,L_sqtransform, transfloormedianslocationpredict, transfpivotalgridlocklocationpre, transfpartycartelopenrule, transfpartycartelclosedrulepred, transinflxmedgrid, transinflxpartunitblock, congress, issue, presidentiallocation, housemedian, senatefilibusterpivot, senatemedian, housevetopivot, senatevetopivot, housemajoritymedian, senatemajoritymedian, houseminoritymedian, senateminoritymedian))

                                        #writeLines(readLines("~/Documents/GitHub/BayesModelSelection/Code/linear_regHorseshoe.stan"))
data <- data %>% ungroup()
data  <- na.omit(data)


scaled <- as.tibble(scale(select(data, -c( congress, issue))))
model.data.scaled <- bind_cols(select(data, c(congress, issue)), scaled)


X <- as.matrix(select(model.data.scaled,-c(sqtransform, congress, issue)))

ratio = 2 / (4)
prior  <- set_prior(horseshoe(df = 1, par_ratio = ratio))
horseshoe <- brm(formula = sqtransform ~ - 1 + transfloormedianslocationpredict +transfpivotalgridlocklocationpre + transfpartycartelopenrule +transfpartycartelclosedrulepred + transinflxmedgrid + transinflxpartunitblock, data =model.data.scaled, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(horseshoe, file = "~/Dropbox/BayesChapter/Model_Results/horseshoe.rda")

prior  <- set_prior(horseshoe(df = 1, par_ratio = ratio, scale_slab = 3, df_slab = 25))
fin_horseshoe <- brm(formula = sqtransform ~ - 1 + transfloormedianslocationpredict +transfpivotalgridlocklocationpre + transfpartycartelopenrule +transfpartycartelclosedrulepred + transinflxmedgrid + transinflxpartunitblock, data =model.data.scaled, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
save(fin_horseshoe, file = "~/Dropbox/BayesChapter/Model_Results/fin_horseshoe.rda")


color_scheme_set("red")
ppc_dens_overlay(y = c(model.data.scaled$sqtransform),
                 yrep = posterior_predict(fin_horseshoe, draws = 50))
ggsave("~/Dropbox/Apps/Overleaf/Bayesian model selection and averaging/fin_horseshoe_pdens.pdf", width = 10 * 1.618, height = 10)

posterior <- as.array(fin_horseshoe)[,,1:7]
vars <- c("Median only", "Pivotal politics", "Party cartel open rule", "Party cartel closed rule", "Inflation (median/pivot)", "Inflation (party)", "Sigma")
dimnames(posterior)[3]$parameters <- vars

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")
p <- mcmc_areas(posterior,
           pars = vars,
           prob = 0.95) + theme_minimal(base_size = 30)
plot(p)
ggsave("~/Dropbox/Apps/Overleaf/Bayesian model selection and averaging/horseshoe_result.pdf", width = 10 * 1.618, height = 10)


#p <-  mcmc_intervals(posterior, point_est = "median", prob = 0.9, prob_outer = 0.95, pars = vars) + theme_minimal(base_size = 26)
#plot(p)
#ggsave("~/Dropbox/Apps/Overleaf/Bayesian model selection and averaging/horse2.pdf", width = 10 * 1.618, height = 10)



forTable <- tidy_stan(fin_horseshoe, prob = c(0.9, 0.95))[1:7, ]
vars <- c( "Median only", "Pivotal politics", "Party cartel open rule", "Party cartel closed rule", "Inflation (median/pivot)", "Inflation (party)", "Sigma")
forTable <- forTable %>% select(c("term", "estimate", "std.error", "hdi.low_0.9", "hdi.high_0.9", "hdi.low_0.95", "hdi.high_0.95", "rhat"))
names(forTable) <- c("Variable", "Mean", "Std Error", "lower 90 HDI", "upper 90 HDI", "lower 95 HDI", "upper 90 HDI", "Rhat")
forTable$Variable <- vars
forTable <- as.matrix(forTable)
colnames(forTable) <- c(" ", " ", " ", "lower 90$\\%$", "upper  $90\\%$", "lower  $90\\%$", "upper  $90\\%$", " ")
forTable <- rbind(c(" ", "Mean", "Std Error", "HDI", "HDI", "HDI", "HDI", "Rhat"), forTable)
                                        #names(forTable)[1] <- ""

print(xtable(forTable, caption = "Model Results: Finnish Horseshoe Model", label = "tab:horseshoe", floating = TRUE, latex.environments = "center", align = "llccccccc", digits = 2), caption.placement = "top", booktabs = TRUE, include.rownames = FALSE,  sanitize.text.function = function(x) {x}, hline.after = c(-1, 1,nrow(forTable)), file = "~/Dropbox/Apps/Overleaf/Bayesian model selection and averaging/horseshoe_coefs.tex")

### model normal priors on scaled
prior = c(set_prior("normal(0,5)", class = "b"))
model_scaled <- brm(formula = sqtransform ~ - 1 + transfloormedianslocationpredict +transfpivotalgridlocklocationpre + transfpartycartelopenrule +transfpartycartelclosedrulepred + transinflxmedgrid + transinflxpartunitblock, data = data, family = gaussian, warmup = 2000, iter = 4000, chains = 4, cores = 4, save_all_pars =T, control = list(adapt_delta = 0.99), seed = 12345, prior = prior)
model_scaled
color_scheme_set("blue")
ppc_dens_overlay(y = c(model.data.scaled$sqtransform),
                 yrep = posterior_predict(model_scaled, draws = 500))
ggsave("~/Dropbox/Apps/Overleaf/Bayesian model selection and averaging/model_pdens.pdf", width = 10 * 1.618, height = 10)
