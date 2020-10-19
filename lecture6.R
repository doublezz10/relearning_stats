library(brms)
library(rstanarm)
data(radon)

formula = log_radon ~ log_uranium + (1 + floor||county)

fit <- brm(formula,radon,family="gaussian")

fit2 <- brm(formula,radon,family="gaussian",prior = c(prior(normal(0,1),class=sd)))

get_prior(formula,radon,family="gaussian",prior = c(prior(normal(0,1),class=sd)))
