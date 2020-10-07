
# Load in modules
library(brms)
library(loo)
library(bayesplot)

data("stackloss")

# Fit all variables in stackloss

fit <- brm(stack.loss ~ ., data=stackloss,family=gaussian())

summary(fit)

loo <- loo(fit, save_psis = TRUE)

plot(loo)

# Fit only an intercept to compare

fit1 <- brm(stack.loss ~ 1, data=stackloss,family=gaussian())

loo1 <- loo(fit1,savepsis=TRUE)

plot(loo1)

# Do the comparison

loo_compare(loo,loo1)

# How does a regular lm do?

plot(lm(stack.loss ~ .,data=stackloss))

# Bayesplot diagnostics

yrep <- posterior_predict(fit)

ppc_ecdf_overlay(y=stackloss$stack.loss, yrep=yrep)

yrep1 <- posterior_predict(fit1)

ppc_ecdf_overlay(y=stackloss$stack.loss,yrep=yrep1)

ppc_dens_overlay(y=stackloss$stack.loss, yrep=yrep)

ppc_stat(y=stackloss$stack.loss,stat=sd,yrep=yrep)
fir <- brm(stack.loss ~ ., data=stackloss,family=gaussian())

summary(fir)


