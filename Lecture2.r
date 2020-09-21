data(Howell1,package='rethinking')

library(brms)

# Fit a Bayesian model without setting priors (it'll do the median for us)

fit <- brm(height ~ 1, data=Howell1, family="normal")

summary(fit)

prior_summary(fit)

# Fit another model by setting priors

fit2 <- brm(height ~ 1, data=Howell1, family="normal",
           prior = c(prior(normal(160,20),class="Intercept"),
                     prior(normal(0,30),class="sigma")
                     ))
summary(fit2)

# Use a bad prior!

fit3 <- brm(height ~ 1, data=Howell1, family="normal",
            prior = c(prior(normal(3600,10),class="Intercept"),
                      prior(normal(0,10),class="sigma")
            ))
summary(fit3)

# Add a covariate (and its prior)

fit4 <- brm(height ~ weight, data=Howell1, family="normal",
            prior = c(prior(normal(160,20),class="Intercept"),
                      prior(normal(0,30),class="sigma"),
                      #prior(normal(0,30),coef="weight"), #prior only on weight
                      prior(normal(0,20),class="b") #prior on all regression weights
            ))
summary(fit4)
