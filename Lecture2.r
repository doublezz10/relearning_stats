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

# Plot the bad prior and the posterior

plot(seq(0, 300, length.out=1000), 
     dnorm(seq(0, 300, length.out=1000), mean=0, sd=10),
     add = TRUE, type='l', xlim=c(0, 900))
hist(extract_draws(fit3)$dpars$sigma, breaks=50,prob=TRUE,add=TRUE)

# What should this look like?

plot(seq(0, 300, length.out=1000), 
     dnorm(seq(0, 300, length.out=1000), mean=0, sd=30),
     add = TRUE, type='l', xlim=c(0, 90))
hist(extract_draws(fit2)$dpars$sigma, breaks=50,prob=TRUE,add=TRUE)

# Add a covariate (and its prior)

fit4 <- brm(height ~ weight, data=Howell1, family="normal",
            prior = c(prior(normal(160,20),class="Intercept"),
                      prior(normal(0,30),class="sigma"),
                      #prior(normal(0,30),coef="weight"), #prior only on weight
                      prior(normal(0,20),class="b") #prior on all regression weights
            ))
summary(fit4)
