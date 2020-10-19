library(rethinking)
library(brms)

data("WaffleDivorce")

d = WaffleDivorce

model = brm(data=d,family=gaussian, 
    Divorce ~ 1 + Marriage + MedianAgeMarriage,
    prior = c(prior(normal(10,10),class=Intercept),
              prior(normal(0,1),class=b),
              prior(uniform(0,10),class=sigma)),
    iter = 2000, warmup = 500, 
    chains = 4, cores = 4, seed = 5)

summary(model)

plot(model)

# Marriage seems to have little effect on Divorce
