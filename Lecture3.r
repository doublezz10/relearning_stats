library(brms)
library(loo)
library(bayesplot)

data("stackloss")

fir <- brm(stack.loss ~ ., data=stackloss,family=gaussian())

summary(fir)