library(brms)
library(ggplot2)

## simulate data with n of 10, 100, 1000

b1 <- 0.2
b2 <- 0.8

# n of 10

n = 10

x10 <- rnorm(n,0,1)

y10 <- 0

for (i in 1:n) {
  y10[i] <- b1 * x10[i] + b2 * x10[i]
}

df10 = data.frame(x10,y10)

# n of 100  

n = 100

x100 <- rnorm(n,0,1)

y100 <- 0

for (i in 1:n) {
  y100[i] <- b1 * x100[i] + b2 * x100[i]
}

df100 = data.frame(x100,y100)

# n of 1000

n = 1000

x1000 <- rnorm(n,0,1)

y1000 <- 0

for (i in 1:n) {
  y1000[i] <- b1 * x1000[i] + b2 * x1000[i]
}

df1000 = data.frame(x1000,y1000)

## Plot simulated data

ggplot(df10,aes(x=x10,y=y10)) + geom_point()

ggplot(df100,aes(x=x100,y=y100)) + geom_point()

ggplot(df1000,aes(x=x1000,y=y1000)) + geom_point()

## Fit using brms without setting any priors

fit10 <- brm(y10 ~ 1+ x10 + x10, data = df10, family="Normal")

fit100 <- brm(y100 ~ x100 + x100, data = df100, family = "Normal")

fit1000 <- brm(y1000 ~ x1000 + x1000, data=df1000,family="Normal")

summary(fit10)
summary(fit100)
summary(fit1000)

## Fit using N(0,1) as prior for both params - this isn't working; is formula wrong?

fit10_prior <- brm(y10 ~ 1, data = df10, family="Normal",
                   prior=c(prior(normal(0,1),class="b")))

fit100_prior <- brm(y100 ~ 1, data = df100, family = "Normal",
                    prior=c(prior(normal(0,1),class="b")))

fit1000_prior <- brm(y1000 ~ 1, data=df1000,family="Normal",
                     prior=c(prior(normal(0,1),class="b")))

summary(fit10_prior)
summary(fit100_prior)
summary(fit1000_prior)
