library(rethinking)

##### EASY

#4E1
#The 1st line (y_i \tilde Normal (\mu, \sigma)) defines the likelihood

#4E2
#Two parameters are in the posterior distribution

#4E3


#4E4
#The second linr \mu_i = \alpha + \beta x_i is the linear model

#4E5
#In the model above, 3 parameters are in the posterior distribution


##### MEDIUM

#4M1
sample_mu <- rnorm(1000, 0, 10)
sample_sigma <- rexp(1000, 1)
prior_y <- rnorm(1000, sample_mu, sample_sigma)
dens(prior_y)

#4M2
flist <- alist(
    reponse ~ dnorm(mu, sigma),
    mu ~ dnorm(0, 10),
    sigma ~ dexp(1)
)

#4M3
# y ~ Normal(\mu, \sigma)
# \mu = a + b*x
# a ~ Normal(0,10)
# b ~ Uniform(0,1)
# \sigma ~ Exp(1)

#4M4
# Assuming that height is measured in cm
# y ~ Normal(mu, sigma)    # y = height
# \mu = a + b*x            # x = year
# a ~ Normal(0,100)
# b ~ Normal(0,10)         # Seems unlikely, that students grew more than 20cm/year
                           # Should go for sth that only produces non-negative values? Maybe lognormal
# sigma ~ Uniform(0,50)    # Making every variation equally likely as we have no additional information

#4M5
# I already mentioned this above: Prob use sth like a log-likelihood distribution for b

#4M6
# Replace sigma ~ Uniform(0,50) with sigma ~ Uniform(0, 64)

#4M7 'NOT DONE YET'
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ] #Only considering adults

m4.3 <- quap(
    alist(
        height ~ dnorm(mu, sigma),
        mu <- a + b * (weight - xbar),
        a ~ dnorm(178, 20),
        b ~ dlnorm(0, 1),
        sigma ~ dunif(0, 50)
    ) ,
    data = d2
)

#4M8 'NOT DONE YET'
data(cherry_blossoms)
d <- cherry_blossoms
d2 <- d[complete.cases(d$doy), ] #Only considering complete cases on doy
num_knots <- 20
knot_list <- quantile(d2$year, probs = seq(0, 1, length.out = num_knots))

library(splines)
B <- bs(d2$year,
    knots = knot_list[-c(1, num_knots)],
    degree = 3, intercept = TRUE)

plot(NULL, xlim=range(d2$year), ylim = c(0,1), xlab = "year", ylab = "basis")
for (i in 1:ncol(B)) lines(d2$year, B[, i])


###### HARD

#4H1
#First: Need to fit the model
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ] #Only considering adults. See Ch. 4 for reasoning

m <- map(alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * weight,
    a ~ dnorm (100, 100),
    b ~ dnorm (0, 10),
    sigma ~ dunif (0, 50)
) ,
data = d2)

#Now, let's sample from the posterior
post <- extract.samples(m)

sample_1 <- rnorm(10000, post$a + post$b*46.95, post$sigma)

#4H2
data(Howell1)
d <- Howell1
d2 <- d[d$age < 18, ]
#a)
# y ~ Normal(mu, sigma), where y is the height
#\mu = a + b * x #where x is the weight
# a ~  Normal(0, 10) #When x = 0, we assume the child to be non-existent (i.e. 0 cm tall)
# b ~ Uniform(0,10)
# sigma ~ Uniform(0, 50) #Not sure, what to choose here

m4.h2 <- quap(
    alist(y ~ dnorm(mu,sigma),
          mu <- a + b * x,
          a ~ dnorm(0,10),
          b ~ dunif(0, 10),
          sigma ~ dunif(0,10)),
    data = d2
)
#b)
plot(d2$weight, d2$height)

#c)

#4H3
data(Howell1)
d <- Howell1

#a)
m4.h3 <- quap(
    alist(y ~ dnorm(mu,sigma),
          mu <- a + b * log(x),
          a ~ dnorm(0,10),
          b ~ dnorm(0, 10),
          sigma ~ dunif(0,10)),
    data = d
)

#b)
plot(height ~ weight, data = Howell1)

#4H4

#4H5
data(cherry_blossoms)
d <- cherry_blossoms

#4H6

#4H7

#4H8
