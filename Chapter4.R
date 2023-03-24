library(rethinking)
library(tidyverse)
library(splines)

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

#Need to define the average weight x-bar
xbar <- mean(d2$weight)

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

#Inspecting the marginal posterior distribution of the parameters
precis(m4.3) #Here, when x_i = x_bar, then mu_i = a_i -> I.e. the intercept is height when a person is of average weight

#Variance-covariance matrix for m4.3
round(vcov(m4.3), 3) # Very little to no covariation among the parameters


#Refitting model m4.3, but omitting the mean weight xbar
m4.3b <- quap(
    alist(
        height ~ dnorm(mu, sigma),
        mu <- a + b * (weight),
        a ~ dnorm(178, 20),
        b ~ dlnorm(0, 1),
        sigma ~ dunif(0, 50)
    ) ,
    data = d2
)

#Again, inspecting the marginal posterior distribution of the parameters
precis(m4.3b) #In comparison to m4.3 the sd for a has drastically increased
#Additionally the mean for intercept a has dropped, while for b and sigma the outputs are very similar to m4.3
# Here, mu_i = a_i, when x_i = 0  -> The intercept is the height of a person of weight 0 -> Here, we run into some issues as a person, who does not weigh anything can't be 114.53cm tall

round(vcov(m4.3b), 3) #Covariation has increased -> between a and b we now see a negative covariation of -0.078 (still not a lot)
                      #And between a and sigma we see a pos covariation of 0.009

#4M8 'NOT DONE YET'
data(cherry_blossoms)
d <- cherry_blossoms
d2 <- d[complete.cases(d$doy), ] #Only considering complete cases on doy
num_knots <- 15 #Here is where we change the number of knots
knot_list <- quantile(d2$year, probs = seq(0, 1, length.out = num_knots))

library(splines)
B <- bs(d2$year,
    knots = knot_list[-c(1, num_knots)],
    degree = 3, intercept = TRUE)

plot(NULL, xlim=range(d2$year), ylim = c(0,1), xlab = "year", ylab = "basis")
for (i in 1:ncol(B)){ 
    lines(d2$year, B[, i])
}

#Adjusting the number of knots : When we increase the number of knots, then the periods of our wave-like-behavior become shorter and shorter
#Adjusting the width of the prior on the weights :
m4.7 <- quap(
            alist(
            T ~ dnorm(mu, sigma),
            mu <- a + B %*% w,
            a ~ dnorm(6,10),
            w ~ dnorm(0,1),
            sigma ~ dexp(1)
            ),
            data=list(T=d2$temp, B=B),
            start=list(w=rep(0, ncol(B))))

post <- extract.samples(m4.7)
w <- apply( post$w , 2 , mean )
plot( NULL , xlim=range(d2$year) , ylim=c(-2,2) ,
xlab="year" , ylab="basis * weight" )
for ( i in 1:ncol(B) ) lines( d2$year , w[i]*B[,i] )

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
d <- d[complete.cases(d), ]

#Let's start with a LINEAR MODEL, doy ~ Normal(mu, sigma), sigma = a + beta * temp
m.lin <- quap(alist(
    doy ~ dnorm(mu, sigma),
    mu <- a + b * temp,
    a ~ dnorm (100, 100),
    b ~ dnorm (0, 10),
    sigma ~ dunif (0, 50)
) ,
data = d)
#Let's sample from the posterior
post <- extract.samples(m.lin, n = 20)

#Display of raw data
plot(d$temp, d$doy)
#plot the lines using the posterior
for(i in 1 : 200) {
    curve(post$a[i]+post$b[i]*x,
    col = col.alpha("black", 0.3), add = TRUE)
}

#Moving on to a QUADRATIC MODEL
# doy ~ Normal(mu, sigma), sigma = a + beta * temp + c * temp^2
d <- d %>% mutate(tempSquared = temp * temp)
m.quad <- quap(alist(
    doy ~ dnorm(mu, sigma),
    mu <- a + b * temp + c  * tempSquared,
    a ~ dnorm (100, 100),
    b ~ dnorm (0, 10),
    c ~ dnorm (0, 10),
    sigma ~ dunif (0, 50)
) ,
data = d)
precis(m.quad) #The mean for the quadratic term is 0 -> No need to include this term?

#Plotting quadratic model (following the example on p 112)
temp.seq <- seq(from = 0, to = 200, length.out = 100)
pred_data <- list(temp = temp.seq, tempSquared = temp.seq^2)
mu <- link(m.quad, data = pred_data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)
sim.doy <- sim(m.quad, data = pred_data)
doy.PI <- apply(sim.doy, 2, PI, prob = 0.89)

plot(doy ~ temp, d, col = col.alpha(rangi2, 0.5))
lines(temp.seq, mu.mean)
shade(mu.PI, temp.seq)
shade(mu.PI, temp.seq) #Looks like a linear regression line -> Confirms what we've inferred from precis a couple of lines prior

#Moving on to the usage of SPLINES
# In the book they use year to predict doy
# follow the example on p 117
num_knots <- 15
knot_list <- quantile(d$temp, prob = seq(0, 1, length.out = num_knots))

B <- bs(d$temp,
        knots = knot_list[-c(1,num_knots)],
        degree = 3, intercept = TRUE)

plot(NULL, xlim = range(d$temp), ylim = c(0, 1), xlab = "temp", ylab = "basis")
for (i in 1 : ncol(B)){
    lines(d$temp, B[, i])
}

#4H6

#4H7
#NO 4H7 in the PDF?

#4H8