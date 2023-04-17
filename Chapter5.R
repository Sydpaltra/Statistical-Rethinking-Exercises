library(rethinking)
library(dagitty)

##### EASY

#5E1
#(2) and (4) are multiple linear regression models

#5E2
#animal_diversity_i ~ Normal(mu_i, sigma_i)
#mu_i = intercept + alpha_i * latitude_i + beta_i * plant_div_i
#We are controlling for "plant diversity" by including it as one of our predictors

#5E3
# time_phd_i ~ Normal(mu_i, sigma_i)
# mu_i = intercept + alpha_i * funding_i -> Bad model
# mu_i = intercept + alpha_i * labsize_i -> Bad model
# mu_i = intercept + alpha_i * funding_i + beta_i * labsize_i -> multiple regression model including both
# My assumption is that the coefficients of both are positive

#5E4
#The models (1), (3), (4) and (5) are inferentially equivalent.
# (1) -> (3) : (left hand side is always the notation of (1), right hand side of (3))
#               alpha = beta_c, beta_a = alpha, rest remain the same
# (3) -> (4) : alpha = alpha_A, alpha + beta_b = alpha_b, alpha + beta_c = alpha_c, alpha + beta_d = alpha_d
# (4) -> (5) : A_i = 1 - B_i - C_i - D_i
# (5) -> (1) : alpha_ = alpha + beta_a, alpha_b = alpha + beta_ b, alpha_c = alpha, alpha_d = alpha + beta_d,

##### MEDIUM

#5M1
#  Invent your own example of a spurious correlation. An outcome variable should be correlated
# with both predictor variables. But when both predictors are entered in the same model, the correlation
# between the outcome and one of the predictors should mostly vanish (or at least be greatly reduced)

# Want to predict how likely it is for students coming from different schools to succeed at university
# Outcome variable we consider: Goes successfuly to college
# Input variables: Rate of people who finish high school, Median age of starting college


#5M2
# Invent your own example of a masked relationship. An outcome variable should be correlated
# with both predictor variables, but in opposite directions. And the two predictor variables should be
# correlated with one another

# Influence of income and having COVID-19 on overall health
# Income is positively associated with overall health -> When you're having a higher-paying job, it's easier to take time off to go see a doctor or call in sick
# Having COVID-19 is negatively associated with overall health
# On the contrary, income and having covid-19 are (negatively correlated)
# Maybe, if this relationship is strong enough (which it prob isn't), then using income or having COVID-19 alone will not show any influence on overall health


#5M3
# It is sometimes observed that the best predictor of fire risk is the presence of firefighters—
# States and localities with many firefighters also have more fires. Presumably firefighters do not cause
# fires. Nevertheless, this is not a spurious correlation. Instead fires cause firefighters. Consider the
# same reversal of causal inference in the context of the divorce and marriage data. How might a high
# divorce rate cause a higher marriage rate? Can you think of a way to evaluate this relationship, using
# multiple regression?

# Similar to the fire example : States and locals with many divorces also have more marriages
# Reason? You're getting divorced to get married again!
# I.e. people have the opportunity to get married more than once
# In consequence, we'd need to introduce an additional variable telling us the rate of 2nd/3rd/4th marriage in state i (or simple an indicator for remarriage)


#5M4
# In the divorce data, States with high numbers of Mormons (members of The Church of Jesus
# Christ of Latter-day Saints, LDS) have much lower divorce rates than the regression models expected.
# Find a list of LDS population by State and use those numbers as a predictor variable, predicting divorce rate
# using marriage rate, median age at marriage, and percent LDS population (possibly standardized). 
# You may want to consider transformations of the raw percent LDS variable.

#5M5

##### HARD

#5H1
# In the divorce example, suppose the DAG is: M → A → D.
# What are the implied conditional independencies of the graph? Are the data consistent with it?

MAD_dag <- dagitty('dag{ M -> A -> D }')
impliedConditionalIndependencies(MAD_dag)
# Output: D _||_ M | A
# Meaning: D and M are independent after conditioning on M
# Recall model m5.3 -> This explores adding both marriage rate and age at marriage -> Model results let us infer that this is consitent with the data

#5H2
# Assuming that the DAG for the divorce example is indeed M → A → D,
# fit a new model and use it to estimate the counterfactual effect of halving a State’s marriage rate M. 
# Use the counterfactual example from the chapter (starting on page 140) as a template.

data(WaffleDivorce)
d <- list()
d$A <- standardize( WaffleDivorce$MedianAgeMarriage )
d$D <- standardize( WaffleDivorce$Divorce )
d$M <- standardize( WaffleDivorce$Marriage )

m5.3_A <- quap(
    alist(
      ## M -> A -> D #Tried to stick to the example on p 140, but I am not sure if this is correct?
        D ~ dnorm( mu , sigma ),
        mu <- a + bM*M + bA*A,
        a ~ dnorm(0, 0.2),
        bM ~ dnorm(0, 0.5),
        bA ~ dnorm(0, 0.5),
        sigma ~ dexp(1),
      ## M -> A
        A ~ dnorm(mu_A , sigma_A),
        mu_A <- aA + bAA*M,
        aA ~ dnorm(0, 0.2),
        bAA ~ dnorm(0, 0.5),
        sigma_A ~ dexp(1)
) , data = d )

M_seq <- seq(from = -2, to = 2, length.out = 30)

# prep data
sim_dat <- data.frame(M = M_seq)
# simulate A and then D, using A_seq
s <- sim(m5.3_A, data = sim_dat, vars = c("A", "D"))

plot(sim_dat$M, colMeans(s$A), ylim = c(-2, 2), type = "l",
    xlab = "manipulated M", ylab = "counterfactual A")
shade(apply(s$A, 2, PI), sim_dat$M)
mtext("Total counterfactual effect of M on A")

#Not really sure if this second part is correct?
plot(sim_dat$M, colMeans(s$D), ylim=c(-2, 2), type = "l",
    xlab = "manipulated M", ylab = "counterfactual D")
shade(apply(s$D, 2, PI), sim_dat$M)
mtext("Total counterfactual effect of M on A on D")

#5H3
# Return to the milk energy model,
#m5.7. Suppose that the true causal relationship among the variables is:
# M -> K <- N && M -> N
# Now compute the counterfactual effect on K of doubling M.
# You will need to account for both the direct and indirect paths of causation.
# Use the counterfactual example from the chapter (starting on page 140) as a template

data(milk)
milk <- milk %>% mutate(log_mass = log(mass))
d <- list()
# Recall K = standardized kilocalories; M = mass, N = neocortex.perc

d$K <- standardize(milk$kcal.per.g)
d$M <- standardize(milk$log_mass)
d$N <- standardize(milk$neocortex.perc)

m5H3_M <- quap(
    alist(
        ## M -> K <- N
        K ~ dnorm(mu, sigma),
        mu <- a + bM*M +bN*N,
        a ~ dnorm(0, 0.2),
        bM ~ dnorm(0, 0.5),
        bN ~ dnorm(0, 0.5),
        sigma ~ dexp(1),

        ## M -> N
        N ~ dnorm(mu_N, sigma_N),
        mu_N <- aN + + bAN*M,
        aN ~ dnorm(0, 0.2),
        bAN ~ dnorm(0, 0.5),
        sigma_N ~ dexp(1)
    ), data = d
)
 
#5H4
#Here is an open practice problem to engage your imagination.
# In the divorce date, States in the southern United States have many of the highest divorce rates. 
#Add the South indicator variable to the analysis.
#First, draw one or more DAGs that represent your ideas for how Southern American culture might influence any of the other three variables (D, M or A). 
#Then list the testable implications of your DAGs, if there are any, and fit one or more models to evaluate the implications. 
#What do you think the influence of “Southerness” is?



#THE FOLLOWING EXERCISES SOMEHOW GOT MIXED UP? THEY ARE NOT THE EXERCISES OF VERSION 2 OF THE BOOK?

#5H1
# Fit two bivariate Gaussian regressions, using map: (1) body weight as a linear function of territory size (area), 
# and (2) body weight as a linear function of groupsize. Plot the results of these
# regressions, displaying the MAP regression line and the 95% interval of the mean. Is either variable
# important for predicting fox body weight?

data(foxes)

#1) weight ~ Normal(mu_i, sigma)
# mu_i = alpha + beta_a * area
# alpha = Normal(0,100) # I have no clue what foxes weigh, so I am choosing a huge variance
# beta_a = Normal(0,10) # Not sure what unit the area is in?
# sigma = Uniform (0,50)

m5.H1a <- map(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + bA*area,
        a ~ dnorm(0, 100),
        bA ~ dnorm(0,10),
        sigma ~ dunif(0,50)
    ),
    data = foxes
)

#2) #1) weight ~ Normal(mu_i, sigma)
# mu_i = alpha + beta_s * groupsize
# alpha = Normal(0, 100)
# beta_s = Normal(0,10) #Assuming that group sizes aren't usually larger than 20? Again, issue that if we choose the normal distribution, group size could be negative
# sigma = Uniform (0,50)

m5.H1b <- map(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + bs*groupsize,
        a ~ dnorm(0, 100),
        bs ~ dnorm(0,10),
        sigma ~ dunif(0,50)
    ),
    data = foxes
)

#Look at the regresssion results
precis(m5.H1a)
precid(m5.H1b)

#Plotting the regression results
#First, let's use m5.H1a
pred.seq <- seq(from = 0, to = 6, by = 0.025)
mu <- link(m5.H1a, data = list(area = pred.seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
plot(weight ~ area, data = foxes, col = rangi2)
lines(pred.seq, mu.mean) #Map regression line
lines(pred.seq, mu.PI[1, ], lty = 2) #Lower bound of 95% interval of the mean
lines(pred.seq, mu.PI[2, ], lty = 2) #Upper bound of 95% interval of the mean

#Now, let's use m5.H1b
pred.seq <- seq(from = 1, to = 9, by = 0.5)
mu <- link(m5.H1b, data = list(groupsize = pred.seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
plot(weight ~ groupsize, data = foxes, col = rangi2)
lines(pred.seq, mu.mean) #Map regression line
lines(pred.seq, mu.PI[1, ], lty = 2) #Lower bound of 95% interval of the mean
lines(pred.seq, mu.PI[2, ], lty = 2) #Upper bound of 95% interval of the mean

#For m5.H1a -> See horizontal line -> no trend at all
#For m5.H1b -> While group size goes from 2 to 8 -> weight decreases by one -> Rahter small association?

#5H2
# Now fit a multiple linear regression with weight as the outcome and both area and groupsize
# as predictor variables. Plot the predictions of the model for each predictor, holding the other predictor
# constant at its mean. What does this model say about the importance of each variable? Why do you
# get different results than you got in the exercise just above?

m5.H1c <- map(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + ba * area + bs * groupsize,
        a ~ dnorm(0, 100),
        ba ~ dnorm(0, 10),
        bs ~ dnorm(0,10),
        sigma ~ dunif(0, 50)
    ),
    data = foxes
)

#Plot, holding area constant
pred.seq <- seq(from = 1, to = 9, by = 0.5)
mu <- link(m5.H1c, data = list(groupsize = pred.seq, area = mean(foxes$area)))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
plot(weight ~ groupsize, data = foxes, col = rangi2)
lines(pred.seq, mu.mean) #Map regression line
lines(pred.seq, mu.PI[1, ], lty = 2) #Lower bound of 95% interval of the mean
lines(pred.seq, mu.PI[2, ], lty = 2) #Upper bound of 95% interval of the mean

#Plot, holding groupsize constant
pred.seq <- seq(from = 0, to = 6, by = 0.025)
mu <- link(m5.H1c, data = list(area = pred.seq, groupsize = mean(foxes$groupsize)))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
plot(weight ~ area, data = foxes, col = rangi2)
lines(pred.seq, mu.mean) #Map regression line
lines(pred.seq, mu.PI[1, ], lty = 2) #Lower bound of 95% interval of the mean
lines(pred.seq, mu.PI[2, ], lty = 2) #Upper bound of 95% interval of the mean

#Now, for area we see a positive association (when holding group size constant)
#And for group size the association has become more negative

#5H3
# Finally, consider the avgfood variable. Fit two more multiple regressions: (1) body weight
# as an additive function of avgfood and groupsize, and (2) body weight as an additive function of
# all three variables, avgfood and groupsize and area. Compare the results of these models to the
# previous models you’ve fit, in the first two exercises. (a) Is avgfood or area a better predictor of body
# weight? If you had to choose one or the other to include in a model, which would it be? Support your
# assessment with any tables or plots you choose. (b) When both avgfood or area are in the same
# model, their effects are reduced (closer to zero) and their standard errors are larger than when they
# are included in separate models. Can you explain this result?

# Weight as an additive function of avgfood and groupsize
m5.H3d <- map(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + bs * groupsize + bavg * avgfood,
        a ~ dnorm(0, 100),
        bs ~ dnorm(0,10),
        bavg ~ dnorm(0,10),
        sigma ~ dunif(0, 50)
    ),
    data = foxes
)

# Looking at model results
precis(m5.H3d) # Here, the estimate vor bavg looks to be sufficiently far from zero? Just like the 89% interval?
               # But based on this alone: I am not sure if area or avgfood is a better predictor

#Similar to question m5.H2, let's hold groupsize constant to plot the regression results
pred.seq <- seq(from = 0, to = 2, by = 0.025)
mu <- link(m5.H3d, data = list(avgfood = pred.seq, groupsize = mean(foxes$groupsize)))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
plot(weight ~ avgfood, data = foxes, col = rangi2)
lines(pred.seq, mu.mean) #Map regression line
lines(pred.seq, mu.PI[1, ], lty = 2) #Lower bound of 95% interval of the mean
lines(pred.seq, mu.PI[2, ], lty = 2) #Upper bound of 95% interval of the mean
#One would now have to compare the plot from before with this one and compare the "relative slope"? I.e. what extreme values are reached using these predictors

# Weight as an additive function of avgfood, groupsize and area
m5.H3e <- map(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + ba * area + bs * groupsize + bavg * avgfood,
        a ~ dnorm(0, 100),
        ba ~ dnorm(0, 10),
        bs ~ dnorm(0,10),
        bavg ~ dnorm(0,10),
        sigma ~ dunif(0, 50)
    ),
    data = foxes
)

precis(m5.H3e)
# I would assume area and avgfood to be highly correlated
# A larger territory has more food?
# Also, we saw that avgfood is positve associated with weight,
# while area is negatively associated with weight
# Hence, including both potentially masks the influence of the other?