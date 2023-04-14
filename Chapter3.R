library(rethinking)

##### EASY

#Easy questions use the samples from the posterior distribution for the globe tossing example.

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob=posterior, size = 1e4, replace=TRUE)


#3E1
#How much posterior probability lies below p = 0.2?
sum(samples < 0.2) / 1e4
# 4e-04

#3E2
#How much posterior probability lies above p = 0.8?
sum(samples > 0.8) / 1e4
# 0.1116

#3E3
#How much posterior lies between p=0.2 and p=0.8?
sum(0.2 < samples & samples < 0.8) / 1e4
# 0.888

#3E4
#20% of the posterior probability lies below which value of p?
quantile(samples, 0.2)
#     20% 
# 0.5185185 

#3E5
#20% of the posterior probability lies above which value of p?
quantile(samples, c(0, 0.8))
#     80% 
# 0.7557558

#3E6
#Which values of p contain the narrowest interval equal to 66% of the posterior probability?
HPDI(samples, prob = 0.66)
#     |0.66     0.66| 
#     0.5085085 0.7737738 

#3E7
#Which values of p contain 66% of the posterior probability, assuming equal posterior probability both below and above the interval?
PI(samples, prob = 0.66)
#      17%       83%
#      0.5025025 0.7697698


##### MEDIUM

#3M1
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

#3M2
set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
HPDI(samples, prob = 0.99)
#     |0.99     0.99| 
#     0.2382382 0.8008008 

#3M3
w <- rbinom(1e4, size = 15, prob = samples)
#Computing pribability
sum(w == 8) / 1e4 #0.1464
simplehist(w) #Plot confirms calculation above

#3M4
w <- rbinom(1e4, size = 9, prob = samples)
sum(w == 6) / 1e4 # 0.1736
simplehist(w) #Plot confirms calculation above

#3M5
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(0, 1000)
for (i in 1:length(p_grid)){
    if (p_grid[i] > 0.5) {
        prior[i] <- 1
    }
}
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

#Repeating 3M2
set.seed(100)
samples <- sample(p_grid, prob=posterior, size = 1e4, replace=TRUE)
HPDI(samples, prob = 0.99)
#  |0.99     0.99|
# 0.5005005 0.8048048 , interval = much narrower

#Repeating 3M3
w <- rbinom(1e4, size = 15, prob = samples)
#Computing pribability
sum(w == 8) / 1e4 #0.163
simplehist(w) #8 isn't in the center anymore

#Repeating 3M4
w <- rbinom(1e4, size = 9, prob = samples)
#Computing pribability
sum(w == 6) / 1e4 #0.2353
simplehist(w)

#3M6
N_toss <- 1e3 * 2.2 #Reached this conclusion by playing around with this value
likelihood <- dbinom(0.7*N_toss, size = N_toss, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
interval <- PI(samples, prob = 0.99)
diff(interval)


##### HARD
birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0,
0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,
1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0,
1,0,1,1,1,0,1,1,1,1)
birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,
1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,
0,0,0,1,1,1,0,0,0,0)

#3H1
#Using a grid approximation, compute the posterior distribution for the probability of a birth of being a boy. Assume a uniform probability.
#Which parameter value maximizes the posterior probability?
p_grid <- seq(from = 0, to = 1, length.out = 1000)
no_boys <- sum(birth1) + sum(birth2)
prior <- rep(1, no_boys) #uniform prior
likelihood <- dbinom(no_boys, size = 200, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
plot(posterior ~ p_grid, type = "l")
abline(v = 0.555) #played arround with the vertical line to find the maximum -> maximum = 0.555?


#3H2
#Using the sample function, draw 10,000 random parameter values from the posterior distribution you calculated above.
#Use these samples to estimate the 50%, 89% and 97% highest posterior density intervals.
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior) #Drawing 10,000 samples
HPDI(samples, prob = 0.5)
HPDI(samples, prob = 0.89)
HPDI(samples, prob = 0.97)

#3H3
# Use rbinom to simulate 10,000 replicates of 200 births.
#You should end up with 10,000 numbers, each one a count of boys out of 200 births. 
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior) #Drawing 10,000 samples
simulation200 <- rbinom(1e4, size = 200, prob = samples)
#Compare the distribution of predicted numbers of boys to the actual count in the data (111 boys out of 200 births). 
#There are many good ways to visualize the simulations, but the dens command (part of the rethinking package) is probably the easiest way in this case. 
#Does it look like the model fits the data well? That is, does the distribution of predictions include the actual observation as a central, likely outcome?
dens(simulation200) #Similar results? Quite interestingly for the density plot we see a dent in the peak?
hist(simulation200) # -> Model fits data well


#3H4
#Now compare 10,000 counts of boys from 100 simulated first borns only to the number of boys in the first births, birth1.
#How does the model look in this light?
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior) #Drawing 10,000 samples
simulation100 <- rbinom(1e4, size = 100, prob = samples)
dens(simulation100)
abline(v = 51) #Model does not fit the data as well as in exercise 3H3 -> Dues to the smaller no. of samples

#3H5
# The model assumes that sex of first and second births are independent.
# To check this assumption, focus now on second births that followed female first borns. 
# Compare 10,000 simulated counts of boys to only those second births that followed girls. 
# To do this correctly, you need to count the number of first borns who were girls and simulate that many births, 10,000 times. 
# Compare the counts of boys in your simulations to the actual observed count of boys following girls. 
# How does the model look in this light? Any guesses what is going on in these data?

# Filter birth2 for those i for which birth1[i] = 0 (i.e. firstborn girl)
birth2_reduced <- birth2[birth1 == 0]
simulation_secondboys <- rbinom(1e4, size = length(birth2_reduced), prob = samples)
dens(simulation_secondboys)
abline(v = sum(birth2_reduced)) #Does not work out well -> Sign that gender of first and second born are not independent
