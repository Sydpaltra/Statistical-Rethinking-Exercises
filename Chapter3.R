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

#3M2

#3M3

#3M4

#3M5

#3M6