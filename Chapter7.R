library(rethinking)
library(tidyverse)

##### EASY

# 7E1
#Three motivating criteria that define information entropy :
# 1) Measuring uncertainty should be defined in a continuous manner 
# -> I.e. a small changes in probabilities results in a small change of measure
# 2) No. of possible events and uncertainty should be proportional to one another 
# -> I.e. the more possible options there are, the more uncertain we are
# 3) Did not remember the last motivation, so looked it up -> measure of uncertainty should be additive

# 7E2
#Entropy of a coin that is weighted such that when its tossed and lands comes up heads 70% of the time
# H(p) = - E[log(p)] = -\sum_{i=1}^n p_i log(p_i)
#In this example
# H([0.7,0.3]) = -0.7 * log(0.7) - 0.3 * log(0.3)
p <- c(0.7, 0.3)
-sum(p*log(p)) # entropy: 0.6108643

# 7E3
#Entropy of the die: 1 -> 20%, 2 -> 25%, 3 -> 35%, 4 -> 30%
p <- c(0.2, 0.25, 0.25, 0.3)
-sum(p*log(p)) # entropy: 1.376227

# 7E4
#Entropy of a die: 1 -> 1/3, 2 -> 1/3, 3 -> 1/3, 4 -> 0
p <- c((1/3), (1/3), (1/3), 0)
-sum(p * log(p)) #Not sure how to deal with this? At least, it cannot be easily plugged into the formula as log(0) is undefined?
#Assumption:
-3*(1/3*log(1/3)) # entropy : 1.098612


# 7M1
# Write down and compare the definitions of AIC and WAIC. Which of these criteria is most general? Which assumptions are required to transform the more general criterion into a less general one?

# 7M2
# Explain the difference between model selection and model comparison. What information is lost under model selection?

# 7M3
# When comparing models with an information criterion, why must all models be fit to exactly the same observations? What would happen to the information criterion values, if the models were fit to different numbers of observations? Perform some experiments, if you are not sure.

# 7M4
# Whathappenstotheeffectivenumberofparameters,asmeasuredbyPSISorWAIC,asaprior becomes more concentrated? Why? Perform some experiments, if you are not sure.

#7M5
# Provide an informal explanation of why informative priors reduce overfitting.

# 7M6
# Provide an informal explanation of why overly informative priors result in underfitting.