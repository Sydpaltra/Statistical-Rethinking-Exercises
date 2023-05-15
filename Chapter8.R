library(rethinking)
library(tidyverse)

##### EASY

#8E1
#(1) Bread dough rises because of yeast.
#(1b) Bread dough rises because of sugar.

#(2) Education leads to higher income.
#(2b) Area you're born into leads to higher income.

#(3) Gasoline makes a car go.
#(3b) The engine makes a car go.

#8E2
#Which of the following explanations invokes an interaction?
#(1) Caramalized onions require cooking over low heat and making sure the onions do not dry out. 
# -> Interaction between cooking over low heat + making sure they dont dry out
#(2) A car will go faster when it has more cylinders or when it has a better fuel injector. 
# -> Interaction in the way that A*B = 0, so only one of them occurs at the same time? -> Apparently, by this definition of interaction, this means there's no interaction
#(3) Most people acquire their political beliefs from their parents, unless they get them instead from their friends. 
# -> Interaction between sources of beliefs? Friends only come in, when there's NO input from parents -> Again, no interaction
#(4) Intelligent animal species tend to be either highly social or have manipulative appendages (hands, tentacles, etc) -> Once more, no interaction
# -> Interaction in the way that A*B = 0, see (2).

#8E3
#For each of the exaplanations in 8E2, write a linear model that expresses the stated relationship
#(1)
#C_i \tilde Normal(mu_i, sigma_i)
#mu_i = alpha + beta * lowHeat_i + gamma * notDry_i + delta lowHeat_i * notDry_i
#(2)
#S_i \tilde Normal(mu_i, sigma_i)
#mu_i = alpha + beta * C_i + gamma FI_i
#(3) PB_i \tilde Normal(mu_i, sigma_i)
#mu_i = alpha + beta * P_i + gamma * F_i
#(4) IA_i \tilde Normal(mu_i, sigma_i)
#mu_i = alpha + beta * S_i + gamma * MA_i

##### MEDIUM
#8M1


