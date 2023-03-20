library(rethinking)

##### EASY

#2E1
# (2) and (4) correspond to the statement

#2E2
# Only (3) corresponds to the expression

#2E3
# (1) and (3) correspond to the expression

#2E4
# 


##### MEDIUM

#2M1
# Define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)

#Define prior
prior <- rep(1, 20)

#Compute likelihood at each value in grid

# (1) W, W, W
 # p^3
 likelihood <- dbinom(3, size = 3, p = p_grid)
# (2) W, W, W, L
 # p^3 * (1-p)
 likelihood <- dbinom(3, size = 4, p = p_grid)

# (3) L, W, W, L, W, W, W
# p^5(1-p)^2
likelihood <- dbinom(5, size = 7, p = p_grid)

#Compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
#Display the posterior distribution
plot(p_grid, posterior, type = "b",
    x_lab = "probability of water", ylab = "posterior probability")
mtext("20 points")


#2M2
#Define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)
#Define prior
prior <- rep(0, 20)
prior[11:20] <- 1
#Now just execute lines 31-47

#2M3
# P(Earth) = P(Mars) = 0.5
# P(Land | Earth) = 0.3
# P(Land | Mars) = 1.0

# P(Earth | Land ) = P (Earth, Land) / P( Land)
#                  = P (Land | Earth) * P (Earth) / P(Land)
#                  = P (Land | Earth) * P (Earth) / (P(Land | Earth) * P(Earth) + P(Land | Mars) * P(Mars))
#                  =  0.3 * 0.5 / (0.3*0.5 + 1 * 0.5)
#                  \approx 0.23

#2M4
# Card 1: BB
# Card 2: BW
# Card 3: WW
# Way cards are in the bag: BB BW WW
# P(First side = black) = 2/3
# P(Second side = black) = 1/2
# P(Second side = black | First side = black) = 1/2 * 2/3 = 1/3
# Way cards are in the bag: BB WB WW
# P(First side = black) = 1/3
# P(Second side = black) = 1
# P(Second side = black | First side = black) = 1/3 * 1
# Need to add these up
# P(Second side = black | First side = Black) = 2/3


#2M5
# The same exercise as #2M4, we only need to consider instead:
# Way cards are in bag: BB BB BW WW
# Way cards are in bag: BB BB WB WW
# Hence, I will not work through this exercise in detail


#2M6
# 1 way to get the BB card
# 2 ways to get the BW card
# 3 ways to get the WW card

# 2 * 1 ways to get the BB card and black in first pull
# 1 * 2 ways to get the BW card and black in the first pull
# 0 * 3 ways to get the WW card and black in the first pull

# 4 ways to obtain a black side in the first pull
# For the second pull to also be black, we must've pulled the BB card
# BB card stands for 2 of these 4 ways
# I.e. p = 2/4 = 1/2.

#2M7
# BB WB WW
# We draw B W
# 2 ways , 1 way -> 2 ways

# BB WW WB
# We draw B W
# 2 ways , 2 ways  -> 4 ways

# BW WW BB
# We draw B W
# 1 way, 2 ways -> 2 ways

# Togehter -> 6 ways possible, 8 ways altogether to produce sequence
# p = 6/8 = 0.75


##### HARD

#2H1
# P(Species A) = P(Species B) = 1/2
# P(twins | Species A) = 0.1
# P(twins | Species B) = 0.2

# We are looking for the following probability P(twins born, t = 2 | twins born, t = 1)
#                                              = P(twins born, t = 2, twins born, t = 1) / P(twins born, t = 1)
# P(twins born, t = 1) = P(twins born | species A)*P(Species A) + P(twins born | species B)*P(Species B)
#                      = 0.1 * 1/2 + 0.2 * 1/2 = 0.15

# P(twins born, t = 2, twins born, t = 1) = P(twins born | A) * P(twins born | A) * P(A) + P(twins born | B) * P(twins born | B) * P(B)
#                                         = 0.1*0.1*1/2 + 0.2*0.2*1/2 = 0.005 + 0.04 = 0.025
 
# P(twins born, t = 2 | twins born, t = 1) = 0.025 / 0.15


#2H2
# P(A | twins, t = 1) = P(A, twins) / P(twins, t = 1)
#                     = 0.1 * 1/2 / 0.15 = 1/3

#2H3
# P(A | single, t = 2 , twins, t = 1) = 0.045 / 0.125
# P(single, twins) = 1/2 * 0.1 * 0.9 + 1/2 * 0.2 * 0.8 = 0.125
# P(A, single, twins) = 1/2 0.1 * 0.9 = 0.045

#2H4
# P(Test spec A | A) = 0.8 = P(Test A, A) / Spec(A)
# P(Test spec B | B) = 0.65 = P(Test B, B) / Spec(B)
# P(Test spec A | B) = 0.35

#Ignoring birth data
# P(A | test A) = P(A, test A) / P(test A)
#               = 0.8 * spec(A) / ((0.8) * spec(A) + 0.35*spec(A))
#               = 0.8 * 0.5 / (0.8 * 0.5 + 0.35 * 0.5)

#Taking birth data into account
# P(A) = 0.045 / 0.125
# Replace 0.5 in the formula above by the result of line 161