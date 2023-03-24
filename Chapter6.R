library(rethinking)

##### EASY

#6E1
#1) Including post-treatment variables
#2) Inclusion of a pipe?
#3) Inclusion of a collider? (Large negative and positive effects might cancel each other out?)

#6E2
#1) Keep the fungus example from the chapter in mind

#6E3
#1) Pipe X -> Z -> Y
#2) Collider X -> Z <- Y
#3) Descendant X-> Z <- Y
#                  |(arrow from Z to Y)
#                  D
# Don't remember the 4th one -> Looked it up
# Fork X <- Z -> Y (Note to myself to remember this: "Overturned Collider")

#6E4
#TODO

##### MEDIUM

#6M1
#In addition to the paths mentioned on p. 186, we obtain two more paths:
# X <- U -> B <- C <- V -> Y #Similar argument to the book? Contains a collider, hence is already closed?
# X <- U <- A -> C <- V -> Y #Open backdoor path -> Conditioning necessary? Condition again on A or C as both U and V are unobserved.

#6M2
#TODO

#6M3
#See notes in pdf for how I've classified the different paths
#Top-Left : No conditioning necessary, as both paths contain a collider
#Bottom-Left : Condition on A? Nothing is necessary for the path X -> Z <- Y
#Top-Right : Condition on Z? -> The path X -> Z -> Y is a pipe, i.e. open (as it does not contain a collider) -> This should be closed via conditioning, but I am not sure if this would hurt the other path?
#Bottom-Right: Again, condition solely on Z as both pipes go through Z

##### HARD

#6H1
data(WaffleDivorce)
waffle_data <- WaffleDivorce

#6H2

#6H3

#6H4

#6H5

#6H6

#6H7