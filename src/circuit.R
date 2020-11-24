U <-c(1:20) #state space
n <- 100
D <- matrix(runif(n=20*20, min=0, max =1), ncol = 20) #uniformly distributed distance matrix
D[lower.tri(D)] = t(D)[lower.tri(D)] #symmetric matrix
diag(D) <- rep(0,20)


#restrictedPaths_list <- list(c(4,6), c(6,4), c(9,13), c(13,9), c(19,1), c(1,19), c(17,10), c(10,17), c(3,8), c(8,3)) #5 forbidden pairs 


S <- list(c(1:20)) #preferably this sequence will be randomly generated and a proper circuit.
for(i in 1:n){
  P <- sample(1:20,size=1, prob=D[x[i],])
  Q <- sample(1:20,size=1, prob=D[x[i],])
  index_p = match(P, unlist(S[i]))
  index_q = match(Q, unlist(S[i]))
  k <- unlist(S[i])
  S[i+1] <- list(c(replace(k, c(P,Q), k[c(Q,P)])))
}
