U <-c(1:20) #vertices
D <- matrix(runif(n=20*20, min=0, max =1), ncol = 20) #uniformly distributed distance matrix
D[4,6] = 0
D[9,13]= 0
D[1,19]= 0 
D[10,17]= 0  
D[3,8] = 0 #NO TRAVEL NO DISTANCE
D[lower.tri(D)] = t(D)[lower.tri(D)] #symmetric matrix
diag(D) <- rep(0,20) #diagonals are 0

indicatorPermissible <- function(z){
  if(D[z[1],z[20]] == 0){
    return(0)
  }
  for(i in 1:19){
    if(D[z[i],z[i+1]] == 0)
      return(0)
  }
  return(1)
}

distance_travelled <- function(L){
  C <- unlist(L)
  f <- 0
  for(i in 1:19){
    f = f + D[C[i],C[i+1]]  
  }
  return(f<- f + D[C[20],C[1]])
}

tspMCMC <- function(n,m){
S <- list(c(1:20)) #preferably this sequence will be randomly generated and a proper circuit.
rejection_counter <- 0
i <- 1
j <- 1
travel_time <- rep(0, m+1)
while(i <= m+n){
  P <- sample(1:20,size=1) #basic sampling 
  Q <- sample(1:20,size=1) #likewise for proposed state, 
  index_p = match(P, unlist(S[i])) #the position of P in the sequence
  index_q = match(Q, unlist(S[i])) #likewise for the second element in the pair
  k <- unlist(S[i])
  z <- c(replace(k, c(index_p,index_q), c(Q,P)))
  if(indicatorPermissible(z) == 1){
    if(i >= n){
      travel_time[j] = distance_travelled(S[i])
      j <- j + 1
    }
    S[i+1] <- list(z) #adds the new sequence to S interchanging P and Q 
    i <- i + 1
  }
  else{
    rejection_counter = rejection_counter+1 #see how many time our proposed transition introduced forbidden travel
  }
}
   answer = mean(travel_time)
   print(length(travel_time))
   print(rejection_counter)
   return(answer)
}
start <- Sys.time()
tspMCMC(5,10)
tspMCMC(100,200)
tspMCMC(200,400)
tspMCMC(400,800)
tspMCMC(800,1600)
tspMCMC(1600,3200)
tspMCMC(3200,6400)
tspMCMC(20000,40000)
tspMCMC(80000,160000)
# tspMCMC(2000000,4000000)
Sys.time()-start