U <-c(1:20) #vertices
D <- matrix(runif(n=20*20, min=0, max =1), ncol = 20) #non-stochastic uniformly distributed distance matrix 
D[4,6] = 0
D[9,13]= 0
D[1,19]= 0 
D[10,17]= 0  
D[3,8] = 0 #NO TRAVEL NO DISTANCE, REMEMBER FORBIDDEN PAIRS MUST FOLLOW D[i,j]=0, i<j
D[lower.tri(D)] = t(D)[lower.tri(D)] #symmetric matrix
diag(D) <- rep(0,20) #diagonals are 0

indicatorPermissible <- function(z){
  if(D[z[1],z[20]] == 0){ #checks the endpoints of the sequence
    return(0)
  }
  for(i in 1:19){ #looking at the vertex at index i, consults D to make sure the edge connecting to the vertex at index i+1 is non-zero
    if(D[z[i],z[i+1]] == 0)
      return(0)
  }
  return(1)
}

distance_travelled <- function(L){
  C <- unlist(L)
  f <- D[C[20],C[1]] #endpoints require special care
  for(i in 1:19){
    f = f + D[C[i],C[i+1]]  #similar to indicatorPermissible, except it tallys the edges
  }
  return(f)
}

tspMCMC <- function(n,m){
v <- sample(U,size = 20, replace=FALSE) #without repetition sample first sequence
while(indicatorPermissible(v)==0){#make sure its valid
  v <- sample(U,size=20, replace=FALSE) 
}
S <- list(v) #record it in the state space
rejection_counter <- 0
trials <- 1
i <- 1
j <- 1
repetition_counter <- 0
travel_time <- rep(0, m+1) #init. the travel time vector
while(i <= m+n){
  P <- sample(U,size=1) #basic sampling 
  Q <- sample(U,size=1) #likewise for proposed state, 
  index_p = match(P, unlist(S[i])) #the position of P in the sequence
  index_q = match(Q, unlist(S[i])) #likewise for the second element in the pair
  k <- unlist(S[i])  
  z <- c(replace(k, c(index_p,index_q), c(Q,P))) #interchange P and Q 
  if(indicatorPermissible(z) == 1){#if the new sequence is valid
    if(i >= n){#has equilibrium been reached yet?
      travel_time[j] = distance_travelled(S[i]) #store travel time
      j <- j + 1
    }
    S[i+1] <- list(z) #adds the new sequence to collection of sampled state spaces
    i <- i + 1 
  }
  else{
    if(i<=n){#this is done to make sure we dont double count travel times
      S[i+1] <- S[i]
      i <- i + 1
    }
    rejection_counter = rejection_counter+1 #see how many time our proposed transition introduced forbidden travel
  }
  trials <- trials + 1
}
   answer = mean(travel_time)
   print(length(travel_time))
   print(rejection_counter)
   print(trials)
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