U <-c(1:20) #state space
D <- matrix(runif(n=20*20, min=0, max =1), ncol = 20) #uniformly distributed distance matrix
D[4,6] = 0
D[9,13]= 0
D[19,1]= 0 #WTF!
D[17,10]= 0  #WTF!
D[3,8] = 0 #NO TRAVEL NO DISTANCE
D[lower.tri(D)] = t(D)[lower.tri(D)] #symmetric matrix
diag(D) <- rep(0,20) #diagonals are 0

indicatorPermissible <- function(C,p,q){
  if(p != 1 && p!= 20 && q!= 1 && q!= 20){ #normal interchange
    for(i in c(-1,1)){
      if(D[C[p+i],C[q]] == 0 || D[C[q+i],C[p]]==0){
        return(0)
      }
    }
  }
  else{#case where interchange occurs at one or both of the endpoints of sequence
    if(p == 1){
      if(D[C[20],C[q]] == 0 || D[C[2],C[q]] == 0)
        return(0)
    }
    else if(p == 20){
      if(D[C[1],C[q]] == 0 || D[C[19],C[q]] == 0)
        return(0)
    }
    else if(q==1){
      if(D[C[20],C[p]] == 0 || D[C[2],C[p]] == 0)
        return(0)
    }
    else{
      if(D[C[1],C[p]] == 0 || D[C[19],C[p]] == 0)
        return(0)
    }
  }
  return(1)
}
return_time <- function(L){
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
j <- 1
travel_time <- rep(0, m-n+1)
for(i in 1:m){
  if(i >= n){
    travel_time[j] = return_time(S[i])
    j <- j + 1
  }
  P <- sample(1:20,size=1) #basic sampling 
  Q <- sample(1:20,size=1) #likewise for proposed state
  index_p = match(P, unlist(S[i])) #the position of P in the sequence
  index_q = match(Q, unlist(S[i])) #likewise for the second element in the pair
  k <- unlist(S[i])
  if(indicatorPermissible(k,index_p,index_q) == 1){
    S[i+1] <- list(c(replace(k, c(P,Q), k[c(Q,P)]))) #adds a new sequence to S interchanging P and Q 
    #return(S[i+1])
  }
  else{
    S[i+1] = S[i]
    rejection_counter = rejection_counter+1 #see how many time our proposed transition introduced forbidden travel
  }
}
  answer = mean(travel_time)
  return(answer)
}

tspMCMC(100,200)
tspMCMC(200,400)
tspMCMC(400,800)
tspMCMC(800,1600)
tspMCMC(1600,3200)
tspMCMC(3200,6400)

