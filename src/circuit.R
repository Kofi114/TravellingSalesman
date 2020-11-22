library(compare)
S <-c(1:20) #state space
n <- 40
D <- matrix(runif(n=20*20, min=0, max =1), ncol = 20) #uniformly distributed distance matrix
D[lower.tri(D)] = t(D)[lower.tri(D)] #symmetric matrix
diag(D) <- rep(0,20)


restrictedPaths_list <- list(c(4,5), c(5,4), c(9,13), c(13,9), c(20,1), c(1,20), c(17,10), c(10,17), c(3,8), c(8,3)) #5 forbidden pairs 

indicatorForbidden <- function(x,y){ #optimize this so you iterate only 5 times.
  indtest_ <- c(x,y)
  for(i in 1:10){
    comp <- identical(indtest_,unlist(restrictedPaths_list[i]))
    if(comp){
      return (0)
    }
  }
  return(1)
}

x <- rep(0,n)
x[1] <- 1
for(i in 1:n){
  Q <- sample(S,size=1, prob=D[x[i],])
  if(indicatorForbidden(x[i],Q)==0){
    x[i+1] = x[i]       
  } else {
    x[i+1] = Q        
  }
}
