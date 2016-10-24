#HEMANTKUMAR NIMJE

#EXERCISE 1

#craps() RETURNS 1 IF PLAYER WINS OR RETURN 0 IF PLAYER LOSES

craps <- function(){
  #boolean VALUE 1 INDICATES THAT THE PLAYER EITHER WINS THE GAME OR LOSES THE GAME AND NO NEED TO ROLL THE DICE AGAIN.
  #boolean VALUE 0 INDICATES THAT THE PLAYER NEEDS TO ROLL THE DICE AGAIN.
  boolean <- 0
  
  #playerStatus VALUE 1 INDICATES THAT PLAYER WINS THE GAME
  #playerStatus VALUE 0 INDICATES THAT PLAYER LOSES THE GAME
  playerStatus <- 0
  
  #sample FUNCTION WILL CREATE A RANDOM OUTPUT FOR THE DICE FACE AFTER THE ROLL.
  x1 <- sample(1:6,1,replace = TRUE)
  x2 <- sample(1:6,1,replace = TRUE)
  
  #rolledSum INDICATES THE SUM OF THE DICE FACES.
  rolledSum <- x1 + x2
  
  #PLAYER WINS IF THE SUM OF DICE IN FIRST ROLL IS EITHER 7 OR 11
  if(rolledSum == 7 || rolledSum == 11){ 
    boolean <- 1
    playerStatus <- 1
    return(playerStatus)
  }
  
  #PLAYER LOSES IF SUM OF DICE IN FIRST ROLL IS 2 OR 3 OR 12
  else if(rolledSum == 2 || rolledSum == 3 || rolledSum == 12){ 
    boolean <- 1
    return(playerStatus)
  }
  
  #PLAYER NEED TO ROLL THE DICE AGAIN IF THE SUM OF THE DICE FACES IS 4/5/6/8/9/10
  #boolean = 0 INDICATES THAT THE PLAYER NEEDS TO ROLL THE DICE AGAIN.
  else if( rolledSum == 4 || rolledSum == 5 || rolledSum == 6 || rolledSum == 8 || rolledSum == 9 || rolledSum == 10){ 
    
    while (boolean == 0) {
      x3 <- sample(1:6,1,replace = TRUE)
      x4 <- sample(1:6,1,replace = TRUE)
      rolledSum = x3 + x4
      
      #PLAYER LOSES IF SUM OF THE DICE FACES IS 7 FROM THE SECOND DICE ROLL OR ONWARDS
      if(rolledSum == 7){
        boolean <- 1
        return(playerStatus)
      }
      
      #PLAYER WINS IF SUM OF THE DICE FACES IS 4/5/6/8/9/10 OBTAINED FROM THE SECOND DICE ROLL OR ONWARDS.
      else if( rolledSum == 4 || rolledSum == 5 || rolledSum == 6 || rolledSum == 8 || rolledSum == 9 || rolledSum == 10){
        boolean <- 1
        playerStatus <- 1
        return(playerStatus)
      }
    }
  }
}



#EXERCISE 2

#FUNCTION f CREATES A RANDOM OUTPUT EITHER 0 OR 1
f <- function(){
  x <- sample(0:1,1)
  return(x)
}

#estimate_bernoulli ACCEPT A FUNCTION, DELTA AND EPSILON. AND RETURNS LAMBDA IF CERTAIN CONDITION SATIFIES
estimate_bernoulli <- function(f,delta,epsilon){
  boolean <- 0
  n <- 0
  sum <- 0
  sumsq <- 0
  lambda <- 0
  
  #THE WHILE LOOP WILL CONTINUE TILL THE REQUIRED EQUATION SATISFIES
  #LAMBDA WILL BE RETURNED ON EQUATION SATISFACTION
  while(boolean == 0){
    n <- n + 10000
    for(i in 1:10000){
      x <- f()
      sum <- sum + x
      sumsq <- sumsq + x*x
    }  
    lambda <- sum / n
    sigmasq <- (sumsq - (lambda * lambda * n) )/ (n - 1)
    se <- sqrt(sigmasq / n)
    qdelta <- qnorm((1 + delta) / 2)
    leftSide <- (se * qdelta)
    
    if(leftSide <= epsilon){
      boolean <- 1
      return(lambda)
    }
  }
}


#EXERCISE 3

#CALL THE estimate_bernoulli FUNCTION USING 
estimate_bernoulli(craps,0.90,0.005)

#OUTPUT
#estimate_bernoulli(craps,0.90,0.005)
#[1] 0.7573



#EXERCISE 4

#network_reliability RETURNS THE VALUE OF LAMBDA, VARIANCE AND NUMBER OF SAMPLES REQUIRED TO REACH 50 BROKEN CONFIGURATIONS
network_reliability <- function(){
  
  countX <- 0
  sum <- 0
  sumsq <- 0
  
  #TAKE A LARGE NUMBER OF BERNOULLI SAMPLES AND IF THE NETWORK FOUND BROKEN ABOUT 50 TIMES THEN PRINT THE VALUES OF LAMBDA, SAMPLE VARIANCE
  #AND NUMBER OF SAMPLES NEEDED TO REACH 50 BROKEN CONFIGURATIONS 
  for (n in 1:100000000) {
    z <- rbern(7,0.999)
    if(all(z[1:2]==0) || all(z[5:7] == 0)){
      X <- 1 
      countX <- countX + 1  
      
      cat("sample #: ",n,", rbern: ",z,"is broken",", Broken Network Configuration Count: ",countX,"\n")
      sum <- sum + X
      sumsq <- sumsq + X*X
      if(countX == 50){
        break     
      }
    }
    else{
      X <- 0
    }
  }
  
  lambda <- sum / n
  sigmasq <- (sumsq - (lambda * lambda * n) )/ (n - 1)
  cat("Lambda: ",lambda," Sample Variance: ",sigmasq," # of samples needed to reach 50 broken configurations: ",n)
  
}



#EXERCISE 5


#TAKE A LARGE NUMBER OF BERNOULLI SAMPLES AND IF THE NETWORK FOUND BROKEN ABOUT 50 TIMES THEN PRINT THE VALUES OF LAMBDA, SAMPLE VARIANCE
#AND NUMBER OF SAMPLES NEEDED TO REACH 50 BROKEN CONFIGURATIONS 

#THE BERNOULLI FUNCTION rbern MUST BE PERFOMED FOR PROBABILITY q WHICH IS CALCULATED FROM r AND m
network_reliability2 <- function(){
  
  countX <- 0
  sum <- 0
  sumsq <- 0
  r <- 2
  m <- 7
  q <- 1 - (r/m)
  
  for (n in 1:100000000) {
    z <- rbern(7,q)
    if(all(z[1:2]==0) || all(z[5:7] == 0)){
      #X <- 1 
      
      X <- ( '^'((0.999/q),(m-r)) * '^'(((1-0.999)/(1-q)),r))
      countX <- countX + 1
      
      cat("sample #: ",n,", rbern: ",z,"is broken",", Broken Network count",countX,"\n")
      sum <- sum + X
      sumsq <- sumsq + X*X
      
      if(countX == 50){
        break     
      }
    }
    else{
      X <- 0
    }
  }
  
  lambda <- sum / n
  sigmasq <- (sumsq - (lambda * lambda * n) )/ (n - 1)
  cat("Lambda: ",lambda," Sample Variance: ",sigmasq," # of samples needed to reach 50 broken configurations: ",n)
  
}


