#HEMANTKUMAR NIMJE

#EXERCISE 1

getwd()

setwd("F:/CSULB Spring 16/CSULB SEM3 Fall 16/Modelling and Simulation/Assignments")

acme.df <- read.csv("acme.csv")
acme.df

cityDailyDemand.df <- read.csv("city.csv")
cityDailyDemand.df

A = matrix(c(30,20,15,0,0,21,14,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,20,0,0,41,0,15,0,7,0,0,0,9,0,0,0,0,0,10,0,0,0,0,10,0,25,5,10,0,10,0,10,0,0,10,10,0,0,0,0,80,0,0,0,0,0,0,0,0,0,0,0,20,0,0,0,0,20,0,0,0,0,0,10,0,0,15,0,10,0,0,0,10,10,10,0,15,0,10,10,0,0,15,0,0,7,8,0,0,0,0,9,6,20,0,15,0,3,6,9,0,15,0,11,7,0,10,0,12,0,0,14,0,0,0,0,13,7,5,6,8,4,5,5,5,5,8,4,10,5,5,4,4,2,2,3,3,3,3,2,2,4,4,5,5,10,4,8,5,5,5,5,4,8,6,5,7,10,10,10,10,0,0,0,0,10,0,15,0,0,15,0,0,0,0,0,20,0,0,0,0,0,0,0,0,0,0,0,0,0,14,21,0,0,15,20,30,0,0,20,0,8,0,0,15,0,0,41,0,9,0,0,0,0,7,0,0,6,6,6,6,6,6,6,6,8,6,0,6,0,6,0,6,4,6,4,6,5,7,8,6,5,4,5,5,8,5,8,0,10,4,0,2,2,2,3,0,0,0,0,0,10,10,10,10,0,15,0,10,0,0,15,0,20,0,0,0),nrow=15,ncol=20,byrow = TRUE)
A

daily_power <- function(){
  MWperStation <- c()
  for(i in 1:nrow(A)){
    MWperGen <- acme.df$MegawattsPerGenerator[i]
    p <- acme.df$NonOperationProbability[i]
    x<- sample(0:1,acme.df$Generators[i],replace=TRUE,prob = c(p,1-p))
    y <- sum(x)            
    MWperStation <- c(MWperStation,y * MWperGen)
  }

  for(j in 1:ncol(A)){
    PowerSupplyContribution <- 0 
    PowerSupply <- 0
    for(i in 1:nrow(A)){
      StationPercentage <- A[i,j]
      PowerSupply <- (StationPercentage * MWperStation[i] * 0.01)
      PowerSupplyContribution <- PowerSupplyContribution + PowerSupply
    }
    if(PowerSupplyContribution < cityDailyDemand.df$Demand[j]){
      cat("City #",cityDailyDemand.df$City[j],", Demand: ",cityDailyDemand.df$Demand[j],", Supply: ",PowerSupplyContribution,", Difference:",(PowerSupplyContribution-cityDailyDemand.df$Demand[j]),": BLACKOUT","\n")
    }
    else{
      cat("City #",cityDailyDemand.df$City[j],", Demand: ",cityDailyDemand.df$Demand[j],", Supply: ",PowerSupplyContribution,", Difference:",(PowerSupplyContribution-cityDailyDemand.df$Demand[j]),"\n")
    }
  }
}


#OUTPUT
#City # 1 , Demand:  6.04 , Supply:  6.9822 , Difference: 0.9422 
#City # 2 , Demand:  7.15 , Supply:  8.1971 , Difference: 1.0471 
#City # 3 , Demand:  9.04 , Supply:  10.4943 , Difference: 1.4543 
#City # 4 , Demand:  10.12 , Supply:  11.9916 , Difference: 1.8716 
#City # 5 , Demand:  5.8 , Supply:  9.2483 , Difference: 3.4483 
#City # 6 , Demand:  5.4 , Supply:  6.2291 , Difference: 0.8291 
#City # 7 , Demand:  6.2 , Supply:  7.3908 , Difference: 1.1908 
#City # 8 , Demand:  6.06 , Supply:  12.0756 , Difference: 6.0156 
#City # 9 , Demand:  7.97 , Supply:  10.5191 , Difference: 2.5491 
#City # 10 , Demand:  6.52 , Supply:  10.1288 , Difference: 3.6088 
#City # 11 , Demand:  9.05 , Supply:  9.4922 , Difference: 0.4422 
#City # 12 , Demand:  5.37 , Supply:  6.2336 , Difference: 0.8636 
#City # 13 , Demand:  3.99 , Supply:  7.3301 , Difference: 3.3401 
#City # 14 , Demand:  6.69 , Supply:  7.3331 , Difference: 0.6431 
#City # 15 , Demand:  5.85 , Supply:  6.1966 , Difference: 0.3466 
#City # 16 , Demand:  5.88 , Supply:  6.9876 , Difference: 1.1076 
#City # 17 , Demand:  4.86 , Supply:  5.9746 , Difference: 1.1146 
#City # 18 , Demand:  5.86 , Supply:  6.7304 , Difference: 0.8704 
#City # 19 , Demand:  7 , Supply:  7.4179 , Difference: 0.4179 
#City # 20 , Demand:  8.3 , Supply:  8.7586 , Difference: 0.4586 

#EXERCISE 2


failure <- function(){
  blackout <- 0
  MWperStation <- c()
  for(i in 1:nrow(A)){
    MWperGen <- acme.df$MegawattsPerGenerator[i]
    p <- acme.df$NonOperationProbability[i]
    x<- sample(0:1,acme.df$Generators[i],replace=TRUE,prob = c(p,1-p))
    y <- sum(x)            
    MWperStation <- c(MWperStation,y * MWperGen)
  }
  
  for(j in 1:ncol(A)){
    PowerSupplyContribution <- 0 
    PowerSupply <- 0
    for(i in 1:nrow(A)){
      StationPercentage <- A[i,j]
      PowerSupply <- (StationPercentage * MWperStation[i] * 0.01)
      PowerSupplyContribution <- PowerSupplyContribution + PowerSupply
    }
    if(PowerSupplyContribution < cityDailyDemand.df$Demand[j]){
      #cat("City #",cityDailyDemand.df$City[j],", Demand: ",cityDailyDemand.df$Demand[j],", Supply: ",PowerSupplyContribution,", Difference:",(PowerSupplyContribution-cityDailyDemand.df$Demand[j]),": BLACKOUT","\n")
      blackout <- 1
    }
    else{
      #cat("City #",cityDailyDemand.df$City[j],", Demand: ",cityDailyDemand.df$Demand[j],", Supply: ",PowerSupplyContribution,", Difference:",(PowerSupplyContribution-cityDailyDemand.df$Demand[j]),"\n")
      
    }
  }
  return(blackout)
}

mttf <- 0

imc <- function(n,delta,failure){
  sum <- 0
  sumsq <- 0
  for(i in 1:n){
    x <- failure()
    sum <- sum + x
    sumsq <- sumsq + x*x
  }  
  lambda <- sum / n
  sigmasq <- (sumsq - (lambda * lambda * n) )/ (n - 1)
  se <- sqrt(sigmasq / n)
  qdelta <- qnorm((1+delta)/2)
  ci_left <- lambda - qdelta * se
  ci_right <- lambda + qdelta * se
  
  mttf <- ((1/lambda)-1)
  
  cat("# Failures: ",sum,"n: ",n," Probability of failure day: ",lambda,"\n")
  cat("Sample Variance: ",sigmasq,"\n")
  cat("Standard Error: ",se,"\n")
  cat(delta,"-confidence interval:[",ci_left,",",ci_right,"]\n",sep = "")
  cat("Mean Time to Failure: ",mttf)
}


#OUTPUT
#> failure()
#[1] 1
#> failure()
#[1] 0
#> failure()
#[1] 1

#>imc(50000,0.90,failure())
# Failures:  30525 n:  50000  Probability of failure day:  0.6105 
#Sample Variance:  0.2377945 
#Standard Error:  0.0021808 
#0.9-confidence interval:[0.6069129,0.6140871]
#Mean Time to Failure:  0.6380016


#EXERCISE 3

failure2 <- function(s,Prob){
  blackout <- 0
  MWperStation <- c()
  for(i in 1:nrow(A)){
    MWperGen <- acme.df$MegawattsPerGenerator[i]
    if(i == s){
      p <- Prob
    }
    else{
      p <- acme.df$NonOperationProbability[i]
    }
    x<- sample(0:1,acme.df$Generators[i],replace=TRUE,prob = c(p,1-p))
    y <- sum(x)            
    MWperStation <- c(MWperStation,y * MWperGen)
  }
  
  for(j in 1:ncol(A)){
    PowerSupplyContribution <- 0 
    PowerSupply <- 0
    for(i in 1:nrow(A)){
      StationPercentage <- A[i,j]
      PowerSupply <- (StationPercentage * MWperStation[i] * 0.01)
      PowerSupplyContribution <- PowerSupplyContribution + PowerSupply
    }
    if(PowerSupplyContribution < cityDailyDemand.df$Demand[j]){
      #cat("City #",cityDailyDemand.df$City[j],", Demand: ",cityDailyDemand.df$Demand[j],", Supply: ",PowerSupplyContribution,", Difference:",(PowerSupplyContribution-cityDailyDemand.df$Demand[j]),": BLACKOUT","\n")
      blackout <- 1
    }
    else{
      #cat("City #",cityDailyDemand.df$City[j],", Demand: ",cityDailyDemand.df$Demand[j],", Supply: ",PowerSupplyContribution,", Difference:",(PowerSupplyContribution-cityDailyDemand.df$Demand[j]),"\n")
    }
  }
  return(blackout)
}

#OUTPUT
#> failure2(5,0.15)
#[1] 1
#> failure2(5,0.15)
#[1] 0


#EXERCISE 4


consortium_report <- function(){
  mttf <- 0
  n <- 50000
  delta <- 0.90
  sum <- 0
  sumsq <- 0
  for(i in 1:n){
    x <- failure()
    sum <- sum + x
    sumsq <- sumsq + x*x
  }  
  lambda <- sum / n
  sigmasq <- (sumsq - (lambda * lambda * n) )/ (n - 1)
  se <- sqrt(sigmasq / n)
  qdelta <- qnorm((1+delta)/2)
  ci_left <- lambda - qdelta * se
  ci_right <- lambda + qdelta * se
    
  mttf <- ((1/lambda)-1)
    
  #cat("# Failures: ",sum,"n: ",n," Probability of failure day: ",lambda,"\n")
  #cat("Sample Variance: ",sigmasq,"\n")
  #cat("Standard Error: ",se,"\n")
  #cat(delta,"-confidence interval:[",ci_left,",",ci_right,"]\n",sep = "")
  #cat("Mean Time to Failure: ",mttf)

  
  mttf2 <- 0
  for(i in 1:15){
    n <- 50000
    delta <- 0.90
    sum <- 0
    sumsq <- 0
    for(j in 1:n){
      x <- failure2(i,acme.df$NonOperationProbability[i]/2)
      sum <- sum + x
      sumsq <- sumsq + x*x
    }  
    lambda <- sum / n
    sigmasq <- (sumsq - (lambda * lambda * n) )/ (n - 1)
    se <- sqrt(sigmasq / n)
    qdelta <- qnorm((1+delta)/2)
    ci_left <- lambda - qdelta * se
    ci_right <- lambda + qdelta * se
    
    mttf2 <- ((1/lambda)-1)
    
    #cat("# Failures: ",sum,"n: ",n," Probability of failure day: ",lambda,"\n")
    #cat("Sample Variance: ",sigmasq,"\n")
    #cat("Standard Error: ",se,"\n")
    #cat(delta,"-confidence interval:[",ci_left,",",ci_right,"]\n",sep = "")
    #cat("Mean Time to Failure: ",mttf)
    #cat(mttf,"  ",mttf2, "\n")
    if(mttf - mttf2 >=4){
      cat("Station ",acme.df$Station[i],"has updated MTTF value greater than 4 as compared to global one.")
    }
    else{cat("No station has updated MTTF value greater by 4 than global one")}
    
    
    cat("Station: ",acme.df$Station[i]," Lambda: ",lambda,", ", delta,"-confidence interval:[",ci_left,",",ci_right,"] ",sep = "","MTTF: ", mttf2,"\n")
    
  }
}

#OUPTOUT
#> consortium_report()
#Station: 1 Lambda: 0.60726, 0.9-confidence interval:[0.6036676,0.6108524] MTTF: 0.6467411
#Station: 2 Lambda: 0.5883, 0.9-confidence interval:[0.5846798,0.5919202] MTTF: 0.699813
#Station: 3 Lambda: 0.58042, 0.9-confidence interval:[0.5767898,0.5840502] MTTF: 0.7228903
#Station: 4 Lambda: 0.61148, 0.9-confidence interval:[0.6078945,0.6150655] MTTF: 0.6353765
#Station: 5 Lambda: 0.61114, 0.9-confidence interval:[0.607554,0.614726] MTTF: 0.6362863
#Station: 6 Lambda: 0.60682, 0.9-confidence interval:[0.6032269,0.6104131] MTTF: 0.6479351
#Station: 7 Lambda: 0.56954, 0.9-confidence interval:[0.5658977,0.5731823] MTTF: 0.7558029
#Station: 8 Lambda: 0.5798, 0.9-confidence interval:[0.5761691,0.5834309] MTTF: 0.7247327
#Station: 9 Lambda: 0.55834, 0.9-confidence interval:[0.5546871,0.5619929] MTTF: 0.7910234
#Station: 10 Lambda: 0.60924, 0.9-confidence interval:[0.6056508,0.6128292] MTTF: 0.6413893
#Station: 11 Lambda: 0.6099, 0.9-confidence interval:[0.6063119,0.6134881] MTTF: 0.6396131
#Station: 12 Lambda: 0.60762, 0.9-confidence interval:[0.6040282,0.6112118] MTTF: 0.6457654
#Station: 13 Lambda: 0.60718, 0.9-confidence interval:[0.6035875,0.6107725] MTTF: 0.6469581
#Station: 14 Lambda: 0.58426, 0.9-confidence interval:[0.5806346,0.5878854] MTTF: 0.7115668
#Station: 15 Lambda: 0.6079, 0.9-confidence interval:[0.6043086,0.6114914] MTTF: 0.6450074
 


#EXERCISE 5

#Since the new MTTF evaluated after reducing the non-operation probability to half is not much different than the
#original MTTF value evaluated from the given data, the station upgradation will not affect any performance increase.
#Even after the upgradation of the stations the blackouts in the cities will be same as before upgradation.
#So there is station upgradation is not recommended.