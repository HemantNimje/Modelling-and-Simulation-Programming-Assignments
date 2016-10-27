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
      cat("City #",cityDailyDemand.df$City[j],", Demand: ",cityDailyDemand.df$Demand[j],", Supply: ",PowerSupplyContribution,", Difference:",(PowerSupplyContribution-cityDailyDemand.df$Demand[j]),": No Blackout","\n")
    }
  }
}