## This is my shiny app application

library(shiny)
library(tidyverse)
library(shinyjs)
library(Stat2Data)



######################################## DATA PREPARATION
data(Clothing)
Clothing <- Clothing[-60,] # Missing Value
Clothing$Card <- as.factor(Clothing$Card) # Categorical variable


card=matrix(0,nrow=nrow(Clothing),ncol=1)
for (i in 1:nrow(card)){
  if(Clothing[i,8]==1){
    card[i]="Private label"
  }
  if(Clothing[i,8]==0){
    card[i]="Public label"
  }
}

Clothing$Card <- as.factor(card)

########################################