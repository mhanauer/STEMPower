library(MASS)
library(psych)
library(reshape)
library(Zelig)

###############################################################
# L1 Equation
# ITPis<-pi0s + pi1s*TIMEis + eis
###########################################################################

###########################################################################

# Let's use the mixed equation
# Unknowns, but we can set the value for.
# ITPis
# b0
# b01;b02
# ETHs
# ris
# TIMEis
##########################################################################################
### Since we have only one equation in the mixed equation, we can only have one unknowns
# Let's make the eis is the unknown
######250###################################################################################
STEMLong <- function(){
  ### High Level Parameters
  n<-250				# Sample Size
  
  b00Val <- 0 # Not expecting change among white women
  
  # Expecting these as the average change
  b11Val <- -.22 ; b12Val <- -.22  # Coefficients for the different ethnicities
  ETHsCategories<-c(1,2,3)
  ETHsLabel<-c("W","B","H")
  ETHsProb<- c(0.50,0.25,.25) # highest prob is whites
  
  
  # Create the ETHs variable # Sampled from the ETHS categories with the probs specied above five times
  ETHs<-as.data.frame(matrix(rep(sample(ETHsCategories,n,replace=TRUE,prob=ETHsProb)),nrow=n,byrow=FALSE))
  head(ETHs)
  # If a row equals a particular value then that is one.  Colmeans should be close to the probabilities, because they are randomly
  # with particular probabilites.
  Black<-as.data.frame(matrix(rep(ifelse(ETHs[,1]==2,1,0)),nrow=n,byrow=FALSE));colMeans(Black)
  Hisp<-as.data.frame(matrix(rep(ifelse(ETHs[,1]==3,1,0)),nrow=n,byrow=FALSE));colMeans(Hisp)
  
  
  l1mean<-0
  l1sd<-.5

  
  ###############################################################
  # L1 error term is known, ITP is unknown
  ###############################################################
  # error term # This is always random.  
  eis<-as.data.frame(matrix(rnorm(n,mean=l1mean,sd=l1sd),nrow=n))
  head(eis)
  
  
  #Create the coefficient matrix for b00
  b00 <-as.data.frame(matrix(rep(b00Val),nrow=n))
  head(b00)
  
  #Create the coefficient matrix for b11
  b11 <-as.data.frame(matrix(rep(b11Val),nrow=n))
  b12 <-as.data.frame(matrix(rep(b12Val),nrow=n))
  head(b11);head(b12)
  
  
  # Regular Equation
  # ITPis<- b00 + b11*Black + b12*Hisp + eis
  
  ITPis<-(b00 + b11*Black+ b12*Hisp + eis)
  head(ITPis)
  
  dat = cbind(ITPis, Black, Hisp)
  colnames(dat) = c("itp", "black", "hisp")
  #Regression model

  m = zelig(itp ~ black + hisp, model = "ls",data = dat)
  m$
  # Here I am grabbing the t-values
  t_value = coef(summary(m))[,"t value"] 
  t_value
}

# Replicate the function above
t_values <- replicate(10, STEMLong())
repNum = 10
# Tranpose so I can grab the t-values for the variables of interest
t_valuesT = as.data.frame(t(t_values)); t_valuesT
t_valuesTimeEth = as.data.frame(cbind(t_valuesT$`time:black`, t_valuesT$`time:hisp`))
colnames(t_valuesTimeEth)  = c("time_black", "time_hisp")

# Changing the t- values that are less than or equal -2, because we are expecting statistically significantly lower ITP scores.
t_valuesOnes = as.data.frame(apply(t_valuesTimeEth, 2, function(x){ifelse(x <= -2, 1, 0)}))
t_valuesPower = as.data.frame(apply(t_valuesOnes, 2, sum))
n250 = 250
# Then divide by the total to get the percentage of model that had t-values above 2, which is the power.    
Power250 = as.data.frame(t(t_valuesPower/repNum))
PowerB250 = cbind(Power250$time_black, n250)
colnames(PowerB250) = c("Power", "N")

PowerH250 = cbind(Power250$time_hisp, n250)
colnames(PowerH250) = c("Power", "N")
