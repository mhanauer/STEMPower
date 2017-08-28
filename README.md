library(MASS)
library(psych)
library(reshape)

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
  
  m = lm(itp ~ black + hisp,data = dat)
  m= summary(m)
  # Here I am grabbing the p-values
  m = m$coefficients[,4]
}

# Replicate the function above
pvalues <- replicate(10000, STEMLong())

pvalues  = t(as.data.frame(pvalues))
pvalues = pvalues[,2:3]

repNum = 10000
# Changing the p-values that are less than or equal .05, because we are expecting statistically significantly lower ITP scores.
pvaluesOnes = as.data.frame(apply(pvalues, 2, function(x){ifelse(x <= .05, 1, 0)}))
pvaluesPower = as.data.frame(apply(pvaluesOnes, 2, sum))
n250 = 250
# Then divide by the total to get the percentage of model that had pvalues at or below .05, which is the power.    
Power250 = as.data.frame(t(pvaluesPower/repNum))
PowerB250 = cbind(Power250$black, n250)
colnames(PowerB250) = c("Power", "N")
PowerB250

PowerH250 = cbind(Power250$hisp, n250)
colnames(PowerH250) = c("Power", "N")
PowerH250
