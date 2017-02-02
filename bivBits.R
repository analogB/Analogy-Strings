bivBits<-function(string1,string2, displayDiagnostics=FALSE, displayPosteriors=FALSE, saveOutput=FALSE, imageType="pdf"){

#Uses rjags & DBDA2E-utilities.R from Kruschke 2014. Doing Bayesian Data Analysis 2nd Ed.
#rm(list=ls(all=TRUE))
require(rjags)
source("DBDA2E-utilities.R")
graphics.off()
#displayDiagnostics=FALSE
#displayPosteriors=FALSE
#saveOutput=FALSE
#imageType="pdf"
fileNameRoot <- 'bivBits-'
if (!saveOutput){fileNameRoot <- NULL}

#--------------------------------------------------------------------------------
# DESCRIPTION 
# Input: This function takes two binary strings (1,2) of same length and predicts 
#        a SINGLE missing target element denoted by "?" in one of the strings. Missing 
#        values in the strings other than the target element are allowed as NA. 
# Output: The function returns a prediction for the target value, based on the 
#        the credibility of the univariate proportion estimates, the bivariate 
#        proportion estimates, and the priors. 
# Priors: 

#--------------------------------------------------------------------------------
# INPUT DATA

x1 <- c("NA","NA","1","0","1","1","0","0","NA","1","0","1")
x2 <- c("NA","0","NA","?","1","1","1","NA","0","1","0","NA")

# x1 <- c(1,1,1,1,1)
# x2 <- c(1,1,1,1,"?" )
# #string 03
# x1 <- c(NA,0 ,0,0,NA,0,1 ,NA,0 ,0, 1 ,0,0, 0, 1)
# x2 <- c(NA,NA,0,1,1 ,0,NA,1 ,NA,1,"?",1,0,NA,NA)

# #string 06
# x1 <- c(1,0,0,NA,1, 1 ,1 ,1,NA,1 ,NA,1 ,0 ,1,1 )
# x2 <- c(1,1,0,NA,0,"?",NA,0,1 ,NA,1 ,NA,NA,1,NA)

# #string 09
# x1 <- c(0,1,1,1 ,0,NA,1 ,1 ,NA,1 ,1 ,NA, 0 ,1,1)
# x2 <- c(1,0,0,NA,1,0 ,NA,NA,1 ,NA,NA,NA,"?",0,0)

# # #string 12
# x1 <- c(1,0,NA,1,1,0 , 1 ,NA,1 ,NA,1,0 ,1 ,1 ,1)
# x2 <- c(0,1,1 ,0,0,NA,"?",1 ,NA,NA,0,NA,NA,NA,0)

#string 15
# x1 <- c(NA,1,1,1 , 0 ,1,0,1 ,1,1 ,NA,1 ,1 ,0,NA)
# x2 <- c(NA,1,0,NA,"?",1,1,NA,1,NA,1 ,NA,NA,1,1 )
# # 
# # #string 18
#  x1 <- c(0,1 ,NA,0 ,0 , 0 ,0 ,NA,0,0,1,0,1,0 ,NA)
#  x2 <- c(0,NA,NA,NA,NA,"?",NA,1 ,1,1,1,1,1,NA,1 )
# 
# #string 21
# x1 <- c(0 ,0,0,1, 1 ,0 ,NA,1 ,0 ,0,0,NA,NA,0 ,1)
# x2 <- c(NA,0,0,1,"?",NA,0 ,NA,NA,0,0,NA,0 ,NA,0)

# #string 24
# x1 <- c(NA,NA,1 ,0,1,0 ,1 ,1,1,0 ,1,1 ,1, 1 ,NA)
# x2 <- c(0 ,NA,NA,1,0,NA,NA,0,0,NA,0,NA,0,"?",0 )

# #01-str003 (0.506)
# x1<-c(NA,1,0 ,NA,0,0 ,NA,0 , 1 ,0 ,0,0,1,0,0 )
# x2<-c(0 ,1,NA,NA,1,NA,1 ,NA,"?",NA,0,0,0,0,NA)

# #01-str006 (0.540)
# x1<-c(0,NA,1 ,0,NA,0 ,0,NA,0, 0 ,1 ,1 ,0,0,0 )
# x2<-c(0,1 ,NA,0,NA,NA,0,0 ,0,"?",NA,NA,1,1,NA)

# #01-str008 (0.574)
# x1<-c(0 ,1 ,0,0 ,1,NA,0,0,0 , 1 ,NA,0,NA,0 ,0)
# x2<-c(NA,NA,1,NA,0,1 ,1,1,NA,"?",NA,1,0 ,NA,0)

# #01-str012 (0.645)
# x1<-c(NA,NA,0 ,0 ,0,0,1 , 0 ,NA,0,0,0 ,1,1,0 )
# x2<-c(1 ,1 ,NA,NA,1,1,NA,"?",NA,1,0,NA,0,0,NA)

# #01-str015 (0.650)
# x1<-c(1 ,1,1,0 ,0,1 , 0 ,1,1,NA,NA,1 ,1,NA,1 )
# x2<-c(NA,0,1,NA,1,NA,"?",1,1,NA,1 ,NA,1,1 ,NA)

# #01-str018 (0.689)
# x1<-c(0,0 ,1,1,1 ,1,NA,1 ,1,1,1 ,NA,0 , 1 ,NA)
# x2<-c(1,NA,1,1,NA,0,1 ,NA,1,1,NA,1 ,NA,"?",NA)

# #01-str021 (0.555)
# x1<-c(1,1,1,NA,NA,1 ,0 ,1,1,1,0 , 0 ,1 ,1 ,NA)
# x2<-c(0,1,1,NA,1 ,NA,NA,1,1,1,NA,"?",NA,NA,1 )

#01-str048 (wanted example with 6 pairs only) (0.716)
# x1<-c( 1 ,1,1,1,1 ,NA,NA,NA,1,1 ,0 ,1,0 ,1,0 )
# x2<-c("?",0,0,0,NA,0 ,NA,0 ,0,NA,NA,0,NA,1,NA)

x1<-string1
x2<-string2

if (length(x1)!=length(x2)){stop('Yikes! String lengths are not equal!')}

#--------------------------------------------------------------------------------
# FORMAT DATA
#Add imputed variable on the end to track predictive posteriors
x1 <- c(x1,NA) 
x2 <- c(x2,NA)
stringLength = length(x1)

#Arrange strings into matrix for convenience
X <- matrix(NA,nrow=2,ncol=stringLength)
X[1,] <- x1
X[2,] <- x2

#Determine target element location 
targetPosition <- which(!is.na(X)&X=="?")
targetElement  <- (targetPosition+1)%/%2
targetString   <- (targetPosition+1)%%2+1

#Replace target element with NA
X[targetPosition] <- NA
X <- matrix(as.numeric(unlist(X)),nrow=nrow(X))
same12<-as.integer(X[1,]==X[2,])

#Create target, lever, and imputed names
numbers <- c(2,1)     # for convenience
targetName    <- paste('X[',targetString,',',targetElement,']',sep='') #question mark
leverName     <- paste('X[',numbers[targetString],',',targetElement,']',sep='')#correspond element to target
leverValue    <- X[numbers[targetString],targetElement]
imputedName1  <- paste('X[1,',stringLength,']', sep='')
imputedName2  <- paste('X[2,',stringLength,']', sep='')

#Data list for rjags
dataList = list(
  X = X ,
  N = stringLength
) #end dataList
#--------------------------------------------------------------------------------
# THE MODEL.
modelString = "
model {
  for (i in 1:N) {
  X[2,i] ~ dbern(theta[i])
  theta[i] <- p[m,i]  
  p[1,i] <- 0.5
  p[2,i] <- 0.5
  p[3,i] <- theta3  
  p[4,i] <- theta4^X[1,i] * (1-theta4)^(1-X[1,i])
  
  X[1,i] ~ dbern(x[i])
  x[i] <- theta5
}

theta3 ~ dbeta(1,1) 
theta4 ~ dbeta(1,1) 
theta5 ~ dbeta(1,1)

m~dcat(m1)
m1[1]<-1
m1[2]<-1
m1[3]<-1
m1[4]<-1
}
" # close quote for modelString
writeLines( modelString , con="TEMPmodel.txt" )

#--------------------------------------------------------------------------------
# RUN CHAINS
# initsList = list( theta=c(0.5,0.5,0.5) , prior=2 ) #not necessary with simple models

parameters = c( "theta3","theta4","m",
               targetName,leverName,imputedName1, imputedName2) 
adaptSteps    =  20000      # Number of steps to "tune" the samplers.
burnInSteps   =  20000      # Number of steps to "burn-in" the samplers.
numSavedSteps = 300000      # Total number of steps to save collectively from all chains. 
nChains   = 3               # Number of chains to run.
thinSteps = 4               # Number of steps to "thin" (1=keep every step).
nPerChain = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.

# Create, initialize, and adapt the model:
jagsModel = jags.model( "TEMPmodel.txt" , data=dataList ,  n.chains=nChains , n.adapt=adaptSteps , quiet = TRUE )  #inits=initsList ,
cat( "Burning in the MCMC chain...\n" ) # Burn-in:
update( jagsModel , n.iter=burnInSteps )
cat( "Sampling final MCMC chain...\n" ) # The saved MCMC chain:
codaSamples = coda.samples( jagsModel , variable.names=parameters , n.iter=nPerChain , thin=thinSteps ,quiet = TRUE )
if (saveOutput){
  save(codaSamples,file=paste0(fileNameRoot,"Mcmc.Rdata"))
}
#---------------------------------------------------------------------------------
# CHAIN DIAGNOSTICS
if (displayDiagnostics){
  parameterNames = varnames(codaSamples) # get all parameter names
  for ( parName in parameterNames ) {
    diagMCMC( codaSamples , parName=parName ,saveName=fileNameRoot , saveType=imageType)
  }
} #end if (displayDiagnostics)

#--------------------------------------------------------------------------------
# ANALYSE RESULTS
# Convert coda-object codaSamples to matrix object for easier handling.
mcmcMat = as.matrix( codaSamples , chains=TRUE )
chainLength <- nrow(mcmcMat)
withinPost = mcmcMat[,"theta3"]
acrossPost = mcmcMat[,"theta4"]
mPost = mcmcMat[ ,"m"]
xPost = mcmcMat[,imputedName1]
yPost = mcmcMat[,imputedName2]
matchPost = as.integer(xPost==yPost)

targetChain = mcmcMat[,targetName]
leverChain  = mcmcMat[,leverName]
targetPrediction <- sum(targetChain)/chainLength
xPrediction <- sum(xPost)/chainLength
yPrediction <- sum(yPost)/chainLength

#--------------------------------------------------------------------------------
# OUTPUT & GRAPHICS
if (displayPosteriors){
  #Open graphics device and specify layout of graphics
  openGraph(width=12,height=4)
  par( mar=0.5+c(4,3,2,3) , mgp=c(2.0,0.7,0) )
  fontSize = 1
  layout( matrix(c(1,2,3,4,5,5,6,6,7,7),nrow=2,byrow=FALSE),  widths=c(1.7,1.7,1,1,1))
  
  #Plots 1-4 are for the distribution on continuous parameters
  plotPost( withinPost, main=bquote( "proportion within" == 1 ) , cenTend="mean",cex.main=fontSize , xlab=bquote(beta) , xlim=c(0,1) )
  plotPost( acrossPost, main=bquote( "proportion across" == 1 ) , cenTend="mean",cex.main=fontSize , xlab=bquote(beta) , xlim=c(0,1) )

  #Plots 5-8 are 0-1 bar graphs
  plotPost( xPost , breaks=seq(-0.1,1.1,0.2) , cenTend="mean" , xlab="proportion string 1" , main="StringA BaseRate" )
  plotPost( yPost , breaks=seq(-0.1,1.1,0.2) , cenTend="mean" , xlab="proportion string 2" , main="StringB BaseRate" )
  plotPost( mPost , breaks=seq(.9,4.1,0.2) , cenTend="mean" , xlab="model"   , main="A & B Match Rate")
  plotPost( matchPost , breaks=seq(-0.1,1.1,0.2) , cenTend="mean" , xlab="match rate"   , main="A & B Match Rate")

  if (saveOutput){
    saveGraph( file=paste0(fileNameRoot,"Post") , type=imageType )
  }
} #end if (displayPosteriors)
#print(targetPrediction)
return(targetPrediction)
} #end funtion