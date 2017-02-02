#  HEY!!! FIRST SET WD TO SOURCE FILE LOCATION

rm(list=ls(all=TRUE))
require('jsonlite')
require('plyr')
require('readr')
require(tidyr)
require(dplyr)
require(R6)
require(ggplot2)
require(scales)
require(jsonlite)
require(plyr)
require(readr)
#source('bivBits.R')

dir=getwd()
dirData=paste(dir,'/data',sep='')
setwd(dir)

txtStims <- read_file("textStims Oct 2016.txt")
stims=fromJSON(txtStims, simplifyVector = TRUE, flatten = FALSE)

chunkSize=1
stimLen=12
nStims = nrow(stims)
results = rep('NA',nStims)
i=1

#RUN RATIONAL ANALYSIS#####
for (i in 1:nStims){
  strA <- substring(stims$stringA[i], seq(1,nchar(stims$stringA[i]),chunkSize), seq(chunkSize,nchar(stims$stringA[i]),chunkSize))
  strA <- gsub(' ','NA',strA)
  strB <- substring(stims$stringB[i], seq(1,nchar(stims$stringB[i]),chunkSize), seq(chunkSize,nchar(stims$stringB[i]),chunkSize))
  strB <- gsub(' ','NA',strB)
  #   results[i] <- bivBits(strA,strB, displayDiagnostics=FALSE, displayPosteriors=FALSE, saveOutput=FALSE, imageType="pdf")
}

#####PASTE RESULTS HERE FOR POSTERITY & EFFICIENCY##########
results<-c(0.45,0.55,0.45,0.55,0.74,0.26,0.74,0.26,0.58,0.42,0.58,0.42,0.34,0.66,0.34,0.66,0.45,0.55,0.45,0.55,0.34,0.66,0.34,0.66,0.66,0.34,0.66,0.34,0.81,0.19,0.81,0.19,0.54,0.74,0.50,0.54,0.45,0.53,0.50,0.48,0.37,0.50)

block1Node<-'9'
block2Node<-'10'
stims[,'rational']<-as.data.frame(round(as.numeric(results),3))
stims[,'rPrediction']<-round(stims[,'rational'])
stims$rConfidence<-lapply(stims$rational,function (X) max(X,1-X))
stims[,'block1NodeID']<-paste('0.0-',block1Node,'.0-',row(stims[1])-1,'.0',sep='')
stims[,'block2NodeID']<-paste('0.0-',block2Node,'.0-',row(stims[1])-1,'.0',sep='')
stims[,'stimID']<-row(stims[1])-1

#####SPECIFY DATA FRAME TYPES#####
stims$stringA       <- as.character  (stims$stringA)
stims$stringB       <- as.character  (stims$stringB)
stims$random        <- as.factor     (stims$random)
stims$N             <- as.factor     (stims$N) 
stims$nA            <- as.factor     (stims$nA)  
stims$nB            <- as.factor     (stims$nB)
stims$nAB           <- as.factor     (stims$nAB)
stims$pA            <- as.factor     (stims$pA)
stims$pB            <- as.factor     (stims$pB)
stims$pAB           <- as.factor     (stims$pAB)
stims$anchored      <- as.factor     (stims$anchored)
stims$rational      <- as.numeric    (stims$rational)*100
stims$rPrediction   <- as.factor     (stims$rPrediction)
stims$rConfidence   <- as.numeric    (stims$rConfidence) 
stims$block1NodeID  <- as.character  (stims$block1NodeID) 
stims$block2NodeID  <- as.character  (stims$block2NodeID)
stims$stimID        <- as.factor     (stims$stimID) 

### READ DATA ###
setwd(dirData)
fileList <- list.files()
for (file in fileList){
  # append data from all other files
  if (exists("rawData")){
    temp_dataset <-read.csv(file, header=TRUE, sep=",")
    rawData<-rbind(rawData, temp_dataset)
    rm(temp_dataset)}
  # create from first data file
  if (!exists("rawData")){
    rawData <- read.csv(file, header=TRUE, sep=",")}
}

rawData$rt            <- as.numeric   (rawData$rt)
rawData$trial_type    <- as.factor    (rawData$trial_type)
rawData$trial_index   <- as.factor    (rawData$trial_index)
rawData$time_elapsed  <- as.numeric   (rawData$time_elapsed)
rawData$internal_node_id <- as.character (rawData$internal_node_id)
rawData$turkID        <- as.factor    (rawData$turkID)
rawData$sonaID        <- as.factor    (rawData$sonaID)
rawData$ExpEndTime    <- as.factor    (rawData$ExpEndTime)
rawData$lastInput     <- as.factor    (rawData$lastInput)
rawData$responseA     <- as.numeric   (rawData$responseA)
rawData$responseB     <- as.numeric   (rawData$responseB)
rawData$randomized    <- as.factor    (rawData$randomized)
rawData$N             <- as.factor    (rawData$N )
rawData$nA            <- as.factor    (rawData$nA)
rawData$nB            <- as.factor    (rawData$nB)
rawData$nAB           <- as.factor    (rawData$nAB)
rawData$pA            <- as.factor    (rawData$pA)
rawData$pB            <- as.factor    (rawData$pA)
rawData$pAB           <- as.factor    (rawData$pA)
rawData$anchored      <- as.factor    (rawData$anchored)

rawData$subjectID <- as.numeric(as.factor(rawData$sonaID))

rawData[,'blockID']<-substr(rawData[,'internal_node_id'],5,nchar(rawData[,'internal_node_id']))
rawData$blockID <- gsub("\\-.*","",rawData$blockID)
rawData$blockID<-substr(rawData$blockID,1,nchar(rawData$blockID)-2)

rawData[,'stimID']<-substr(rawData[,'internal_node_id'],5,nchar(rawData[,'internal_node_id']))
rawData$stimID<-substr(rawData$stimID,nchar(rawData$blockID)+4,nchar(rawData$stimID))
rawData$stimID<-substr(rawData$stimID,1,nchar(rawData$stimID)-2)

rawData$stimID[rawData$blockID=='6'] <- '' #replace training stimulus stimID with blank

###   Merge trial stimulus assignments and stimulus characteristics into each line
data <- merge(rawData,stims,by = c('stimID'))
data$stimID <- as.factor(data$stimID)
data$blockID <- as.factor(data$blockID)

data$prediction<-1*(data$responseA<data$responseB)
data$correct <- 1*(data$prediction==data$rPrediction)

participants<-unique(data$sonaID)
nParticipants<-length(participants)
exclusions <- c("xx")
filter <- !(data$sonaID %in% exclusions)
filter2<-filter & (data$rational>0) #& ((data$responseB==0) | (data$responseB==100))  

pCorrect <- aggregate(correct~stimID, data=data,'median')
names(pCorrect)<-c('stimID','pCorrect')
medFilter <- filter2 #& data$correct>0
medValues <- aggregate(responseB~stimID, data = subset(data,medFilter),'median') 
medValues2 <- aggregate(responseB~stimID, data = data,'median') #operation validity check
names(medValues) <- c('stimID','stimMed')
data <- merge(data,medValues,by='stimID')
data <- merge(data,pCorrect,by='stimID')
data$stimID_ordered <- reorder(data$stimID, data$rational)
propCorrect <- aggregate(pCorrect~rational,data=data,'median')

#Format Plots
# boxPlot <- with(data, plot(stimID_ordered, responseB))
xlabels <- unique(c(data$stimID_ordered,data$pattern))
len <- length(unique(data$stimID))

data$ratCorr     <- as.numeric   (-99)
data$pValofCorr     <- as.numeric (99)

for (i in 1:nParticipants){
  filter4 <- filter2 & (data$sonaID %in% participants[i])
  data[filter4,]$ratCorr <- round(cor.test(with(data[filter4,], tapply(rational, stimID, mean)),
                                           with(data[filter4,], tapply(responseB, stimID, mean)))$estimate,digits=2)
  data[filter4,]$pValofCorr <- formatC(signif(cor.test(with(data[filter4,], tapply(rational, stimID, mean)),
                                                       with(data[filter4,], tapply(responseB, stimID, mean)))$p.value,digits=2), digits=2,format="fg", flag="#")  
}

# for (i in 1:nParticipants){
#   filter3<-filter2 & (data$sonaID %in% participants[i]) 
#   rat <- data[data$stimID %in% levels(data$stimID_ordered),]
#   rat$stimID <- as.character(rat$stimID)
#   levels(rat$stimID) <- levels(data$stimID_ordered)
#   
#   plotName<- paste('participant',i,'responses v. rational probability')
#   plot2<-with(data[filter3,],plot(jitter(x=rational/100,10),y=jitter(responseB/100,5), col=(rgb(0,0,0,0.7)),xlim = c(0,1),ylim=c(0,1.0),pch=16,cex=.8,xlab = 'rational probability', ylab='participant reported probability', main=plotName));
#   plot2<-plot2+with(data[filter3,],abline(0,1,lty=5, lwd=1.8))
#   #     plot2<-plot2 +  text(0,0.95,labels = paste('corr =',participantCorrelation,'
#   # p-value = ',pCorrpVal),adj=c(0,0))
#   
#   #points(x=data$rational[filter3]/100, y=data$stimMed[filter3]/100,col='black', pch=1, cex=1.)
# }
# 
# filter5<-filter2 &  (data$pValofCorr <= 0.01)
# medValues3<- aggregate(responseB~stimID, data = subset(data,filter5),'median') 
# names(medValues3) <- c('stimID','stimMed3')
# data <- merge(data,medValues3,by='stimID')
# plotName<- paste('participant responses v. rational probability
#                  (un-correlated participants only)')
# plot3<-with(data[filter5,],plot(jitter(x=rational/100,20),y=jitter(responseB/100,15), col=(rgb(0,0,0,0.07)),xlim = c(0,1),ylim=c(0,1.0),pch=16,cex=.8,xlab = 'rational probability', ylab='participant reported probability', main=plotName));
# plot3<-plot3+with(data[filter5,],abline(0,1,lty=5, lwd=1.8))
# points(x=data$rational[filter5]/100, y=data$stimMed3[filter5]/100,col='black', pch=1, cex=1.)

# 
# length(unique(data[filter5,]$sonaID))
# 
# 
# hist(tapply(1:nParticipants,1:nParticipants,function(i){
#   cor.test(with(data[filter5 & (data$sonaID %in% participants[i]),], tapply(rational, stimID, mean)),
#            with(data[filter5 & (data$sonaID %in% participants[i]),], tapply(responseB, stimID, mean)))$estimate}),
#   breaks=7*4,
#   xlab = 'participant correlation to model result',
#   ylab = 'number of participants',
#   main =  paste('histogram of participant correlation with analytic result'),probability=FALSE)
# 
# plot3<-with(data[filter2,],plot(x=rational/100, y=stimMed,main=))
# plot3<-with(data[filter2,],abline(0,1))
# plot4<-with(data[filter2,],plot(jitter(x=rational/100,20),y=jitter(adjustedValue/100,10), col=(rgb(0,0,0,0.15)),xlim = c(0.5,0.91),ylim=c(0,1),pch=16,cex=.9,xlab = 'rational probability', ylab='participant reported probability', main='participant responses v. rational probability'));
# plot4<-plot4+with(data[filter2,],abline(0,1,lty=5, lwd=1.8))
# 
# points(x=data$rational[filter2]/100, y=data$stimAdjMed[filter]/100,col='black', pch=1, cex=1.)
# 
# plot5 <- plot(propCorrect,col='black', pch=1, cex=1.,lwd=2, xlab = 'rational probability', ylab='proportion of responses', main='proportion of responses consistent with rational analysis')
# 
# 
# nSets <- 15
# pattern<-data.frame(same = factor(rep(0,nSets)), opp = factor(rep(0,nSets)), pRational = (rep(0,nSets)))
# pattern$same<-unique(data$stimID[data$pattern %in% 'same'])
# pattern$opp <-unique(data$stimID[data$pattern %in% 'opp'])
# pattern$pRational <-(stims$rational[stims$stimID %in% pattern$opp])
# 
# delta<-data.frame(sameBias = rep(0,nSets), pRational = rep(0,nSets))
# for (i in 1:nSets){
#   delta$sameBias[i]<-(medValues$stimMed[(medValues$stimID %in% pattern$same[i])] - 
#                         medValues$stimMed[(medValues$stimID %in% pattern$opp [i])]) 
#   delta$pRational[i]<-pattern$pRational[i]
# }
# 
# nf <- layout(matrix(c(1,2),1,2,byrow=TRUE), widths=c(2.5,1), heights=c(2,1), TRUE)
# par(mar=c(1,3,1,0))
# diffPlot <- with(delta,plot(x=pRational/100, y=sameBias/100,ylim = c(-.20,.20),cex=1.5,cex.main = 0.8,col=1, pch=18, xlab = 'rational probability',ylab = 'same-opp median probability',main = "")) #'difference between same-opposite median responses'
# #lines(delta$pRational/100, delta$sameBias/100)
# diffPlot <- diffPlot + abline(h=0,lty=5,lwd=1.5)
# par(mar=c(1,0,1,2))
# boxPlot <- boxplot(delta$sameBias/100, axes=FALSE, pch=18, cex=1.5,ylim = c(-.20,.20))  
# 
# cor(data$rational[filter2],data$value[filter2], use="complete.obs", method="pearson") 
# cor(x=data$rational[filter2], y=data$stimMed[filter2], use="complete.obs", method="pearson") 
# cor.test(data$rational[filter2],data$adjustedValue[filter2], use="complete.obs", method="pearson") 
# cor.test(x=data$rational[filter2], y=data$stimAdjMed[filter2], use="complete.obs", method="pearson") 
# t.test(delta$sameBias)

#dataFit <- function(a,b,displayDiagnostics=FALSE, displayPosteriors=FALSE, saveOutput=FALSE, imageType="pdf"){
displayDiagnostics=FALSE
displayPosteriors=FALSE
saveOutput=FALSE
imageType="pdf"

setwd(dir)

#  brogers: uses rjags & DBDA2E-utilities.R from Kruschke, J. K. (2014). Doing Bayesian Data Analysis
graphics.off()

require(rjags)
source("DBDA2E-utilities.R")
fileNameRoot <- 'dataFit'

#------------------------------------------------------------------------------
# FUNCTION DESCRIPTION
# Input: This function takes ...
#------------------------------------------------------------------------------
# INPUT DATA
if (!saveOutput){fileNameRoot <- NULL}

#------------------------------------------------------------------------------
# FORMAT DATA

#Add dummy variable on the end to track predictive posteriors

#need to rank subject for jags?
subject <- data$subjectID
response <- data$responseB
rational <- data$rational
nSubject <- length(unique(subject))
nResponse <- length(response)
  
  #Data list for rjags
  dataList = list(
    subject = subject ,
    response = response ,
    rational = rational/100 , 
    nSubject = nSubject,
    nResponse = nResponse
    #pInt = 
    #pExt=
  ) #end dataList
#------------------------------------------------------------------------------
# THE MODEL.
modelString = "
model {
for ( i in 1:nResponse ) {
response[i] ~ dnorm(predicted[i],tau[subject[i]])
predicted[i] <- (0^(flip[i]) * rawPredicted[i]) + (0^(1-flip[i]) * (1 - rawPredicted[i]))
flip[i] ~ dbin(errorRate[subject[i]],1)
rawPredicted[i] <- bump[subject[i]]+scale[subject[i]]/(1+exp(-shape[subject[i]] * (rational[i] - location[subject[i]]))) 
#                      scale[subject[i]]/(1+exp(-shape[subject[i]] * (pInt[i] - location[subject[i]]))) +
#                      scale[subject[i]]/(1+exp(-shape[subject[i]] * (pExt[i] - location[subject[i]]))) 

}
for ( j in 1:nSubject ) {
tau[j] ~ dgamma(3,1)
shape[j] ~ dgamma(3,1)
scale[j] ~ dbeta(1.1,1.1)
errorRate[j] <-0 #~ dbeta(1.1,1.1)
location[j] ~ dbeta(1.1,1.1)
bump[j] ~ dbeta(1.1,1.1)
}
}
" # close quote for modelString
writeLines( modelString , con="TEMPmodel.txt" )

#------------------------------------------------------------------------------
# RUN CHAINS
# initsList = list( theta=c(0.5,0.5,0.5) , m=3 ) #not necessary with simple models

parameters = c("shape","tau","errorRate","scale","location","predicted","rawPredicted") 
adaptSteps    =  300      # Number of steps to "tune" the samplers.
burnInSteps   =  1000      # Number of steps to "burn-in" the samplers.
numSavedSteps = 500      # Total number of steps to save collectively from all chains. 
nChains   = 3               # Number of chains to run.
thinSteps = 1               # Number of steps to "thin" (1=keep every step).
nPerChain = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.

# Create, initialize, and adapt the model:
jagsModel = jags.model( "TEMPmodel.txt" , data=dataList ,  n.chains=nChains , n.adapt=adaptSteps )  #inits=initsList ,
cat( "Burning in the MCMC chain...\n" ) # Burn-in:
update( jagsModel , n.iter=burnInSteps )
cat( "Sampling final MCMC chain...\n" ) # The saved MCMC chain:
codaSamples = coda.samples( jagsModel , variable.names=parameters , n.iter=nPerChain , thin=thinSteps )
if (saveOutput){
  save(codaSamples,file=paste0(fileNameRoot,"Mcmc.Rdata"))
}
#------------------------------------------------------------------------------- 
# CHAIN DIAGNOSTICS
if (displayDiagnostics){
  parameterNames = varnames(codaSamples) # get all parameter names
  for ( parName in parameterNames ) {
    diagMCMC( codaSamples , parName=parName ,saveName=fileNameRoot , saveType=imageType)
  }
} #end if (displayDiagnostics)
#------------------------------------------------------------------------------
# ANALYSE RESULTS

# Convert coda-object codaSamples to matrix object for easier handling.
mcmcMat = as.matrix( codaSamples , chains=TRUE )
chainLength <- nrow(mcmcMat)

combined <- data.frame(rbind(codaSamples[[1]],codaSamples[[2]],codaSamples[[3]]))
data$predicted<-colMeans(combined)[grep('predicted',names(combined))] 
data$rawPredicted<-colMeans(combined)[grep('rawPredicted',names(combined))] 

with(data,(plot(rational,predicted)))

# 
# #------------------------------------------------------------------------------
# # OUTPUT & GRAPHICS
# 
# if (displayPosteriors){
#   #Open graphics device and specify layout of graphics
#   openGraph(width=12,height=4)
#   par( mar=0.5+c(4,3,2,3) , mgp=c(2.0,0.7,0) )
#   fontSize = 1
#   #Plots 1-4 are for the distribution on the categorical proportion
#   plotPost( model , breaks=seq( 0.9,2.1,0.2) , cenTend="mean" , xlab="Chance vs Pattern" , main="Prior Selection")
#   if (saveOutput){
#     saveGraph( file=paste0(fileNameRoot,"Post") , type=imageType )
#   }
#   
#end if (displayPosteriors)
#return(#targetPrediction)
#end function 
#}
