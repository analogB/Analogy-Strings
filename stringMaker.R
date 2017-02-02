require('jsonlite')
require('plyr')

outputFilename = 'textStims.txt'
creationDate = date()
index=100
reps=1  #will be multiplied by 4 for string value flipping: ones/zeros & same/oppososite
nRands=10 #number of completely random strings
stimuli = matrix(nrow=0,ncol=12)

cond_N   = c(12)   #total number of elements in each string
cond_nA  = c(9)    #number of sampled elements in string A
cond_nB  = c(7)    #number of sampled elements in string B
cond_nAB = c(5)    #number of sampled element pairs in strings A&B
cond_pA  = c(0.6,0.8)   #proportion in string A sample
cond_pB  = c(0.6,0.8)   #proportion in string B sample
cond_pAB = c(0.8,1)   #proportion in pairwise sample

#define pairSpace
defPairs=c(' ','hi','lo','hi','lo','hi','lo',' ',' ','hi','lo',
           ' ','hi','lo','lo','hi',' ',' ','hi','lo','?','?')
defPairs = matrix(defPairs,nrow=11,ncol=2)

for (x1 in 1:length(cond_N))  {N=cond_N[x1] 
for (x2 in 1:length(cond_nA)) {nA=cond_nA[x2] 
for (x3 in 1:length(cond_nB)) {nB=cond_nB[x3] 
for (x4 in 1:length(cond_nAB)){nAB=cond_nAB[x4] 
for (x5 in 1:length(cond_pA)) {pA=cond_pA[x5] 
for (x6 in 1:length(cond_pB)) {pB=cond_pB[x6] 
for (x7 in 1:length(cond_pAB)){pAB=cond_pAB[x7] 

#solve design based on parameters
design = designer(N,nA,nB,nAB,pA,pB,pAB)

#place target in design
design = place_targ(design,pA)

#check to ensure parameter compatibility
if (min(design)<0){
  print(paste('Design parameters are incompatible:N =',N,',nA =',nA,',nB =',nB,',nAB =',nAB,',pA =',pA,',pB =',pB,',pAB =',pAB))
  break
}

#build stimulus core from design
core = c()
for (i in 1:11){
  j<-0
  while (j < design[i]){ 
    core = c(core,defPairs[i,])
    j<-j+1
  }
} 
core=matrix(core,ncol=N,nrow=2)

j=1
for (valueCondition in 1:4){
  for (repetition in 1:reps){
    randSeq=sample(c(1:N)) #create a random sequence
    stim=core[,randSeq] #reorder stimulus core randomly
    stim=assign_values(stim,valueCondition) #assign values based on valueCondition
    stim=format_stim(stim,N) #format into csv text string
    stimID=paste(date,index,sep=' - ')
    stim=c(stim,0,N,nA,nB,nAB,pA,pB,pAB,1,stimID)
    stimuli<-rbind(stimuli, stim) #concat to list of stims #concat to list of stims
    j=j+1
    index=index+1
  }
}
}}}}}}}

#add on pure random stims
for (i in 1:nRands){
  stim=rand_maker(N,nA,nB,nAB) #make random stimuli
  stim=format_stim(stim,N) #format into csv text string
  stimID=paste(date,index,sep=' - ')
  stim=c(stim,1,N,nA,nB,nAB,'NA','NA','NA',1,stimID)
  stimuli<-rbind(stimuli, stim) #concat to list of stims
  index=index+1
}

#stimuli[length(stimuli)]=substr(stimuli[length(stimuli)],1,2*N+1) #remove last comma

stimuli = data.frame(stimuli)
names(stimuli)[1]='stringA'
names(stimuli)[2]='stringB'
names(stimuli)[3]='random'
names(stimuli)[4]='N'
names(stimuli)[5]='nA'
names(stimuli)[6]='nB'
names(stimuli)[7]='nAB'
names(stimuli)[8]='pA'
names(stimuli)[9]='pB'
names(stimuli)[10]='pAB'
names(stimuli)[11]='anchored'
names(stimuli)[12]='stimID'

jsonStims=toJSON(stimuli)
write(jsonStims,file=outputFilename) #make txt file

##################################################
############## FUNCTIONS #########################
##################################################

#1 design function: solves system of equations based on design parameters
designer <- function(N,nA,nB,nAB,pA,pB,pAB){
  mpAB=(pA+pB)/2 #ratio of pairwise proportion from dominant element
  coefMat=c(1,0,0,0,0,0,0,0,0,
            1,1,1,1,1,1,1,1,0,
            1,1,1,1,0,0,0,1,0,
            1,1,1,1,1,0,1,0,1,
            1,1,1,1,0,1,0,0,0,
            1,1,0,0,1,0,0,0,0,
            1,1,0,0,0,0,0,0,0,
            1,0,1,0,0,1,0,0,0,
            1,0,1,0,0,0,0,0,0)
  coefMat=matrix(coefMat,nrow=9,ncol=9)
  constMat=matrix(c(N,nA,nB,nAB,round(pA*nA),round(pB*nB),round(mpAB*nAB),round(pAB*nAB),round((1-pAB)*nAB*pA/(pA+pB))),nrow=9,ncol=1)
  out=solve(coefMat,constMat)
  return(out)
}

#2 value assignment function
assign_values <- function(stringPair, a){
  top=stringPair[1,]
  bot=stringPair[2,]
  ifelse (a==1,{ #top hi=1, bot hi=1
    top=gsub('hi','1',top)
    top=gsub('lo','0',top)
    bot=gsub('hi','1',bot)
    bot=gsub('lo','0',bot)},
    ifelse (a==2,{ #top hi=1, bot hi=0
      top=gsub('hi','1',top)
      top=gsub('lo','0',top)
      bot=gsub('hi','0',bot)
      bot=gsub('lo','1',bot)},
      ifelse (a==3,{#top hi=0, bot hi=1
        top=gsub('hi','0',top)
        top=gsub('lo','1',top)
        bot=gsub('hi','1',bot)
        bot=gsub('lo','0',bot)},
        #ifelse a==4  #top hi=0, bot hi=0
          {top=gsub('hi','0',top)
           top=gsub('lo','1',top)
           bot=gsub('hi','0',bot)
           bot=gsub('lo','1',bot)}
      )))
  out = matrix(c(top,bot),nrow=2,byrow=TRUE)
  return(out)
}
  
#3 string generation fuction #flatten, collapse, format
format_stim <- function(stimulus, N){
  out=matrix(nrow=1,ncol=2)
  for (j in 1:2){
    str=c()
    for (i in 1:N){
      str<-c(str,stimulus[j,i])
    }
    out[1,j]=as.character(paste(str,collapse=""))
  }
  return(out)
}

#4 target placement in non-random stims
place_targ <- function(design,p){
  if(runif(1,min=0,max=1)>pA) {
    if(design[7]>0) {
      design[7]=design[7]-1
      design[10]=0
      design[11]=1
    }else{
      if(design[6]>0) {
        design[6]=design[6]-1
        design[10]=1
        design[11]=0
        }else{
        print(paste('WARNING: No Empty String B Cells for Anchored Target'))
        break}}
  }else{
    if(design[6]>0) {
      design[6]=design[6]-1
      design[10]=1
      design[11]=0
    }else{
      print(paste('WARNING: No Empty String B Cells for Anchored Target'))
      break}}
return(design)
}
  
#5 random strings
rand_maker <- function(N,nA,nB,nAB){
  core=c()
  ABpairs=c('1','1','1','0','0','1','0','0')
  ABpairs=matrix(ABpairs,nrow=2)
  i=0
  while (i<nAB){
    rand=ceiling(4*runif(1,min=0,max=1))
    pair=ABpairs[,rand]
    core=c(core,pair)
    i=i+1
  }
  Apairs=c('1',' ','0',' ')
  Atarg=c('1','?','0','?')
  Apairs=matrix(Apairs,nrow=2)
  Atarg=matrix(Atarg,nrow=2)
  i=0
  while (i<nA-nAB){
    if (i==0){ #place a target on first one
      rand=ceiling(2*runif(1,min=0,max=1))
      pair=Atarg[,rand]
      core=c(core,pair)
    }else{
      rand=ceiling(2*runif(1,min=0,max=1))
      pair=Apairs[,rand]
      core=c(core,pair)
    }
    i=i+1
  }
  Bpairs=c(' ','1',' ','0')
  Bpairs=matrix(Bpairs,nrow=2)
  i=0
  while (i<nB-nAB){
    rand=ceiling(2*runif(1,min=0,max=1))
    pair=Bpairs[,rand]
    core=c(core,pair)
    i=i+1
  }
  Epairs=c(' ',' ')
  i=0
  while (i<N-nA-nB+nAB){
    pair=Epairs
    core=c(core,pair)
    i=i+1
  }
  core=matrix(core,nrow=2)
  randSeq=sample(c(1:N)) #create a random sequence
  stim=core[,randSeq] #reorder stimulus core randomly
  return(stim)
}

function(){a <<- data.fram(a=10)}() 

