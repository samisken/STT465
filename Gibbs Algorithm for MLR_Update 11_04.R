### Gibbs sampler for a Muliple Linear Regression Model (MLR) ###

###### DATE : 11/01/2019    #########
  

##  y (nx1) response  ###
### X (nxp) the covariate matrix for effects  ###
### nIter, the number of samples to be collected, set by default to 10,000.
##  Hyper-parameters: df0 and S0 (the scale and degree of freedom for the scaled-inverse chi-square prior)
##                  : b0 and varB (the mean and variance of the normal prior assigned to effects).

 
#### Use the GOUT data as an example:
set.seed(1112019)
GOUT=read.table("C:/Users/STTSurface001Admin/Desktop/Teaching/STT 465 Bayesian/gout.txt",header=FALSE)
colnames(GOUT)=c('sex','race','age','serum_urate','gout') # You don't need this!

GOUT$sex=factor(GOUT$sex,levels=c('M','F'))
GOUT$race=as.factor(GOUT$race)

head(GOUT, 11)
dim(GOUT)                                                                                                                                                                                   

### Covariate Matrix 

dF=ifelse(GOUT$sex=='F',1,0) # a dummy variable for female sex
dW=ifelse(GOUT$race=='W',1,0) # a dummy variable for White race

# Incidence matrix for intercept and effects of sex, race and age
X=cbind(1,dF,dW,GOUT$age) 
head(X)
dim(X)
y=GOUT$serum_urate
head(y)

### Gibbs Sampler #######

gibbsMLR=function(y,X,nIter=10000,df0=4,S0=var(y)*0.8*(df0-2),b0=0,varB=1e12,verbose=500){
  
  ## Objects to store samples
  p=ncol(X); n=nrow(X)
  B=matrix(nrow=nIter,ncol=p,0) # create a matrix to store the gibbs sample for beta
  varE=rep(NA,nIter)      # .. for error variance
  
  ## Initialize
  B[1,]=0     # initial values for slopes
  B[1,1]=mean(y)  # initial value for y-intercept
  b=B[1,]
  varE[1]=var(y)  # initial error variance
  resid=y-B[1,1]  # centered y (orthogonal)
  
  ## Computing sum x'x for each column
  SSx=colSums(X^2)
  
  for(i in 2:nIter){
    # Sampling regression coefficients
    for(j in 1:p){
      A=SSx[j]/varE[i-1]+1/varB
      Xy= sum(X[,j]*resid)+SSx[j]*b[j]  # equivalent to X[,j]'(y-X[,-j]%*%b[-j])
      rhs=Xy/varE[i-1]  + b0/varB  # Numerator of beta^tilda_k
      condMean=rhs/A
      condVar=1/A
      b_old=b[j]
      b[j]=rnorm(n=1,mean=condMean,sd=sqrt(condVar))
      B[i,j]=b[j]  
      resid=resid-X[,j]*(b[j]-b_old) # updating residuals
    }
    # Sampling the error variance  
    RSS=sum(resid^2)
    DF=n+df0
    S=RSS+S0
    varE[i]=S/rchisq(df=DF,n=1)
    
   ## if(i%%verbose==0){ cat(' Iter ', i, '\n') }
  }
  
  out=list(effects=B,varE=varE)
  return(out)
}

samples=gibbsMLR(y,X,nIter=10000,varB=1000) #large varB should give estimates close to OLS
head(samples$varE) # samples for the error variance
head(samples$effects)    # samples for the effects


colMeans(samples$effects[-(1:1000),]) # our estimates are very similar to OLS, except for the intercept 
# because internally we centered all the columns of X, except the 1st one.

lm(y~X-1) # OLS


######## Gibbs Analysis  11/04/2019 #######

library(rjags)
library(coda)

x = as.ts(samples$effects[,2])
SAMPLES<-as.mcmc(x) # converting your samples into an MCMC object
autocorr(SAMPLES, lags = c(0, 1, 5, 10, 50), relative=TRUE)


plot(SAMPLES) # this produces both density and trace plot
plot(as.vector(SAMPLES),type='o',cex=.5,col=4)

burnIn=1:100
SAMPLES1=SAMPLES[-burnIn] # removing burn-in
plot(as.vector(SAMPLES1),type='o',cex=.5,col=2)
summary(SAMPLES1) # Discuss Monte Carlo Error

effectiveSize(SAMPLES1)

