####### Chapter 5 ###
###Part 1: Inference on mu assuming sigma is known ####

# First, let us see how we can generate random samples from a normal distribution

set.seed(10142019)
mu=20  #mean = 20
Vr = 3  # variance = 3

x=rnorm(10000,mean=mu,sd=sqrt(Vr)) # generate  a sample of 10,000 values from normal dist with mean = 20 , sd = sqrt(3)


mean(x)  # mean of our sample
var(x)   # variance of our sample

hist(x,30, prob = TRUE)  # histogram of our sample
curve(dnorm(x, mean=mean(x), sd=sd(x)), add = TRUE, col = "blue")

y=100+2*x

var(y)
mean(y)

hist(y,30)

#################################################

### Compare C.I for MLE estimate & Cred. Int for Bayesian Inference on mu with known sigma

## Assume generate Data with sample mean of 45 (same mean, varying sample size)


ybar=45 #Set as fixed
n=c(5,20,50) # 3 different sample sizes
Vry=10 # variance of the data #Set as fixed

#### Parameters for prior density (usually called hyperparameters)
#Priors 
# mu~N(20,5)
mu0=20 # prior mean
Vmu=5  # prior variance

## MLE
varMLE=Vry/n

# Approx. 95% CI for the the Max. Likelihod estimator.
CI=rbind(45+c(-1.96,1.96)*sqrt(varMLE[1]),
         45+c(-1.96,1.96)*sqrt(varMLE[2]),
         45+c(-1.96,1.96)*sqrt(varMLE[3])
)
#3 confidence intervals , one for each sample size 
print(CI)  # Note the changes in the width of the C.I with sample size.

## Posterior density

tau_data=1/Vry  # precision of a single data-point , easier than writing 1/\sigma^2 all the time 
tau_mu=1/Vmu   # prior precision

print(tau_data)
print(tau_mu)



postMean=c(  (tau_mu*mu0 + n[1]*ybar*tau_data)/( tau_mu + n[1]*tau_data) ,
             (tau_mu*mu0 + n[2]*ybar*tau_data)/( tau_mu + n[2]*tau_data),
             (tau_mu*mu0 + n[3]*ybar*tau_data)/( tau_mu + n[3]*tau_data)
)

print(postMean)

postVar=1/c(	n[1]*tau_data+tau_mu ,
             n[2]*tau_data+tau_mu  ,
             n[3]*tau_data+tau_mu
)

show(postVar)

## Posterior 95% cred. Interval ( Quantile-based)
CRI=cbind(qnorm(mean=postMean,sd=sqrt(postVar),p=.025),
         
         qnorm(mean=postMean,sd=sqrt(postVar),p=.975))
show(CRI)
show(CI)
## In this case, sample size affects the width of the frequentist CI but not the midpoint
CI[,1]+(CI[,2]-CI[,1])/2 ## Mid point of C.I

## However, because the prior mean is not equal to the data mean, in the Bayesian CR, the midpoint also changes.
CRI[,1]+(CRI[,2]-CRI[,1])/2 ##Mid point of CR.I

### Plot of Prior and Posterior densities:

MU<-seq(0,50,length=600)
plot(MU,dnorm(MU,postMean[3],sqrt(postVar[3])),type="l",col="black",xlab=expression(mu),
     ylab=expression(paste(italic("p("),theta,"|",italic(y[1]),"...",
                           italic(y[n]),",",sigma^2,")",sep="")),lwd=2)
lines(MU,dnorm(MU,mu0,sqrt(Vmu)),type="l",col="gray",lwd=2)
dev.off()
 
##################################################################

###Part 2: Inference on sigma^2 assuming mu is known ####

## Using Class example:

y = c(8.4, 10.1, 9.4) # data
n = length(y)
mu=8 # known mean

#### Parameters for prior density (usually called hyperparameters)

a0=3 # prior mean
b0=2  # prior variance
options(digits=4)
a=a0+n/2; sigmu2=mean((y-mu)^2); b=b0+(n/2)*sigmu2 #a* and b* 
c(a,sigmu2,b)


# Inference on sigma2 = 1/lambda 

sig2priormean=b0/(a0-1)  # Prior mean for sigma^2

sig2postmean=b/(a-1)

### 95% Cred. Int

sig2cr=1/qgamma(c(0.975,0.025),a,b)

c(sig2priormean, sig2postmean, sig2cr)

sig2v=seq(0.01,10,0.01)
prior=dgamma(1/sig2v,a0,b0)/sig2v^2
post=dgamma(1/sig2v,a,b)/sig2v^2;
like=dgamma(1/sig2v,a-a0-1,b-b0+0)/sig2v^2
plot(c(0,10),c(0,1.2),type="n",
     main="Inference on the model variance parameter",
     xlab="sigma^2 = 1/lambda",ylab="density/likelihood")
lines(sig2v,prior,lty=1,lwd=2); lines(sig2v,like,lty=2,lwd=2)
lines(sig2v,post,lty=3,lwd=2)

points(sig2cr,c(0,0),pch=rep(16,2),cex=rep(1.5,2))
legend(1.8,1.2,
       c("Prior density","Likelihood function (normalised)","Posterior density"),
       lty=c(1,2,3),lwd=c(2,2,2))

