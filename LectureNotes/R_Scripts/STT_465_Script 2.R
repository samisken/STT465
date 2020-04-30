############################################################################
### Form Example 2: Student Happiness about social life########

theta <- seq(from=0,to=1,length=250)
n=200   # sample size
ysu = 140 # number of successes
yfl = n - ysu
par(mfrow=c(1,2)) # set up Plotting environment

# Plot of likelihood vs theta
plot(theta, theta^ysu*(1-theta)^yfl, type="l",xlab=expression(theta))

# Plot of Posterior vs theta
plot(theta,dbeta(theta,ysu+1,yfl+1),type="l",ylab= "posterior",
     xlab=expression(theta))
abline(h=1,col="gray")

dev.off()

# Posterior Means & Variance
a= ysu +1
b= yfl +1
Pmean = a/(a+b)
Pmean
Pvar = a*b/((a+b)^2*(a+b+1))
Pvar
Psd=sqrt(Pvar)
Psd

### Compare to MLE##
Smean = ysu/n
Smean
Svar = Smean*(1-Smean)/n
Svar
Ssd = sqrt(Svar)
Ssd

##### Consider a small sample size ???