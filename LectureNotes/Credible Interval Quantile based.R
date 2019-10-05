############################################################################
### Credible Interval for Student Happiness about social life########

theta <- seq(from=0,to=1,length=250)
n=200   # sample size
ysu = 140 # number of successes
yfl = n - ysu



# Quantile Based Credible interval ( 95% C.I)
a= ysu +1
b= yfl +1

Bd = qbeta(c(0.025,0.975),a,b)
Bd
# Plot of Posterior vs theta ( include the Quantile based C.I)
plot(theta,dbeta(theta,ysu+1,yfl+1),type="l",ylab= "posterior",
     xlab=expression(theta))
abline(v=Bd, col ="Red")
