############################################################################
### HBD Credible Interval for Student Happiness about social life########

theta <- seq(from=0,to=1,length=250)
n=200   # sample size
ysu = 140 # number of successes
yfl = n - ysu



# HPD Based Credible interval ( 95% C.I)
a= ysu +1
b= yfl +1


# Plot of Posterior vs theta ( include the Quantile based C.I)
plot(theta,dbeta(theta,ysu+1,yfl+1),type="l",
     xlab=expression(theta),ylab=expression(paste(italic("p("),theta,"|y)")))

pth<-dbeta(theta, ysu+1, yfl+1)
pth<-pth
pth
ord<- order(-pth)
xpx<-cbind(theta[ord], pth[ord])
xpx<-cbind(xpx,cumsum(xpx[,2])/sum(xpx[,2]))

hpd<-function(x,dx,p){
  md<-x[dx==max(dx)]
  px<-dx/sum(dx)
  pxs<--sort(-px)
  ct<-min(pxs[cumsum(pxs)< p])
  list(hpdr=range(x[px>=ct]),mode=md) }

hpdb_d<-hpd(xpx[,1],xpx[,2],.95)$hpdr
hpdb_d
abline( v=hpdb_d  ,col=gray(0),lwd=2   )
     
Bd <- qbeta(c(0.025,0.975),a,b)
Bd     
abline(v=Bd, col ="Red")
