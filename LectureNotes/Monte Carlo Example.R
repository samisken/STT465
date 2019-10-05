a=2 
b= 1
sy= 66
n =44


set.seed(12345)
mc100 = rgamma(100,a+sy,b+n)
mc1000 = rgamma(1000, a+sy, b+n)
mc10000 = rgamma(10000, a+sy, b+n)

mean(mc100)
mean(mc1000)
mean(mc10000)

#### 09_25_2019 Class Lecture #######

# Approximating Probabilities for arbitrary set of theta

# "Exact" Prob (1.1<theta<1.85|y):

pgamma(1.85,a+sy,b+n)-pgamma(1.1,a+sy,b+n)

# MC appproximation

mean(mc100<1.85 & mc100>1.1) 

mean(mc1000<1.85 & mc1000>1.1)

mean(mc10000<1.85 & mc10000>1.1)
