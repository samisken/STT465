
### Simple Computations in R#########

 2+3-19*45 # R as a calculator

rmIlix= c(23,25, 24, 13, 25, 20) # Numerical Data are stored as vectors
plot(x) # Plot points of data set
hist(x) # histogram of your data
mean(x)
sd(x)
var(x)
summary(x)

y=c(45,12,45,22,50,45)
summary(x,y)
5*x  # multiple all elements in x by 5
x+y # Vector addition
x*y  # elementwise multiplication

######## Histogram for weight of cars ######
install.packages("openintro")
require(openintro)
head(cars)  # view first few data points in cars data

hist(cars$weight, main = "Weight of cars", xlab="weights(lb)")
ct <- table(cars$type)
barplot(ct)

##############################################
## Basic R for Bayesian Needs######

firstVec = c(20, 19, 65, 55)
length(firstVec)

######Vector Subsetting ########
# Use the vector x above;

x[3] # extracting one element
x[3]=22 # replacing the third element
x[-3] # `-` can be used to remove entries
x[x<=20] # subset all x less than or equal to 20
# Sequence
w=1:10 # creates a sequence from 1:10
w
w[3]=25 # replace third element by 25
w

# Indexing and replacement can also be done with TRUE/FALSE
x=1:4
x[c(TRUE,FALSE,FALSE,FALSE)]

# Vectors can be of any type
x=c("a","b","hello")
x

####Data Frame####
### Use to store data of different types: numeric, character, etc##
N=90
x1=sample(c("F","M"),size=N,replace=T)
x2=runif(min=20,max=65,n=N) # samples 90 values from a uniform distribution with support on [20,65]
DATA=data.frame(sex=x1,age=x2) # build data table for sex and age
DATA$height=ifelse(DATA$sex=="F",170,175)+rnorm(n=N,sd=sqrt(40)) # adding a new variable can be done this way

head(DATA)    # prints the first rows of the data to the screen
tail(DATA)    # prints the last rows of the data to the screen
str(DATA)     # tells you the structure (class, dimensions) of the object
fix(DATA)     # shows the data frame in a spread-sheet-like fashion
summary(DATA) # most objects in R have a summary method, note summaries depend upon the type.

## Indexing  
DATA[,1]
DATA$sex  # you can index by variable name, same for replacement.


#####Loops#####
###for & while loops##
for(i in 1:10){
  print(i)
}

## We can iterate over any vector
for(i in c("a","b","zzz")){
  print(i)
}

## While loop
x=0
while(x<=5){
  x=x+2
  print(x)
}
 ####Functions###
###In built functions####
x=c(10,20,30)
log(x)
##write your own functions####
first_func = function(x,y)
{
  result =3*x^2+y^2 -1
  print(result)
}
first_func(10,12)

#### Simulating from a random probability distribution###
x1 <- rnorm(10000,10,2.2)   # draw 10,000 samples from a normal distribution with mean=10 and sd=2.2
x2 <- rnorm(10000,11.5,3.5)   # draw 10,000 samples from a normal distribution with mean=11.5 and sd=3.5
plot(density(x1),ylab="Density",col="red")
lines(density(x2),col="blue")
legend("topright",legend=c("mean=10, sd=2.2","mean=11.5, sd=3.5"),col=c("red","blue"),pch=20)


###### Gibbs Sampling Tools###
install.packages("R2OpenBUGS")
library(R2OpenBUGS)

### You can installl "JAGS" (rjags) in similar manner.