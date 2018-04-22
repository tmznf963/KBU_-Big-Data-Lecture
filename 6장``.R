plot(x,y)
install.packages("mlbench")
library(mlbench)
data(Ozone)
plot(Ozone$V8,Ozone$V9)

plot(Ozone$V8,Ozone$V9,xlab="ST",ylab="EMT",
     pch=21,cex=1,col="#ff0000",
     xlim=c(0,100),ylim=c(0,90))

data(cars)
str(cars)
head(cars)
plot(cars,type="l")
plot(cars,type="o",cex=1)

tapply(cars$dist,cars$speed,mean)
plot(tapply(cars$dist,cars$speed,mean),type="o",cex=0.5,
     xlab="speed",ylab="dist")

#¼±À¯Çü(lty)
plot(cars,type="l",lty="dashed")

par(mfrow=c(1,1))
opar<-par(mfrow=c(1,2))

plot(Ozone$V8,Ozone$V9,xlab="ST",ylab="EMT",main="Ozone")
plot(Ozone$V8,Ozone$V9,xlab="ST",ylab="EMT",main="Ozone2")
par(opar)

head(Ozone)

plot(Ozone$V6,Ozone$V7,xlab="W",ylab="H",main="Ozone",pch=20,cex=.5)

plot(jitter(Ozone$V6),jitter(Ozone$V7),xlab="W",ylab="H",main="Ozone",pch=20,cex=.5)

plot(iris$Sepal.Width,iris$Sepal.Length,cex=.5,pch=20,
     xlab="w",ylab="l",main="iris")
points(iris$Petal.Width,iris$Petal.Length,cex=.5,
       pch="%",col="#ff0000")

with(iris,{
  plot(NULL,xlim = c(0,5),ylim = c(0,10),
       xlab="w",ylab="l",main="iris",type="n")
  points(Sepal.Width,Sepal.Length,cex=.5,pch=20)
  points(Petal.Width,Petal.Length,cex=.5,pch="%",col="#ff0000")
})

x<-seq(0,2*pi,0.1)
x
y<-sin(x)

plot(x,y,cex=.6,col="red")
lines(x,y)

plot(cars)
lines(lowess(cars))
plot(lowess(cars))
plot(cars,xlim=c(0,25))
abline(a=-5,b=3.5,col="red")

plot(iris$Sepal.Width,iris$Sepal.Length,cex=.5,pch=20,
     xlab="width",ylab = "length")
points(iris$Petal.Width,iris$Petal.Length,cex=.5,
       pch="+",col="#FF0000")
legend("topright",legend=c("Sepal","Petal"),
       pch=c(20,43),cex=.9,col=c("black","red"),bg="gray")

boxplot(iris$Sepal.Width)
boxplot(iris$Sepal.Length)
boxstats<-boxplot(iris$Sepal.Width)
boxstats

sv<- subset(iris, Species=="setosa" | Species=="versicolor")
sv
sv$Species <- factor(sv$Species)
boxplot(Sepal.Width~Species,data=sv,notch=T)

sv<-subset(iris,Species=="setosa" | Species=="versicolor")
sv$Species <- factor(sv$Species)
boxplot(Sepal.Width~Species,data=sv,notch=T)

hist(iris$Sepal.Width)
x<-hist(iris$Sepal.Width,freq = F)
hist(iris$Sepal.Width,breaks = c(1,2,3,4,5))
x

plot(density(iris$Sepal.Width))
hist(iris$Sepal.Width,freq=F)
lines(density(iris$Sepal.Width))
barplot(x)
barplot(tapply(iris$Sepal.Width,iris$Species,mean))


str(Titanic)
Titanic

mosaicplot(~Class+Survived,data=Titanic,color=T)
