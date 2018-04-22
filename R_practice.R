library(MASS)
data(Boston)
head(Boston)

boston.sub <- Boston[c('lstat','indus','nox','rm','medv')]
boston.sub

library("corrgram")
corrgram(cor(boston.sub),upper.panel=panel.conf)

par(mfrow=c(2,2))

plot(boston.sub[,name],boston.sub[,'medv'],
     xlab=name,ylab='medv')

sapply(names(boston.sub[1:4]),function(name){
  plot(boston.sub[,name],boston.sub[,'medv'],
       xlab=name,ylab = "medv")
})

par(mfrow=c(1,1))
par(mfrow=c(2,2))
sapply(names(boston.sub[1:4]),function(name){
  plot(boston.sub[,name],boston.sub[,"medv"],
       xlab=name,ylab = "medv")
})
pairs(boston.sub)
boston.sub
cor.test(c(1,2,3,4,5),c(1,0,3,4,5),method = "pearson")

boston.sub<-Boston[c("","","","")]

corrgram(cor(boston.sub),upper.panel = panel.conf)

corrgram(cor(boston.sub),upper.panel = panel.conf)

corrgram(cor(boston.sub),upper.panel = panel.conf)

par(mfrow=c(2,2))
sapply(names(boston.sub[1:4]),function(name){
  plot(boston.sub[,name],boston.sub[,"medv"],
       xlab=name,ylab = "medv")
})
pairs(boston.sub)
cor.test(c(""),c(""),method = "pearson")

cor.test(c(1,2,3,4,5),c(1,0,3,4,5),method="pearson")

corrgram(cor(boston.sub),upper.panel = panel.conf)
boston.sub<-Boston[c("","","","")]

with(iris,{
  plot(NULL,xlim=c(0,5),ylim=(0,10),
       xlab="width",ylab="length",main="iris",type="n")
  points(Sepal.Width,Sepal.Length,cex=.5,pch=20)
  points(Petal.Width,Petal.Length,cex=.5,pch="+",col="ff0000")
})
par(mfrow=c(1,1))
with(iris,{
  plot(NULL,xlim=c(0,5),ylim=c(0,10),
       xlab="w",ylab="l",main="iris",type="n")
  points(Sepal.Width,Sepal.Length,cex=.5,pch=20)
  points(Petal.Width,Petal.Length,cex=.5,pch="+",col="#ff0000")
})

x<-seq(0,2*pi,0.1)
x
y<-sin(x)
y
plot(x,y,cex=.5,col="red")
lines(x,y)

boxplot(iris$Sepal.Length)
boxstats<-boxplot(iris$Sepal.Width)
boxstats
data(iris)
sv<-subset(iris,Species=="setosa"|Species=="versicolor")
sv
sv$Species <- factor(sv$Species)
sv$Species

boxplot(Sepal.Width~Species,data=sv,notch=T)

sv<-subset(iris,Species=="setosa"|Species=="versicolor")
sv$Species <- factor(sv$Species)
boxplot(Sepal.Width~Species,data=sv,notch=T)

plot(density(iris$Sepal.Width))
hist(iris$Sepal.Width,freq=F)
lines(density(iris$Sepal.Width))
