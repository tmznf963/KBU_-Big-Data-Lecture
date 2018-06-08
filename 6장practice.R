m<-matrix(data=cbind(rnorm(30,0),rnorm(30,2),rnorm(30,5)),
          nrow=30,ncol=3)
m

apply(m,2,function(x)length(x[x<0]))

apply(m,2,function(x)mean(x[x>0]))

library(lattice)

lapply(barley, function(x)unique(x))
lapply(barley,function(x)unique(x))
sapply(barley,function(x)unique(x))

sapply(barley, function(x)length(unique(x)))

tapply(iris$Petal.Length,iris$Species, mean)
tapply(vector, index, function)

plot(iris$Petal.Length,iris$Petal.Width,pch=21,
     bg=c("red","green","blue")[unclass(iris$Species)])

#ЦђБе tapply
pl.mean<-tapply(iris$Petal.Length,iris$Species,mean)
barplot(pl.mean)

apply(m,2,function(x)length(x[x<0]))

apply(m,2,function(x)mean(x[x>0]))

lapply(barley, function(x)unique(x))
sapply(barley,function(x)unique(x))

sapply(barley,function(x)length(unique(x)))
head(barley)

pl.mean<-tapply(iris$Petal.Length,iris$Species,mean)
barplot(pl.mean)
pl.mean
plot(iris$Petal.Length,iris$Petal.Width,pch=21,
     bg=c("red","green","blue")[unclass(iris$Species)])

pl.mean<-tapply(iris$Petal.Length,iris$Species,mean)
barplot(pl.mean)
apply(m,2,function(x)length(x[x<0]))
apply(m,2,function(x)mean(x[x>0]))
