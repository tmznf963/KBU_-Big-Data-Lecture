library(MASS)
data(Boston)
head(Boston)
boston.sub<-Boston[c("lstat",'indus','nox','rm','medv')]
head(boston.sub)

install.packages("corrgram")
library("corrgram")
corrgram(cor(boston.sub),upper.panel = panel.conf)

par(mfrow=c(2,2))
sapply(names(boston.sub[1:4]),
       function(name){
  plot(boston.sub[,name],boston.sub[,'medv'],pch=21,bg="blue",
       xlab=name,ylab='medv')
})

par(mfrow=c(2,2))
sapply(names(boston.sub[1:4]),function(name){
  plot(boston.sub[,name],boston.sub[,'medv'],
       xlab=name,ylab='medv')
})
par(mfrow=c(2,2))
par(mfrow=c(2,2))
par(mfrow=c(2,2))
par(mfrow=c(2,2))
sapply(names(boston.sub[1:4]),function(name){
  plot(boston.sub[,name],boston.sub[,'medv'],
       xlab=name,ylab = 'medv')
})

pairs(boston.sub)
pairs(boston.sub)
par(mfrow=c(2,2))
sapply(names(boston.sub[1:4]),function(name){
  plot(boston.sub[,name],boston.sub[,'medv'],
       xlab=name,ylab='medv')
})

pairs(boston.sub)

cor.test(c(1,2,3,4,5),c(1,0,3,4,5),method="pearson")

cor.test(c(1,2,3,4,5),c(1,0,3,4,5),method="pearson")

cor.test(c(1,2,3,4,5),c(1,0,3,4,5),method="pearson")

#난수생성 및 분포함수
x<-seq(-3,3,length=200)
x
plot(x,dnorm(x,mean=0,sd=1),type='l',
     main="Normal distribution,X~N(0,1)")

plot(x,pnorm(x,mean=0,sd=1),type='l',
     main="Cumulation normal distribution,X~N(0,1)")

r<-rnorm(1000000,mean=0,sd=1)
hist(r)

rnorm(100,0,10)#갯수,0,평균값
plot(density(rnorm(1000000,0,10)))

mean(1:5)
var(1:5)
sum((1:5-mean(1:5))^2)/(5-1)
fivenum(1:11)
summary(1:11)

x<-factor(c("a","b","c","c","c","d","d"))
x
table(x)
which.max(table(x))
names(table(x))[3]

table(c("a","b","b","b","c","c","d"))
table(c('a','b','a','b','b','b'),c(1,2,1,2,1,2))

sample(1:45,6)
sample(1:10,replace = T,6)


xtabs(formula,data)
d<- data.frame(x=c("1","2","2","1"),
               y=c("A","B","A","B"),
               num=c(3,5,8,7))
d
xt<-xtabs(num~x+y,data=d)
xt

xy<- xtabs(num~x+y,data=d)
xy

d2<-data.frame(x=c('A','A','A','B','B'),
               result=c(3,2,4,7,6))
d2
xtabs(~x,d2)
cor(iris$Sepal.Width,iris$Sepal.Length)

cor(iris[,1:4])
