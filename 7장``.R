x <- seq(-3,3,length=200)
x

#난수 rnorm
#확률밀도 dnorm
#누적분포 pnorm
#분위수 qnorm

plot(x,dnorm(x,mean=0,sd=1),type="l",
     main="N d,X~N(0,1")

plot(x,pnorm(x,mean=0,sd=1),type="l",
     main="C d,X~N(0,1)")

#sd = 표준 편차
r<- rnorm(1000000,mean=0,sd=1)
hist(r)

rnorm(100,0,10)

#density = 밀도그림
plot(density(rnorm(1000000,0,10)))

mean(1:5)
median(1:5)

var(1:5)
sum((1:5-mean(1:5))^2/(5-1))

fivenum(1:11)
summary(1:11)

x<-factor(c("a","b","c","c","c","d","d"))
x
table(x)

which.max(table(x))
names(table(x))[3]

table(c("a","b","b","b","c","c","d"))

table(c('a','b','a','b','b','b'),c(1,2,1,2,1,2))

sample(1:10,5)
sample(1:10,replace=T)

install.packages("sampling")
library(sampling)
x<-strata(c("Species"),size=c(3,3,3),method="srswor",data=iris)
x

getdata(iris,x)
strata(c("Species"),size=c(3,1,1),method="srswr",data=iris)

d<-data.frame(x=c("1",'2','2','1'),
              y=c('A','B','A','B'),
              num=c(3,5,8,7))
d
xt<-xtabs(num~x+y,data=d)
xt

xtabs(num~x+y,data=d)

d2 <- data.frame(x=c("A","A","A","B",'B'),
                 result=c(3,2,4,7,6))
d2

xtabs(~x,data=d2)


#상관분석(correlation analysis)
cor(iris$Sepal.Width,iris$Sepal.Length)
cor(iris[,1:4])
install.packages("corrgram")
library(corrgram)
corrgram(cor(iris[,1:4]),type="corr",upper.panel=panel.conf)
cor.test(c(1,2,3,4,5),c(1,0,3,4,5),method="pearson")
