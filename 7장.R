#아래 코드를 이용해 Boston 데이터 불렁기

library(MASS)
data(Boston)
head(Boston)
#열'lstat','indus','nox','rm','medv'만을 선택하여 변수

boston.sub<-Boston[c('lstat','indus','nox','rm','medv')]
boston.sub[1:4]

install.packages("corrgram")
library("corrgram")

corrgram(cor(boston.sub),upper.panel = panel.conf)

sapply
names(boston.sub)
plot(boston.sub[,name],boston.sub[,'medv'],
     xlab=name, ylab='medv')

par(mfrow = c(2,2))

#sapply(boston.sub[1:4],function(x)unique(x))

sapply(names(boston.sub[1:4]),function(name){
  plot(boston.sub[,name],boston.sub[,'medv'],
       xlab=name, ylab = 'medv')
})

pairs(boston.sub)

cor.test(c(1,2,3,4,5),c(1,0,3,4,5),method = "pearson")

m <- matrix(data=cbind(rnorm(30,0),rnorm(30,2),rnorm(30,5)),nrow=30,ncol=3)
m
