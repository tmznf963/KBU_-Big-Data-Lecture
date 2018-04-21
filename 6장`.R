plot(x,y)
install.packages("mlbench")
library(mlbench)
data(Ozone)
plot(Ozone$V8,Ozone$V9)

head(Ozone)

#pch= 점의 종류
#cex= 점의 크기
plot(Ozone$V8,Ozone$V9,xlab="Sandurg Temperature",
     ylab="E1 Monte Temperature",main="Ozone",pch=20,cex=.5,
     col="#ff0000")

max(Ozone$V8)
max(Ozone$V8,na.rm = T)
max(Ozone$V9,na.rm = T)

#xlim,ylim (좌표축 값의 범위)
plot(Ozone$V8,Ozone$V9,xlab="Sandburg Temperat",
     ylab="E1 Monte Temperature",main="Ozone",
     xlim=c(0,100),ylim=c(0,90))

data(cars)
str(cars)
head(cars)
plot(cars)

plot(cars,type="l")
plot(cars,type="o",cex=.5)
#평균 tapply
plot(tapply(cars$dist,cars$speed,mean),type="o",cex=0.5,
     xlab = "speed",ylab="dist")
plot(cars,type='l',lty='dashed')
opar<-par(mfrow=c(1,2))
plot(Ozone$V8,Ozone$V9,xlab="ST",ylab="EMT",main="Ozone")
plot(Ozone$V8,Ozone$V9,xlab="ST",ylab="EMT",main="Ozone2")
par(opar)
par(mfrow=c(1,1))

#jitter = 중복을 보기위해 노이즈
plot(jitter(Ozone$V6),jitter(Ozone$v7),
     xlab="Windspeed",ylab="Humidity",main="Ozone",
     pch=20,cex=.5)

#points() = 이미 생성된 plot에 점을 추가
plot(iris$Sepal.Width,iris$Sepal.Length,cex=.5,pch=20,
     xlab="width",ylab="length",main="iris")
points(iris$Petal.Width,iris$Petal.Length,cex=.5,
       pch="+",col="#ff0000")

#type=n , 빈그래프 먼저 그리고 나중에 그리기
with(iris,{
  plot(NULL,xlim=c(0,5),ylim=c(0,10),
       xlab="width",ylab="length",main = "iris",type="n")
  points(Sepal.Width,Sepal.Length,cex=.5,pch=20)
  points(Petal.Width,Petal.Length,cex=.5,pch="+",col="#ff0000")
})

#선(lines)
x<- seq(0,2*pi,0.1)
x
y<-sin(x) #사인
y
plot(x,y,cex=.5,col="red")
lines(x,y)

#회귀분석(추세선 구하기)
plot(cars)
lines(lowess(cars))
plot(lowess(cars))

#직선(abline)
plot(cars,xlim=c(0,25))
abline(a=-5,b=3.5,col="red")

#평균직선그리기
plot(cars,xlim=c(0,25))
abline(a=-5,b=3.5,col="red")
abline(h=mean(cars$dist),lty=2,col="blue")
abline(v=mean(cars$speed),lty=2,col="green")

#곡선[curve(expr,form,to)]
curve(sin,0,2*pi)

#legend
plot(iris$Sepal.Width,iris$Sepal.Length,cex=.5,pch=20,
     xlab="width",ylab="length")
points(iris$Petal.Width,iris$Petal.Length,cex=.5,
       pch="+",col="#ff0000")
legend("topright",legend=c("Sepal","Petal"),
       pch=c(20,43),cex=.8,col=c("black","red"),bg="gray")

#상자그림(boxplot)
boxplot(iris$Sepal.Width)
iris$Sepal.Width

#conf = 신뢰구간
boxstats <- boxplot(iris$Sepal.Width)
boxstats

sv<-subset(iris,Species=="setosa"|Species=="versicolor")
sv
sv$Species<-factor(sv$Species)
sv$Species
boxplot(Sepal.Width~Species,data=sv,notch=T)

#히스토그램(hist)
hist(
  x,
  breaks="Struge",
  freq=NULL,
)
hist(iris$Sepal.Width)
x<-hist(iris$Sepal.Width,freq=FALSE)
x<-hist(iris$Sepal.Width,breaks=c(1,2,3,4,5))
x

#alfehrmfla(density)
plot(density(iris$Sepal.Width))
hist(iris$Sepal.Width,freq=F)
lines(density(iris$Sepal.Width))
iris$Sepal.Width
