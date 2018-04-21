v <- c(10,20,30,40,50)
v

x<- c(1,"a",2,"b")
x

fruits <- c("apple","melon","banana","grape","tomato","potato")
fruits

sort(fruits)
sort(fruits,decreasing = T)

fruits[c(2,4,6)]

fruits[seq(2,6,by=2)]

b<-c(1:8,11:18,111:118)
b
dim(b) <-c(2,4,3)
b

x = matrix(1:12,nrow=3)
x

t(x)
xr1=x[1,]
xr1

xc3 = x[,3]
xc3

xs=x[2:3,2:3]
xs

data("state")
head(state.x77)
state.x77[3,8]
state.x77[c(5,22,44),c(1,4,7)]
state.x77[-(5:49),3:5]
state.x77[state.x77[,"Income"]>4000,]
state.x77[state.x77[,2]>4000,]

data("iris")
head(iris)
iris[iris[,"Sepal.Width"]>3.5,]
iris[iris[,"Species"]=="versicolor",]

names(iris)
names(iris)[grep("al.wid",names(iris),ignore.case = T)]
head(iris[,grep("Petal",names(iris))])

names(iris)
names(iris)[grep("al.wid",names(iris),ignore.case = T)]
head(iris[,grep("Petal",names(iris))])

names(iris)
names(iris)[grep("al.wid",names(iris),ignore.case = T)]
names(iris[,grep("Petal",names(iris))])

iris[iris[,"Sepal.Width"]>3.5,]
iris[iris[,"Species"]=="versicolor",]
