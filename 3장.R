myVec<-c(1,2,NA,4)
myVec[!is.na(myVec)]

data("iris")
head(iris)
for (i in 1:nrow(iris)) {
  if(iris$Sepal.Length[i]>5)
    output[i] <- "greater than 5"
  else
    output[i] <- "less than 5"
}
output[i]

for(i in nrow(iris)){
  if(iris$Sepal.Length[i]>5)
    output[i]<-"greater than 5"
  else
    output[i]<-"less than 5"
}
output[i]

output <- ifelse(iris$Sepal.Length>5,"greater than 5","less than 5")
output[6]


myFunc<-function(x){
  if(x['Sepal.Length']>5)
    "greater than 5"
  else
    "less than 5"
}
output <- apply(iris,1,FUN = myFunc)
output


which(iris$Species =="versicolor")
subset(iris,iris$Species=="versicolor")
iris[which(iris$Species=="versicolor"),]

A <- matrix(1:9,nrow=3)
A

B<- matrix(10:18,nrow=3)
B
cbind(A,B)
rbind(A,B)
