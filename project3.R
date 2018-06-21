getwd()
setwd("C:/R_project")

eq <- read.csv("Earthquake1.csv")
head(eq)

#불필요 컬럼 삭제
eq <- eq[, !names(eq) %in%
           c("Depth.Error","Depth.Seismic.Stations","Magnitude.Error",
             "Magnitude.Seismic.Stations","Azimuthal.Gap","Horizontal.Distance",
             "Horizontal.Error","Root.Mean.Square")]

str(eq)
head(eq)
View(eq)

install.packages("bindrcpp")
library(bindrcpp)
# [ctrl + shit + M]으로 %>% 기호 입력
# NE == 핵폭발 135행 데이터
# eq2 == 지진 135행 데이터
# earthquake 135행까지 뽑아내기

NE <- eq %>% filter(Type == "Nuclear Explosion") #타입이 핵폭발인 것만 추출
eq <- eq %>% filter(Type == "Earthquake ") # 타입이 지진인 것만 추출
eq2 <- eq[c(1:135),] # 핵폭발의 데이터와 갯수를 맞추기위해 135개의 행 넣기

#데이터 확인
head(NE)
head(eq2)

# 핵폭발 + 지진 데이터 합치기
new_eq <- rbind(eq2,NE)
View(new_eq)


str(new_eq)

#Factor levels가 많은 Date, Time의 변환
new_eq$Date <- as.character(new_eq$Date)
new_eq$Time <- as.character(new_eq$Time)

str(new_eq)

#Factor 수준(level)에 NA값 확인
levels(new_eq$Type)
table(new_eq$Type)

levels(new_eq$Magnitude.Type)
table(new_eq$Magnitude.Type)



#테스트 데이터 분리
install.packages("caret")
library(caret)

#난수 생성시 seed 지정, 교차 검증 createDataPartition() 사용
#30%의 데이터는 test, 70%의 데이터는 train을 위해
set.seed(137)
test_idx <- createDataPartition(new_eq$Type, p=0.1)$Resample1
new_eq.test <- new_eq[test_idx, ]
new_eq.train <- new_eq[-test_idx, ]
NROW(new_eq.test)
prop.table(table(new_eq.test$Type))

NROW(new_eq.train) 
prop.table(table(new_eq.train$Type))

#의사결정나무의 형성
install.packages("tree")
library(tree)
treemod <- tree(Type ~ Date + Time + Latitude + Longitude + Magnitude.Type + Depth + Magnitude , data=new_eq.train)
treemod <- tree(Type ~ Magnitude.Type + Depth + Magnitude , data=new_eq.train)
treemod <- tree(Type ~ Latitude + Longitude + Magnitude.Type  , data=new_eq.train)
plot(treemod)
text(treemod)

cv.trees <- cv.tree(treemod, FUN = prune.misclass)
plot(cv.trees)

prune.trees <- prune.misclass(treemod, best =4)
plot(prune.trees)
text(prune.trees, pretty = 0)

#예측하기 & 모델 평가
install.packages("e1071")
library(e1071)
treepred <- predict(prune.trees, new_eq.test, type='class')
confusionMatrix(treepred, new_eq.test$Type)

#데이터 확인
new_eq.test
head(new_eq.test)
str(new_eq.test)
View(new_eq.test)

new_eq.train
head(new_eq.train)
str(new_eq.train)
View(new_eq.train)



#new_eq, new_eq.test, new_eq.train 데이터를 new_eq.RData 라는 파일에 저장
save(new_eq, new_eq.test, new_eq.train, file="new_eq.RData")

#교차 검증 준비
createFolds(new_eq.train$Type, k=10)

#Fold에는 검증 데이터로 사용할 데이터 번호가 저장

#10겹 교차 검증 데이터를 만드는 함수
#set.seed()는 해당 함수를 반복해서 호출해도 
#매번 같은 결과가 나오게 하기 위해 사용

create_ten_fold_cv <- function(){
  set.seed(137)
  lapply(createFolds(new_eq.train$Type, k=10),function(idx){
    return(list(train=new_eq.train[-idx, ],
                validation = new_eq.train[idx, ]))
  })
}
#함수는 Fold01, Fold02, ..., Fold10을 가진 리스트를 반환하며 
#각 Fold에는 train과 validation이라는
#이름에 훈련 데이터와 검증 데이터 저장

x <- create_ten_fold_cv()
str(x)

#Fold01에서 훈련데이터를 가져오려면
head(x$Fold01$train)
x[[1]]$train

#summary ( survived ∼ pclass + sex + age + sibsp + parch + fare +
#embarked , data = data )

#데이터 탐색
#Hmisc 패키지에는 summary()에 formula를 지정해 
#데이터의 요약정보를 얻을 수 있는 기능
install.packages("Hmisc")
library(Hmisc)
str(new_eq)

data = new_eq.train
str(data)
# Date + Time + Latitude + Longitude + Magnitude.Type + Depth + Magnitude
# Latitude + Longitude + Magnitude.Type + Depth + Magnitude

summary(Type ~ Latitude + Longitude + Magnitude.Type + Depth + Magnitude , data = data, method = "reverse")

str(data)
data = new_eq.train
data

#caret::featurePlot()을 사용해 데이터를 시각화
#num 컬럼만 선택하여 X에 지정
install.packages("ellipse")
library(ellipse)

data.complete <- data[complete.cases(data),]
featurePlot(
  data.complete[,
                sapply(names(data.complete),
                       function(n){
                         is.numeric(data.complete[, n])
                       })],
  data.complete[, c("Type")],
  "ellipse"
)

#모자이크 플롯 규모, 진폭유형 별 타입
mosaicplot(Type ~ Magnitude + Magnitude.Type , data = data , color = TRUE,
           main=" Magnitude and Magnitude.Type")

mosaicplot(Type ~ Magnitude , data = data , color = TRUE,
           main=" Type")

mosaicplot(Type ~ Magnitude.Type , data = data , color = TRUE,
           main="Magnitude.Type")

str(data)

levels(new_eq$Type)

# Magnitude, Type 별 Magnitude.Type을 보고 싶다면
xtabs( ~ Magnitude.Type + Magnitude, data=data)
xtabs( ~ Magnitude.Type, data=data)

#[1] "Earthquake"        "Explosion"         "Nuclear Explosion"
# Explosion == 갯수가 0개 skip
xtabs(Type == "Earthquake" ~ Magnitude.Type + Magnitude,data=data)
xtabs(Type == "Nuclear Explosion" ~ Type + Magnitude,data=data)

str(data)

# 규모,진폭유형 별 타입이 ("Earthquake")일 확률
# 규모,진폭유형 별 타입이 ("Nuclear Explosion")일 확률
xtabs(Type == "Earthquake" ~ Magnitude.Type + Magnitude,data=data) /
  + xtabs( ~ Magnitude.Type + Magnitude,data=data)

str(new_eq)
#나무 모형 중 하나로 다양한 변수의 상호작용을 잘 표현
#rpart는 이를 대리 변수(surrogate variable)로 처리
library(rpart)
m <- rpart(
  Type ~ Latitude + Longitude + Magnitude.Type + Depth + Magnitude,
  data=new_eq.train)
p <- predict(m, newdata = new_eq.train, type="class")

head(p)

#교차 검증 데이터에 대해 예측값 구하기
#10개 Fold에 대한 예측값과 실제값 데이터
library(foreach)

#actual에 지진 크기타입부의 실제값
#predicted에 지진 크기타입의 예측값을 저장한 리스트로 반환
#Latitude + Longitude + Type + Depth + Magnitude
folds <- create_ten_fold_cv()
rpart_result <- foreach(f=folds) %do%{
  model_rpart <- rpart(#
    Type ~ Latitude + Longitude + Magnitude.Type + Depth + Magnitude ,
    data=f$train)
  predicted <- predict(model_rpart, newdata = f$validation,#
                       type="class")
  return(list(actual=f$validation$Type, predicted=predicted))
}

head(rpart_result)


#Accuracy 계산함수
#evaluation() = rpart result를 입력으로 받아 sapply() 수행
#sapply는 각 fold에
#대한 결과에 대해 Accuracy를 계산하며 이를 벡터 묶음
#편의를 위해 평균과 표준편차를 계산한 뒤 
#Accuracy의 벡터를 결과로 반환
evaluation <- function(lst){
  accuracy <- sapply(lst, function(one_result){
    return(sum(one_result$predicted == one_result$actual)
           / NROW(one_result$actual))
  })
  print(sprintf("정확도 +/- SD : %.3f 오차범위 +/- : %.3f",
                mean(accuracy),sd(accuracy)))
  return(accuracy)
}
(rpart_accuracy <-evaluation(rpart_result))

str(new_eq)
#다른 모델링 기법을 적용
install.packages("party")
library(party)
ctree_result <- foreach(f=folds) %do%{
  model_ctree <- ctree(
    Type ~ Latitude + Longitude + Magnitude.Type + Depth + Magnitude,
    data=f$train)
  predicted <- predict(model_ctree, newdata=f$validation,
                       type="response")
  return(list(actual=f$validation$Type, predicted=predicted))
}
(ctree_accuracy <- evaluation(ctree_result))

#Accuracy 벡터에서 밀도 그림(density)을 그려 정확도의
#분포를 살펴볼 수도 있다.
plot(density(rpart_accuracy), main = "rpart")
plot(density(ctree_accuracy), main = "ctree")

plot(density(rpart_accuracy), main = "rpart VS ctree")
lines(density(ctree_accuracy),col="black", lty ="solid")
lines(density(ctree_accuracy),col="blue", lty ="dotted")
lines(density(rpart_accuracy),col="green", lty ="dotted")

str(new_eq)



#new_eq$Latitude,new_eq$Longitude,new_eq$Depth,new_eq$Magnitude
is.na(new_eq$Latitude)
cor(new_eq$Latitude,new_eq$Longitude)
cor(new_eq$Latitude,new_eq$Depth)
cor(new_eq$Latitude,new_eq$Magnitude)
cor(new_eq$Depth,new_eq$Magnitude)
cor(new_eq$Longitude,new_eq$Depth)

# 열의 피어슨상관계수 0.5 이상이면 관계가 높다.
cor(new_eq.train[c("Latitude","Longitude","Depth","Magnitude")])
symnum(cor(new_eq.train[c("Latitude","Longitude","Depth","Magnitude")]))

install.packages("corraram")
library(corrgram)
corrgram(cor(new_eq.train[c("Latitude","Longitude","Depth","Magnitude")]),type="corr",upper.panel = panel.conf)


