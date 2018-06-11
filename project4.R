getwd()
setwd("C:/R_project")

eq <- read.csv("Earthquake1.csv")

#불필요 컬럼 삭제
eq <- eq[, !names(eq) %in%
           c("Depth.Error","Depth.Seismic.Stations","Magnitude.Error",
             "Magnitude.Seismic.Stations","Azimuthal.Gap","Horizontal.Distance",
             "Horizontal.Error","Root.Mean.Square")]

str(eq)

#결측치 제거
install.packages("dplyr")
library(dplyr)
eq<- na.omit(eq)
eq


eq
str(eq)
head(eq)
View(eq)

#level의 수가 너무 많은 것은 단순 문자열로 취급
# eq$Date <- as.character(eq$Date)#Factor
# eq$Time <- as.character(eq$Time)#Factor
# eq$Latitude <- as.character(eq$Latitude)#num
# eq$Longitude <- as.character(eq$Longitude)#num
# eq$Depth <- as.character(eq$Depth)#num
# eq$Magnitude <- as.character(eq$Magnitude)#Factor

str(eq)

#Factor 수준(level)에 NA값 확인
levels(eq$Type)
table(eq$Type)

levels(eq$Magnitude.Type)
table(eq$Magnitude.Type)

table(eq$Date)
table(eq$Time)
table(eq$Latitude)
table(eq$Longitude)
table(eq$Depth)


# 빈 문자열 NA를 수정하기
# table() 함수는 NA 값을 제외하고 값을 출력시키기에
# useNA에 always를 지정해 NA에 대한 갯수도 출력하도록 하여 빈도를 확인
# levels(eq$Magnitude.Type)[1] <- NA
# table(eq$Magnitude.Type, useNA = 'always')


# 마지막으로 str(), head(), View() 등의 명령어를 
# 사용해 데이터가 잘 변경되었는지 확인
levels(eq$Magnitude.Type)
table(eq$Magnitude.Type)
str(eq)
head(eq)
View(eq)

#테스트 데이터 분리
install.packages("caret")
library(caret)

#난수 생성시 seed 지정, 교차 검증 createDataPartition() 사용
set.seed(137)
test_idx <- createDataPartition(eq$Magnitude.Type, p=0.1)$Resample1
eq.test <- eq[test_idx, ]
eq.train <- eq[-test_idx, ]

#데이터 확인
eq.test
head(eq.test)
str(eq.test)
View(eq.test)

eq.train
head(eq.train)
str(eq.train)
View(eq.train)

NROW(eq.test) #780행의 테스트 데이터
prop.table(table(eq.test$Magnitude.Type))

NROW(eq.train) # 6990행의 훈련 데이터
prop.table(table(eq.train$Magnitude.Type))


#eq, eq.test, eq.train 데이터를 eq.RData 라는 파일에 저장
save(eq, eq.test, eq.train, file="eq.RData")

#교차 검증 준비
createFolds(eq.train$Magnitude.Type, k=10)

#Fold에는 검증 데이터로 사용할 데이터 번호가 저장

#10겹 교차 검증 데이터를 만드는 함수
#set.seed()는 해당 함수를 반복해서 호출해도 
#매번 같은 결과가 나오게 하기 위해 사용

create_ten_fold_cv <- function(){
  set.seed(137)
  lapply(createFolds(eq.train$Magnitude.Type, k=10),function(idx){
    return(list(train=eq.train[-idx, ],
                validation = eq.train[idx, ]))
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
str(eq)

data = eq.train

# Date + Time + Latitude + Longitude + Type + Depth + Magnitude
# Latitude + Longitude + Type + Depth + Magnitude

summary(Magnitude.Type ~ Latitude , data = data, method = "reverse")

str(data)
data = eq.train
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
  data.complete[, c("Magnitude.Type")],
  "ellipse"
)

#모자이크 플롯 크기, 타입 별  크기타입
mosaicplot(Magnitude.Type ~ Magnitude , data = data , color = TRUE,
           main=" Magnitude")

mosaicplot(Magnitude.Type ~ Type , data = data , color = TRUE,
           main=" Type")

str(data)

levels(eq$Magnitude.Type)

# Magnitude, Type 별 Magnitude.Type을 보고 싶다면
xtabs( ~ Type + Magnitude, data=data)

#[1] "MB"  "ML"  "MS"  "MW"  "MWB" "MWC" "MWR" "MWW"
xtabs(Magnitude.Type == "MB" ~ Type + Magnitude,data=data)
xtabs(Magnitude.Type == "ML" ~ Type + Magnitude,data=data)
xtabs(Magnitude.Type == "MS" ~ Type + Magnitude,data=data)
xtabs(Magnitude.Type == "MW" ~ Type + Magnitude,data=data)
xtabs(Magnitude.Type == "MWB" ~ Type + Magnitude,data=data)
xtabs(Magnitude.Type == "MWC" ~ Type + Magnitude,data=data)
xtabs(Magnitude.Type == "MWR" ~ Type + Magnitude,data=data)
xtabs(Magnitude.Type == "MWW" ~ Type + Magnitude,data=data)

# 타입,크기별 크기타입("MB")일 확률
xtabs(Magnitude.Type == "MB" ~ Type + Magnitude,data=data) /
  + xtabs( ~ Type + Magnitude,data=data)


str(eq)
#나무 모형 중 하나로 다양한 변수의 상호작용을 잘 표현
#rpart는 이를 대리 변수(surrogate variable)로 처리
library(rpart)
m <- rpart(
  Magnitude.Type ~ Latitude + Longitude + Type + Depth + Magnitude,
  data=eq.train)
p <- predict(m, newdata = eq.train, type="class")

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
    Magnitude.Type ~ Latitude + Longitude + Type + Depth + Magnitude ,
    data=f$train)
  predicted <- predict(model_rpart, newdata = f$validation,#
                       type="class")
  return(list(actual=f$validation$Magnitude.Type, predicted=predicted))
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
  print(sprintf("MEAN +/- SD : %.3f +/- %.3f",
                mean(accuracy),sd(accuracy)))
  return(accuracy)
}
(rpart_accuracy <-evaluation(rpart_result))


#다른 모델링 기법을 적용
install.packages("party")
library(party)
ctree_result <- foreach(f=folds) %do%{
  model_ctree <- ctree(
    Magnitude.Type ~ Latitude + Longitude + Type + Depth + Magnitude,
    data=f$train)
  predicted <- predict(model_ctree, newdata=f$validation,
                       type="response")
  return(list(actual=f$validation$Magnitude.Type, predicted=predicted))
}
(ctree_accuracy <- evaluation(ctree_result))

#Accuracy 벡터에서 밀도 그림(density)을 그려 정확도의
#분포를 살펴볼 수도 있다.
plot(density(rpart_accuracy), main = "rpart")
plot(density(ctree_accuracy), main = "ctree")

plot(density(rpart_accuracy), main = "rpart VS ctree")
lines(density(ctree_accuracy),col="red", lty ="dashed")
lines(density(rpart_accuracy),col="red", lty ="dashed")

str(eq)


# 또 다른 특징(Feature)의 발견
# 검증 데이터를 분리하지 않은
# eq.train 을 Date 에 따라 정렬해 표시
View(eq.train[order(eq.train$Date),
              c("Depth","Type","Magnitude")])

#NA인 행의 수를 확인하는 코드
sum(is.na(eq.train$Date))

#Latitude + Longitude + Type + Depth + Magnitude
Depth_result <- foreach(f=folds) %do%{
  Depth_model_ctree <- ctree(
    Magnitude.Type ~  Depth + Magnitude + Type,
    data=f$train)
  predicted <- predict(Depth_model_ctree, newdata=f$validation,
                       type="response")
  return(list(actual=f$validation$Magnitude.Type, predicted=predicted))
}
(Depth_accuracy <-evaluation(Depth_result))

plot(density(rpart_accuracy), main = "rpart VS ctree")
lines(density(ctree_accuracy),col="red", lty ="dashed")
lines(density(Depth_accuracy),col="green", lty ="dashed")

#eq$Latitude,eq$Longitude,eq$Depth,eq$Magnitude
is.na(eq$Latitude)
cor(eq$Latitude,eq$Longitude)
cor(eq$Latitude,eq$Depth)
cor(eq$Latitude,eq$Magnitude)
cor(eq$Depth,eq$Magnitude)
cor(eq$Longitude,eq$Depth)

# 열의 피어슨상관계수 0.5 이상이면 관계가 높다.
cor(eq.train[c("Latitude","Longitude","Depth","Magnitude")])
symnum(cor(eq.train[c("Latitude","Longitude","Depth","Magnitude")]))

install.packages("corraram")
library(corrgram)
corrgram(cor(eq.train[c("Latitude","Longitude","Depth","Magnitude")]),type="corr",upper.panel = panel.conf)
