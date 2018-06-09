getwd()

eq <- read.csv("Earthquake1.csv")

#불필요 컬럼 삭제
eq <- eq[, !names(eq) %in%
           c("Depth.Error","Depth.Seismic.Stations","Magnitude.Error",
             "Magnitude.Seismic.Stations","Azimuthal.Gap","Horizontal.Distance",
             "Horizontal.Error","Root.Mean.Square")]

#결측치 제거
library(dplyr)
eq<- na.omit(eq)
eq


eq
str(eq)
head(eq)
View(eq)

#level의 수가 너무 많은 것은 단순 문자열로 취급
eq$Date <- as.character(eq$Date)#Factor
eq$Time <- as.character(eq$Time)#Factor
eq$Latitude <- as.character(eq$Latitude)#num
eq$Longitude <- as.character(eq$Longitude)#num
eq$Depth <- as.character(eq$Depth)#num
eq$Magnitude <- as.character(eq$Magnitude)#Factor

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
levels(eq$Magnitude.Type)[1] <- NA
table(eq$Magnitude.Type, useNA = 'always')


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

NROW(eq.test) #781행의 테스트 데이터
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
# Date + Time + Latitude + Longitude + Type + Depth
summary(Magnitude.Type ~ Latitude + Longitude + Type + Depth, data = data, method = "reverse")

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

#모자이크 플롯
mosaicplot(Magnitude.Type ~ Magnitude + Type , data = data , color = TRUE,
           main="Type and Magnitude")
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

# 타입 크기별 크기타입("MB")일 확률
xtabs(Magnitude.Type == "MB" ~ Type + Magnitude,data=data) /
  + xtabs( ~ Type + Magnitude,data=data)

# 평가 메트릭
predicted <- c(1, 0, 0, 1, 1)
actual <- c(1, 0, 0, 0, 0)
sum( predicted == actual ) / NROW ( predicted )

str(eq)
#나무 모형 중 하나로 다양한 변수의 상호작용을 잘 표현
#rpart는 이를 대리 변수(surrogate variable)로 처리
library(rpart)
m <- rpart(
  Magnitude.Type ~ Latitude + Longitude + Type + Depth,
  data=eq.train)
p <- predict(m, newdata = eq.train, type="class")

head(p)

#10개 Fold에 대한 예측값과 실제값 데이터
library(foreach)

folds <- create_ten_fold_cv()
rpart_result <- foreach(f=folds) %do%{
  model_rpart <- rpart(
    Magnitude.Type ~ Latitude + Longitude + Type + Depth,
    data=f$train)
  predicted <- predict(model_rpart, newdata = f$validation,
                        type="class")
  return(list(actual=f$validation$Magnitude.Type, predicted=predicted))
}

head(rpart_result)


evaluation <- function(lst){
  accuracy <- sapply(lst, function(one_result){
    return(sum(one_result$predicted == one_result$actual)
           / NROW(one_result$actual))
  })
  print(sprintf("MEAN +/- SD : %.3f +/- $.3f",
                mean(accuracy),sd(accuracy)))
  return(accuracy)
}

evaluation(rpart_result)
