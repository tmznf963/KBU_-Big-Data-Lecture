getwd()
setwd("C:/R_project")

eq <- read.csv("Earthquake1.csv")

#���ʿ� �÷� ����
eq <- eq[, !names(eq) %in%
           c("Depth.Error","Depth.Seismic.Stations","Magnitude.Error",
             "Magnitude.Seismic.Stations","Azimuthal.Gap","Horizontal.Distance",
             "Horizontal.Error","Root.Mean.Square")]

str(eq)

#����ġ ����
install.packages("dplyr")
library(dplyr)
eq<- na.omit(eq)
eq


eq
str(eq)
head(eq)
View(eq)

#level�� ���� �ʹ� ���� ���� �ܼ� ���ڿ��� ���
# eq$Date <- as.character(eq$Date)#Factor
# eq$Time <- as.character(eq$Time)#Factor
# eq$Latitude <- as.character(eq$Latitude)#num
# eq$Longitude <- as.character(eq$Longitude)#num
# eq$Depth <- as.character(eq$Depth)#num
# eq$Magnitude <- as.character(eq$Magnitude)#Factor

str(eq)

#Factor ����(level)�� NA�� Ȯ��
levels(eq$Type)
table(eq$Type)

levels(eq$Magnitude.Type)
table(eq$Magnitude.Type)

table(eq$Date)
table(eq$Time)
table(eq$Latitude)
table(eq$Longitude)
table(eq$Depth)


# �� ���ڿ� NA�� �����ϱ�
# table() �Լ��� NA ���� �����ϰ� ���� ��½�Ű�⿡
# useNA�� always�� ������ NA�� ���� ������ ����ϵ��� �Ͽ� �󵵸� Ȯ��
# levels(eq$Magnitude.Type)[1] <- NA
# table(eq$Magnitude.Type, useNA = 'always')


# ���������� str(), head(), View() ���� ���ɾ 
# ����� �����Ͱ� �� ����Ǿ����� Ȯ��
levels(eq$Magnitude.Type)
table(eq$Magnitude.Type)
str(eq)
head(eq)
View(eq)

#�׽�Ʈ ������ �и�
install.packages("caret")
library(caret)

#���� ������ seed ����, ���� ���� createDataPartition() ���
set.seed(137)
test_idx <- createDataPartition(eq$Magnitude.Type, p=0.1)$Resample1
eq.test <- eq[test_idx, ]
eq.train <- eq[-test_idx, ]

#������ Ȯ��
eq.test
head(eq.test)
str(eq.test)
View(eq.test)

eq.train
head(eq.train)
str(eq.train)
View(eq.train)

NROW(eq.test) #780���� �׽�Ʈ ������
prop.table(table(eq.test$Magnitude.Type))

NROW(eq.train) # 6990���� �Ʒ� ������
prop.table(table(eq.train$Magnitude.Type))


#eq, eq.test, eq.train �����͸� eq.RData ��� ���Ͽ� ����
save(eq, eq.test, eq.train, file="eq.RData")

#���� ���� �غ�
createFolds(eq.train$Magnitude.Type, k=10)

#Fold���� ���� �����ͷ� ����� ������ ��ȣ�� ����

#10�� ���� ���� �����͸� ����� �Լ�
#set.seed()�� �ش� �Լ��� �ݺ��ؼ� ȣ���ص� 
#�Ź� ���� ����� ������ �ϱ� ���� ���

create_ten_fold_cv <- function(){
  set.seed(137)
  lapply(createFolds(eq.train$Magnitude.Type, k=10),function(idx){
    return(list(train=eq.train[-idx, ],
                validation = eq.train[idx, ]))
  })
}
#�Լ��� Fold01, Fold02, ..., Fold10�� ���� ����Ʈ�� ��ȯ�ϸ� 
#�� Fold���� train�� validation�̶��
#�̸��� �Ʒ� �����Ϳ� ���� ������ ����

x <- create_ten_fold_cv()
str(x)

#Fold01���� �Ʒõ����͸� ����������
head(x$Fold01$train)
x[[1]]$train

#summary ( survived �� pclass + sex + age + sibsp + parch + fare +
#embarked , data = data )

#������ Ž��
#Hmisc ��Ű������ summary()�� formula�� ������ 
#�������� ��������� ���� �� �ִ� ���
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

#caret::featurePlot()�� ����� �����͸� �ð�ȭ
#num �÷��� �����Ͽ� X�� ����
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

#������ũ �÷� ũ��, Ÿ�� ��  ũ��Ÿ��
mosaicplot(Magnitude.Type ~ Magnitude , data = data , color = TRUE,
           main=" Magnitude")

mosaicplot(Magnitude.Type ~ Type , data = data , color = TRUE,
           main=" Type")

str(data)

levels(eq$Magnitude.Type)

# Magnitude, Type �� Magnitude.Type�� ���� �ʹٸ�
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

# Ÿ��,ũ�⺰ ũ��Ÿ��("MB")�� Ȯ��
xtabs(Magnitude.Type == "MB" ~ Type + Magnitude,data=data) /
  + xtabs( ~ Type + Magnitude,data=data)


str(eq)
#���� ���� �� �ϳ��� �پ��� ������ ��ȣ�ۿ��� �� ǥ��
#rpart�� �̸� �븮 ����(surrogate variable)�� ó��
library(rpart)
m <- rpart(
  Magnitude.Type ~ Latitude + Longitude + Type + Depth + Magnitude,
  data=eq.train)
p <- predict(m, newdata = eq.train, type="class")

head(p)

#���� ���� �����Ϳ� ���� ������ ���ϱ�
#10�� Fold�� ���� �������� ������ ������
library(foreach)

#actual�� ���� ũ��Ÿ�Ժ��� ������
#predicted�� ���� ũ��Ÿ���� �������� ������ ����Ʈ�� ��ȯ
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


#Accuracy ����Լ�
#evaluation() = rpart result�� �Է����� �޾� sapply() ����
#sapply�� �� fold��
#���� ����� ���� Accuracy�� ����ϸ� �̸� ���� ����
#���Ǹ� ���� ��հ� ǥ�������� ����� �� 
#Accuracy�� ���͸� ����� ��ȯ
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


#�ٸ� �𵨸� ����� ����
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

#Accuracy ���Ϳ��� �е� �׸�(density)�� �׷� ��Ȯ����
#������ ���캼 ���� �ִ�.
plot(density(rpart_accuracy), main = "rpart")
plot(density(ctree_accuracy), main = "ctree")

plot(density(rpart_accuracy), main = "rpart VS ctree")
lines(density(ctree_accuracy),col="red", lty ="dashed")
lines(density(rpart_accuracy),col="red", lty ="dashed")

str(eq)


# �� �ٸ� Ư¡(Feature)�� �߰�
# ���� �����͸� �и����� ����
# eq.train �� Date �� ���� ������ ǥ��
View(eq.train[order(eq.train$Date),
              c("Depth","Type","Magnitude")])

#NA�� ���� ���� Ȯ���ϴ� �ڵ�
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

# ���� �Ǿ������ 0.5 �̻��̸� ���谡 ����.
cor(eq.train[c("Latitude","Longitude","Depth","Magnitude")])
symnum(cor(eq.train[c("Latitude","Longitude","Depth","Magnitude")]))

install.packages("corraram")
library(corrgram)
corrgram(cor(eq.train[c("Latitude","Longitude","Depth","Magnitude")]),type="corr",upper.panel = panel.conf)