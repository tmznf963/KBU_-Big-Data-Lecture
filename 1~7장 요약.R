#문자열을 fruits라는 벡터변수에 저장
fruits<-c("apple","banana","orange","peach","tomato","potato")
fruits

#저장된 문자열을 오름차순 또는 내림차순으로 정렬
fruits <- sort(fruits) #오름
fruits <- sort(fruits,decreasing = T) #내림

#짝수번째 성분을 출력
fruits[c(2,4,6)]
fruits[seq(2,6,by=2)] #★

#1~8, 11~18,111~118로 이루어진 2,4,3차원의 배열 만들기
b<-c(1:8,11:18,111:118)
dim(b) <- c(2,4,3) #디멘젼 = 차원
b

#행렬X만들기
x = matrix(1:12,nrow=3)
x
#전치행렬 x만들기(매개변수x를 t라는 전치행렬함수사용)
t(x)
#x의 첫번째 행만 뽑아 xr1만들기
xr1 =x[1,]
xr1
#x의 세번째 열만 뽑아 xc3 만들기
xc3 =x[,3]
xc3
#x에서 5,8,6,9를 원소로 가지는 부분 행렬 xs 만들기
xs = x[2:3,2:3]
xs



#state.x77의 데이터 이용
#data("state") 명령어를 입력하면, 변수 state.x77에 행렬이 담김

#3번째 행과 8번째 열이 교차하는 원소 추출하기
state.x77[3,8]
#5,22,44행과 1,4,7열에 해당하는 값 추출하기
state.x77[c(5,22,44),c(1,4,7)]
#5행부터 49행까지를 제외한 나머지 행과 3열부터 5열까지의 원소 추출하기
state.x77[-(5:49),3:5] #제외는 -
#state.x77의 두 번째 열인 Income이 4000보다 큰 것만 추출하기
state.x77[state.x77[,"Income"]>4000,]
state.x77[state.x77[,2]>4000,]


#iris데이터 이용
#data("iris")
#꽃잎(petal),꽃받침(sepal)에 따른 붓꽃 종류 데이터

#Sepal.Width >3.5 데이터 찾기
iris[iris[,"Sepal.Width"]>3.5,] #모든행,모든열에 대해서 3.5이상

#Species == versicolor 데이터 찾기
iris[iris[,"Species"]=="versicolor",] #행조건★

#iris의 모든 컬럼(속성)명 조회
names(iris)
#Width가 들어간 컬럼명만 조회(대소문자 구별없이)
names(iris)[grep("al.wid",names(iris),ignore.case = TRUE)]
#"Petal"이 들어간 컬럼명의 데이터의 상단의 일부 값만 조회
head(iris[,grep("Petal",names(iris))]) #grep특정값 지정해서 가져오는것

#3-4장
#c(1,2,NA,4)에서 결측치를 제거
#is.na()함수를 사용하면 결측치의 인덱스를 알 수 있음.
myVec <- c(1,2,NA,4)
myVec[!is.na(myVec)]

#iris데이터의 해과 동일한 길이를 갖는 문자열 벡터를 생성하ㅣ오
#위 벡터의 요소들은 다음과 같은 조건을 갖는다
#"Sepal.Length" >5 이면 "greater than 5",아니면"less than 5"
#위 조건을 if-else와 for-loop를 사용하여 작성하시오
output <- NA

for(i in 1:nrow(iris)){
  if(iris$Sepal.Length[i]>5)
    output[i] <- "greater than 5"
  else
    output[i] <- "less than 5"
}
output[i]

#위 조건을 ifelse()함수를 사용하여 작성하시오
output <- ifelse(iris$Sepal.Length>5,"greater than 5","less than 5")
output
#위 조건을 apply() 함수를 사용하여 작성하시오
myFunc <- function(x){
  if(x['Sepal.Length']>5)
    "greater than 5"
  else
    "less than 5"
}
output<-apply(iris,1,FUN=myFunc) #1=데이터 , 2번째=방향 (1=행 2=열),FUN = 함수

#iris 데이터 셋에서 Species가 "versicolor"인 행들의 인덱스 출력
which(iris$Species=="versicolor")
#iris 데이터 셋에서 Species가 "versicolor"인 행들의 subset생성
subset(iris,iris$Species=="versicolor")
subset[which(iris$Species=="versicolor")]

#1~9를 인자로 갖는 3x3 행렬 A를 생성오
A<-matrix(1:9,nrow=3)
A
#10~18을 인자로 갖는 3x3 행렬 b를 생성
B<-matrix(10:18,nrow=3)
B
#A와B를 합쳐 3x6 행렬을 출력하시오
cbind(A,B) #열합침
#A와B를 합쳐 6x3 행렬을 출력하시오
rbind(A,B) #행합ㅊ

#행렬m
m<-matrix(data=cbind(rnorm(30,0),rnorm(30,2),rnorm(30,5)),nrow=30,ncol=3)
m
#랜덤샘플링 정규분포 rnorm(30,5) 5는 평균값

#apply() 함수를 이용하여 각 행의 0보다 작은 원소들의 개수를 구하기침
#apply(데이터값,방향,함수)
apply(m,2,function(x)length(x[x<0])) #length=개수반환

#apply() 함수를 이용하여 각 행의 0보다 큰 원소들의 평균을 구하기
apply(m,2,function(x)mean(x[x>0])) #mean=평균

library(lattice)
#lapply()함수를 이용해 barley 데이터 각 열의 유일한 값들 구하기
#list형태로 반환 , Hint : unique() 함수 사용
lapply(barley, function(x)unique(x))

#위와 동일한 문제를 sapply에 적용해보기
#하나의 테이블(매트릭스) 형태로 반환
sapply(barley,function(x)unique(x))

#sapply() 함수를 이용해 barley 데이터 각 열의 유일한 값들의 개수 구하기
sapply(barley,function(x)length(unique(x)))

#tapply()함수를 이용해 iris데이터에서 Species 별로 Petal.Length평균을 구해보세요.
#그룹별로처리
tapply(iris$Petal.Length, iris$Species, mean)
#tapply(구할값 , 값조건 , 평균or개수조건)

#iris데이터의 petal.length , petal.width 산점도로 그려보세요
#plot(x,y)
plot(iris$Petal.Length,iris$Petal.Width)
#위 산점도를 Species 별로 색상을 "red","green","blue"로 다르게 그려보세요
plot(iris$Petal.Length,iris$Petal.Width,pch=21,bg=c("red","green","blue")[unclass(iris$Species)])
#Hint: (점의종류)pch=21 , 점의 색상은 bg옵션으로 가능 , unclass(iris$Species) 사용하면 Species를 숫자로 매핑해줌

#iris데이터에서 Species별 Petal.Length 평균을 막대 그래프로 그려보세요
pl.mean<-tapply(iris$Petal.Length,iris$Species,mean)
barplot(pl.mean)
