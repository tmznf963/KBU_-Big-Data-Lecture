getwd()
setwd("C:/R_project")

install.packages("ggplot2")
install.packages("ggmap")
library(ggmap)
library(ggplot2)

ggmap(get_map(location = "south korea", zoom=7))

map<-get_map(location = "south korea",zoom = 7,source = "stamen",maptype ="watercolor")
map<-get_map(location='south korea', zoom=7, source='google', maptype='satellite')
ggmap(get_map(location = "south korea",zoom = 7,source = "stamen",maptype ="toner"))
ggmap(get_map(location = "south korea",zoom = 7,source = "stamen",maptype ="terrain"))
ggmap(get_map(location='south korea', zoom=7, source='google', maptype='terrain'))
ggmap(get_map(location='south korea', zoom=7, source='google', maptype='satellite'))
ggmap(get_map(location='south korea', zoom=7, source='google', maptype='roadmap'))
ggmap(get_map(location='south korea', zoom=7, source='google', maptype='hybrid'))
ggmap(get_map(location = "south korea",zoom = 7,source = "osm"))


map<-get_map(location = "south korea",zoom = 7,source = "stamen",maptype ="toner") #검은 지도
#######################################################지역 구호소
wifi <- read.csv("전국지진실내구호소.csv", header=T, as.is=T)
ggmap(map)+geom_point(data=wifi, aes(x=LON, y=LAT, color=address))

#######################################################지역 대피소
wifi <- read.csv("전국지진옥외대피소.csv", header=T, as.is=T)
ggmap(map)+geom_point(data=wifi, aes(x=LON, y=LAT, color=address))
head(wifi)


#######################################################지진지역 빈도수
wifi <- read.csv("지진관측데이터.csv",header=T,as.is=T)
ggmap(map) + stat_density_2d(data=wifi, aes(x=LON, y=LAT, fill=..level.., alpha=..level..), geom='polygon', size=7, bins=30)

p<-ggmap(map) + stat_density_2d(data=wifi, aes(x=LON, y=LAT, fill=..level.., alpha=..level..), geom='polygon', size=7, bins=30)
#밀도 색 변경
p + scale_fill_gradient(low="yellow",high = "red",guide = F)+scale_alpha(range=c(0.02,0.8),guide=F)


#미농지 그리기
install.packages("raster")
library(raster)

korea <- getData("GADM", country="kor",level=1) #0,1,2
ggplot() + geom_polygon(data=korea, aes(x=long,y=lat,group=group), fill="white",color="black")


#지진3.0이상 + 대피소
wifi <- read.csv("지진관측데이터.csv",header=T,as.is=T)
wifi2 <- read.csv("전국지진옥외대피소.csv", header=T, as.is=T)
map <- get_map(location='south korea', zoom=7, maptype='roadmap', color='bw') #흑백지도
#대피소 지도 #지진지역 지도
ggmap(map)+geom_point(data=wifi2, aes(x=LON, y=LAT, color=address),alpha=.75) + stat_density_2d(data=wifi,aes(x=LON,y=LAT))
