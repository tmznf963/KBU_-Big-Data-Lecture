library(ggplot2)

getwd()
setwd("c:/R_project")

welfare <- read.csv("����ó���������Ȳ.csv", header=T, as.is=T)
welfare
head(welfare)

building <- welfare$�뵵��
count <- welfare$���๰.��.
state <- welfare$����������Ȳ


ggplot(data=welfare,aes(x = building,y=count,fill = state))+
  geom_col(position ="dodge") +
  scale_x_discrete(limits = building )


##############################################################################
library(ggmap)
library(grid)
pop<-read.csv('��������������.csv',header=T)
pop
lon<-pop$LON
lat<-pop$LAT
data<-pop$�Ը�
df<-data.frame(lon,lat,data)
df

map1<-get_map(location = "south korea",zoom = 7,source = "stamen",maptype ="watercolor")

p <- ggmap(map1)+geom_point(aes(x=lon,y=lat,colour=data,size=data),data=df)

p + scale_colour_gradient(low="yellow",high = "red")