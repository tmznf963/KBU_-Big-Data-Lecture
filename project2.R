##############################################################################
library(ggmap)
library(grid)
pop<-read.csv('지진관측데이터.csv',header=T)
pop
lon<-pop$LON
lat<-pop$LAT
data<-pop$규모
df<-data.frame(lon,lat,data)
df

map1<-get_map(location = "south korea",zoom = 7,source = "stamen",maptype ="watercolor")

p <- ggmap(map1)+geom_point(aes(x=lon,y=lat,colour=data,size=data),data=df)

p + scale_colour_gradient(low="yellow",high = "red")
