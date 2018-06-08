getwd()
setwd("C:/easy_r")
install.packages('ggmap')
install.packages('dplyr')
library(ggplot2)
library(ggmap)
library(dplyr)

#import data
house.data <- read.csv('kc_house_data.csv',header = TRUE)

head(house.data)
str(house.data)

#drop data from model
house.data <- house.data[ ,c(1,3:21)]
house.data

#Make var names available in script
attach(house.data)

# 1. Data Exploration

#view the structure of the data
glimpse(house.data)

# view a summary of the house data
summary(house.data)

# make prices in $100k's
pricesIn100k <- house.data$price / 100000

# price distribution
hist(pricesIn100k,
     data = house.data,
     main = 'Distribution of Price',
     xlab = 'Pirce In $100k',
     ylab = 'Frequency',
     col = 'blue',
     bins = 10
     )

# Bedrooms Distribution
hist(house.data$bedrooms,
     main = 'Distribution of Bedrooms',
     xlab = 'Number Of Bedrooms',
     ylab = 'Frequency',
     col = 'green'
     )

# Distribution Of condition
hist(house.data$condition,
     main = 'Distribution of Condition',
     xlab = 'House Condition',
     ylab = 'Frequency',
     col = 'yellow',
     )

squareFt <- house.data$sqft_lot15 /100
# Price by sqft
plot(y = pricesIn100k, x = squareFt, xlab = 'square Feet' , ylab='Price IN $100K', main = 'Price By Sqft_lot15',col ='red')

#price by bedrooms
plot(pricesIn100k,
     bedrooms,
     data = house.data,
     main = 'Price By Bedroom',
     col = 'purple',
     xlab = 'Price In $100k',
     ylab = 'Number Of Bedrooms'
     )

# 2. MLR Model

# Drop DATE from model, too many factors to create dummy variables
# for more complete analysis you would refactor these dates into
# more meaningful data

# Create baseline model
house.model <-lm(price ~ ., data = house.data)

# Summary of model
summary(house.model)

# Round Coefficients Table
coeffs <- summary(house.model)$coefficients
coeffs <- round(coeffs,4)
coeffs

# Create a Scatter PLot
plot (price ~ yr_built,
      data = house.data,
      cex = .2,
      col = 'red',
      main = 'Price By year',
      xlab = 'Year',
      ylab = 'Price of house in $100k'
      )

# 3. Anova

# I want to see if house prices on
# average vary by quarter centuries

# Grab the price and year to convert year into decade factor
priceByDecade <- data.frame(Price = house.data$price, Decade = house.data$yr_built)
priceByDecade

#Find the Earliest Year Built
min(priceByDecade$Decade)

#Find the Lastest Year Built
max(priceByDecade$Decade)

#fins the Distribution By year
hist(priceByDecade$Decade,
     bins = 10,
     main = ' Distribution Of Houses By Year',
     xlab = 'Decade Built', ylab = 'Count',
     col = c('orange')
     )

#Create a Break Every 25 years
for(i in 1:5000){
  if(priceByDecade$Decade[i] < 1925){
    priceByDecade$Decade[i] <- '1900 - 1925'
  }
  else if(priceByDecade$Decade[i] > 1925 && priceByDecade$Decade[i]<1950){
    priceByDecade$Decade[i] <- '1925 - 1950'
  }
  else if(priceByDecade$Decade[i] > 1950 && priceByDecade$Decade[i]<1975){
    priceByDecade$Decade[i] <- '1950 - 1975'
  }
  else if(priceByDecade$Decade[i] > 1975 && priceByDecade$Decade[i]<2000){
    priceByDecade$Decade[i] <- '1975 - 2000'
  }
  else{
    priceByDecade$Decade[i] <- '2000 - current'
  }
}

head(priceByDecade$Decade[1])

#Make sure Each year is
priceByDecade$Decade <- as.factor(priceByDecade$Decade)
priceByDecade$Decade

#new that the years are grouped into factors of 20year spans,
#we can run on anova
anova <- aov(Price ~ Decade, data = priceByDecade)
anova

#see summary stats on anova
summary(anova)

# Run Tukey's Test On Anova
TukeyHSD(anova)

#plot ANOVA relationship
plot(pricesIn100k ~ Decade,
     data = priceByDecade,
     main = 'ANOVA Price ~ Qtr. Century',
     xlab = 'Qrt. Century 1900 - Present',
     ylab = 'Price In $100k',
     col = c('orange', 'blue', 'green', 'yellow', 'pink')
     )
#4. GGAMPS
# find what should be the cutoff point for 'expensive'!
# cut ot 3rd quorter and up
summary(house.data$price)

#Create New Table Of the Most Expensive Houses
mostExpensiveHouses <- house.data[house.data[,2]>650000,]

#Select Only The Columns You Need
#Price Longitude & Latitude
mostExpensiveHouses <- mostExpensiveHouses[,c(2,17,18)]
mostExpensiveHouses

#store Map of king county
map <- get_map(location = "King County, Washington",
               zoom = 10,
               maptype = 'hybrid',
               source = 'google',
               color = 'bw'
               )
# Store Map
m<- ggmap(map)
m + geom_point(data=mostExpensiveHouses,
               aes(x=mostExpensiveHouses$long,
                   y=mostExpensiveHouses$lat),
               color="green",
               size=2,
               alpha=0.2
               )+labs(title="Most Expensive Houses In King County")
