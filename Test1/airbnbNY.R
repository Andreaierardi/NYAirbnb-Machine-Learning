#https://www.kaggle.com/dgomonov/new-york-city-airbnb-open-data
library(ggplot2)
library(ggmap)
library(tidyr)
library(cowplot)
library(magick)
world_map <- map_data("newyork")
dataset = read.csv("AB_NYC_2019.csv")
theme_set(theme_cowplot())

# NOTES
# Remove NA, empty
#
#
#
#
myplot = ggplot2::ggplot(dataset, aes(x = longitude, y = latitude, color= neighbourhood_group))+geom_point()

#ggdraw() +
#  draw_image("New_York_City_.png") +
#  draw_plot(myplot)

mean(dataset$price )


man = dataset  %>% 
  filter( neighbourhood_group== "Manhattan")
man$price

mean(man$price)



bronx = dataset  %>% 
  filter( neighbourhood_group== "Bronx")
bronx$price
mean(bronx$price)


averages =  dataset %>%
  group_by(neighbourhood_group) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE)
  )
averages


#RANDOM FOREST
#https://www.guru99.com/r-random-forest-tutorial.html

library(randomForest)
library(caTools)
data_clean = select (dataset,-c(id,name,host_id,host_name, minimum_nights,number_of_reviews,neighbourhood,last_review,availability_365,reviews_per_month,,calculated_host_listings_count))

sample = sample.split(data_clean, SplitRatio = .75)
train = subset(data_clean, sample == TRUE)
test  = subset(data_clean, sample == FALSE)

dim(train)
dim(test)
dim(data_clean)
rf <- randomForest(
  price ~ .,
  data=train
)
rf

pred = predict(rf, newdata=test,na.action = na.pass)

#cm = table(test[,14], pred)

library(caret)
confusionMatrix(as.factor(pred), as.factor(test$price))

confusionMatrix(
  factor(pred, levels = 1:19558),
  factor(test$price, levels = 1:19558)
)
