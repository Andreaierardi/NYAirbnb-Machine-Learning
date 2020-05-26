#https://www.kaggle.com/dgomonov/new-york-city-airbnb-open-data
library(ggplot2)
library(ggmap)
library(tidyr)
library(cowplot)
library(magick)
world_map <- map_data("newyork")
ds = read.csv("AB_NYC_2019.csv")

# NOTES
# Remove NA, empty
#
#
#
#
myplot = ggplot2::ggplot(dataset, aes(x = longitude, y = latitude, color= neighbourhood_group))+geom_point()

clean_data = function(ds)
{
  ds$id = ds$host_id = ds$host_name = NULL
  return(ds)
}
#ggdraw() +
#  draw_image("New_York_City_.png") +
#  draw_plot(myplot)

dataset = clean_data(ds)
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
library(dplyr)
data_clean =select (dataset,-c(host_id, id, host_name, name,minimum_nights,number_of_reviews,neighbourhood,last_review,availability_365,reviews_per_month,calculated_host_listings_count))

sample = sample.split(data_clean, SplitRatio = .75)
train = subset(data_clean, sample == TRUE)
test  = subset(data_clean, sample == FALSE)

dim(train)
dim(test)
dim(data_clean)
rf <- randomForest(
  neighbourhood_group ~ . ,
  data=train,
 # importance = TRUE
)
rf

pred = predict(rf, newdata=test,na.action = na.pass)

table(pred,test$neighbourhood_group)

#https://www.r-bloggers.com/how-to-implement-random-forests-in-r/
varImpPlot(rf)

plot(pred)
#cm = table(test[,14], pred)

#library(caret)
#confusionMatrix(as.factor(pred), as.factor(test$price))

#confusionMatrix(
#  factor(pred, levels = 1:19558),
#  factor(test$price, levels = 1:19558)
#)
library(ROCR)
#ROCR
roc_pred = predict(rf,type="prob")
perf = prediction(roc_pred[,2], train$neighbourhood_group)

#==
#FIND OPTIMAL VALUE WITH MIN OUT OF BAG
#https://www.listendata.com/2014/11/random-forest-with-r.html


#================
# DECISION  TREE
library(rpart)
library(caret)
library(e1071)


dt = train( neighbourhood_group~ ., data = train,method = "rpart")
pred2 = predict(dt, data = test)
table(pred2, test)
