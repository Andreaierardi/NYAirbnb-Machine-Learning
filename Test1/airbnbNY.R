#https://www.kaggle.com/dgomonov/new-york-city-airbnb-open-data
library(ggplot2)
library(ggmap)
library(tidyr)
library(cowplot)
library(magick)
#world_map <- map_data("newyork")
ds = read.csv("AB_NYC_2019.csv")

#Check for NA
apply(ds,2,function(x) sum(is.na(x)))

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


# GOOD VIDEO FOR PARAMETER OPTIMISATION
# https://www.youtube.com/watch?v=6EXPYzbfLCE

# TUNING 
# https://uc-r.github.io/random_forests
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



rf2 <- randomForest(
  price ~ . ,
  data=train,
  # importance = TRUE
)
rf2
varImpPlot(rf2)

library(randomForest)
library(caret)
library(e1071)

trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

rf_default <- train(
  neighbourhood_group ~ . ,
  data=train,
  method = "rf",
  metric="Accuracy",
  trControl = trControl
    # importance = TRUE
)
print(rf_default)


set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(
                  neighbourhood_group~.,
                 data = train,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
print(rf_mtry)


best_mtry <- rf_mtry$bestTune$mtry 
best_mtry
#[1] 4

max(rf_mtry$results$Accuracy)
#[1] 0.9987047


store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 15)) {
  set.seed(1234)
  rf_maxnode <- train(neighbourhood_group~.,
                      data = train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)



for (maxnodes in c(15 : 30)) {
  set.seed(1234)
  rf_maxnode <- train(neighbourhood_group~.,
                      data = train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

#30


#Step 4) Search the best ntrees
#Now that you have the best value of mtry and maxnode, 
#you can tune the number of trees. The method is exactly the same as maxnode.

store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(neighbourhood_group~.,
                       data = train,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 24,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)





#Step 5) Evaluate the model
fit_rf <- train(neighbourhood_group~.,
                train,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 2000,
                maxnodes = 30)




prediction <-predict(fit_rf, test)

prediction_default <-predict(rf, test)

conf = confusionMatrix(prediction, test$neighbourhood_group)

conf_default = confusionMatrix(prediction_default, test$neighbourhood_group)

conf

conf_default
#Step 6) Visualize Result
varImpPlot(fit_rf)



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








#============================
## NEURAL NETWORKS

# https://medium.com/@brscntyz/neural-network-in-r-e275302b6e44