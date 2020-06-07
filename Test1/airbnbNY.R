#https://www.kaggle.com/dgomonov/new-york-city-airbnb-open-data
library(ggplot2)
library(ggmap)
library(tidyr)
library(cowplot)
library(magick)
library(dplyr)
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

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


clean_data = function(ds)
{
  ds = select (ds,-c(host_id, id, host_name, name,minimum_nights,number_of_reviews,neighbourhood,last_review,availability_365,reviews_per_month,calculated_host_listings_count))
 

  numerical = c("price","longitude", "latitude")
  categorical = c("neighbourhood_group")
  
  ds[numerical] = normalize(ds[numerical])
  ds$neighbourhood_group = factor(ds$neighbourhood_group, level= c("Brooklyn","Manhattan","Queens","Staten Island", "Bronx"), labels=c(1,2,3,4,5))
  ds$room_type = factor(ds$room_type, level= c("Private room","Entire home/apt","Shared room"), labels=c(1,2,3))
  
  return(ds)
}
#ggdraw() +
#  draw_image("New_York_City_.png") +
#  draw_plot(myplot)

dataset = clean_data(ds)



# ================== DATA VISUALISATION ==================

#myplot = ggplot2::ggplot(dataset, aes(x = longitude, y = latitude, color= neighbourhood_group))+geom_point()

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
# =========================================================


# ================== TRAIN AND TEST SETS ==================


library(caTools)
library(caret)

data_clean = dataset
sample = sample.split(data_clean, SplitRatio = .75)
train = subset(data_clean, sample == TRUE)
test  = subset(data_clean, sample == FALSE)

dim(train)
dim(test)
dim(data_clean)

# =========================================================



# ================== RANDOM FOREST ==================

#https://www.guru99.com/r-random-forest-tutorial.html


# GOOD VIDEO FOR PARAMETER OPTIMISATION
# https://www.youtube.com/watch?v=6EXPYzbfLCE

# TUNING 
# https://uc-r.github.io/random_forests
library(randomForest)

rf <- randomForest(
  neighbourhood_group ~ . ,
  data=train,
 # importance = TRUE
)
rf

pred = predict(rf, test)

table(pred,test$neighbourhood_group)
confusionMatrix(pred, test$neighbourhood_group)

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
#[1] 3

max(rf_mtry$results$Accuracy)
#[1] 0.9987048


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
                ntree = 1000,
                maxnodes = 30)




prediction <-predict(fit_rf, test)

prediction_default <-predict(rf, test)

conf = confusionMatrix(prediction, test$neighbourhood_group)

conf_default = confusionMatrix(prediction_default, test$neighbourhood_group)

conf

conf_default
#Step 6) Visualize Result
varImpPlot(fit_rf)


# =========================================================

# ================== RANGER RANDOM FOREST =================
library(ranger)
library(tuneRanger)
ranger <- ranger( neighbourhood_group~ ., data = train, write.forest = TRUE, classification = T)

ranger_pred = predict(ranger, data = test)

tab = table(test$neighbourhood_group, predictions(ranger_pred))

plot(tab)




rangerReg <- ranger( price~ ., data = train, write.forest = TRUE, classification = F)
rangerReg
rangerReg_pred = predict(rangerReg, data = test)
rangerReg_pred
tab = table(test$price, predictions(rangerReg_pred))
tab
plot_ly(x = test$price, y = predictions(rangerReg_pred))



# https://github.com/PhilippPro/tuneRanger
# https://mlr.mlr-org.com/articles/tutorial/measures.html
task = makeRegrTask(data = train, target = "price")
estimateTimeTuneRanger(task, iters = 70, num.threads = 6, num.trees = 1000)
res = tuneRanger(task, measure = list(mse), num.trees = 1000, 
                 num.threads = 4, iters = 70,  show.info = getOption("mlrMBO.show.info", TRUE))


# Mean of best 5 % of the results
res
#Recommended parameter settings: 
#  mtry min.node.size sample.fraction
#1    2            60       0.2010013
#Results: 
#  mse exec.time
#1 0.0004812156     3.446

# Model with the new tuned hyperparameters
res$model
#Model for learner.id=regr.ranger; learner.class=regr.ranger
#Trained on: task.id = train; obs = 29337; features = 4
#Hyperparameters: num.threads=4,verbose=FALSE,respect.unordered.factors=order,mtry=2,
#min.node.size=60,sample.fraction=0.201,num.trees=1e+03,replace=FALSE

tuned_rangerReg <- ranger( price~ ., data = train, write.forest = TRUE, classification = F, mtry= 2, 
                           min.node.size = 60, sample.fraction = 0.201,num.trees = 1000, replace= FALSE)
tuned_rangerReg
tuned_rangerReg_pred = predict(tuned_rangerReg, data = test)
tuned_rangerReg_pred

# ==================== ROCK CURVE =========================

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



# =========================================================




# ===================== DECISION  TREE ====================

# 
library(rpart)
library(caret)
library(e1071)


dt = train( neighbourhood_group~ ., data = train,method = "rpart")
pred2 = predict(dt, data = test)
table(pred2, test)








# =========================================================



# =================== NEURAL NETWORKS ??=====================


# https://medium.com/@brscntyz/neural-network-in-r-e275302b6e44
# https://datascienceplus.com/fitting-neural-network-in-r/
# https://www.kdnuggets.com/2016/08/begineers-guide-neural-networks-r.html/2
library(ISLR)
library(tidyverse)
library("keras")
library(neuralnet)
library(Hmisc)

m <- model.matrix( 
  ~price+neighbourhood_group+room_type+longitude+latitude,
  data = train 
)

m_test  <- model.matrix( 
  ~price+neighbourhood_group+room_type+longitude+latitude,
  data = test 
)

head(m)

nn=neuralnet(price~ neighbourhood_group2+ neighbourhood_group3 +neighbourhood_group4+ neighbourhood_group5+ room_type2+ room_type3+longitude+latitude,data=m, hidden=10,act.fct = "logistic",
             linear.output = TRUE,stepmax=10^5,threshold = 0.01)


#Also you can change your hidden layers by specifiying with numbers in vector like this
nn=neuralnet( price~ neighbourhood_group2+ neighbourhood_group3 +neighbourhood_group4+ neighbourhood_group5+ room_type2+ room_type3+longitude+latitude,data=m, hidden=c(7,6,5),act.fct = "logistic",
             linear.output = TRUE,stepmax=10^5,threshold = 0.01)

hidden=c(7,6,5)


#Then, prediction and calculation of error comes. I calculate the error with Root mean error method.
nn_pred=compute(nn,test[,1:13])
nn_pred$net.resultRMSE <- function(actual,predicted) {
  return(sqrt(sum(actual^2-predicted^2)/length(actual)))
}
summary(nn_pred)
nn_pred <- is.numeric(nn_pred)
RMSE(test$price,nn_pred)

plot(test$price,nn_pred$net.result)

pr.nn_ <- nn_pred$net.result*(max(dataset$price)-min(dataset$price))+min(dataset$price)
# =========================================================


# ================= LINEAR REGRESSION =====================
# https://datascienceplus.com/fitting-neural-network-in-r/
lm.fit <- glm(price~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$price)^2)/nrow(test)



par(mfrow=c(1,2))
plot(test$price,nn_pred$net.result,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')
plot(test$price,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

