library(ranger)
library(caret)
library(data.table)

df <- read.csv("C:/Users/aakas/OneDrive/Desktop/Work Space/R Project/creditcard.csv")
head(df)
sum(is.na(df))

dim(df)
table(df$Class)
summary(df$Amount)
names(df)
var(df$Amount)
sd(df$Amount)
df$Amount=scale(df$Amount)
df_1=df[,-c(1)]
head(df_1)

library(caTools)
set.seed(123)
split = sample.split(df_1$Class,SplitRatio=0.80)
train_data = subset(df_1,split==TRUE)
test_data = subset(df_1,split==FALSE)

dim(train_data)

dim(test_data)

lm=glm(Class~.,test_data,family=binomial())
summary(lm)

plot(lm)

lr_predict <- predict(lm,train_data, probability = TRUE)
cm = table(train_data[, 30], lr_predict > 0.5)
cm

lr_predict_test <- predict(lm,test_data, probability = TRUE)
cm = table(test_data[, 30], lr_predict_test > 0.5)
cm

library(rpart)
library(rpart.plot)
decisionTree_model <- rpart(Class ~ . , df, method = 'class')
predicted_val <- predict(decisionTree_model, df, type = 'class')
probability <- predict(decisionTree_model, df, type = 'prob')
rpart.plot(decisionTree_model)

# library(neuralnet)
# ANN_model =neuralnet (Class~.,train_data,linear.output=FALSE)
# plot(ANN_model)
# 
# predANN=compute(ANN_model,test_data)
# resultANN=predANN$net.result
# resultANN=ifelse(resultANN>0.5,1,0)

library(gbm, quietly=TRUE)

# Get the time to train the GBM model
system.time(
  model_gbm <- gbm(Class ~ .
                   , distribution = "bernoulli"
                   , data = rbind(train_data, test_data)
                   , n.trees = 500
                   , interaction.depth = 3
                   , n.minobsinnode = 100
                   , shrinkage = 0.01
                   , bag.fraction = 0.5
                   , train.fraction = nrow(train_data) / (nrow(train_data) + nrow(test_data))
  )
)

gbm.iter = gbm.perf(model_gbm, method = "test")
plot(model_gbm)


library(pROC)

gbm_test = predict(model_gbm, newdata = test_data, n.trees = gbm.iter)
gbm_auc = roc(test_data$Class, gbm_test, plot = TRUE, col = "red")

print(gbm_auc)
