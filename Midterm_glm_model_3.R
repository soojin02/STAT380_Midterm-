rm(list=ls())
library(data.table)
library(plotmo)
library(Metrics)
library(caret)
library(glmnet)
library(lubridate)
set.seed(7)

train <- fread("./project/volume/data/raw/Stat_380_train.csv")
test <- fread("./project/volume/data/raw/Stat_380_test.csv")
covar <- fread("./project/volume/data/raw/covar_data.csv")
example_sub <- fread("./project/volume/data/raw/Example_Sub.csv")

sample_id <- test$sample_id
test$ic50_Omicron <- 0

train <- subset(train, select = -c(sample_id))
test <- subset(test, select = -c(sample_id))

train_y <- train$ic50_Omicron

missing_index <- which(is.na(train), arr.ind = TRUE)
train[missing_index] <- 1

missing_index <- which(is.na(test), arr.ind = TRUE)
test[missing_index] <- 1

# dummyVars
dummies <- dummyVars(ic50_Omicron ~ ., data = train)
saveRDS(dummies, "./project/volume/models/dummies")
train <- predict(dummies, newdata = train)
test <- predict(dummies, newdata = test)



# data table
train <- data.table(train)
test <- data.table(test)
# matrix
train <- as.matrix(train)
test <- as.matrix(test)


train <- subset(train, select = -c(dose_3mRNA1272))
# regularization Model
regularization_model <- cv.glmnet(train, train_y, alpha = 1, family = "poisson", type.measure = 'mse')
# min lambda
bestlam <- regularization_model$lambda.min
# Save regularization_model 
saveRDS(regularization_model, "./project/volume/models/regularization_model")
# logistic
regularization_log <- glmnet(train, train_y, alpha = 1, family = "poisson")
pred_regularization <- predict(regularization_log, s = bestlam, newx = test, type = "response")

# submit file
test$ic50_Omicron <- pred_regularization
submit <- data.table(sample_id)
submit$ic50_Omicron <- test$ic50_Omicron
# write csv
fwrite(submit, "./project/volume/data/processed/submit.csv")
