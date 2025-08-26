library(data.table)
library(dplyr)
library(Metrics)
library(caret)
library(glmnet)

# Load interim data
train <- fread("./project/volume/data/interim/train.csv")
test <- fread("./project/volume/data/interim/test.csv")

sample <- test$sample_id

# subset_drop
train <- subset(train, select = -c(sample_id))
test <- subset(test, select = -c(sample_id))

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

# drop
train <- subset(train, select = -c(dose_3mRNA1272))

# regularization Model
regularization_model <- cv.glmnet(train, train_y, alpha = 0.1, family = "poisson", type.measure = 'mse')
# min lambda
bestlam <- regularization_model$lambda.min
# Save regularization_model 
saveRDS(regularization_model, "./project/volume/models/regularization_model")
# logistic
regularization_log <- glmnet(train, train_y, alpha = 0.1, family = "poisson")

# regularization_model model for prediction
pred_regularization <- predict(regularization_log, s = bestlam, newx = test, type = "response")

# submit file
test$ic50_Omicron <- pred_regularization
submit <- data.table(sample)
submit <- submit %>% rename_at('sample', ~'sample_id')
submit$ic50_Omicron <- test$ic50_Omicron
# write csv
fwrite(submit, "./project/volume/data/processed/regularization_1.csv")
