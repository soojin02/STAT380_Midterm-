# Load libraries
library(data.table)
library(caret)
library(Metrics)
library(glmnet)
library(plotmo)
library(lubridate)

# Load data
train <- fread("./project/volume/data/raw/Stat_380_train.csv")
test <- fread("./project/volume/data/raw/Stat_380_test.csv")
covar <- fread("./project/volume/data/raw/covar_data.csv")
example_sub <- fread("./project/volume/data/raw/Example_Sub.csv")

# Extract sample_id
ID <- covar$sample_id
covar <- subset(covar, select = -c(sample_id))
total <- data.table(rowMeans(covar))
total$sample_id <- ID

# Merge covariate data
train <- merge(x = train, y = total, by = "sample_id")
test <- merge(x = test, y = total, by = "sample_id")

# Extract and handle target variable
train_y <- train$ic50_Omicron
test$ic50_Omicron <- 0
test_id <- test$sample_id
train_id <- train$sample_id

# Subset unnecessary columns
train <- subset(train, select = -c(sample_id))
test <- subset(test, select = -c(sample_id))

# Impute missing values with 1
train[is.na(train)] <- 0
test[is.na(test)] <- 0

# Restore sample_id
test$sample_id <- test_id
train$sample_id <- train_id

# Save data
fwrite(train, "./project/volume/data/interim/train.csv")
fwrite(test, "./project/volume/data/interim/test.csv")
