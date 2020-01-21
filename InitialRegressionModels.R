# Importing data
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")
sample_sub <- read.csv("data/sample_submission.csv")
submission <- data.frame(Id=test$Id)

#### CORRELATION STUFF #####
# Let's look at some correlation things!
cor(train$SalePrice, train$OverallQual)

cor(train$SalePrice, train$X1stFlrSF + train$X2ndFlrSF + train$GarageArea +
      train$OpenPorchSF + train$WoodDeckSF + train$X3SsnPorch)

cor(train$SalePrice, train$YrSold - train$YearBuilt)
cor(train$SalePrice, train$YrSold - train$YearRemodAdd)
cor(train$SalePrice, train$GarageArea)
cor(train$SalePrice, train$GarageCars)
cor(train$SalePrice, train$YrSold - train$GarageYrBlt, use="complete.obs")


# MAKING SIMPLE LINEAR MODEL USING OVERALLQUAL #
# This is the simplest possible regression model
# This is only to be used for testing purposes
q_model <- lm(SalePrice~OverallQual, data=train)
summary(q_model)

submission$SalePrice <- predict(q_model, newdata=data.frame(OverallQual = test$OverallQual))

write.csv(submission, file="submission.txt", quote=FALSE, row.names=FALSE)

################################################
# End Simple OverallQual Regression Model #
################################################

################################################
# This model looks more at Time since initial build and remodel of each house #
################################################
train['TimeSinceBuild'] <- train$YrSold - train$YearBuilt
train['TimeSinceRemodel'] <- train$YrSold - train$YearRemodAdd

test['TimeSinceBuild'] <- test$YrSold - test$YearBuilt
test['TimeSinceRemodel'] <- test$YrSold - test$YearRemodAdd

test[is.na(test$GarageCars),"GarageCars"] <- mean(test$GarageCars, na.rm = TRUE)


model <- lm(SalePrice ~ OverallQual*TimeSinceBuild*TimeSinceRemodel*GarageCars, data=train)
summary(model)

submission$SalePrice <- predict(model, newdata=test)

write.csv(submission, file="submission.txt", quote=FALSE, row.names=FALSE)
################################################
# END
################################################


train['TimeSinceBuild'] <- train$YrSold - train$YearBuilt
train['TimeSinceRemodel'] <- train$YrSold - train$YearRemodAdd
train['AreaThings'] <- train$X1stFlrSF + train$X2ndFlrSF + train$GarageArea +
  train$OpenPorchSF + train$WoodDeckSF + train$X3SsnPorch

test['TimeSinceBuild'] <- test$YrSold - test$YearBuilt
test['TimeSinceRemodel'] <- test$YrSold - test$YearRemodAdd
test['AreaThings'] <- test$X1stFlrSF + test$X2ndFlrSF + test$GarageArea +
  test$OpenPorchSF + test$WoodDeckSF + test$X3SsnPorch

model <- lm(SalePrice ~ OverallQual*TimeSinceBuild*TimeSinceRemodel*AreaThings, data=train)
summary(model)

submission$SalePrice <- predict(model, newdata=test)

write.csv(submission, file="submission.txt", quote=FALSE, row.names=FALSE)

