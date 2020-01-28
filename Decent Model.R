# Importing data
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")

train.df <- data.frame(SalePrice = train$SalePrice)
test.df <- data.frame()

# Let's deal with NA Values:
train[is.na(train$Neighborhood),"Neighborhood"] <- "Veenker"
test[is.na(test$Neighborhood),"Neighborhood"] <- "Veenker"

train[is.na(train$KitchenQual),"KitchenQual"] <- "TA"
test[is.na(test$KitchenQual),"KitchenQual"] <- "TA"

train[is.na(train$TotalBsmtSF), "TotalBsmtSF"] <- mean(train$TotalBsmtSF, na.rm = TRUE)
test[is.na(test$TotalBsmtSF), "TotalBsmtSF"] <- mean(test$TotalBsmtSF, na.rm = TRUE)

train[is.na(train$GarageArea), "GarageArea"] <- mean(train$GarageArea, na.rm = TRUE)
test[is.na(test$GarageArea), "GarageArea"] <- mean(test$GarageArea, na.rm = TRUE)

train[is.na(test$GarageCars),"GarageCars"] <- mean(train$GarageCars, na.rm = TRUE)
test[is.na(test$GarageCars),"GarageCars"] <- mean(test$GarageCars, na.rm = TRUE)

# Now, let's get our dummy variables in order:
train.neighborhood.df <- data.frame(model.matrix( ~ Neighborhood - 1, data=train))
train.kitchen.df <- data.frame(model.matrix( ~ KitchenQual - 1, data=train))

test.neighborhood.df <- data.frame(model.matrix( ~ Neighborhood - 1, data=test))
test.kitchen.df <- data.frame(model.matrix( ~ KitchenQual - 1, data=test))

train.df <- cbind(train.df, train.kitchen.df, train.neighborhood.df)
test.df <- cbind(test.kitchen.df, test.neighborhood.df)


# Now for our other variables:
train.df['OverallQual'] <- train$OverallQual
test.df['OverallQual'] <- test$OverallQual

train.df['ImportantAreas'] <- train$TotalBsmtSF + train$X1stFlrSF + train$X2ndFlrSF + train$GarageArea + train$WoodDeckSF
test.df['ImportantAreas'] <- test$TotalBsmtSF + test$X1stFlrSF + test$X2ndFlrSF + test$GarageArea + test$WoodDeckSF

train.df['GarageCars'] <- train$GarageCars
test.df['GarageCars'] <- test$GarageCars


# Let's make this model!
model <- lm(SalePrice ~ ImportantAreas*OverallQual+., data = train.df)
summary(model)

submission <- data.frame(Id=test$Id, SalePrice=predict(model, newdata=test.df))
paste("Submission Good To Go?", sum(is.na(submission$SalePrice)) == 0)
write.csv(submission, file="submission.txt", quote=FALSE, row.names=FALSE)
