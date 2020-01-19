# Simple Linear Regression using OverallQual

# Importing data
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")
sample_sub <- read.csv("data/sample_submission.csv")

q_model <- lm(SalePrice~OverallQual, data=train)

submission <- data.frame(Id = test$Id)

submission$SalePrice <- predict(q_model, newdata=data.frame(OverallQual = test$OverallQual))

write.csv(submission, file="submission.txt", quote=FALSE, row.names=FALSE)