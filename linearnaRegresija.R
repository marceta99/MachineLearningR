library(MASS)
library(ggplot2)
library(corrplot)

str(Boston)

boston_cor <- cor(Boston)

corrplot(boston_cor)

corrplot(boston_cor, type = "upper", diag = FALSE)
corrplot.mixed(boston_cor)

ggplot(Boston, aes(x= medv,y = lstat))+geom_point()
ggplot(Boston, aes(x= medv, y= rm))+geom_point()

library(caret)

set.seed(1)
train_indicies <- createDataPartition(Boston$medv, p=0.8, list = FALSE)

boston_train <- Boston[train_indicies, ]
boston_test <- Boston[-train_indicies, ]

lm1 <- lm(medv ~ lstat + rm + indus+ptratio, data = Boston)
summary(lm1)

par(mfrow = c(2,2))
plot(lm1)

lm1_pred <- predict(lm1, newdata= boston_test)
lm1_pred

boston_test$medv_pred <- lm1_pred 

RSS <- sum((boston_test$medv - boston_test$medv_pred)^2)
TSS <- sum((boston_test$medv - mean(boston_train$medv))^2)
R2 <- (TSS - RSS)/TSS
RMSE <- sqrt(RSS / nrow(boston_test))


