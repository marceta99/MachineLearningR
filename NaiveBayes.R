sales3Q <- quantile(Carseats$Sales , 0.75)

Carseats$HighSales <- ifelse(Carseats$Sales > sales3Q, 
                             yes = "Yes", 
                             no="No")
Carseats$HighSales <- as.factor(Carseats$HighSales)

Carseats$Sales <- NULL

num_vars <- c(1:5, 7,8)

apply(Carseats[, num_vars], 2, shapiro.test)

library(bnlearn)

to_discretize <- c(2,4, 7,8)

transformed_data <- discretize(Carseats[,to_discretize], 
                               method = "quantile", 
                               breaks = 5)
summary(transformed_data)

transformed_data <- cbind(transformed_data, Carseats[,-to_discretize])
transformed_data

library(caret)
set.seed(1)
train_indicies <- createDataPartition(transformed_data$HighSales, p=0.8, list = FALSE)

train_data <- transformed_data[train_indicies,]  
test_data <- transformed_data[-train_indicies, ]  

library(e1071)

nb1 <- naiveBayes(HighSales ~ . , data = train_data)

nb1_pred <- predict(nb1, newdata = test_data, type = "class")
nb1_pred

cm1 <- table(actual = test_data$HighSales, predicted = nb1_pred)
cm1

#eval1 <- computeEvalMeasures(cm)

nb2_pred <- predict(nb1, test_data, type = "raw")
nb2_pred

library(pROC)

nb2_roc <- roc(response = as.integer(test_data$HighSales), 
               predictor = nb2_pred[,1], 
               levels= c(2,1)
               )

plot.roc(nb2_roc)

plot.roc(nb2_roc, 
         print.thres = "best", 
         print.thres.best.method = "youden")

nb3_pred <- ifelse(test = nb2_pred[,1]>0.79, 
                   yes = "No", 
                   no = "Yes")
nb3_pred

cm2 <-table(actual = test_data$HighSales, predicted = nb3_pred)
cm2
cm1
#eval2 <- computeEvalMeausures(cm2)








