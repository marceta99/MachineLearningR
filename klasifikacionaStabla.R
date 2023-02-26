?Carseats

sales_75_percentile <- quantile(Carseats$Sales, probs = 0.75)

Carseats$HighSales <- ifelse(test = Carseats$Sales > sales_75_percentile, 
                             yes = "Yes", 
                             no= "No")
Carseats$HighSales <- as.factor(Carseats$HighSales)

Carseats$Sales <- NULL

library(caret)

set.seed(1)
train_indicies <- createDataPartition(Carseats$HighSales, p=0.8, list = FALSE)

train_data <- Carseats[train_indicies, ]
test_data <- Carseats[-train_indicies, ]

library(rpart)

set.seed(1)
tree1 <- rpart(HighSales ~ . , data = train_data)

library(rpart.plot)
rpart.plot(tree1, extra = 104)

tree1_predict <- predict(tree1, newdata = test_data, type = "class")
head(tree1_predict)

cm1 <- table(actual = test_data$HighSales, predicted = tree1_predict)
cm1

compute_eval_measures <- function(cm){
  a <- sum(diag(cm))/sum(cm) #odnosn tacno predvidjenih obzervacija i svih obzervacija
  p <- cm[1,1] / sum(cm[,1])
  r <- cm[1,1] / sum(cm[1,])
  f1 <- 2*p*r / (p+r)
  
  c(accuracy = a, precision = p, recall = r, F1 = f1)
}

eval1 <- compute_eval_measures(cm1)
eval1

#Now create better model and  with cross validation find optimal value for cp parameter

library(e1071)
tr_control <- trainControl(method = "cv", number = 10)

cp_grid <- expand.grid(.cp = seq(0.001, 0.02, 0.0005))

set.seed(1)
tree_cv <- train(HighSales ~ . ,
                 train_data, 
                 control = rpart.control(minsplit = 10), 
                 method = "rpart", 
                 tuneGrid = cp_grid, 
                 trControl = tr_control
                 )
tree_cv

set.seed(1)
tree2 <- rpart(HighSales ~. , 
               data = train_data, 
               control = rpart.control(minsplit = 10, cp = 0.02)
               )

tree_pred <- predict(tree2, newdata = test_data, type="class")
tree_pred

cm2 <- table(actual = test_data$HighSales, predicted = tree_pred)

eval2 <- compute_eval_measures(cm2)

eval1
eval2













