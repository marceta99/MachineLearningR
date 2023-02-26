sales3Q <- quantile(Carseats$Sales, probs = 0.75)

Carseats$HighSales <- ifelse(test = Carseats$Sales > sales3Q, 
                             yes="Yes", 
                             no="No")
Carseats$HighSales <- as.factor(Carseats$HighSales)

Carseats$Sales <- NULL

summary(Carseats)

nums_vars <- c(1:5, 7,8)

apply(Carseats[,nums_vars], 2, shapiro.test)

non_norm_vars<- c(2, 3, 4, 7,8)

trainsformed_data <- apply(Carseats[, non_norm_vars],2, 
        function(a) scale(x= a, center = median(a), scale = IQR(a))
                           )
trainsformed_data <- as.data.frame(trainsformed_data)

trainsformed_data$Price <- scale(Carseats$Price,
                                 center = mean(Carseats$Price), 
                                 scale = sd(Carseats$Price))
trainsformed_data$Price <- as.vector(trainsformed_data$Price)

trainsformed_data$CompPrice <- scale(Carseats$CompPrice, 
                                     center = mean(Carseats$CompPrice), 
                                     scale = sd(Carseats$CompPrice))
trainsformed_data$CompPrice <- as.vector(trainsformed_data$CompPrice)

trainsformed_data$Urban <- as.integer(Carseats$Urban)
trainsformed_data$Us <- as.integer(Carseats$US)

levels(Carseats$ShelveLoc)

trainsformed_data$ShelveLoc <- factor(x = Carseats$ShelveLoc, 
                                      levels = c("Bad", "Medium","Good"))
trainsformed_data$ShelveLoc <- as.integer(trainsformed_data$ShelveLoc)

trainsformed_data$HighSales <- Carseats$HighSales

set.seed(1)
train_indicies <- createDataPartition(trainsformed_data$HighSales, p=0.8,
                                      list = FALSE)

train_data <- trainsformed_data[train_indicies, ]
test_data <- trainsformed_data[-train_indicies, ]


library(class)
knn1_pred <- knn(train = train_data[,-11], 
                 test = test_data[,-11], 
                 cl = train_data$HighSales, 
                 k=3
                 )

library(e1071)

tr_control <- trainControl(method = "cv", number = 10)

k_grid <- expand.grid(.k = seq(3, 25, 2))

knn_cv <- train(x = train_data[,-11], 
                y = train_data$HighSales, 
                method = "knn", 
                trControl = tr_control, 
                tuneGrid = k_grid)

best_k <- knn_cv$bestTune

knn2_pred <- knn(train = train_data[, -11], 
                 test = test_data[,-11], 
                 cl = train_data$HighSales, 
                 k = best_k)
cm1 <- table(actual = test_data$HighSales, predicted = knn2_pred)
cm1

compute_eval_measures <- function(cm){
  a <- sum(diag(cm))/sum(cm)
  p <- cm[1,1]/sum(cm[,1])
  r <- cm[1,1]/sum(cm[1,])
  f1 <- 2*r*p/(r+p)
  
  c(accuracy=a, precision = p, recall = r, F1 = f1)
}

eval1 <- compute_eval_measures(cm1)
eval1
 





