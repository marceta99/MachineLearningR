ds <- read.csv(file.choose())

summary(ds)

retail_data <- subset(ds, ds$Channel == "Retail")

retail_data$Channel <- NULL

apply(retail_data[,-1], 2, function(x)length(boxplot.stats(x)$out))

library(DescTools)

boxplot(retail_data$Grocery)

grocery_w <- Winsorize(retail_data$Grocery, probs = c(0, 0.95))

boxplot(grocery_w)

boxplot(retail_data$Frozen)

frozen_w <- Winsorize(retail_data$Frozen, probs = c(0, 0.95))

boxplot(frozen_w)

frozen_w <- Winsorize(retail_data$Frozen, probs = c(0, 0.94))

boxplot(frozen_w)

retail_data$Grocery <- grocery_w
retail_data$Frozen <- frozen_w 

summary(retail_data)

normalize_var <- function(x){
  (x-min(x)) / (max(x)-min(x))
}

retail_norm <- as.data.frame(apply(retail_data[,-1], 2, normalize_var))

summary(retail_norm)

retail_cor <- cor(retail_norm)

corrplot.mixed(retail_cor)

retail_norm$Grocery <- NULL

km1 <- kmeans(retail_norm, iter.max = 20, nstart = 1000, centers = 4)
km1

eval_measures <- data.frame()

for(k in 2:8){
  set.seed(1)
  km = kmeans(retail_norm, centers = k, iter.max = 20, nstart = 1000)
  eval_measures = rbind(eval_measures, 
                        c(k , km$tot.withinss, km$betweenss/km$totss))
  
  }
eval_measures

colnames(eval_measures) <- c("k", "tot.withinss", "ratio")

eval_measures

library(ggplot2)

ggplot(data=eval_measures, 
       mapping = aes(x=k, y=tot.withinss))+
       geom_line()+geom_point()+
       scale_x_continuous(breaks = 2:8)

km2 <- kmeans(x=retail_norm, centers = 3,iter.max = 20, nstart = 1000)
km2

source("Utility.R")

km3_stats <- summary.stats(retail_norm, km2$cluster, 3)
km3_stats















