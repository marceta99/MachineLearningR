train_data <- read.csv(file.choose())
test_data <- read.csv(file.choose())

summary(train_data)

no_cabin <-which(train_data$Cabin[train_data$Pclass == 1] == "")

length(which(train_data$Cabin[train_data$Pclass == 1] == ""))

char_vars <- c(4,5,12)

apply(train_data[,char_vars], 2,
      function(x)length(which((x =="" | x =="-" | x==" "))))

train_data$Embarked[train_data$Embarked =="" | 
                    train_data$Embarked =="-"|
                    train_data$Embarked ==" "] <- NA
summary(train_data)

shapiro.test(train_data$Age)

meadian_value <- median(train_data$Age, na.rm = TRUE)

train_data$Age[is.na(train_data$Age)] <- meadian_value

summary(train_data)

table(train_data$Sex)

train_data$Sex[is.na(train_data$Sex)] <- "male"

train_data$Name <- NULL

apply(train_data, 2, function(x)length(which(is.na(x) )) )

table(train_data$Embarked)

train_data$Embarked[is.na(train_data$Embarked)] <-"S"

apply(train_data, 2, function(x)length(which(is.na(x) )))













