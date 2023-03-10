#Pulling the dataframe
df <- read.csv("C:/Users/Ncheo/Downloads/diamonds.csv")
head(df)
str(df)

#Changing the character columns to factors
df <- as.data.frame(unclass(df), stringsAsFactors = T)
str(df)

library(ggplot2)

ggplot(df, aes(cut)) + geom_bar()

#Per Kaggle page, there are many duplicate diamonds on here, and you can filter them out by removing depth, table, x, y , and z.
df1 <- df[!duplicated(df[c(1, 2, 3, 4, 7)]),]
str(df1)


ggplot(df1, aes(cut)) + geom_bar()
ggplot(df1, aes(clarity)) + geom_bar()
ggplot(df1, aes(x = factor(cut, level = c("b'Fair'", "b'Good'", "b'Very Good'", "b'Premium'", "b'Ideal'")), price)) + geom_point(aes(color = cut), alpha = .6, position = 'jitter')
ggplot(df1, aes(x = factor(cut, level = c("b'Fair'", "b'Good'", "b'Very Good'", "b'Premium'", "b'Ideal'")), price)) + geom_boxplot()
ggplot(df1, aes(carat, price)) + geom_point()

library(dplyr)
library(caTools)
set.seed(101)

sample <- sample.split(df1$price, SplitRatio = .7)
train <- subset(df1, sample == T)
test <- subset(df1, sample == F)

model <- lm(price ~., train)
summary(model)

price.predictions <- predict(model, test)

results <- cbind(price.predictions, test$price)
colnames(results) <- c('pred', 'real')
results <- as.data.frame(results)


head(results)

to_zero <- function(x){
  if (x < 0){
    return(0)
  }else{
    return(x)
  }
}

results$pred <- sapply(results$pred, to_zero)

mse <- mean((results$real - results$pred)^2)
mse


SSE = sum((results$pred - results$real)^2)
SST = sum( (mean(df1$price) - results$real)^2)

R2 = 1 - SSE/SST
R2

