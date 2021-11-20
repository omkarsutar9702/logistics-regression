#import library
library(tidyverse)
library(caret)
library(ggplot2)
library(Amelia)
#import data
df<-mtcars

head(df)
str(df)

sum(is.na(df))
missmap(df)

#split the data
index<-createDataPartition(df$am, p=0.80,list = FALSE)
train<-df[index,]
test<-df[-index,]

#fitt the model 
model <-glm(am~. , data = train , family = binomial)

#model suammary
summary(model)
print(summary(model))

probabilities <- model %>% predict(test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 0, 1)

#predict the accuracy
mean(predicted.classes == test$am)

x <- as.integer(probabilities)
y <- test$am
l <- union(x, y)
Table2 <- table(factor(x, l), factor(y, l))
confusionMatrix(Table2)








