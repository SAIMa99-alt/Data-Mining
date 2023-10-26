colnames(new_train)
colnames(new_test)
library(dplyr)
head(new_train)

unique(new_train$job)
unique(new_train$education)
unique(new_train$contact)
unique(new_train$poutcome)

new_train$job <- factor(new_train$job, levels = c("blue-collar","entrepreneur","retired","admin.","student","services","technician","self-employed","management","unemployed","unknown","housemaid"),
                        labels = c(1,2,3,4,5,6,7,8,9,10,11,12))

unique(new_train$marital)
new_train$marital <- factor(new_train$marital,
                            levels = c("married","divorced","single","unknown"),
                            labels = c(1,2,3,4))

new_train$education <- factor(new_train$education,
                              levels= c("basic.9y","university.degree","basic.4y","high.school","professional.course","unknown","basic.6y","illiterate"),
                              labels = c(1,2,3,4,5,6,7,8))
unique(new_train$default)

new_train$default <- factor(new_train$default,
                              levels= c("unknown","no","yes"),
                              labels = c(1,2,3))


unique(new_train$housing)

new_train$housing <- factor(new_train$housing,
                            levels = c("yes","no","unknown"),
                            labels = c(1,2,3))

unique(new_train$loan)

new_train$loan <- factor(new_train$loan,
                            levels = c("yes","no","unknown"),
                            labels = c(1,2,3))

unique(new_train$contact)

new_train$contact <- factor(new_train$contact,
                         levels = c("cellular","telephone"),
                         labels = c(0,1))


unique(new_train$month)

new_train$month <- factor(new_train$month,
                            levels = c("mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),
                            labels = c(1,2,3,4,5,6,7,8,9,10))
length(unique(new_train$month))


unique(new_train$day_of_week)
length(unique(new_train$day_of_week))

new_train$day_of_week <- factor(new_train$day_of_week,
                          levels = c("mon","tue","wed","thu","fri"),
                          labels = c(1,2,3,4,5))


unique(new_train$poutcome)

new_train$poutcome <- factor(new_train$poutcome,
                                levels = c("success","failure","nonexistent"),
                                labels = c(1,2,3))


write.csv(test, file = "D:/11th Semester/test.csv", row.names = FALSE)


new_train$job <- as.numeric(new_train$job)


new_train$marital <- as.numeric(new_train$marital)

new_train$education <- as.numeric(new_train$education)

new_train$default <- as.numeric(new_train$default)

new_train$housing <- as.numeric(new_train$housing)

new_train$loan <- as.numeric(new_train$loan)

new_train$contact <- as.numeric(new_train$contact)

new_train$month <- as.numeric(new_train$month)

new_train$day_of_week <- as.numeric(new_train$day_of_week)

new_train$poutcome <- as.numeric(new_train$poutcome)

new_train$y <- as.factor(new_train$y)


library(tidyverse)
new_train <- new_train%>% filter(new_train$pdays == 999)

#Sampling

#train
total_instances <- nrow(new_train)

train_indices <- sample(total_instances, 5000)


train_data <- new_train[train_indices, ]

#Test

remaining_indices <- setdiff(1:total_instances, train_indices)

test_indices <- sample(remaining_indices, 2000)

test_data <- new_train[test_indices, ]

train_data <- subset(train_data, select = -c(pdays, previous))

test_data <- subset(test_data, select = -c(pdays, previous))


train_data <- subset(train_data, select = -c(pdays, previous))

test_data <- subset(test_data, select = -c(pdays, previous))
test_data$y <- factor(test_data$y,
                             levels = c("yes","no"),
                             labels = c(1,0))


train_data$y <- factor(train_data$y,
                      levels = c("yes","no"),
                      labels = c(1,0))

train[1,]
View(train)
train<- rename(train, "Class"= y)
test<- rename(test, "Class"= y)


train$Class <- factor(train$Class,
                       levels = c(1,0),
                       labels = c("yes","no"))

test$Class <- factor(test$Class,
                      levels = c(1,0),
                      labels = c("yes","no"))


train_data<- rename(train_data, "Class"=y)
test_data<- rename(test_data, "Class"=y)

train_data$Class <- factor(train_data$Class,
                      levels = c(1,0),
                      labels = c("yes","no"))

test_data$Class <- factor(test_data$Class,
                     levels = c(1,0),
                     labels = c("yes","no"))

