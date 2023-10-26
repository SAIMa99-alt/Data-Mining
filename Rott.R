#Functions to calculate the distances
euclidean_distance <- function(a1, a2,a3, b1, b2,b3) {
  sqrt((a1 - b1)^2 + (a2 - b2)^2 + (a3 - b3)^2)
}


manhattan_distance <- function(a1, a2,a3, b1, b2,b3) {
  abs(a1 - b1) + abs(a2 - b2) +  abs(a3 - b3)
}


max_dimensional_distance <- function(a1, a2,a3, b1, b2,b3) {
  max(abs(a1 - b1), abs(a2 - b2) ,abs(a3 - b3))
}





# Function to perform k-NN classification
knn_classification <- function(test_data, train_data, k, distance_function) {
  predictions <- character(nrow(test_data))
  
  for (i in 1:nrow(test_data)) {
    distances <- numeric(nrow(train_data))
    
    for (j in 1:nrow(train_data)) {
      distances[j] <- distance_function(test_data$age[i], test_data$job[i], test_data$loan[i],
                                        train_data$age[j], train_data$job[j], train_data$loan[i])
    }
    
    k_indices <- order(distances)[1:k]
    k_class <- train_data$Class[k_indices]
    predictions[i] <- names(which.max(table(k_class)))
  }
  
  return(predictions)
}



# Define K values
k_values <- c(1,3,5,7,9,11,13,15,17,19,20)


#Euclidean
euAccuracy <- numeric(length(k_values))
m =1
for (k in k_values) {
  
  predictions <- knn_classification(test, train, k, euclidean_distance)
  accuracy <- sum(predictions == test$Class) / nrow(test)
  euAccuracy[m] <- accuracy
  recall <- sum(predictions == "yes" & test$Class == "yes") /
    sum(test$Class == "yes")

  cat("k:", k, "\n")
  cat("Accuracy:", accuracy * 100, "%\n")
  cat("Recall:", recall * 100, "%\n")
  cat("----------------------------\n")
  m=m+1
}


#Manhattan
train <- train[sample(nrow(train)), ]
test <- test[sample(nrow(test)), ]

manAccuracy <- numeric(length(k_values))
i =1
for (k in k_values) {
  
    
  
  predictions <- knn_classification(test, train, k, manhattan_distance)
  accuracy <- sum(predictions == test$Class) / nrow(test)
  manAccuracy[i] <- accuracy
  recall <- sum(predictions == "yes" & test$Class == "yes") /
    sum(test$Class == "yes")
  
  
  cat("k:", k, "\n")
  cat("Accuracy:", accuracy * 100, "%\n")
  cat("Recall:", recall * 100, "%\n")
  cat("----------------------------\n")
  i=i+1
}

#Max_Dimensional_Distance
p =1
train <- train[sample(nrow(train)), ]
test <- test[sample(nrow(test)), ]

maxAccuracy <- numeric(length(k_values))

for (k in k_values) {
  
  predictions <- knn_classification(test, train, k, max_dimensional_distance)
  accuracy <- sum(predictions == test$Class) / nrow(test)
  maxAccuracy[p] <- accuracy
  recall <- sum(predictions == "yes" & test$Class == "yes") /
    sum(test$Class == "yes")
  
  
  cat("k:", k, "\n")
  cat("Accuracy:", accuracy * 100, "%\n")
  cat("Recall:", recall * 100, "%\n")
  cat("----------------------------\n")
 p=p+1 
}






