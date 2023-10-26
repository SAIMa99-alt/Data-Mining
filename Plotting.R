

library(ggplot2)
library(dplyr)

library(hrbrthemes)

library(scales)




data <- data.frame(euAccuracy,manAccuracy,maxAccuracy, k_values)

  ggplot(data,aes(x=data$k_values, y=data$euAccuracy)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  scale_x_continuous(breaks = seq(0,20, 1)) +
  theme_ipsum() +
  labs(x = "K Values", y= "Accuracy")+
  ggtitle("KNN Using Euclidean Method")
  
  
  
  
  
  ggplot(data,aes(x=data$k_values, y=data$manAccuracy)) +
    geom_line( color="grey") +
    geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
    scale_x_continuous(breaks = seq(0,20, 1)) +
    theme_ipsum() +
    labs(x = "K Values", y= "Accuracy")+
    ggtitle("KNN Using Manhatten Method")
  
  
  
  ggplot(data,aes(x=data$k_values, y=data$maxAccuracy)) +
    geom_line( color="grey") +
    geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
    scale_x_continuous(breaks = seq(0,20, 1)) +
    theme_ipsum() +
    labs(x = "K Values", y= "Accuracy")+
    ggtitle("KNN Using Max Dimensional Distance")
  
  
eumean <- mean(data$euAccuracy)
manmean <- mean(data$manAccuracy)
maxmean <- mean(data$maxAccuracy)


# Create a data frame
df <- data.frame(Category = c("Euclidean", "Manhatten", "Max Dimensional"),
                 Value = c(max(data$euAccuracy),max(data$manAccuracy),max(data$maxAccuracy)))

# Create a bar plot
ggplot(data = df, aes(x = Category, y = Value)) +
  geom_bar(stat = "identity", fill = "pink") +
  xlab("Method") +
  ylab("MAX Accuracy") +
  ggtitle("MAX Accuracy of Methods")



