#loading libraries
install.packages("mltools")
library(dplyr)
library(corrplot)
library(mltools)
library(caret)
library(philentropy)
library(tidyr)
library(caTools)
library(caret)

#loading data
data<- read.csv("combined_data.csv")
View(data)
unique(data$country)
summary(data)
str(data)
data<- data[,-c(1, 9,12,13, 15,18,20,22)]
any(is.na(data))


# One Hot Encoding
one_hot_encode_dataframe <- function(data) {
  column_types <- sapply(data, class)
  categorical_columns <- names(data)[sapply(data, is.character)]
  
  for (col in categorical_columns) {
    data <- cbind(data, model.matrix(~ . - 1, data = data[col]))
  }
  
  data <- data[, !(names(data) %in% categorical_columns)]
  
  return(data)
}

result_data <- one_hot_encode_dataframe(data)
View(result_data)
str(result_data)
df<- as.data.frame(lapply(result_data, as.numeric))
View(df)
str(df)


#k-means post one hot encoding 
set.seed(123)  # For reproducibility
k_values <- 2:10  # Choose a range of k values to evaluate

#kmeans_results <- lapply(k_values, function(k) kmeans(df, centers = k))

#wss <- sapply(kmeans_results, function(km) sum(km$withinss))

#plot(k_values, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Total Within-Cluster Sum of Squares (WSS)")

optimal_k <- 2
kmeans_results <- kmeans(df, centers = optimal_k)
summary(kmeans_results)
kmeans_results$size

cluster_assignments <- kmeans_results$cluster
data_with_clusters <- cbind(df, Cluster = cluster_assignments)

library(factoextra)
fviz_cluster(kmeans_results, df) # error in plotting

summary(kmeans_results$centers)


########
hc<- hclust(dist(df), method="complete")
plot(hc)

clusters<-cutree(hc, h=6000)
centroids<- tapply(df, clusters, FUN = colMeans)
print(centroids)

#Assign dataset to variable df.
df <- combined_data

#Checking number of rows in df.
nrow(df)

#Checking column names of df.
colnames(df)

#Making subscriber column numerical in df.
df$sub <- ifelse(df$purchased_subscription, 1, 0)
print(df$sub)




#Splitting the data for training and testing the model.
split = sample.split(df$sub, SplitRatio = 0.8)
train.df = subset(df, split == TRUE)
test.df = subset(df, split == FALSE)

#Training the model on key variables
duo.sub.model.train <- glm(sub ~ n_days_on_platform 
                           + n_active_days 
                           + country 
                           + n_lessons_completed, 
                           data = train.df, 
                           family = binomial())

#Predicting the Logistic model on test data.
predicted_test <- predict(duo.sub.model.train,
                          newdata = test.df,
                          type = "response")

#Creating a data frame of test and predicted data.
data.frame(test.df$sub,
           predicted_test)[1:10,]

#Checking the number of 0's and 1's in test data.
table(test.df$sub)

#Confusion matrix of the Logistic model
confusionMatrix(data = as.factor(predicted_test>0.5), 
                reference = as.factor(test.df$sub>0.5))

#Loading library.
library(ROCR)


LR_pred <- predict(duo.sub.model.train,
                   type='response',
                   newdata=subset(df))

df.pred <- prediction(LR_pred,df$sub)
df.perf <- performance(df.pred,"tpr","fpr")

#Calculate the area under the curve.
perf2 <- performance(df.pred, "auc")
auroc<- perf2@y.values
print(auroc)


#Plotting the model for DuoLingo.
plot(df.perf,col="red") + abline(coef = c(0, 1))








