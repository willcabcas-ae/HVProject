# title: "Cancer Diagnostic"
# subtitle: 'HarvardX - PH125.9x Data Science - Capstone - Project 2'
# author: "William C. C."
# date: "April-2020"

# The code has 4 parts:
# 1. Project initialization
# 2. Dataset exploration1
# 3. Building machine learning algorithms
# 4. Overview of results


#_.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
#_.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# Part 1 - Project initialization
#_.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
#_.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._

# P1 S1 Initialization_code

# Install libraries

if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) 
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ranger)) 
  install.packages("ranger", repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) 
  install.packages("naivebayes", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) 
  install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) 
  install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(knitr)) 
  install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(e1071)) 
  install.packages("e1071", repos = "http://cran.us.r-project.org")

# Dataset
url1 <- 'https://raw.githubusercontent.com/gmineo/Breast-Cancer-Prediction-Project/master/'
url2 <- 'data.csv'
url_data <- (paste(url1,url2,sep=""))
rm(url1,url2)

# importing dataset
df <- read.csv(url_data) %>% select(-X)

# Split data into train (75%) and test sets (25%)
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead

test_index <- createDataPartition(y = df$diagnosis, times = 1, p = 0.25, list = FALSE)

train <- df[-test_index,]
test <- df[test_index,]


#_.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
#_.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# Part 2 - Dataset exploration
#_.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
#_.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._

# P2_s2_head

# Extracting top records

head(train)


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# P3_s2_summary 

# Overview of summary information

summary(train)


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART_1 

# Radius mean by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,3])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Radius mean incidence') +
  ggtitle('Radius mean by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART2 

# Texture mean by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,4])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Texture mean') +
  ggtitle('Texture mean by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART_3 

# Perimeter mean by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,5])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Perimeter mean') +
  ggtitle('Perimeter mean by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART_4 

# Area mean by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,6])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Area mean') +
  ggtitle('Area mean by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART5 

# smoothness mean by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,7])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('smoothness mean') +
  ggtitle('smoothness mean by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART6 

# Compactness mean by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,8])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Compactness mean') +
  ggtitle('Compactness mean by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART7 

# Concavity mean by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,9])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Concavity mean') +
  ggtitle('Concavity by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART8 

# Concave points by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,10])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Concave points') +
  ggtitle('Concave points by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART9 

# Symmetry means by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,11])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Symmetry means') +
  ggtitle('Symmetry means by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART10 

# Fractal dimension means by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,12])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Fractal dimension means') +
  ggtitle('Fractal dimension means by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART11

# Radius SE by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,13])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Radius SE') +
  ggtitle('Radius SE by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART12

# Texture SE by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,14])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Texture SE') +
  ggtitle('Texture SE by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART13

# Perimeter SE by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,15])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Perimeter SE') +
  ggtitle('Perimeter SE by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART14

# Area SE by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,16])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Area SE') +
  ggtitle('Area SE by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART15

# Smooth SE by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,17])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Smooth SE') +
  ggtitle('Smooth SE by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART16

# compactness SE by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,18])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('compactness SE') +
  ggtitle('compactness SE by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART23

# Perimeter worst by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,25])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Perimeter worst') +
  ggtitle('Perimeter worst by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART24

# Radius worst by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,23])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Radius worst') +
  ggtitle('Radius worst by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# CHART25

# Area worst by classes chart

train %>% 
  ggplot(aes(x=diagnosis, y=train[,26])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Area worst') +
  ggtitle('Area worst by classes') +
  theme_economist()


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# P_10_S2 Correlation table

# Creating table
Cor_Tab <- cor(train[,3:32])

formatC(Cor_Tab, digits = 2, format = 'f', big.mark = ',') %>% knitr::kable()


#_.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# Part 3 - Building machine learning algorithms
#_.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._

# P11_S2 Key Parameters

# Define train and test sets

Train_prediction <- as.matrix(train[,3:32]) #Train_Predictors
Train_class <- train$diagnosis #Train_Class
Test_prediction <- as.matrix(test[,3:32]) #Test_Predictors
Test_class <- test$diagnosis #Test_Class


CrlF <- trainControl(method="cv",          #Control the computational nuances of thetrainfunction
                           number = 15,    #Either the number of folds or number of resampling iterations
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# Alg 1 - Generalized Linear Model
# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._

# P_12_S2 GLM model

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead

train_glm <- train(x = Train_prediction, y = Train_class, method = "glm",
                   metric = "ROC",
                   preProcess = c("scale", "center"),  # in order to normalize the data
                   trControl= CrlF)

y_hat_glm <- predict(train_glm, Test_prediction)

m1acc <- confusionMatrix(y_hat_glm, Test_class)$overall['Accuracy']

cat('Model accuracy is equal to ', 
    formatC(m1acc, digits = 5, format = 'f', big.mark = ','), '. ',  
    'Below is the confusion matrix:', sep = "")

confusionMatrix(y_hat_glm, Test_class)$table


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# Alg 2 - k-Nearest Neighbors
# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._

# P13 S2 KNN_model

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead

train_knn <- train(x = Train_prediction, y = Train_class, method="knn",
                   metric="ROC",
                   preProcess = c('center', 'scale'),
                   tuneLength=10, #The tuneLength parameter tells the algorithm to try different default values for the main parameter
                   #In this case we used 10 default values
                   trControl=CrlF)

y_hat_knn <- predict(train_knn, Test_prediction)

m2acc <- confusionMatrix(y_hat_knn, Test_class)$overall['Accuracy']

cat('Model accuracy is equal to ', 
    formatC(m2acc, digits = 5, format = 'f', big.mark = ','), '. ',  
    'Below is the confusion matrix:', sep = "")

confusionMatrix(y_hat_knn, Test_class)$table

# Shows optimal k from finalModel. 
cat('Optimal k is ', 
    formatC(train_knn$finalModel$k, digits = 0, 
            format = 'f', big.mark = ','), 
    sep = "")


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# Alg 3 - Random Forest
# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._

# P 14 S2 RF model

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead

# Setting tuning parameters for the train function
grid <- expand.grid(mtry=seq(1, 5, 1), 
                    splitrule = c("extratrees", "gini"), 
                    min.node.size = seq(1, 50, 10))

train_rf <- train(x = Train_prediction, y = Train_class, method="nnet",
                  metric="ROC",
                  preProcess=c('center', 'scale', 'pca'),
                  tuneLength=10,
                  trace=FALSE,
                  trControl=CrlF)

y_hat_rf <- predict(train_rf, Test_prediction)

m3acc <- confusionMatrix(y_hat_rf, Test_class)$overall['Accuracy']

cat('Model accuracy is equal to ', 
    formatC(m3acc, digits = 5, format = 'f', big.mark = ','), '. ',  
    'Below is the confusion matrix:', sep = "")

confusionMatrix(y_hat_rf, Test_class)$table



# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# Alg 4 - Naive Bayes
# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._

# P 15 S2 NB model

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead

# Setting train control argument for the train function
control <- trainControl(method = "cv", number = 10, p = .9)

train_nb <- train(x = Train_prediction, y = Train_class, method="nb",
                  metric="ROC",
                  preProcess=c('center', 'scale'), #in order to normalize de data
                  trace=FALSE,
                  trControl=CrlF) 

y_hat_nb <- predict(train_nb, Test_prediction)

m4acc <- confusionMatrix(y_hat_nb, Test_class)$overall['Accuracy']

cat('Model accuracy is equal to ', 
    formatC(m4acc, digits = 5, format = 'f', big.mark = ','), '. ',  
    'Below is the confusion matrix:', sep = "")

confusionMatrix(y_hat_nb, Test_class)$table


# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# Approach 5 - SVM with Polynomial Kernel
# _.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._

# P 16 S2 SVM model

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead

train_svm <- train(x = Train_prediction, y = Train_class, method = "svmPoly")

y_hat_svm <- predict(train_svm, Test_prediction)

m5acc <- confusionMatrix(y_hat_svm, Test_class)$overall['Accuracy']

cat('Model accuracy is equal to ', 
    formatC(m5acc, digits = 5, format = 'f', big.mark = ','), '. ',  
    'Below is the confusion matrix:', sep = "")

confusionMatrix(y_hat_svm, Test_class)$table


#_.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
#_.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# Part 4 - Overview of results
#_.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
#_.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._

# P 17 S3 Results_table

models <- c('1. Generalized Linear Model', 
            '2. k-Nearest Neighbors', 
            '3. Random Forest', 
            '4. Naive Bayes', 
            '5. Support Vector Machines with Polynomial Kernel')

acc_values <- formatC(c(m1acc, m2acc, m3acc, m4acc, m5acc), digits = 5, big.mark=",")

acc_table  <- data.frame(Accuracy = acc_values, row.names = models)

acc_table %>% knitr::kable()



#_.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
# TopImpact KNN 

# Top predictors for KNN

plot(varImp(train_knn), top=30, main="Top predictors - KNN")


