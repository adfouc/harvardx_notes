## Module 8 MACHINE LEARNING
##


## En résumé
# 
# logistic regression
# glm_fit <- train_set %>% 
#   mutate(y = as.numeric(sex == "Female")) %>%
#   glm(y ~ height, data=., family = "binomial")
# p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response") # => conditional probabilities
# y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
# confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]
#
# train_glm <- train(train_x, train_y,
#                    method = "glm")
# glm_preds <- predict(train_glm, test_x)
# 
# bin smoothing
# fit <- with(polls_2008, ksmooth(day, margin, x.points = day, kernel="box", bandwidth =7))
# fit<-loess(margin~day,degree = 1, span = span, data=polls_2008)
# nota: loess = local regression model degree d + kernel smoothing
#
# QDA /LDA
# library(caret)
# train_qda <- train(y ~., method = "qda", data = mnist_27$train)
# # Obtain predictors and accuracy
# y_hat <- predict(train_qda, mnist_27$test)
# confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
#
# K nearest neighbours
# train(x, y, method ="knn", tuneGrid = data.frame(k=seq(1,7,2)) )
#
# modelLookup("rf")
# getModelInfo("rf")
# random forests
# train (x,y, method="rf", tuneGrid = data.frame(mtry=seq(50,200,25)), nodesize=1) #, ntree=1)
#
# variable importance
# varImp(object =  fit) 
# varImp(fit_rf$finalModel)
#
# sweep(x, 2, colMeans(x))
# 
# library(matrixStats)
# sds <- colSds(x, na.rm = TRUE)
#
# d <- dist(x)
# h <- hclust(d)
# groups <- cutree(h, k = 10)
# names(groups)[groups==4]
#
# k <- kmeans(x_0, centers = 10, nstart = 25)
#
# colors <- brewer.pal(7, "Dark2")[as.numeric(y)]
# heatmap(x, col = RColorBrewer::brewer.pal(11, "Spectral"), ColSideColors = colors)
#   col = RColorBrewer::brewer.pal(11, "Spectral")
#   col = brewer.pal(11, "RdBu")


# Caret package, training and test sets, and overall accuracy

# http://topepo.github.io/caret/available-models.html

# To mimic the ultimate evaluation process, we randomly split our data into two — a training set and a test set — and act as if we don’t know the outcome of the test set. 
# We develop algorithms using only the training set; the test set is used only for evaluation.
# The createDataPartition()  function from the caret package can be used to generate indexes for randomly splitting data.
# The simplest evaluation metric for categorical outcomes is overall accuracy: the proportion of cases that were correctly predicted in the test set.

library(tidyverse)
library(caret)
library(dslabs)
data(heights)

# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2007)
test_index <- createDataPartition( y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]
nrow(heights)
nrow(test_set)
nrow(train_set)

# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

# compute accuracy
mean(y_hat == test_set$sex)
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
max(accuracy) #  optimal on train set
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex) # accuracy on test set

##

mnist <- read_mnist()
i <-5
image(1:28, 1:28, matrix(mnist$test$images[i,], nrow=28)[ , 28:1], 
      col = gray(seq(0, 1, 0.05)), xlab = "", ylab="")
## the labels for this image is: 
mnist$test$labels[i]

str(mnist)
str(mnist$train)
str(mnist$train$images)
nrow(mnist$train$images)+nrow(mnist$test$images)
levels(mnist$train$labels)
levels(factor(mnist$train$labels))
?levels
?read_mnist
# How many features are available to us for prediction in the mnist digits dataset?
ncol(mnist$train$images) # 784

#-----------------------------------------------
# Confusion matrix

# Overall accuracy can sometimes be a deceptive measure because of unbalanced classes.
# A general improvement to using overall accuracy is to study sensitivity and specificity separately. 
# Sensitivity, also known as the true positive rate or recall, is the proportion of actual positive outcomes correctly identified as such. 
# Specificity, also known as the true negative rate, is the proportion of actual negative outcomes that are correctly identified as such.
# A confusion matrix tabulates each combination of prediction and actual value. You can create a confusion matrix in R using the table() function or the confusionMatrix() function from the caret package.

# tabulate each combination of prediction and actual value
?table
table(predicted = y_hat, actual = test_set$sex)

test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))

prev <- mean(y == "Male")

confusionMatrix(data = y_hat, reference = test_set$sex)


#                     Actually Positive	    Actually Negative
# Predicted positive	True positives (TP)	  False positives (FP)
# Predicted negative	False negatives (FN)	True negatives (TN)

# sensitivity = TP / (TP+FN)  This quantity is referred to as the true positive rate (TPR) or recall.
# Specificity = TN (TN + FP)  This quantity is also called the true negative rate (TNR).
# positive predictive value PPV = TP / (TP + FP)  or precision 

#-----------------------------------------------
# Balanced accuracy and F1 score
# For optimization purposes, sometimes it is more useful to have a one number summary than studying both specificity and sensitivity. 
# One preferred metric is balanced accuracy. 
# Because specificity and sensitivity are rates, it is more appropriate to compute the harmonic average. 
# In fact, the F1-score, a widely used one-number summary, is the harmonic average of precision and recall. 
# Depending on the context, some type of errors are more costly than others. 
# The F1-score can be adapted to weigh specificity and sensitivity differently. 
# You can compute the F1-score using the F_meas() function in the caret package.

# maximize F-score
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})
max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

confusionMatrix(data = y_hat, reference = test_set$sex)

#-----------------------------------------------
# Prevalence matters in practice
# A machine learning algorithm with very high sensitivity and specificity may not be useful in practice when prevalence is close to either 0 or 1. 
# For example, if you develop an algorithm for disease diagnosis with very high sensitivity, but the prevalence of the disease is pretty low, 
# then the precision of your algorithm is probably very low based on Bayes' theorem.
# P(y = 1 | yhat=1) = P(yhat=1 | y=1 ) * p(y=1) / p (yhat=1) 
# precision = sensitivity * prevalence of the disease / prevalence of the desease in the dataset
# precision = sensitivity * 5/1000 / 0.5 = sensitivity * .01 so precision < 0.01


#-----------------------------------------------
# ROC and precision-recall curves
# 
# A very common approach to evaluating accuracy and F1-score is to compare them graphically by plotting both. 
# A widely used plot that does this is the receiver operating characteristic (ROC) curve. 
# The ROC curve plots sensitivity (TPR) versus 1 - specificity or the false positive rate (FPR).
# # However, ROC curves have one weakness and it is that neither of the measures plotted depend on prevalence. 
# In cases in which prevalence matters, we may instead make a precision-recall plot, which has a similar idea with ROC curve.
# 
p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# plot precision against recall
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

### note utilisation de RELEVEL ! ###

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
#-----------------------------------------------
# exercise

library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# proportion of the inclass group that is female
inclass_ratio <- sum(x=="inclass" & y=="Female")/sum(x=="inclass")

# proportion of the online group that is female
online_ratio <- sum(x=="online" & y=="Female")/sum(x=="online")

dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

# In the course videos, height cutoffs were used to predict sex. Instead of using height, use the type variable. 
# Use what you learned about Q1 to make an informed guess about sex based on the most prevalent sex for each type. 
# Report the accuracy of your prediction of sex based on type. You do not need to split the data into training and test sets.

y_hat <- ifelse(x=="online","Male","Female") %>% factor(levels = levels(y)) # factor est nécessaire par exemple pour la fonction sensitivity()
mean(y_hat == y) # accuracy = 0.6333

# Write a line of code using the table() function to show the confusion matrix between y_hat and y. 
# Use the exact format function(a, b) for your answer and do not name the columns and rows.
table(y_hat,y) # Q3  sucks
table(predicted = y_hat, actual = y)
# What is the sensitivity of this prediction? 
sensitivity(data= y_hat, reference = y)
26/(26+42)
specificity(data= y_hat, reference = y)
69/(69+13)
# What is the prevalence (% of females) in the dat dataset defined above?
mean(dat$sex == "Female")

#-----------------------------------------------
# We will practice building a machine learning algorithm using a new dataset, iris, that provides multiple predictors for us to use to train. 
# To start, we will remove the setosa species and we will focus on the versicolor and virginica iris species using the following code:
  
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition( y, times = 1, p = 0.5, list = FALSE)

test <- iris[test_index,]
train <- iris[-test_index,]

# Using only the train iris dataset, for each feature, perform a simple search to find the cutoff that produces the highest accuracy,
# predicting virginica if greater than the cutoff and versicolor otherwise. 
# Use the seq function over the range of each feature by intervals of 0.1 for this search.

head(iris)

calc_cutoff<- function(feature) {
  cutoff <- seq(min(feature),max(feature),0.1)
  accuracy <- map_dbl(cutoff, function(x){
     y_hat <- ifelse(feature > x, "virginica", "versicolor") %>% factor(levels = levels(y))
    mean(y_hat == train$Species)
  })
  list(  accuracy = max(accuracy), best_cutoff = cutoff[which.max(accuracy)])
}
calc_cutoff(train$Sepal.Length) 
calc_cutoff(train$Sepal.Width) 
calc_cutoff(train$Petal.Length) 
calc_cutoff(train$Petal.Width) 

# $accuracy
# [1] 0.96
# 
# $best_cutoff
# [1] 4.7

# correction
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5] , 2 , foo)  # 2 = apply on columns
sapply(predictions,max)	

0.96 # Petal.Length  

# For the feature selected in Q8, use the smart cutoff value from the training data to calculate overall accuracy in the test data. What is the overall accuracy?
y_test_hat<-ifelse(test$Petal.Length > 4.7, 'virginica','versicolor') %>% factor(levels = levels(y))
mean(y_test_hat == test$Species)

# correction
predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)

# Notice that we had an overall accuracy greater than 96% in the training data, but the overall accuracy was lower in the test data. This can happen often if we overtrain. In fact, it could be the case that a single feature is not the best choice. For example, a combination of features might be optimal. Using a single feature and optimizing the cutoff as we did on our training data can lead to overfitting.
# Given that we know the test data, we can treat it like we did our training data to see if the same feature with a different cutoff will optimize our predictions.
# Which feature best optimizes our overall accuracy?

calc_cutoff2<- function(feature, reference ) {
  cutoff <- seq(min(feature),max(feature),0.1)
  accuracy <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(feature > x, "virginica", "versicolor") %>% factor(levels = levels(y))
    mean(y_hat == reference)
  })
  data.frame(  accuracy = max(accuracy), best_cutoff = cutoff[which.max(accuracy)])
}
apply(test[-5],2, calc_cutoff2, test$Species)
# 
# $Petal.Width
# accuracy best_cutoff
# 1     0.94         1.6

##
## # Now we will perform some exploratory data analysis on the data.

plot(iris,pch=21,bg=iris$Species)

# Notice that Petal.Length and Petal.Width in combination could potentially be more information than either feature alone.

# Optimize the the cutoffs for Petal.Length and Petal.Width separately in the train dataset by using the seq function with increments of 0.1. 
# Then, report the overall accuracy when applied to the test dataset by creating a rule that predicts virginica if Petal.Length is greater than the length cutoff OR Petal.Width is greater than the width cutoff, and versicolor otherwise.
# What is the overall accuracy for the test data now?

apply(train[-5],2, calc_cutoff2, train$Species)

# $Petal.Length
# accuracy best_cutoff
# 1     0.96         4.7
# 
# $Petal.Width
# accuracy best_cutoff
# 1     0.94         1.5

y_test_hat <- ifelse( (test$Petal.Length > 4.7) | (test$Petal.Width > 1.5), 'virginica','versicolor') %>% factor(levels = levels(y))
mean(y_test_hat == test$Species)

test[test$Petal.Length>4.7,]


# correction
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)


#-----------------------------------------------
# conditional probabilities questions

# We have a hypothetical population of 1 million individuals with the following conditional probabilities as described below:
#   
#   The test is positive 85% of the time when tested on a patient with the disease (high sensitivity):  𝑃(test+|disease)=0.85 
# The test is negative 90% of the time when tested on a healthy patient (high specificity):  𝑃(test−|heathy)=0.90 
# The disease is prevalent in about 2% of the community:  𝑃(disease)=0.02 
# Here is some sample code to get you started:
#   
set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

# What is the probability that a test is positive?
mean(test == 1)
# What is the probability that an individual has the disease if the test is negative?
mean(disease[test==0])
# What is the probability that you have the disease if the test is positive?
# Remember: calculate the conditional probability the disease is positive assuming a positive test.
mean(disease[test==1])
# Compare the prevalence of disease in people who test positive to the overall prevalence of disease.
# If a patient's test is positive, how much does that increase their risk of having the disease?
# First calculate the probability of having the disease given a positive test, then divide by the probability of having the disease.
mean(disease[test==1])/mean(disease)

##
# We are now going to write code to compute conditional probabilities for being male in the heights dataset. 
# Round the heights to the closest inch. Plot the estimated conditional probability  𝑃(𝑥)=Pr(Male|height=𝑥)  for each  𝑥 .

library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

# for any numeric vector x, you can create groups based on quantiles like this: cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE).

cut(heights$height, quantile(heights$height, ps), include.lowest = TRUE)

ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(heights$height, quantile(heights$height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>% 
  qplot(height, p, data =.)

# You can generate data from a bivariate normal distrubution using the MASS package using the following code:
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
head(dat)  
plot(dat)

# Using an approach similar to that used in the previous exercise, let's estimate the conditional expectations and make a plot. 
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%  qplot(x, y, data =.)
#-----------------------------------------------
#Linear Regression for Prediction

# Linear regression can be considered a machine learning algorithm. Although it can be too rigid to be useful, it works rather well for some challenges. 
# It also serves as a baseline approach: if you can’t beat it with a more complex approach, you probably want to stick to linear regression. 

library(HistData)
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

# fist approach : guess = train set mean
m <- mean(train_set$son)
# squared loss
mean((m - test_set$son)^2)

# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
# the loss is lower 
mean((y_hat - test_set$son)^2)

#-----------------------------------------------
# predict function
# The predict() function takes a fitted object from functions such as lm() or glm() and a data frame with the new predictors for which to predict. We can use predict like this:
# y_hat <- predict(fit, test_set)
# predict() is a generic function in R that calls other functions depending on what kind of object it receives. To learn about the specifics, you can read the help files using code like this: 
?predict.lm    # or ?predict.glm

y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

# read help files
?predict.lm
?predict.glm
#-----------------------------------------------
#Comprehension Check: Linear Regression

#Create a data set using the following code:
  
set.seed(1, sample.kind="Rounding") 
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# We will build 100 linear models using the data above and calculate the mean and standard deviation of the combined models. 
# First, set the seed to 1 again (make sure to use sample.kind="Rounding" if your R is version 3.6 or later). 
# Then, within a replicate() loop, 
# (1) partition the dataset into test and training sets with p=0.5 and using dat$y to generate your indices, 
# (2) train a linear model predicting y from x, 
# (3) generate predictions on the test set, and 
# (4) calculate the RMSE of that model. Then, report the mean and standard deviation (SD) of the RMSEs from all 100 models.
set.seed(1, sample.kind="Rounding")
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  dat_train <- dat %>% slice(-test_index)
  dat_test <- dat %>% slice(test_index)
  fit <- lm(y~x, data= dat_train)
  y_hat <- predict(fit, dat_test)
  # ROOT MEAN SQUARED ERROR  (RMSE)
  RMSE(dat_test$y, y_hat)
  # sqrt(mean((dat_test$y-y_hat)^2)) # identique!
})
mean(rmse)
sd(rmse)

##
## Now we will repeat the exercise above but using larger datasets. 
# Write a function that takes a size n, then 
# (1) builds a dataset using the code provided at the top of Q1 but with n observations instead of 100 and without the set.seed(1), 
# (2) runs the replicate() loop that you wrote to answer Q1, which builds 100 linear models and returns a vector of RMSEs, and 
# (3) calculates the mean and standard deviation of the 100 RMSEs.
# Set the seed to 1 and then use sapply() or map() to apply your new function to n <- c(100, 500, 1000, 5000, 10000).
# 
# Hint: You only need to set the seed once before running your function; do not set a seed within your function.
# Also be sure to use sapply() or map() as you will get different answers running the simulations individually due to setting the seed.

rmse_func <- function (n) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
    dat_train <- dat %>% slice(-test_index)
    dat_test <- dat %>% slice(test_index)
    fit <- lm(y~x, data= dat_train)
    y_hat <- predict(fit, dat_test)
    # root mean squared error (RMSE)
    RMSE(dat_test$y, y_hat)
    # sqrt(mean((dat_test$y-y_hat)^2)) # identique!
  })
  data.frame( mean = mean(rmse) , sd =sd(rmse))
}
set.seed(1, sample.kind="Rounding")
n <- c(100, 500, 1000, 5000, 10000)
sapply(n, rmse_func)

# => On average, the RMSE does not change much as n gets larger, but the variability of the RMSE decreases.

# Now repeat the exercise from Q1, this time making the correlation between x and y larger, as in the following code:
set.seed(1, sample.kind="Rounding") 
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)  # correl 0.95
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# code iso
set.seed(1, sample.kind="Rounding")
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  dat_train <- dat %>% slice(-test_index)
  dat_test <- dat %>% slice(test_index)
  fit <- lm(y~x, data= dat_train)
  y_hat <- predict(fit, dat_test)
  # root mean squared error (RMSE)
  RMSE(dat_test$y, y_hat)
  # sqrt(mean((dat_test$y-y_hat)^2)) # identique!
})
mean(rmse)
sd(rmse)

#=> When we increase the correlation between x and y, x has more predictive power and thus provides a better estimate of y

## Q6
set.seed(1, sample.kind="Rounding")

# Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3) # Q6
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3) # Q8
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
# Note that y is correlated with both x_1 and x_2 but the two predictors are independent of each other, as seen by cor(dat). ????
cor(dat)


# use the caret package to partition into a test and training set of equal size. 
# Compare the RMSE when using just x_1, just x_2 and both x_1 and x_2. Train a single linear model for each (not 100 like in the previous questions).
# Which of the three models performs the best (has the lowest RMSE)?
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
dat_train <- dat %>% slice(-test_index)
dat_test <- dat %>% slice(test_index)

set.seed(1, sample.kind="Rounding")

fit1 <- lm(y~x_1, data= dat_train)
y_hat1 <- predict(fit1, dat_test)
RMSE(dat_test$y, y_hat1)

fit2 <- lm(y~x_2, data= dat_train)
y_hat2 <- predict(fit2, dat_test)
RMSE(dat_test$y, y_hat2)

fit12 <- lm(y~x_1+x_2, data= dat_train)
y_hat12 <- predict(fit12, dat_test)
RMSE(dat_test$y, y_hat12)

# Q8 Repeat the exercise from Q6 but now create an example in which x_1 and x_2 are highly correlated.
# => Adding extra predictors can improve RMSE substantially, but not when the added predictors are highly correlated with other predictors.

#-----------------------------------------------
# Regression for a Categorical Outcome
# 
# The regression approach can be extended to categorical data. For example, we can try regression to estimate the conditional probability:
#   𝑝(𝑥)=𝑃𝑟(𝑌=1|𝑋=𝑥)=𝛽0+𝛽1𝑥 
# Once we have estimates  𝛽0  and   𝛽1 , we can obtain an actual prediction  𝑝(𝑥) . Then we can define a specific decision rule to form a prediction.

library(dslabs)
data("heights")
y <- heights$height

set.seed(2, sample.kind = "Rounding") #if you are using R 3.6 or later

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

train_set %>% 
  filter(round(height)==66) %>%
  summarize(y_hat = mean(sex=="Female"))

heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>% 
  ggplot(aes(x, prop)) +
  geom_point()
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]

#-----------------------------------------------
# Logistic regression

# Logistic regression is an extension of linear regression that assures that the estimate of conditional probability  
#   P(Y=1|X=x) is between 0 and 1
# This approach makes use of the logistic transformation: 
#   g(p) = log(p / (1-p))
# With logistic regression, we model the conditional probability directly with:
#   g(   P(Y=1|X=x)  )  = b0 + b1 * x
# Note that with this model, we can no longer use least squares. Instead we compute the MAXIMUM LIKELIHOOD ESTIMATE (MLE). 
# In R, we can fit the logistic regression model with the function glm() (generalized linear models). 
# If we want to compute the conditional probabilities, we want type="response" since the default is to return the logistic transformed values.

# plot of the LM solution 
heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() + 
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])

range(p_hat) # [1] -0.397868  1.123309

# fit logistic regression model
glm_fit <- train_set %>% 
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial")
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]

# => range of proba is btw 0 and 1
range (p_hat_logit) # 0.0019765 0.9929258

# plot of the LM solution and GLM cond prob
p_hat_h <- predict(glm_fit, newdata = heights, type = "response")
heights %>% 
  mutate(x = round(height), p = p_hat_h) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female"), prob_glm=mean(p)) %>%
  ggplot(aes(x, prop)) +
  geom_point() + 
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2]) + 
  geom_line(aes(x, prob_glm), col="blue")

#-----------------------------------------------
#Case Study: 2 or 7

# In this case study we apply logistic regression to classify whether a digit is two or seven. 
# We are interested in estimating a conditional probability that depends on two variables:
#   𝑔{𝑝(𝑥1,𝑥2}=𝑔{𝑃𝑟(𝑌=1|𝑋1=𝑥1,𝑋2=𝑥2)}=𝛽0+𝛽1𝑥1+𝛽2𝑥2 
# Through this case, we know that logistic regression forces our estimates to be a plane and our boundary to be a line. 
# This implies that a logistic regression approach has no chance of capturing the non-linear nature of the true  𝑝(𝑥1,𝑥2)
# Therefore, we need other more flexible methods that permit other shapes.
# 
# https://rafalab.github.io/dsbook/introduction-to-machine-learning.html#two-or-seven

mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)

# largest and smallest x_1
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

# plot of the two predictors and colored labels:
data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)

# largest and smallest x_2
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

# fit model
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)$overall["Accuracy"]

# plot the real conditional probas

mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
  geom_raster()

# with curve for p = 0.5

mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 

# plot prediction using our linear model

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 

# data + boundary
p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)

# --- assessment
# --------------
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()
# Note that we have defined a variable x that is predictive of a binary outcome y: 
dat$train %>% 
  ggplot(aes(x, color = y)) + geom_density()

# Set the seed to 1, then use the make_data() function 
# defined above to generate 25 different datasets with mu_1 <- seq(0, 3, len=25). 
# Perform logistic regression on each of the 25 different datasets (predict 1 if p>0.5) 
# and plot accuracy (res in the figures) vs mu_1 (delta in the figures).
set.seed(1, sample.kind="Rounding")

mu_1 <- seq(0, 3, len=25)

logit_fun <- function(mu_1) {
  dat <- make_data(mu_1 = mu_1)
  fit_glm <- glm(y ~ x, data=dat$train, family = "binomial")
  p_hat <- predict(fit_glm, newdata = dat$test, type = "response")
  y_hat <- factor(ifelse(p_hat > 0.5, 1, 0),levels = levels(dat$test$y))
  acc <- confusionMatrix(data = y_hat, reference = dat$test$y)$overall["Accuracy"]
  c(mu_1, acc)
}

str(dat$test)
tab <- sapply(mu_1, logit_fun) 
res<-data.frame(mu = as.numeric(tab[1,]), accuracy=tab[2,])
str(res)
res %>% ggplot(aes(mu,accuracy))+geom_point()

# correction alternative
set.seed(1, sample.kind="Rounding") 
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)


#-----------------------------------------------
# Smoothing (= curve fitting = low band pass filtering)
# Smoothing is a very powerful technique used all across data analysis. 
# It is designed to detect trends in the presence of noisy data in cases in which the shape of the trend is unknown. 
# The concepts behind smoothing techniques are extremely useful in machine learning because conditional expectations/probabilities 
#  can be thought of as trends of unknown shapes that we need to estimate in the presence of uncertainty.

data("polls_2008")
qplot(day, margin, data = polls_2008)

#-----------------------------------------------
# Bin Smoothing and Kernels
# The general idea of smoothing is to group data points into strata in which the value of  𝑓(𝑥)  can be assumed to be constant
# . We can make this assumption because we think  𝑓(𝑥)  changes slowly and, as a result,  𝑓(𝑥)  is almost constant in small windows of time. 
# This assumption implies that a good estimate for  𝑓(𝑥)  is the average of the  𝑌𝑖  values in the window. The estimate is:
# f_hat(x0) = 1/N0 sum (Yi) for i in A0
# In smoothing, we call the size of the interval  |𝑥−𝑥0|  satisfying the particular condition the window size, bandwidth or span.

# bin smoothers = kernel "box" = consider only points inside a window  
span <- 7 
fit <- with(polls_2008,ksmooth(day, margin, x.points = day, kernel="box", bandwidth =span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

polls_2008 %>% filter(day<=(-155+3) ) %>% pull(margin) %>% mean

# kernel = a function used to compute a weight in order to give more weight to the central value of the range
# kernel "normal" = gaussian weight
span <- 7
fit <- with(polls_2008, ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

#-----------------------------------------------
# Local Weighted Regression (loess)

# A limitation of the bin smoothing approach is that we need small windows for the approximately 
# constant assumptions to hold which may lead to imprecise estimates of f(x))
# Local weighted regression (loess) permits us to consider larger window sizes.
# One important difference between loess and bin smoother is that we assume the smooth function is locally linear in a window instead of constant.
# The result of loess is a smoother fit than bin smoothing because we use larger sample sizes to estimate our local parameters.

total_days <-diff(range(polls_2008$day))
span<-21/total_days
# span = ratio of the total number of points
# degree = degree of the polynomial approximation
fit<-loess(margin~day,degree = 1, span = span, data=polls_2008)
polls_2008 %>%mutate(smooth=fit$fitted) %>% 
  ggplot(aes(day, margin)) +
  geom_point(size=3, alpha= 0.5, color="grey") + 
  geom_line(aes(day, smooth), color="red")

# geom_smooth uses by default loess method
polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth() 

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(color="red", span = 0.15, method="loess", method.args = list(degree=1))

#-----------------------------------------------
# Comprehension Check: Smoothing

# In the Wrangling course of this series, PH125.6x, we used the following code to obtain mortality counts 
# for Puerto Rico for 2015-2018:
  
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")

# Use the loess() function to obtain a smooth estimate of the expected number of deaths as a function of date. 
# Plot this resulting smooth function. Make the span about two months long.
total_days<-as.numeric(diff(range(dat$date)))
span<-60/total_days

dat2 <- dat %>% filter (! is.na(deaths)) %>% mutate(datenum=as.numeric(date))
which(is.na(as.numeric(dat$date)))

fit<-loess(deaths~datenum, degree = 1, span = span, data = dat2)

dat2 %>%mutate(smooth=fit$fitted) %>% 
  ggplot(aes(date, deaths)) +
  geom_point(size=3, alpha= 0.5, color="grey") + 
  geom_line(aes(date, smooth), color="red")

# alternative
span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)

dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = "red")

# plot smooth estimates against day of the year, all on the same plot, but with different colors for each year
dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

## Q3 Suppose we want to predict 2s and 7s in the mnist_27 dataset with just the second covariate. 
# Can we do this? On first inspection it appears the data does not have much predictive power.
# In fact, if we fit a regular logistic regression the coefficient for x_2 is not significant!
#   This can be seen using this code:
library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

# Plotting a scatterplot here is not useful since y is binary:
qplot(x_2, y, data = mnist_27$train)

# Fit a loess line to the data above and plot the results. What do you observe?
span <- 0.5
fit <- mnist_27$train %>% mutate(y=as.numeric(y)) %>% loess(y ~ x_2, span=span, degree = 1, data=.)   
mnist_27$train %>% mutate(smooth=predict(fit, x_2)) %>%
  ggplot(aes(x_2,y))+ 
  geom_point(alpha=0.5, size=1) + 
  geom_line(aes(x_2,smooth), col="red")

# Note that there is indeed predictive power, but that the conditional probability is non-linear.
# The loess line can be plotted using the following code:
  
mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")
  
#-----------------------------------------------
# MATRICES

# The main reason for using matrices is that certain mathematical operations needed to develop efficient code can be performed 
# using techniques from a branch of mathematics called linear algebra.
# Linear algebra and matrix notation are key elements of the language used in academic papers describing machine learning techniques. 
# 

library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

class(mnist$train$images)

x <- mnist$train$images[1:1000,] 
y <- mnist$train$labels[1:1000]
str(x)
class(x)
range(x)
nrow(x)
ncol(x)
str(y)
class(y)
range(y)

# Matrix Notation
# In matrix algebra, we have three main types of objects: scalars, vectors, and matrices.
# In R, we can extract the dimension of a matrix with the function dim(). We can convert a vector into a matrix using the function as.matrix().

length(x[,1]) # column containing 1st pixel
x_1 <- 1:5
x_2 <- 6:10
cbind(x_1, x_2)
rbind(x_1,x_2)
dim(x)
dim(x_1) # null : dim est pour une matrice seulement
dim(as.matrix(x_1))

# Converting a Vector to a Matrix
# In R, we can convert a vector into a matrix with the matrix() function. 
# The matrix is filled in by column, but we can fill by row by using the byrow argument. 
# The function t() can be used to directly transpose a matrix. 
# 
# Note that the matrix function recycles values in the vector without warning if the product of columns and rows does not match the length of the vector.

my_vector <- 1:15

# fill the matrix by column
mat <- matrix(my_vector, 5, 3)
mat

# fill by row
mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t

identical(t(mat), mat_t)

matrix(my_vector, 5, 5)
grid <- matrix(x[3,], 28, 28)
class(grid)
image(1:28, 1:28, grid)

# flip the image back
image(1:28, 1:28, grid[1,28:1])

#-----------------------------------------------
# Row and Column Summaries and Apply

# The function rowSums() computes the sum of each row.
# The function rowMeans() computes the average of each row.
# We can compute the column sums and averages using the functions colSums() and colMeans().

sums <- rowSums(x)
avg <- rowMeans(x)

data_frame(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")

# The matrixStats package adds functions that performs operations on each row or column very efficiently, including the functions rowSds() and colSds().

# The apply() function lets you apply any function to a matrix. 
# The first argument is the matrix, the second is the dimension (1 for rows, 2 for columns), and the third is the function. 

avgs <- apply(x, 1, mean)
sds <- apply(x, 2, sd)


#-----------------------------------------------
# Filtering Columns Based on Summaries

# The operations used to extract columns: 
x[,c(351,352)]
# The operations used to extract rows: 
x[c(2,3),]

# We can also use logical indexes to determine which columns or rows to keep:  
library(matrixStats)
new_x <- x[ ,colSds(x) > 60]

# Important note: if you select only one column or only one row, the result is no longer a matrix but a vector. 
# We can preserve the matrix class by using the argument drop=FALSE. 

library(matrixStats)

sds <- colSds(x) # ecart type des 784 colonnes de x (= de chacun des pixels)
sd(x[,100])
sds[100]

qplot(sds, bins = "30", color = I("black")) # histogramme des ecarts types des pixels
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])  # visualisation des SD par pixels 

#extract columns and rows
x[ ,c(351,352)]
x[c(2,3),]
new_x <- x[ ,colSds(x) > 60] # only pixels with sd > 60 are kept
dim(new_x)

class(x[,1])
dim(x[1,])
#preserve the matrix class
class(x[ , 1, drop=FALSE])
dim(x[, 1, drop=FALSE])

#-----------------------------------------------
# Indexing with Matrices and Binarizing the Data

# We can use logical operations with matrices:
mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0

# We can also binarize the data using just matrix operations:
bin_x <- x
bin_x[bin_x < 255/2] <- 0 
bin_x[bin_x > 255/2] <- 1

#index with matrices
mat <- matrix(1:15, 5, 3)
as.vector(mat)
qplot(as.vector(x), bins = 30, color = I("black"))
new_x <- x
new_x[new_x < 50] <- 0

mat <- matrix(1:15, 5, 3)
mat[mat < 3] <- 0
mat

mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat

#binarize the data
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
class(bin_x)
# methode 2
bin_X <- (x > 255/2)*1
class(bin_X)

image(1:28, 1:28, matrix(bin_x[3,], 28, 28)[,28:1] ) 


#-----------------------------------------------
# Vectorization for Matrices and Matrix Algebra Operations

# We can scale each row of a matrix using this line of code:
(x - rowMeans(x)) / rowSds(x)
# To scale each column of a matrix, we use this code:
t(t(x) - colMeans(x)) 
# => ceci parce que l'opération d'addition / soustraction ne se fait pas sur chaque élément d'une matrice mais suivant les éléments de chaque colonne
# colonne 1 ligne 1 puis colonne 1 ligne 2 etc.
# de plus en répétant l'un des deux opérandes si les dimensions ne correspondent pas.
# matrix X(i,j) - vector A(i) => matrix X(i,j) - A(i)

# We can also use a function called sweep() that works similarly to apply(). 
# It takes each entry of a vector and subtracts it from the corresponding row or column:
#take each entry of a vector and subtracts it from the corresponding row or column
x_mean_0 <- sweep(x, 2, colMeans(x))
x_mean_0[1,100]
x[1,100]-colMeans(x)[100]

# sweep default FUN argument is substraction.

# divide by the standard deviation
x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/")

# Matrix multiplication: t(x) %*% x
t(mat) %*% mat

# The cross product: crossprod(x)
crossprod(mat)

# The inverse of a function: solve(crossprod(x))
z<-crossprod(mat)
z_1<- solve(z)
z %*% z_1

# The QR decomposition: qr(x)
qr(z)

## comprehension check
# creates a 100 by 10 matrix of randomly generated normal numbers and assigns it to x
x <- matrix(rnorm(100*10), 100, 10)
xx <-matrix(rnorm(3*2), 3, 2)
dim(x)
nrow(x)
ncol(x)
# add the scalar 1 to row 1, the scalar 2 to row 2, and so on, for the matrix x
xx + seq(nrow(xx))
sweep(xx, 1, 1:nrow(xx),"+")
# add the scalar 1 to column 1, the scalar 2 to column 2, and so on, for the matrix x
sweep(xx, 2, 1:ncol(xx), FUN = "+")
# computes the average of each row of x
rowMeans(xx)
colMeans(xx)
# For each observation in the mnist training data, compute the proportion of pixels that are in the grey area, defined as values between 50 and 205 
# (but not including 50 and 205). (To visualize this, you can make a boxplot by digit class.)
# What proportion of the 60000*784 pixels in the mnist training data are in the grey area overall, defined as values between 50 and 205?
pix <- mnist$train$images 
pixcount <- sum(pix> 50 & pix< 205)

pix [pix<= 50 | pix>= 205] <-0
pix [pix> 50 & pix< 205] <- 1
sum(pix) == pixcount
numgrey <- rowCounts(pix)
sum(numgrey) == pixcount
sum(numgrey)/( nrow(pix)*ncol(pix) )  # ratio = 0.0618 

data_frame(labels = as.factor(mnist$train$labels), greyrate = numgrey/ncol(pix)) %>%
  qplot(labels, greyrate, data = ., geom = "boxplot")

## alternative
# mnist <- read_mnist()
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
mean(y)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")



#-----------------------------------------------
# Distance
# Most clustering and machine learning techniques rely on being able to define distance between observations, using features or predictors.
# With high dimensional data, a quick way to compute all the distances at once is to use the function dist(), 
# which computes the distance between each row and produces an object of class dist():
d <- dist(x)

# We can also compute distances between predictors. If N is the number of observations, the distance between two predictors, say 1 and 2, is:
# dist(1,2)= SQRT ( SUM (I=1...N) (xi,1 -xi,2)^2 )
# To compute the distance between all pairs of the 784 predictors, we can transpose the matrix first and then use dist():
d <- dist(t(x))
  
xx <-matrix(rnorm(3*2), 3, 2)
dist(xx)
sqrt(sum((xx[1,]-xx[2,])^2))
as.matrix(dist(xx))
image(as.matrix(dist(xx)))

##
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

set.seed(1995, sample.kind="Rounding")
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]
y[1:3]
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

#distance between two numbers
sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

#compute distance using matrix algebra
sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

#compute distance between each row
d <- dist(x)
class(d)  # dist
dim(as.matrix(d))
as.matrix(d)[1:3,1:3]

#visualize these distances
image(as.matrix(d))

#order the distance by labels
image(as.matrix(d)[order(y), order(y)])

#compute distance between predictors
d <- dist(t(x))
dim(as.matrix(d))
d_492 <- as.matrix(d)[492,]
image(1:28, 1:28, matrix(d_492, 28, 28))

#-----------------------------------------------
# Comprehension Check: Distance

library(dslabs)
data(tissue_gene_expression)
dim(tissue_gene_expression$x) # 189 x 500
# This matrix has the gene expression levels of 500 genes from 189 biological samples representing seven different tissues. 
# The tissue type is stored in y:
table(tissue_gene_expression$y)
# Euclidean distance between each observation 
dim(as.matrix(dist(tissue_gene_expression$x)))
d <- dist(tissue_gene_expression$x)
# Q2 compare the distances between observations 1 and 2 (both cerebellum), 
# observations 39 and 40 (both colon), and observations 73 and 74 (both endometrium).
tissue_gene_expression$y[c(1,2,39,40,73,74)]
as.matrix(d)[1:10,1:10]
sqrt(crossprod(tissue_gene_expression$x[1,]-tissue_gene_expression$x[2,]))
sqrt(crossprod(tissue_gene_expression$x[39,]-tissue_gene_expression$x[40,]))
sqrt(crossprod(tissue_gene_expression$x[73,]-tissue_gene_expression$x[74,]))

sqrt(crossprod(tissue_gene_expression$x[1,]-tissue_gene_expression$x[39,]))
sqrt(crossprod(tissue_gene_expression$x[1,]-tissue_gene_expression$x[73,]))
sqrt(crossprod(tissue_gene_expression$x[39,]-tissue_gene_expression$x[73,]))

ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

image(as.matrix(d))

#-----------------------------------------------
# Knn

# K-nearest neighbors (kNN) estimates the conditional probabilities in a similar way to bin smoothing. However, kNN is easier to adapt to multiple dimensions.
# Using kNN, for any point  (𝑥1,𝑥2)  for which we want an estimate of  𝑝(𝑥1,𝑥2) , we look for the k nearest points to  (𝑥1,𝑥2)  and take an average of
# the 0s and 1s associated with these points. We refer to the set of points used to compute the average as the neighborhood. 
# Larger values of k result in smoother estimates, while smaller values of k result in more flexible and more wiggly estimates. 
#
# To implement the algorithm, we can use the knn3() function from the caret package. There are two ways to call this function:
# - We need to specify a formula and a data frame. The formula looks like this:  
#   outcome∼predictor1+predictor2+predictor3 . 
# The predict() function for knn3 produces a probability for each class.
# - We can also call the function with the first argument being the matrix predictors and the second a vector of outcomes, like this:
#   x <- as.matrix(mnist_27$train[,2:3])
#   y <- mnist_27$train$y
#   knn_fit <- knn3(x,y)

#logistic regression
library(caret)
fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test, type="response")
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

#fit knn model
knn_fit <- knn3(y ~ ., data = mnist_27$train)  # default k=5
# 2nd method
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)

knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class") 

confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#-----------------------------------------------
# Overtraining and Oversmoothing

# Over-training is the reason that we have higher accuracy in the train set compared to the test set. 
# Over-training is at its worst when we set  𝑘=1 . With  𝑘=1 , the estimate for each  (𝑥1,𝑥2)  in the training set is obtained with just the  𝑦  corresponding to that point. 
#
# When we try a larger  𝑘 , the  𝑘  might be so large that it does not permit enough flexibility. We call this over-smoothing.
#
# Note that if we use the test set to pick this  𝑘 , we should not expect the accompanying accuracy estimate to extrapolate to the real world.
# This is because even here we broke a golden rule of machine learning: we selected the  𝑘  using the test set.
# Cross validation also provides an estimate that takes this into account.

# train set accuracy
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class") 
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]

# test set accuracy
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")  
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#fit knn with k=1 : OVER-TRAINING
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
# accuracy is perfect on the training set
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall[["Accuracy"]]
# accuracy is bad on the test set
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall[["Accuracy"]]

#fit knn with k=401 : OVER-SMOOTHING
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]

#pick the k in knn
ks <- seq(3, 251, 2)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})

#pick the k that maximizes accuracy using the estimates built on the test data
ks[which.max(accuracy$test)]
max(accuracy$test)

accuracy %>% ggplot(aes(x=ks, y=train))+geom_line()+geom_line(aes(y=test),col="red")
#-----------------------------------------------
# comprehension check
# Previously, we used logistic regression to predict sex based on height. Now we are going to use knn to do the same. 
# Set the seed to 1, then use the caret package to partition the dslabs heights data into a training and test set of equal size. 
# Use the sapply() function to perform knn with k values of seq(1, 101, 3) 
# and calculate F1 scores with the F_meas() function using the default value of the relevant argument.
# What is the max value of F_1?

library(dslabs)
set.seed(1, sample.kind = "Rounding")
#set.seed(1)
list_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
train_set<-heights[-list_index,]
test_set<-heights[list_index,]
s<-seq(1,103,3)
F1s <- sapply(s,  FUN = function(x) { 
  fit <- knn3( sex~height, data = train_set, k = x )
  y_hat <- predict(fit, test_set, type="class") %>% factor(levels = levels(test_set$sex))
  F_meas(data= y_hat, reference = factor(test_set$sex))
  })

max(F1s)
which.max(F1s)
s[F1s == max(F1s)]
plot(s, F1s)

## Q2 
library(dslabs)
data("tissue_gene_expression")
#First, set the seed to 1 and split the data into training and test sets. 
#Then, report the accuracy you obtain from predicting tissue type using KNN with k = 1, 3, 5, 7, 9, 11 using sapply() or map_df(). 
# Note: use the createDataPartition() function outside of sapply() or map_df().
set.seed(1, sample.kind = "Rounding")
#list_index <- createDataPartition(tissue_gene_expression$x[,1], times = 1, p = 0.5, list = FALSE)
list_index <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)

str(tissue_gene_expression  )
length(tissue_gene_expression$y)
str(tissue_gene_expression$y)
length(tissue_gene_expression$x[,1])
length(list_index)

train_set<-tissue_gene_expression$x[-list_index,]
train_y<-tissue_gene_expression$y[-list_index]
test_set<-tissue_gene_expression$x[list_index,]

k_func <-function(k){
  fit<-knn3(train_set, train_y,  k=k)
  y_hat<-predict(fit, test_set, type="class") %>% factor(levels=levels(tissue_gene_expression$y))
  cm <- confusionMatrix(data = y_hat, reference = tissue_gene_expression$y[list_index])
  error <- cm$overall["Accuracy"]
  tibble(k,error)
}

ks <- seq(1,11,2)
k_err <- map_df(ks, k_func)
k_err
ks[which.max(k_err$error)]

# alternative
set.seed(1)
library(caret)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})

#-----------------------------------------------
# k-fold Cross Validation

# https://rafalab.github.io/dsbook/cross-validation.html

# Cross Validation. Randomly generate data sets that are not used for training but to estimate the true error. 
#   E(MSE) => TRUE ERROR
#   MSE => APPARENT ERROR, generally understimate the true error. 
# For  𝑘 -fold cross validation, we divide the dataset into a training set and a test set.
# We train our algorithm exclusively on the training set and use the test set only for evaluation purposes. 
# For each set of algorithm parameters being considered, we want an estimate of the MSE and then we will choose the parameters with the smallest MSE. 
# Final estimation of the MSE is calculated on the test set, using the optimal parameter derived from the training set.
# In  𝑘 -fold cross validation, we randomly split the observations into  𝑘  non-overlapping sets, and repeat the calculation for MSE for each of these sets
# . Then, we compute the average MSE and obtain an estimate of our loss. Finally, we can select the optimal parameter that minimized the MSE.
# In terms of how to select  𝑘  for cross validation, larger values of  𝑘  are preferable but they will also take much more computational time
# For this reason, the choices of  𝑘=5  and  𝑘=10  are common.
# Now, one way we can improve the variance of our final estimate is to take more samples. To do this, we would no longer require that training set be
# partitioned into non-overlapping sets. Instead we would just pick k sets of some size at random.

# Comprehension Check: Cross-validation
#Q1 Generate a set of random predictors and outcomes using the following code:
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]
# Because x and y are completely independent, you should not be able to predict y using x with accuracy greater than 0.5. 
# Confirm this by running cross-validation using logistic regression to fit the model. 
# Because we have so many predictors, we selected a random sample x_subset. Use the subset when training the model.

# use the caret train function. The results component of the output of train shows you the accuracy. Ignore the warnings.
?train
names(getModelInfo())

fit <- train(x_subset, y , method ="glm")
fit$results

# Q3 Now, instead of using a random selection of predictors, we are going to search for those that are most predictive of the outcome. 
# We can do this by comparing the values for the  𝑦=1  group to those in the  𝑦=0  group, for each predictor, using a t-test
# You can do perform this step like this:
# install.packages("BiocManager")
# BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value
# Create an index ind with the column numbers of the predictors that were "statistically significantly" associated with y. 
# Use a p-value cutoff of 0.01 to define "statistically significantly."
# How many predictors survive this cutoff?
ind<-pvals<0.01
sum(ind)
pvals[ind]
colnames(x)[ind]
# Q4 Now re-run the cross-validation after redefinining x_subset to be the subset of x defined by the columns showing "statistically significant" association with y.
# What is the accuracy now?
x_subset <- x[ ,ind]
head(x_subset)
dim(x_subset)
#library(caret)
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
fit <- caret::train(x_subset, y , method ="glm")
fit$results$Accuracy # 0.750

#Q5 Re-run the cross-validation again, but this time using kNN. Try out the following grid k = seq(101, 301, 25) of tuning parameters. Make a plot of the resulting accuracies.
getModelInfo(model="knn")

fit <- train(x_subset, y , method ="knn", tuneGrid=data.frame(k=ks))
ggplot(fit)

# Q6 In the previous exercises, we see that despite the fact that x and y are completely independent, we were able to predict y with accuracy higher than 70%. 
# We must be doing something wrong then. Because we used the entire dataset to select the columns in the model, the accuracy is too high. 
# The selection step needs to be included as part of the cross-validation algorithm, and then the cross-validation itself is performed after the column selection step.
# As a follow-up exercise, try to re-do the cross-validation, this time including the selection step in the cross-validation algorithm. The accuracy should now be close to 50%.

x_subset <- x[ ,sample(p, 100)]
fit <- train(x_subset, y , method ="knn", tuneGrid=data.frame(k=ks))
fit

# Q7 . Use the train() function with kNN to select the best k for predicting tissue from gene expression on the tissue_gene_expression dataset from dslabs. 
# Try k = seq(1,7,2) for tuning parameters. For this question, do not split the data into test and train sets 
# (understand this can lead to overfitting, but ignore this for now).
# What value of k results in the highest accuracy?

library(dslabs)
data("tissue_gene_expression")
train(tissue_gene_expression$x, tissue_gene_expression$y, method ="knn", tuneGrid = data.frame(k=seq(1,7,2)) )

#-----------------------------------------------
# Bootstrap

# When we don't have access to the entire population, we can use bootstrap to estimate the population median  𝑚  .
# The bootstrap permits us to approximate a Monte Carlo simulation without access to the entire distribution. 
# The general idea is relatively simple. We act as if the observed sample is the population. 
# We then sample datasets (with replacement) of the same sample size as the original dataset. 
# Then we compute the summary statistic, in this case the median, on this bootstrap sample.
# Note that we can use ideas similar to those used in the bootstrap in cross validation: instead of dividing the data into equal partitions, 
# we simply bootstrap many times.
# 
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))

10^mean(log10(income)) # 45013
10^sd(log10(income)) # 2.998

qplot(log10(income), bins = 30, color = I("black"))
qplot(income, bins = 30, color = I("black"))

mean(income)
m <- median(income)
m

set.seed(1995, sample.kind="Rounding") 
N <- 250
X <- sample(income, N)
M<- median(X)
M

# From the Monte Carlo : the distribution of the median is approx. normal
library(gridExtra)
B <- 10^4
M <- replicate(B, {
    X <- sample(income, N)
    median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)

mean(M)
sd(M)


# Bootstrap sample
B <- 10^4
M_star <- replicate(B, {
    X_star <- sample(X, N, replace = TRUE)
    median(X_star)
})

# compare distributions of the bootstrap samples and of the actual one: they are close
tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
    qplot(monte_carlo, bootstrap, data = .) + 
    geom_abline()

# confidence intervals are close too
quantile(M, c(0.05, 0.95))
quantile(M_star, c(0.05, 0.95))

# we cannot apply CLT for the median: this confidence interval is wrong !
median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)

# If we know the distribution is normal, we can use a bootstrap to estimate the mean of the median, its standard error,
# and then form a confidence interval that way.
mean(M) + 1.96 * sd(M) * c(-1,1)
mean(M_star) + 1.96 * sd(M_star) * c(-1, 1)

## Comprehension Check: Bootstrap
###############
# Q1 The createResample() function can be used to create bootstrap samples. 
# For example, we can create the indexes for 10 bootstrap samples for the mnist_27 dataset like this:
set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)
# How many times do 3, 4, and 7 appear in the first resampled index?
sum(indexes$Resample01==3)
sum(indexes$Resample01==4)
sum(indexes$Resample01==7)

# Q2 We see that some numbers appear more than once and others appear no times. This has to be this way for each dataset to be independent. Repeat the exercise for all the resampled indexes.
# What is the total number of times that 3 appears in all of the resampled indexes?
sum(sapply(1:10, function(x) { sum(indexes[[x]] ==3)}))
# or...
x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)

# Q3 Generate a random dataset using the following code:
y <- rnorm(100, 0, 1)
# Estimate the 75th quantile, which we know is qnorm(0.75), with the sample quantile: quantile(y, 0.75).
q <- quantile(y, 0.75)
# Set the seed to 1 and perform a Monte Carlo simulation with 10,000 repetitions, generating the random dataset and estimating the 75th quantile each time. 
# What is the expected value and standard error of the 75th quantile?
set.seed(1, sample.kind="Rounding")
B<-10000
qv <- replicate(B, { quantile( rnorm(100,0,1), 0.75)})
mean(qv)
sd(qv)
# Q4 In practice, we can't run a Monte Carlo simulation. Use the sample:
set.seed(1, sample.kind="Rounding")
y <- rnorm(100, 0, 1)
# Set the seed to 1 again after generating y and use 10 bootstrap samples to estimate the expected value and standard error of the 75th quantile.
set.seed(1, sample.kind="Rounding")
idx <- createResample(y, 10)
vq <- sapply(idx, function(ind){ quantile(y[ind],0.75) })
vq
mean(vq) # 0.7312
sd(vq) # 0.074

# Q5 Repeat the exercise from Q4 but with 10,000 bootstrap samples instead of 10. Set the seed to 1.
set.seed(1, sample.kind="Rounding")
idx <- createResample(y, 10000)
vq <- sapply(idx, function(ind){ quantile(y[ind],0.75) })
mean(vq) # 0.6737512
sd(vq) # 0.0930575
# Q6 When doing bootstrap sampling, the simulated samples are drawn from the empirical distribution of the original data.
# True or False: The bootstrap is particularly useful in situations in which a tractable variance formula exists.
# False. The bootstrap is particularly useful in situations in which a tractable variance formula does NOT exist.-
#-----------------------------------------------
# 
#-------------------------
---------------------
# 
#-------------------------
# Generative Models
# Discriminative approaches estimate the conditional probability directly and do not consider the distribution of the predictors. 
# Generative models are methods that model the joint distribution and X  (we model how the entire data,X and Y , are generated 

# Naive Bayes
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#naive-bayes

# Generating train and test set
library("caret")
data("heights")
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# Estimating averages and standard deviations
params <- train_set %>%
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params

# Estimating the prevalence
pi <- train_set %>% summarize(pi=mean(sex=="Female")) %>% pull(pi)
pi

# Getting an actual rule
x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2]) # conditional prob density for females
f1 <- dnorm(x, params$avg[1], params$sd[1]) # for males
p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))

data.frame(h=x, p_h_cond_fem = f0, p_h_cond_mal = f1, p_fem_cond_h = p_hat_bayes, sex=test_set$sex) %>% 
  ggplot(aes(h,p_fem_cond_h))+geom_line(col="red")+
  geom_line(aes(h,p_h_cond_fem),col="grey")+geom_line(aes(h,p_h_cond_mal),col="black")

## controling prevalence
# The Naive Bayes approach includes a parameter to account for differences in prevalence pi = Pr(Y=1)
# The Naive Bayes approach gives us a direct way to correct the imbalance between sensitivity and specificity by simply forcing PI_hat 
# to be whatever value we want it to be in order to better balance specificity and sensitivity. 

# Computing sensitivity
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Computing specificity
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Changing the cutoff of the decision rule
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))

# Draw plot
qplot(x, p_hat_bayes_unbiased, geom = "line") +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_vline(xintercept = 67, lty = 2)
# 
#-----------------------------------------------
# QDA and LDA

# Quadratic discriminant analysis (QDA) is a version of Naive Bayes in which we assume that the distributions Px|Y=1(x) and Px|Y=0(x) 
# are multivariate normal. The boundary must be a quadratic function of the form x2
# 
# QDA can work well with a few predictors, but it becomes harder to use as the number of predictors increases. 
# Once the number of parameters approaches the size of our data, the method becomes impractical due to overfitting.
# 
# LDA : Forcing the assumption that all predictors share the same standard deviations and correlations, the boundary will be a line, 
# just as with logistic regression. For this reason, we call the method linear discriminant analysis (LDA).
# 
# In the case of LDA, the lack of flexibility does not permit us to capture the non-linearity in the true conditional probability function.

# Load data
data("mnist_27")

# Estimate parameters from the data
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))

# Contour plots
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm", lwd = 1.5)

# Fit model
library(caret)
train_qda <- train(y ~., method = "qda", data = mnist_27$train)
# Obtain predictors and accuracy
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

# Draw separate plots for 2s and 7s (=> assumption of normality is not accurate for the 7s)
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm") +
  facet_wrap(~y)
# LDA
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))
params <- params %>% mutate(sd_1 = mean(sd_1), sd_2 = mean(sd_2), r = mean(r))
train_lda <- train(y ~., method = "lda", data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

####
## Case Study: More than Three Classes
# In this case study, we will briefly give a slightly more complex example: one with 3 classes instead of 2. 
# Then we will fit QDA, LDA, and KNN models for prediction.
# Generative models can be very powerful, but only when we are able to successfully approximate the joint distribution of predictors 
# conditioned on each class.

if(!exists("mnist"))
  mnist <- read_mnist()

set.seed(3456, sample.kind="Rounding") 
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127] 
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)

# get the quadrants
# temporary object to help figure out the quadrants
row_column <- expand.grid(row=1:28, col=1:28)
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)

# binarize the values. Above 200 is ink, below is no ink
x <- x > 200 

# cbind proportion of pixels in upper right quadrant and proportion of pixels in lower right quadrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x),
           rowSums(x[ ,lower_right_ind])/rowSums(x)) 

train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1],
                        x_2 = x[index_train,2])

test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1],
                       x_2 = x[-index_train,2])

train_set %>%  ggplot(aes(x_1, x_2, color=y)) + geom_point()

train_qda <- train(y ~ ., method = "qda", data = train_set)
predict(train_qda, test_set, type = "prob") %>% head()
predict(train_qda, test_set) %>% head()
confusionMatrix(predict(train_qda, test_set), test_set$y)$table
confusionMatrix(predict(train_qda, test_set), test_set$y)$overall["Accuracy"]
confusionMatrix(predict(train_qda, test_set), test_set$y)

predqda<- predict(train_qda, test_set) 
test_set  %>% mutate(y_hat = predqda ) %>% ggplot(aes(x_1,x_2,col=y_hat))+geom_point()

train_lda <- train(y ~ ., method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$y)$overall["Accuracy"]

train_knn <- train(y ~ ., method = "knn", tuneGrid = data.frame(k = seq(15, 51, 2)),
                   data = train_set)
confusionMatrix(predict(train_knn, test_set), test_set$y)$overall["Accuracy"]

train_set %>% mutate(y = factor(y)) %>% ggplot(aes(x_1, x_2, fill = y, color=y)) + geom_point(show.legend = FALSE) + stat_ellipse(type="norm")
# => assumption of bivariate normaly ditributed is wrong at least for the 1s
# So in summary, generating models can be very powerful but only when we're able to successfully approximate the joint distribution
# of predictor's condition on each class.

#-----------------------------------------------
# Comprehension Check: Generative Models
# Q1 Create a dataset of samples from just cerebellum and hippocampus, two parts of the brain, 
# and a predictor matrix with 10 randomly selected columns using the following code:

library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
# Use the train() function to estimate the accuracy of LDA. For this question, use the entire tissue_gene_expression dataset: 
# do not split it into training and test sets (understand this can lead to overfitting). Report the accuracy from the train() results (do not make predictions).
train_lda<-train(x,y,method="lda")
train_lda$results["Accuracy"]

# Q2 In this case, LDA fits two 10-dimensional normal distributions. Look at the fitted model by looking at the finalModel component of the result of train(). 
# Notice there is a component called means that includes the estimated means of both distributions. 
# Plot the mean vectors against each other and determine which predictors (genes) appear to be driving the algorithm.
# Which TWO genes appear to be driving the algorithm (i.e. the two genes with the highest means)?
  
train_lda$finalModel$means
as_tibble(x=x)%>%mutate(y=y)%>%group_by(y)%>%summarize_all(mean)
str(train_lda$finalModel$means)
unlist (train_lda$finalModel$means[,0], use.names = FALSE)

as_tibble(train_lda$finalModel$means) %>% 
  mutate(tissue=c("cerebellum","hippocampus")) %>% 
  gather(predictor, value, -tissue) %>%
  spread(tissue,value) %>%
  ggplot(aes(cerebellum, hippocampus, label=predictor))+geom_text()

# alternative...
t(train_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

# Q3 Repeat the exercise in Q1 with QDA.
# Create a dataset of samples from just cerebellum and hippocampus, two parts of the brain, and a predictor matrix with 10 randomly 
# selected columns using the following code:
library(dslabs)      
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding") 
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
# Use the train() function to estimate the accuracy of QDA. For this question, use the entire tissue_gene_expression dataset: do not split it into training and test sets (understand this can lead to overfitting).
fit_qda<-train(x,y,method="qda")
fit_qda$results["Accuracy"]
#Which TWO genes drive the algorithm when using QDA instead of LDA (i.e. the two genes with the highest means)?
t(fit_qda$finalModel$means) %>% data.frame() %>% 
  mutate(predictor_name = rownames(.)) %>%
  arrange(cerebellum)

#Q5 One thing we saw in the previous plots is that the values of the predictors correlate in both groups: some predictors are low in both groups 
# and others high in both groups. The mean value of each predictor found in colMeans(x) is not informative or useful for prediction and often for purposes 
# of interpretation, it is useful to center or scale each column. This can be achieved with the preProcess argument in train(). 
# Re-run LDA with preProcess = "center". Note that accuracy does not change, but it is now easier to identify the predictors that differ more between 
# groups than based on the plot made in Q2.
# Which TWO genes drive the algorithm after performing the scaling?
colMeans(x) 
fit_lda <- train(x,y,method="lda", preProcess="center")
fit_lda$results
fit_lda$finalModel$means

t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_text() +
  geom_abline()

  # You can see that it is different genes driving the algorithm now. This is because the predictor means change.
  
  # In the previous exercises we saw that both LDA and QDA approaches worked well. 
  # For further exploration of the data, you can plot the predictor values for the two genes with the largest differences between the two groups in 
  # a scatter plot to see how they appear to follow a bivariate distribution as assumed by the LDA and QDA approaches, coloring the points by 
  # the outcome, using the following code:
    
  d <- apply(fit_lda$finalModel$means, 2, diff)
  ind <- order(abs(d), decreasing = TRUE)[1:2]
  plot(x[, ind], col = y)

# Q6 Repeat the LDA analysis from Q5 but using all tissue types. Use the following code to create your dataset:

library(dslabs)      
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding") 
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
# What is the accuracy using LDA?
fit_lda <- train(x,y,method="lda", preProcess="center")
fit_lda$results
fit_lda$finalModel$means
# We see that the results are slightly worse when looking at all of the tissue types instead of only selected ones. 
# You can use the confusionMatrix function to learn more about what type of errors we are making, like this: confusionMatrix(fit_lda).
confusionMatrix(fit_lda)


#-----------------------------------------------
# Section 5. Classification with More than Two Classes and the Caret Package

# LDA and QDA are not meant to be used with many predictors  𝑝  because the number of parameters needed to be estimated becomes too large.
# Curse of dimensionality: For kernel methods such as kNN or local regression, when they have multiple predictors used,  the span/neighborhood/window made to include a given percentage of the data become large. With larger neighborhoods, our methods lose flexibility. The dimension here refers to the fact that when we have  𝑝  predictors, the distance between two observations is computed in  𝑝 -dimensional space.
  
#-----------------------------------------------
# Classification and Regression Trees (CART)
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#cart-motivation
#
# A tree is basically a flow chart of yes or no questions. The general idea of the methods we are describing is to define an algorithm that uses data to create these trees with predictions at the ends, referred to as nodes.
# When the outcome is continuous, we call the decision tree method a regression tree.
# Regression and decision trees operate by predicting an outcome variable  𝑌  by partitioning the predictors.
# The general idea here is to build a decision tree and, at end of each node, obtain a predictor  𝑦̂  . Mathematically, we are partitioning the predictor space into  𝐽  non-overlapping regions,  𝑅1  ,  𝑅2 , ...,  𝑅𝐽  and then for any predictor  𝑥  that falls within region  𝑅𝑗 , estimate  𝑓(𝑥)  with the average of the training observations  𝑦𝑖  for which the associated predictor  𝑥𝑖  in also in  𝑅𝑗 .
# To pick  𝑗  and its value  𝑠 , we find the pair that minimizes the residual sum of squares (RSS):
#   To fit the regression tree model, we can use the rpart() function in the rpart package.
#
# Two common parameters used for partition decision are the COMPLEXITY PARAMETER (cp) : a minimum for how much the RSS must improve for another partition to be added 
# the minimum number of observations required in a partition before partitioning it further (MINSPLIT in the rpart package). the default is 20
# minimum number of observations in each node. The argument is MINBUCKET and defaults to round(minsplit/3).
# If we already have a tree and want to apply a higher cp value, we can use the prune() function. We call this pruning a tree because we are snipping off partitions that do not meet a cp criterion. 

# Regression trees
# 
# Load data
library(tidyverse)
library(dslabs)
data("olive")
olive %>% as_tibble()
table(olive$region)
olive <- select(olive, -area)

# Predict region using KNN
library(caret)
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = olive)
ggplot(fit)

# Plot distribution of each predictor stratified by region
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank())

# plot values for eicosenoic and linoleic
p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
p + geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)

# load data for regression tree
data("polls_2008")
qplot(day, margin, data = polls_2008)

library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)
class(fit)

# visualize the splits 
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# change parameters
fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# use cross validation to choose cp
library(caret)
train_rpart <- train(margin ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = polls_2008)
ggplot(train_rpart)

# access the final model and plot it
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# prune the tree 
pruned_fit <- prune(fit, cp = 0.01)
plot(fit, margin = 0.1)
plot(pruned_fit, margin = 0.1)

#-----------------------------------------------
# Classification (Decision) Trees
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#classification-decision-trees 
# 
# Classification trees, or decision trees, are used in prediction problems where the outcome is categorical. 
# Decision trees form predictions by calculating which class is the most common among the training set observations within the partition, rather than taking the average in each partition.
#
# Two of the more popular metrics to choose the partitions are the Gini index and entropy.
#
# Pros: Classification trees are highly interpretable and easy to visualize. They can model human decision processes and don’t require use of dummy predictors for categorical variables.
# Cons: The approach via recursive partitioning can easily over-train and is therefore a bit harder to train than  for example, linear regression or kNN.
# Furthermore, in terms of accuracy, it is rarely the best performing method since it is not very flexible and is highly unstable to changes in training data. 

# fit a classification tree and plot it
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
plot(train_rpart)

# compute accuracy
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]


#-----------------------------------------------  
# Random Forests

# https://rafalab.github.io/dsbook/examples-of-algorithms.html#random-forests

# Random forests are a very popular machine learning approach that addresses the shortcomings of decision trees. The goal is to improve prediction performance and reduce instability by averaging multiple decision trees (a forest of trees constructed with randomness).
# The general idea of random forests is to generate many predictors, each using regression or classification trees, and then forming a final prediction based on the average prediction of all these trees. To assure that the individual trees are not the same, we use the bootstrap to induce randomness. 
# A disadvantage of random forests is that we lose interpretability.
# An approach that helps with interpretability is to examine variable importance. To define variable importance we count how often a predictor is used in the individual trees. The caret package includes the function varImp that extracts variable importance from any model in which the calculation is implemented. 

library(randomForest)
fit <- randomForest(margin~., data = polls_2008) 
plot(fit)

polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")

library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# smoothing : changing the parameter that controls the minimum number of data points in the nodes of the tree

# use cross validation to choose parameter (possible with Rborist but not with randomForest)
train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# with Random Forest
nodesize <- seq(1, 51, 10)
acc <- sapply(nodesize, function(ns){
  train(y ~ ., method = "rf", data = mnist_27$train,
        tuneGrid = data.frame(mtry = 2),
        nodesize = ns)$results$Accuracy
})
qplot(nodesize, acc)

train_rf_2 <- randomForest(y ~ ., data=mnist_27$train,
                           nodesize = nodesize[which.max(acc)])

confusionMatrix(predict(train_rf_2, mnist_27$test),
                mnist_27$test$y)$overall["Accuracy"]

#-----------------------------------------------  
# Comprehension Check: Trees and Random Forests
# 

# Create a simple dataset where the outcome grows 0.75 units on average for every increase in a predictor, using this code:
  
library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1, sample.kind = "Rounding") 
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

# Q1 uses rpart() to fit a regression tree and saves the result to fit
fit <- rpart(y~., data=dat)


plot(fit, margin = 0.1)
text(fit, cex = 0.75)

# Q3 scatter plot of y versus x along with the predicted values based on the fit.

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) + geom_step(aes(x, y_hat), col=2)
  
# Q4 Now run Random Forests instead of a regression tree using randomForest() from the randomForest package, and remake the scatterplot with the prediction line. 
library(randomForest)
fit <-  randomForest(y~., data=dat) 
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
# Q5 Use the plot() function to see if the Random Forest from Q4 has converged or if we need more trees.
plot(fit)

# Q6 It seems that the default values for the Random Forest result in an estimate that is too flexible (unsmooth). 
# Re-run the Random Forest but this time with a node size of 50 and a maximum of 25 nodes. Remake the plot.

library(randomForest)
fit <- randomForest(y~., data=dat, nodesize = 50, maxnodes=25) 
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

#-----------------------------------------------  
# # Caret Package
# http://topepo.github.io/caret/available-models.html
# 
# http://topepo.github.io/caret/train-models-by-tag.html
# 
# The caret package helps provides a uniform interface and standardized syntax for the many different machine learning packages in R. 
# Note that caret does not automatically install the packages needed.

library(tidyverse)
library(dslabs)
data("mnist_27")

library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

# #-----------------------------------------------  
# # Tuning Parameters with Caret

# https://rafalab.github.io/dsbook/caret.html#caret-cv
# 
# https://topepo.github.io/caret/available-models.html
# 
# https://topepo.github.io/caret/train-models-by-tag.html
# 
# The train() function automatically uses cross-validation to decide among a few default values of a tuning parameter.
# The getModelInfo() and modelLookup() functions can be used to learn more about a model and the parameters that can be optimized.
# We can use the tunegrid() parameter in the train() function to select a grid of values to be compared.
# The trControl parameter and trainControl() function can be used to change the way cross-validation is performed.
#
# Note that not all parameters in machine learning algorithms are tuned. We use the train() function to only optimize parameters that are tunable.

getModelInfo("knn")
modelLookup("knn")

# train with default set of tune parameters
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = TRUE)

# with specific set of tuning parameters
train_knn <- train(y ~ ., method = "knn", 
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = TRUE)

train_knn$bestTune

train_knn$finalModel

# accuracy against the testing set

confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

# change the method using trainControl()
# ex: 10-fold cross-validation (instead of 25)  
control <- trainControl(method = "cv", number = 10, p = .9)
class(control)

train_knn_cv <- train(y ~ ., method = "knn", 
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

train_knn$results %>% 
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, 
                    ymin = Accuracy - AccuracySD,
                    ymax = Accuracy + AccuracySD))

plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2]) # knn Condit.Prob.
# nb pb réelle : 
plot_cond_prob(mnist_27$true_p$p)

# Generalized Additive Model using LOESS
# install.packages("gam")
modelLookup("gamLoess")

grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

train_loess <- train(y ~ ., 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1  # conditional prob estimate is smoother than knn

#-----------------------------------------------  
# Comprehension Check: Caret Package

#Q1 Use the rpart() function to fit a classification tree to the tissue_gene_expression dataset. 
# Use the train() function to estimate the accuracy. Try out cp values of seq(0, 0.1, 0.01). 
# Plot the accuracies to report the results of the best model. Set the seed to 1991.
# Which value of cp gives the highest accuracy

library(dslabs)
library(rpart)
data(tissue_gene_expression)
set.seed(1991,sample.kind = "Rounding")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
#fit<-rpart(y~x,data=tissue_gene_expression)
train_rpart <-train( x, y , method="rpart", tuneGrid = data.frame(cp = seq(0,0.1,0.01)))
plot(train_rpart,  highlight = TRUE)
train_rpart$results[which.max(train_rpart$results$Accuracy),]
confusionMatrix(train_rpart)


# Q2 Note that there are only 6 placentas in the dataset. By default, rpart() requires 20 observations before splitting a node. 
# That means that it is difficult to have a node in which placentas are the majority. 
# Rerun the analysis you did in the exercise in Q1, but this time, allow rpart() to split any node by using the argument control = rpart.control(minsplit = 0). 
# Look at the confusion matrix again to determine whether the accuracy increases. Again, set the seed to 1991.
set.seed(1991,sample.kind = "Rounding")
fit_rpart <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
                  control = rpart.control(minsplit = 0) ))
fit_rpart
plot(fit_rpart,  highlight = TRUE)
fit_rpart$results[which.max(fit_rpart$results$Accuracy),]
# confusionMatrix(data=predict(fit_rpart, x), reference = y) # faux ! 
confusionMatrix(fit_rpart) # bon

# Q3 Plot the tree from the best fitting model of the analysis you ran in Q2. Which gene is at the first split?
plot(fit_rpart$finalModel, margin=0.1)
text(fit_rpart$finalModel, cex = 0.75)

# Q4 We can see that with just seven genes, we are able to predict the tissue type. 
# Now let's see if we can predict the tissue type with even fewer genes using a Random Forest. 
# Use the train() function and the rf method to train a Random Forest model and save it to an object called fit. 
# Try out values of mtry ranging from seq(50, 200, 25) (you can also explore other values on your own). 
# What mtry value maximizes accuracy? 
# To permit small nodesize to grow as we did with the classification trees, use the following argument: nodesize = 1.
# Note: This exercise will take some time to run. 
# If you want to test out your code first, try using smaller values with ntree. Set the seed to 1991 again.
# 
# What value of mtry maximizes accuracy
modelLookup("rf")
getModelInfo("rf")
#train (x,y, method="rf", nodesize =1)
set.seed(1991,sample.kind = "Rounding")
fit <- with(tissue_gene_expression,
            train (x,y, method="rf", tuneGrid = data.frame(mtry=seq(50,200,25)), nodesize=1) #, ntree=1)
)
plot(fit)
fit

# Q5 Use the function varImp() on the output of train() and save it to an object called imp:
imp <- varImp(object =  fit) 
imp

# Q6 The rpart() model we ran above produced a tree that used just seven predictors. 
# Extracting the predictor names is not straightforward, but can be done. 
#If the output of the call to train was fit_rpart, we can extract the names like this:
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms
# Calculate the variable importance in the Random Forest call for these seven predictors and examine where they rank.
# What is the importance of the CFHR4 gene in the Random Forest call?
data.frame(imp$importance)  %>% 
  mutate(var = rownames(.)) %>%
  arrange(desc(Overall)) %>% 
  mutate(rank = row_number())%>% 
  filter (var %in% tree_terms) 

# alt...
data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)


#-----------------------------------------------  
# Titanic Exercises Part 1

library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# Q1 Split titanic_clean into test and training sets - after running the setup code, it should have 891 rows and 9 variables.
# Set the seed to 42, then use the caret package to create a 20% data partition based on the Survived column. Assign the 20% partition to test_set and the remaining 80% partition to train_set.
str(titanic_clean)
set.seed(42, sample.kind = "Rounding")
train_index <- createDataPartition(y=titanic_clean$Survived, p=0.2, list = FALSE)
test_set <- titanic_clean %>% slice(train_index)
train_set <- titanic_clean %>% slice(-train_index)
nrow(train_set)
nrow(test_set)
mean(train_set$Survived == 1)

# Question 2: Baseline prediction by guessing the outcome
# The simplest prediction method is randomly guessing the outcome without using additional predictors. These methods will help us determine whether our machine learning algorithm performs better than chance. 
# How accurate are two methods of guessing Titanic passenger survival?
# Set the seed to 3. For each individual in the test set, randomly guess whether that person survived or not by sampling from the vector c(0,1) 
# (Note: use the default argument setting of prob from the sample function). Assume that each person has an equal chance of surviving or not surviving.
set.seed(3, sample.kind = "Rounding")

random_fit <- sample(c(0,1),size=nrow(test_set),replace=TRUE)  %>% factor(levels(test_set$Survived))

mean(test_set$Survived == random_fit) # accuracy

# Q3a Use the training set to determine whether members of a given sex were more likely to survive or die. Apply this insight to generate survival predictions on the test set.
# What proportion of training set females survived?
survprob <- train_set %>% group_by(Sex) %>%  summarise(survprob=mean(Survived==1))
survprob

# Q3b Predict survival using sex on the test set: if the survival rate for a sex is over 0.5, predict survival for all individuals of that sex, and predict death if the survival rate for a sex is under 0.5.
# What is the accuracy of this sex-based prediction method on the test set?
sex_model <- factor( ifelse(test_set$Sex=="male",0,1) , levels(test_set$Survived))
mean(test_set$Survived == sex_model)

# Q4a In which class(es) (Pclass) were passengers more likely to survive than die?
titanic_clean %>% group_by(Pclass) %>% summarize(SurvPro = mean(Survived==1))

# Q4b Predict survival using passenger class on the test set: predict survival if the survival rate for a class is over 0.5, otherwise predict death.
# What is the accuracy of this class-based prediction method on the test set?
class_model <- factor( ifelse(test_set$Pclass==1,1,0) , levels(test_set$Survived))
mean(test_set$Survived == class_model)

# Q4c Group passengers by both sex and passenger class. Which sex and class combinations were more likely to survive than die?
train_set %>% group_by(Sex,Pclass) %>% summarize(SurvPro = mean(Survived==1)) 

# Q4d Predict survival using both sex and passenger class on the test set. Predict survival if the survival rate for a sex/class combination is over 0.5, otherwise predict death.
mix_model <- with(test_set, ifelse ( (Pclass==1 |  Pclass==2 )& Sex=="female",1,0)  ) %>% factor(levels = c(0,1))
mean(test_set$Survived == mix_model)

# Q5 Use the confusionMatrix() function to create confusion matrices for the sex model, class model, and combined sex and class model. 
# You will need to convert predictions and survival status to factors to use this function.
mat1 <- confusionMatrix(sex_model, test_set$Survived)
mat2 <- confusionMatrix(class_model, test_set$Survived)
mat3 <- confusionMatrix(mix_model, test_set$Survived)
mat1$positive
mat1$overall
mat1$byClass
mat2$byClass
mat3$byClass
# What is the "positive" class used to calculate confusion matrix metrics? => 0
# Which model has the highest sensitivity? combined
# Which model has the highest specificity? sex
# Which model has the highest balanced accuracy? sex
# What is the maximum value of balanced accuracy? 0.806
# Q6 Which model has the highest  𝐹1  score?
F_meas(data= sex_model, reference = test_set$Survived)
F_meas(data= class_model, reference = test_set$Survived)
F_meas(data= mix_model, reference = test_set$Survived)

# Question 7: Survival by fare - LDA and QDA
# Set the seed to 1. Train a model using linear discriminant analysis (LDA) with the caret lda method using fare as the only predictor. What is the accuracy on the test set for the LDA model?
set.seed(1, sample.kind = "Rounding")
fit_lda <- train(Survived~Fare, method = "lda", data = train_set)
fit_lda$results
y_hat <- predict(fit_lda,test_set)
confusionMatrix(factor(y_hat),test_set$Survived)

# Set the seed to 1. Train a model using quadratic discriminant analysis (QDA) with the caret qda method using fare as the only predictor. What is the accuracy on the test set for the QDA model?
set.seed(1, sample.kind = "Rounding")
fit_qda <- train(Survived~Fare, method = "qda", data = train_set)
fit_qda$results
y_hat <- predict(fit_qda,test_set)
confusionMatrix(factor(y_hat),test_set$Survived)

titanic_clean %>% mutate (Fare = round(Fare/10)*10)  %>% group_by(Fare) %>% summarize(Survival=mean(Survived==1)) %>% ggplot(aes(Fare,Survival))+geom_col()
titanic_clean %>% mutate (Age = round(Age/5)*5)  %>% group_by(Age) %>% summarize(Survival=mean(Survived==1)) %>% ggplot(aes(Age,Survival))+geom_col()

# Question 8: Logistic regression models
# Set the seed to 1. Train a logistic regression model with the caret glm method using age as the only predictor.
# What is the accuracy on the test set using age as the only predictor?
set.seed(1, sample.kind = "Rounding")
fit_glm<-train(Survived~Age, method="glm", data=train_set)
pred_glm <- predict(fit_glm,test_set)
mean(pred_glm == test_set$Survived)
fit_glm$finalModel

# Set the seed to 1. Train a logistic regression model with the caret glm method using four predictors: sex, class, fare, and age.
# What is the accuracy on the test set using these four predictors?
set.seed(1, sample.kind = "Rounding")
fit_glm<-train(Survived~Age+Sex+Pclass+Fare, method="glm", data=train_set)
pred_glm <- predict(fit_glm,test_set)
mean(pred_glm == test_set$Survived) # 0.849

fit_glm$finalModel

# Set the seed to 1. Train a logistic regression model with the caret glm method using all predictors. Ignore warnings about rank-deficient fit.
# What is the accuracy on the test set using all predictors?
set.seed(1, sample.kind = "Rounding")
fit_glm<-train(Survived~., method="glm", data=train_set)
pred_glm <- predict(fit_glm,test_set)
mean(pred_glm == test_set$Survived) # 0.849

fit_glm$finalModel

# Question 9a: kNN model
# Set the seed to 6. Train a kNN model on the training set using the caret train function. Try tuning with k = seq(3, 51, 2).
# What is the optimal value of the number of neighbors k?
set.seed(6, sample.kind = "Rounding")
fit_knn<-train(Survived~., method="knn", data=train_set, tuneGrid=data.frame(k = seq(3, 51, 2)))
fit_knn$bestTune$k # 11

# Q9b Plot the kNN model to investigate the relationship between the number of neighbors and accuracy on the training set.
# Of these values of  𝑘 , which yields the highest accuracy?=> 11
plot(fit_knn)

# 9c What is the accuracy of the kNN model on the test set?
mean(predict(fit_knn, test_set) == test_set$Survived)  # 0.709

fit_knn$finalModel

# Question 10: Cross-validation
# Set the seed to 8 and train a new kNN model. Instead of the default training control, use 10-fold cross-validation where each partition consists of 10% of the total.
# Try tuning with k = seq(3, 51, 2). What is the optimal value of k using cross-validation?
set.seed(8, sample.kind = "Rounding")
fit_knn2<-train(Survived~., method="knn", data=train_set, tuneGrid=data.frame(k = seq(3, 51, 2)), trControl= trainControl(method = "cv", number = 10, p = .9))
fit_knn2$bestTune$k

# What is the accuracy on the test set using the cross-validated kNN model?
mean(predict(fit_knn2, test_set) == test_set$Survived) # 0.648

# Question 11a: Classification tree model
#
# Set the seed to 10. Use caret to train a decision tree with the rpart method. Tune the complexity parameter with cp = seq(0, 0.05, 0.002).
# What is the optimal value of the complexity parameter (cp)?
set.seed(10, sample.kind = "Rounding")
fit_rpart<-train(Survived~., method="rpart", data=train_set, tuneGrid=data.frame(cp = seq(0, 0.05, 0.002)))
fit_rpart$bestTune  # 0.016
plot(fit_rpart)

# What is the accuracy of the decision tree model on the test set?
mean(predict(fit_rpart, test_set) == test_set$Survived)  # 0.838

# Question 11b: Classification tree model
# Inspect the final model and plot the decision tree. Which variables are used in the decision tree?
fit_rpart$finalModel
plot(fit_rpart$finalModel, margin = 0.1)
text(fit_rpart$finalModel) #=> sex age fare pclass
fit_rpart$coefnames results

# Question 12: Random forest model
# Set the seed to 14. Use the caret train() function with the rf method to train a random forest. 
# Test values of mtry ranging from 1 to 7. Set ntree to 100.
set.seed(14, sample.kind = "Rounding")
fit_rf<-train(Survived~., method="rf", data=train_set, tuneGrid=data.frame(mtry = 1:7),ntree=100)

# What mtry value maximizes accuracy?
fit_rf$bestTune# 2

# What is the accuracy of the random forest model on the test set?
mean(predict(fit_rf,test_set)==test_set$Survived)    # 0.844

# Use varImp() on the random forest model object to determine the importance of various predictors to the random forest model.
# What is the most important variable? Sexmale
varImp(fit_rf$finalModel)

#-----------------------------------------------  
#-----------------------------------------------  
# section 6
#-----------------------------------------------  
#-----------------------------------------------  
#  Case Study: MNIST

library(dslabs)
mnist <- read_mnist()

names(mnist)
dim(mnist$train$images)

class(mnist$train$labels)
table(mnist$train$labels)

# sample 10k rows from training set, 1k rows from test set
set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

#-----------------------------------------------  
# Preprocessing MNIST Data

# Common preprocessing steps include:
#   standardizing or transforming predictors and removing predictors that are not useful, 
# are highly correlated with others, have very few non-unique values, or have close to zero variation. 

library(matrixStats)
sds <- colSds(x)
class(x)
dim(x)
class(sds)
length(sds)
qplot(sds, bins = 256, color = I("black"))
image(matrix(sds,28,28))

library(caret)
nzv <- nearZeroVar(x)
length(nzv)
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)
#-----------------------------------------------  
# Model Fitting for MNIST Data

# Note on Rborist 0.1-17
# There appears to be an issue with Version 0.1-17 of the Rborist package that causes R sessions to abort/terminate. We recommend using an older version of Rborist or not running the code at 3:07.
# 
# The caret package requires that we add column names to the feature matrices.
# In general, it is a good idea to test out a small subset of the data first to get an idea of how long your code will take to run.

x[1:10,1:20]
x_test[1:10,1:20]
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

# k-nearest neighbor
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y,
                   method = "knn", 
                   tuneGrid = data.frame(k = c(1,3,5,7)),
                   trControl = control)
ggplot(train_knn)
train_knn

# try a test run with a subset of the data to get an idea of timing 
n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)

# Once we optimize our algorithm, we can fit it to the entire dataset:
fit_knn <- knn3(x[ ,col_index], y,  k = 3)

y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"] # 0.955

# From the specificity and sensitivity, we also see that 8s are the hardest to detect and the most commonly incorrectly predicted digit is 7.
cm$byClass[,1:2]

# random forest

# Tuning:
# nSamp : random sample of the observations when constructing each tree
# nTree : number of trees that are fit
# cv: cross validation
# predFixed : num of Randomly Selected Predictors
# minNode : min nb of observations in a node
library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
grid
train_rf <-  train(x[, col_index], y,
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune
# Now that we have optimized our algorithm, we are ready to fit our final model:
fit_rf <- Rborist(x[, col_index], y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

rafalib::mypar(3,4)
for(i in 1:12){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste("Our prediction:", y_hat_rf[i]),
        xaxt="n", yaxt="n")
}

#-----------------------------------------------  
# Variable Importance
# The Rborist package does not currently support variable importance calculations, but the randomForest package does.
# An important part of data science is visualizing results to determine why we are failing.

library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y,  ntree = 50)
imp <- importance(rf)
imp

image(matrix(imp, 28, 28))

p_max <- predict(fit_knn, x_test[,col_index]) # pour rappel y_hat_knn =  predict de type "class"
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_knn != y_test) 
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1],
        main = paste0("Pr(",y_hat_knn[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

p_max <- predict(fit_rf, x_test[,col_index])$census  
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_rf != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
# install.packages("rafalib")
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}
#-----------------------------------------------  
# Ensembles
# Ensembles combine multiple machine learning algorithms into one model to improve predictions.
p_rf <- predict(fit_rf, x_test[,col_index])$census
p_rf[1:20,]
y_test[1:20]
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[,col_index])
p_knn[1:20,]
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)
#-----------------------------------------------  
# Comprehension Check: Ensembles

# Q1 Use the training set to build a model with several of the models available from the caret package. 
# We will test out 10 of the most common machine learning models in this exercise:

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

# Apply all of these models using train() with all the default parameters. You may need to install some packages. 
# Keep in mind that you will probably get some warnings. Also, it will probably take a while to train all of the models - be patient!
  
#Run the following code to train the various models:
  
library(caret)
library(dslabs)
set.seed(1, sample.kind = "Rounding")
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models
names(fits)

# Q2 Now that you have all the trained models in a list, use sapply() or map() to create a matrix of predictions for the test set. 
# You should end up with a matrix with length(mnist_27$test$y) rows and length(models) columns.
fits[1]
dim(mnist_27$train)
str(mnist_27$train)
str(mnist_27$test)
length(mnist_27$test$y)
predict(fits[1],mnist_27$test)

preds <- map_dfc(fits, function(x) {
  predict(x, mnist_27$test)
})
str(preds)
preds<-as.matrix(preds)
dim(preds)
# simpler : 
preds <- sapply(fits, function(object) {predict(object, mnist_27$test)})

# Q3 accuracy for each model on the test set + mean accuracy across all models

mnist_27$test$y
confusionMatrix(factor(preds[,1],c(2,7)), mnist_27$test$y)

confs <- apply(preds,  2, function(x) { confusionMatrix(factor(x,c(2,7)), mnist_27$test$y)})
sapply(confs, function(x) x$overall[["Accuracy"]])
mean(sapply(confs, function(x) x$overall[["Accuracy"]]))

# simpler :  
mean ( colMeans(preds == mnist_27$test$y) ) # 0.789

# Q4 Next, build an ensemble prediction by majority vote and compute the accuracy of the ensemble. 
# Vote 7 if more than 50% of the models are predicting a 7, and 2 otherwise.
# What is the accuracy of the ensemble?
rowSums(preds[1:10,]==7)
ifelse(rowSums(preds[1:10,]==7)>5 , 7,2)
ens_pred <- ifelse(rowSums(preds==7)>5 , 7,2)
mean_ens_pred <- mean(ens_pred == mnist_27$test$y)
mean_ens_pred # 0.815

# Q5 In Q3, we computed the accuracy of each method on the test set and noticed that the individual accuracies varied.
# How many of the individual methods do better than the ensemble?
methods_accurary <- colMeans(preds == mnist_27$test$y)
sum( methods_accurary > mean_ens_pred ) # knn, gamLoess, qda

#Q6 It is tempting to remove the methods that do not perform well and re-do the ensemble. 
# The problem with this approach is that we are using the test data to make a decision. 
# However, we could use the minimum accuracy estimates obtained from cross validation with the training data for each model. 
# Obtain these estimates and save them in an object. Report the mean of these training set accuracy estimates.
# What is the mean of these training set accuracy estimates?
min(fits[[10]]$results$Accuracy)

min_acc<- sapply(fits, function(x) { min(x$results$Accuracy)})
mean(min_acc) # 0.808

# Q7 Now let's only consider the methods with an estimated accuracy of greater than or equal to 0.8 when constructing the ensemble.
# What is the accuracy of the ensemble now?
min_acc>=0.8
preds[1:10,min_acc>=0.8]
new_ens_pred <- ifelse(rowMeans(preds[,min_acc>=0.8]==7)>0.5 , 7,2)
mean(new_ens_pred == mnist_27$test$y) # 0.83


#-----------------------------------------------  
# Dimension Reduction

# Preserving distance
#Each point is a pair of twins. We use the mvrnorm function from the MASS package to simulate bivariate normal data.
set.seed(1988)
library(MASS)
n <- 100
Sigma <- matrix(c(9, 9 * 0.9, 9 * 0.92, 9 * 1), 2, 2)
x <- rbind(mvrnorm(n / 2, c(69, 69), Sigma),
           mvrnorm(n / 2, c(55, 55), Sigma))
plot(x)

# distance btw points 1 & 2 and btw 2 and 51
d <- dist(x)
as.matrix(d)[1, 2]
#> [1] 1.98
as.matrix(d)[2, 51]
#> [1] 18.7

# specific challenge: we want a one-dimensional summary of our predictors from which we can approximate the distance between any two observations.

z <- x[,1]
plot(dist(x),dist(z))+abline(a=0,b=1,col="red")
sd(dist(x) - dist(z)*sqrt(2))

z <- sqrt((x[,1]+x[,2])^2) /sqrt(2)
plot(dist(x),dist(z))+abline(a=0,b=1,col="red")
sd(dist(x) - dist(z))

z  <- cbind((x[,2] + x[,1])/2,  x[,2] - x[,1])
# z[,1] is the first principal component of the matrix x
plot(z)
sd(dist(x) - dist(z[,1])*sqrt(2))

# to achieve orthogonality; Z is called an orthogonal rotation of X : it preserves the distances between rows
z[,1] <- (x[,1] + x[,2]) / sqrt(2)
z[,2] <- (x[,2] - x[,1]) / sqrt(2)
# transformation that preserves the distance between any two points:
max(dist(z) - dist(x))
# if we use just the first dimension:
sd(dist(x) - dist(z[,1]))
library(tidyverse)
qplot(z[,1], bins = 20, color = I("black"))
# We successfully reduced the number of dimensions from two to one with very little loss of information.
# The reason we were able to do this is because the columns of  X were very correlated:
cor(x[,1], x[,2])
# and the transformation produced uncorrelated columns with “independent” information in each column:
cor(z[,1], z[,2])

# --------------------------------------------------------------
# Principal component analysis (PCA)
# --------------------------------------------------------------

colMeans(x^2)

# if we apply an orthogonal transformation as above, then the total variation remains the same:

sum(colMeans(x^2))
sum(colMeans(z^2))
# in the transformed version Z, 99% of the variability is included in just the first dimension:
v <- colMeans(z^2)
v/sum(v)
# The first principal component (PC) of a matrix X is the linear orthogonal transformation of  X
# that maximizes this variability. The function prcomp provides this info:
pca <- prcomp(x)
pca$rotation
# The function PCA returns both the rotation needed to transform  X so that the variability of the columns is decreasing 
# from most variable to least (accessed with $rotation) as well as the resulting new matrix (accessed with $x). 

# By default the columns of X are first centered.
a <- sweep(x, 2, colMeans(x))
colMeans(a)

b <- pca$x %*% t(pca$rotation)
max(abs(a - b)) # this shows that B = pca$x . tR = Z . tR = X

a <- sweep(x, 2, colMeans(x)) %*% pca$rotation
b <- pca$x 
max(abs(a - b)) # this shows that pca$x = B = X.R

# Iris example
##################
names(iris)
iris
iris$Species # data is ordered by the species.

x <- iris[,1:4] %>% as.matrix()
d <- dist(x)
# three species with one species very different from the other two
image(as.matrix(d), col = rev(RColorBrewer::brewer.pal(9, "RdBu")))

# Our predictors here have four dimensions, but three are very correlated:

cor(x)
pca <- prcomp(x)  #  *********
summary(pca)  # *********

head(pca$x)

data.frame(pca$x[,1:2], Species=iris$Species) %>% 
  ggplot(aes(PC1,PC2, fill = Species))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)

d_approx <- dist(pca$x[, 1:2])
qplot(d, d_approx) + geom_abline(color="red")

# MNIST example
##################

library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
col_means <- colMeans(mnist$test$images)

pca <- prcomp(mnist$train$images)
dim(pca$rotation)
dim(pca$x)
length(pca$sdev)

# Plot : variance des différentes composantes
pc <- 1:ncol(mnist$test$images)
qplot(pc, pca$sdev)

# the first few PCs already explain a large percent of the variability:
summary(pca)$importance[,1:5]
# by looking at the first two PCs we see information about the class. Here is a random sample of 2,000 digits:
data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2],
           label=factor(mnist$train$label)) %>%
  sample_n(2000) %>% 
  ggplot(aes(PC1, PC2, fill=label))+
  geom_point(cex=3, pch=21)

# We can also see the linear combinations on the grid to get an idea of what is getting weighted:
image(1:28, 1:28, matrix(pca$rotation[,1], nrow=28)[ , 28:1], col = rev(RColorBrewer::brewer.pal(9, "RdBu")))
image(1:28, 1:28, matrix(pca$rotation[,2], nrow=28)[ , 28:1], col = rev(RColorBrewer::brewer.pal(9, "RdBu")))
# ne marche pas
# library(gridExtra)
# grid.arrange(p1, p2, ncol = 2)

# Now let’s apply the transformation we learned with the training data to the test data, 
# reduce the dimension and run knn on just a small number of dimensions.
# 
# We try 36 dimensions since this explains about 80% of the data. First fit the model:
summary(pca)$importance[,34:37]

library(caret)
k <- 36
x_train <- pca$x[,1:k]

# On calibre le modèle sur les K composantes principales du jeu d'apprentissage
y <- factor(mnist$train$labels)
fit <- knn3(x_train, y)

# Now transform the test set: Z = (X-mean(X)) . R
# On applique la PCA sur le jeu de test
x_test <- sweep(mnist$test$images, 2, col_means) %*% pca$rotation

# On limite le jeu de test aux K premiers facteurs
x_test <- x_test[,1:k]

# On effectue une prediction sur le jeu de test réduit à K composantes
y_hat <- predict(fit, x_test, type = "class")
confusionMatrix(y_hat, factor(mnist$test$labels))$overall["Accuracy"]

#> Accuracy 
#>    0.975
# With just 36 dimensions we get an accuracy well above 0.95.

#-----------------------------------------------  
# Comprehension Check: Dimension Reduction
# Q1 We want to explore the tissue_gene_expression predictors by plotting them.

data("tissue_gene_expression")
dim(tissue_gene_expression$x) # 189 x 500

head(tissue_gene_expression$x)

# We want to get an idea of which observations are close to each other, but, as you can see from the dimensions, 
# the predictors are 500-dimensional, making plotting difficult. Plot the first two principal components with color representing tissue type.

pca <- prcomp(tissue_gene_expression$x)
str(pca$x[,1])
dim(pca$x) # 189 x 189 
dim(pca$rotation) # 500 x 189
data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2],
           label=factor(tissue_gene_expression$y)) %>%
  ggplot(aes(PC1, PC2, fill=label))+
  geom_point(cex=3, pch=21)

# Which tissue is in a cluster by itself? Liver

# Q2 The predictors for each observation are measured using the same device and experimental procedure. 
# This introduces biases that can affect all the predictors from one observation. 
# For each observation, compute the average across all predictors, and then plot this against the first PC with color representing tissue. 
# Report the correlation.
data.frame(PC1 = pca$x[,1], avg = rowMeans(tissue_gene_expression$x),
           label=factor(tissue_gene_expression$y)) %>%
  ggplot(aes(PC1, avg, fill=label))+
  geom_point(cex=3, pch=21)

cor(pca$x[,1], rowMeans(tissue_gene_expression$x))

# Q3 We see an association with the first PC and the observation averages. 
# Redo the PCA but only after removing the center. 

# x<-tissue_gene_expression$x-rowMeans(tissue_gene_expression$x) # equivalent
x <- sweep(tissue_gene_expression$x, 1, rowMeans(tissue_gene_expression$x))

pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

# Q4 For the first 10 PCs, make a boxplot showing the values for each tissue.
# For the 7th PC, which two tissues have the greatest median difference?
data.frame(pc = pc$x[,7], tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc, tissue)) +
  geom_boxplot()

for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

# Q5 Plot the percent variance explained by PC number. Hint: use the summary function.
# How many PCs are required to reach a cumulative percent variance explained greater than 50%?

str(summary(pc))
plot(summary(pc)$importance["Proportion of Variance",])
summary(pc)$importance[3,1:10] # 3 PC

# ----------------------------------------
# -------------------------------
## Recommendation Systems

# https://rafalab.github.io/dsbook/large-datasets.html#recommendation-systems
#
# For more information about the "Netflix Challenge," you can check out these sites:
#   
#   https://bits.blogs.nytimes.com/2009/09/21/netflix-awards-1-million-prize-and-starts-a-new-contest/
#   http://blog.echen.me/2011/10/24/winning-the-netflix-prize-a-summary/
#   https://www.netflixprize.com/assets/GrandPrize2009_BPC_BellKor.pdf
# Key points
# Recommendation systems are more complicated machine learning challenges because each outcome has a different set of predictors. For example, different users rate a different number of movies and rate different movies.
# To compare different models or to see how well we’re doing compared to a baseline, we will use root mean squared error (RMSE) as our loss function. We can interpret RMSE similar to standard deviation.
# If N is the number of user-movie combinations,  y(u,i) is the rating for movie i by user u,  and y_hat  is our prediction, then RMSE is defined as follows: 
#   sqrt ( sum(u,i) (y_hat(u,i)-y(u,i))^2 )
# 
library(dslabs)
library(tidyverse)
data("movielens")

head(movielens)
movielens %>% as_tibble()

movielens %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

keep <- movielens %>%
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)
tab <- movielens %>%
  filter(userId %in% c(13:20)) %>% 
  filter(movieId %in% keep) %>% 
  select(userId, title, rating) %>% 
  spread(title, rating)
tab %>% knitr::kable()

# To see how sparse the matrix is, here is the matrix for a random sample of 100 movies and 100 users with yellow indicating a user/movie combination for which we have a rating.

users <- sample(unique(movielens$userId), 100)
rafalib::mypar()
movielens %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

# distribution of number of ratings per movie
movielens %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

# distribution of number of ratings per user
movielens %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

# create a test 
library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

#To make sure we don’t include users and movies in the test set that do not appear in the training set, we remove these entries using the semi_join function:
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# semi_join() keeps the part of first table for which we have information in the second.
# test_set$movieId[!(test_set$movieId %in% train_set$movieId)]
# test_set%>%filter(movieId==1181) %>% semi_join(train_set, by = "movieId")

# function that computes the RMSE for vectors of ratings and their corresponding predictors

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Building the Recommendation System
# ----------------------------------------

# first mmodel
mu_hat <- mean(train_set$rating)
mu_hat
# if we predict all unknown ratings with mu we obtain the following RMSE:
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse
# Keep in mind that if you plug in any other number, you get a higher RMSE. For example:
predictions <- rep(3, nrow(test_set))
RMSE(test_set$rating, predictions)
# From looking at the distribution of ratings, we can visualize that this is the standard deviation of that distribution. 
train_set %>% group_by(movieId) %>% summarize(rating=mean(rating)) %>% ggplot(aes(rating))+geom_histogram(bins = 30, color = "black")

rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)

#### Modeling movie effects: Yui = mu + bi +epsilon(ui)

# fit <- lm(rating ~ as.factor(movieId), data = movielens)
# Because there are thousands of bi (= bias = effects) as each movie gets one, the lm() function will be very slow here. 
# We therefore don’t recommend running the code above
# But in this particular situation, we know that the least squares estimate b_hat is just the average of Y - mu for each movie i
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
# We can see that these estimates vary substantially:
qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
RMSE(predicted_ratings, test_set$rating)

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

### User effects
# Let’s compute the average rating for user u for those that have rated over 100 movies:
  
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

# Y = mu + bu + bi + eps
# To fit this model, we could again use lm like this:
# lm(rating ~ as.factor(movieId) + as.factor(userId))
# but, for the reasons described earlier, we won’t. 
# Instead, we will compute an approximation by computing mu_hat and bi_hat 
# and estimating  bu_hat as the average of  Y - mu - bi
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# We can now construct predictors and see how much the RMSE improves:

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
RMSE(predicted_ratings, test_set$rating)

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

###
## Comprehension Check: Recommendation Systems
library(lubridate)
library(dslabs)
data("movielens")

# Q1 Compute the number of ratings for each movie and then plot it against the year the movie came out. Use the square root transformation on the counts.
# What year has the highest median number of ratings?

movielens %>% as_tibble()

movielens %>%
  group_by(movieId,title,year)%>% 
  summarize(count=n()) %>%
  ggplot(aes(year, count)) + 
  geom_boxplot(outlier.shape = NA, aes(group = cut_width(year,5))) +
  geom_jitter(width = 0.2, alpha = 0.2)+
  scale_y_sqrt()

movielens %>% 
  group_by(year,movieId)%>% 
  summarize(count=n()) %>%
  group_by(year) %>%
  summarize(median=median(count)) %>% arrange(desc(median))
  
# solution
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Q2 We see that, on average, movies that came out after 1993 get more ratings. We also see that with newer movies, starting in 1993, the number of ratings decreases with year: 
# the more recent a movie is, the less time users have had to rate it.
# Among movies that came out in 1993 or later, select the top 25 movies with the highest average number of ratings per year (n/year), and caculate the average rating of each of them. 
# To calculate number of ratings per year, use 2018 as the end year.
movielens %>% filter(year<=2018 & year>=1993) %>%
  group_by(movieId,title,year)%>% 
  summarize(count=n(), avgrating=mean(rating))%>%
  mutate(yearcount=(2018-year), ratingperyear=count/yearcount) %>%
  arrange(desc(ratingperyear)) %>% top_n(ratingperyear,25)

# What is the average rating for the movie The Shawshank Redemption?
# What is the average number of ratings per year for the movie Forrest Gump?

movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))

# Q3 From the table constructed in Q2, we can see that the most frequently rated movies tend to have above average ratings. 
# This is not surprising: more people watch popular movies. To confirm this, stratify the post-1993 movies by ratings per year and compute their average ratings. 
# To calculate number of ratings per year, use 2018 as the end year. Make a plot of average rating versus ratings per year and show an estimate of the trend.

rate_per_year <- movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year)) %>%
  mutate(rate_p_year = n/years)

movielens %>% 
  filter(year >= 1993) %>%
  left_join(x=., y=rate_per_year, by="movieId")  %>%
  group_by(rate_p_year) %>% 
  summarize(rating=mean(rating))%>%
  ggplot(aes(rate_p_year,rating))+geom_point()

# => The more often a movie is rated, the higher its average rating.
# correction
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

# Q4 Suppose you are doing a predictive analysis in which you need to fill in the missing ratings with some value.
# Given your observations in the exercise in Q3, which of the following strategies would be most appropriate?
  
# -> Fill in the missing values with a lower value than the average rating across all movies.
# Because a lack of ratings is associated with lower ratings, it would be most appropriate to fill in the missing value with a lower value than the average. 
# You should try out different values to fill in the missing value and evaluate prediction in a test set.
  
# Q5 The movielens dataset also includes a time stamp. This variable represents the time and data in which the rating was provided. 
# The units are seconds since January 1, 1970. Create a new column date with the date.

movielens <- mutate(movielens, date = as_datetime(timestamp)) 

# Q6 Compute the average rating for each week and plot this average against date. Hint: use the round_date() function before you group_by().
# What type of trend do you observe?
movielens %>% mutate(week=round_date(date,unit="week")) %>% 
  group_by(week) %>% 
  summarize(avgrateweek = mean(rating)) %>%
  ggplot(aes(week, avgrateweek)) +
  geom_point() +
  geom_smooth()
# -> There is some evidence of a time effect on average rating. (pas évident, controversé dans les commentaires)

# Q7 Consider again the plot you generated in Q6.

# If we define d(u,i) as the day for user's u rating of movie i , which of the following models is most appropriate?
# => Y(u,i) = mu + b(i) + b(u) + f(d(u,i)) + epsilon(u,i) , with f a smooth function

movielens %>% mutate(date=weekdays(round_date(date,unit="day"))) %>% 
  group_by(date) %>% 
  ggplot(aes(date, rating)) +
  geom_boxplot() # bof # en fait ça dépend du temps et pas du jour de la semaine

# Q8  The movielens data also has a genres column. This column includes every genre that applies to the movie. 
# Some movies fall under several genres. Define a category as whatever combination appears in this column. Keep only categories with more than 1,000 ratings. 
# Then compute the average and standard error for each category. Plot these as error bar plots.

unique(movielens$genres)
head(movielens)

movielens %>% group_by(genres) %>%
  summarize(n=n() , avg=mean(rating),sd=sd(rating)/sqrt(n)) %>%
  filter(n>1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x=genres, y=avg, ymin=avg-2*sd,ymax=avg+2*sd))+
  geom_point()+geom_errorbar() + coord_flip()

# Q9 The plot you generated in Q8 shows strong evidence of a genre effect. Consider this plot as you answer the following question.
# If we define g(u,i) as the genre for user u's rating of movie i, which of the following models is most appropriate?
# => Y(u,i)= mu + b(i) + b(u) + sum (k=1...K) { x(u,i,k) beta(k) } + epsilon (u,i) with x(u,i,k) = 1 if g(u,i) is genre k

# ----------------------------------------
# Regularization

#10 largest mistakes:
test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%  
  slice(1:10) %>% 
  pull(title)

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()

# Here are the 10 best movies according to our estimate:
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10)  %>% 
  pull(title)
# 10 worst
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10)  %>% 
  pull(title)
# They all seem to be quite obscure. Let’s look at how often they are rated.
train_set %>% count(movieId) %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(n)

train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10) %>% 
  pull(n)

# => few users = uncertainty. Regularization permits us to penalize large estimates that are formed using small sample sizes

# https://rafalab.github.io/dsbook/large-datasets.html#regularization

# To improve our results, we will use regularization. 
# Regularization constrains the total variability of the effect sizes by penalizing large estimates that come from small sample sizes.
# To estimate the  𝑏 ’s, we will now minimize this equation, which contains a penalty term:lamba*sum(bi^)2
# => b_i = sum(rating - mu)/(n()+lambda) 
# The larger lambda is, the more we shrink.
# Lambda  is a tuning parameter, so we can use cross-validation to choose it. 
# We should be using full cross-validation on just the training set, without using the test set until the final assessment.
# We can also use regularization to estimate the user effect. 


# compute the regularized estimates of bi
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize ( b_i = sum(rating - mu)/(n()+lambda) , 
              n_i = n() ) 
# plot of the regularized estimates versus the least squares estimates.
tibble(original = movie_avgs$b_i, 
       regularlized = movie_reg_avgs$b_i, 
       n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

# top 10 best movies based on the penalized estimates
train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(title)

# top 10 worst movies:

train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  pull(title)

# Do we improve our results?
  
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
RMSE(predicted_ratings, test_set$rating)
# The penalized estimates provide a large improvement over the least squares estimates
model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

## 
## Choosing the penalty terms (cross validation)

lambdas <- seq(0, 10, 0.25)

mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  
lambdas[which.min(rmses)] # min pour lambda=3
# nb. in practice we should be using full cross-validation just on the train set

# with the user effects :

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

# ----------------------------------------
# ----------------------------------------
# ----------------------------------------
### Comprehension Check: Regularization

options(digits=7)

# An education expert is advocating for smaller schools. The expert bases this recommendation on the fact that among the best performing schools, 
# many are small schools. Let's simulate a dataset for 1000 schools. First, let's simulate the number of students in each school, using the following code:
set.seed(1986, sample.kind="Rounding")
n <- round(2^rnorm(1000, 8, 1))

# Now let's assign a true quality for each school that is completely independent from size. This is the parameter we want to estimate in our analysis. The true quality can be assigned using the following code:

set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
# We can see the top 10 schools using this code: 
schools %>% top_n(10, quality) %>% arrange(desc(quality))

# Now let's have the students in the school take a test. There is random variability in test taking, so we will simulate the test scores as normally distributed 
# with the average determined by the school quality with a standard deviation of 30 percentage points. This code will simulate the test scores:

set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
       scores <- rnorm(schools$size[i], schools$quality[i], 30)
       scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

# Q1 What are the top schools based on the average score? Show just the ID, size, and the average score.Report the ID of the top school and average score of the 10th school.
schools %>% top_n(n = 10, wt=score) %>% 
    arrange(desc(score)) 

#Q2 Compare the median school size to the median school size of the top 10 schools based on the score.
# What is the median school size overall? What is the median school size of the of the top 10 schools based on the score?
median(schools$size)
schools %>% top_n(n = 10, wt=score) %>% arrange(desc(score))  %>% summarize(median(size))

# Q3 According to this analysis, it appears that small schools produce better test scores than large schools. Four out of the top 10 schools have 100 or fewer students. 
# But how can this be? We constructed the simulation so that quality and size were independent. Repeat the exercise for the worst 10 schools.
# What is the median school size of the bottom 10 schools based on the score?
schools %>% top_n(n = 10, wt=-score) %>%  arrange(score)
schools %>% top_n(n = 10, wt=-score) %>% .$size %>% median

# Q4 From this analysis, we see that the worst schools are also small. Plot the average score versus school size to see what's going on. Highlight the top 10 schools based on the true quality.
schools %>% ggplot(aes(size,score, color = rank(score)>=990))+geom_point()
# => The standard error of the score has larger variability when the school is smaller, which is why both the best and the worst schools are more likely to be small
schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)
# We can see that the standard error of the score has larger variability when the school is smaller. 
# This is a basic statistical reality we learned in PH125.3x: Data Science: Probability and PH125.4x: Data Science: Inference and Modeling courses! 
# Note also that several of the top 10 schools based on true quality are also in the top 10 schools based on the exam score: schools %>% top_n(10, score) %>% arrange(desc(score))

# Q5 Let's use regularization to pick the best schools. Remember regularization shrinks deviations from the average towards 0. 
# To apply regularization here, we first need to define the overall average for all schools, using the following code:
overall <- mean(sapply(scores, mean))
mean(schools$score)
# Then, we need to define, for each school, how it deviates from that average.
# Write code that estimates the score above the average for each school but dividing by n+alpha  instead of n, with n the school size and alpha a regularization parameter.
alpha<-25
scores_sum <- sapply(scores, function (x) {sum(x-overall)} ) 
schools %>% 
  mutate(reg_score = overall +(scores_sum)/(size+alpha)) %>% 
  top_n(n=10, reg_score) %>% arrange(desc(reg_score))
# alt
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))

# Q6 Notice that this improves things a bit. The number of small schools that are not highly ranked is now lower. 
# Is there a better  𝛼 ? Using values of  𝛼  from 10 to 250, find the  𝛼  that minimizes the RMSE.
rmse <- sapply(10:250, function(alpha){
  score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
  rmse <- sqrt( mean((score_reg - schools$quality)^2) )
  rmse
}) 

rmse <- data.frame(alpha=10:250, rmse=rmse)
plot(rmse)
rmse[which.min(rmse$rmse),] # 135

# Q7 Rank the schools based on the average obtained with the best alpha. Note that no small school is incorrectly included.
# What is the ID of the top school now? 191. What is the regularized average score of the 10th school now? 85.48
alpha<-rmse$alpha[which.min(rmse$rmse)]
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% 
  mutate(score_reg = score_reg ) %>% 
  top_n(n=10, score_reg) %>% arrange(desc(score_reg))

# Q8 A common mistake made when using regularization is shrinking values towards 0 that are not centered around 0. 
# For example, if we don't subtract the overall average before shrinking, we actually obtain a very similar result. 
# Confirm this by re-running the code from the exercise in Q6 but without removing the overall mean.
# What value of  𝛼  gives the minimum RMSE here?
alphas <- seq(10,250)
rmse_bad <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x)  sum(x)/(length(x)+alpha))
  rmse <- sqrt( mean((score_reg - schools$quality)^2) )
  rmse
}) 
rmse_bad_df <- data.frame(alpha=alphas, rmse=rmse_bad)
plot(rmse_bad_df)
rmse_bad_df[which.min(rmse_bad_df$rmse),] # 10

###  --------------------------------
### Matrix Factorization
###
# Matrix factorization is a widely used concept in machine learning. 
# It is very much related to factor analysis, singular value decomposition (SVD), and principal component analysis (PCA). 
# Here we describe the concept in the context of movie recommendation systems.

# For illustrative purposes, we will only consider a small subset of movies with many ratings and users that have rated many movies. We also keep Scent of a Woman (movieId == 3252) because we use it for a specific example:
train_small <- movielens %>% 
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% 
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

# a matrix so that each user gets a row, each movie gets a column
y <- train_small %>% 
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

# We add row names and column names:
rownames(y)<- y[,1]
y <- y[,-1]

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

# and convert them to residuals by removing the column and row effects:
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))

# If the model above explains all the signals, and the epsilon are just noise, then the residuals for different movies should be independent from each other. 
# But they are not. Here are some examples:
  
m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
p1 <- qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)

m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
p2 <- qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

m_4 <- "You've Got Mail" 
m_5 <- "Sleepless in Seattle" 
p3 <- qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)

gridExtra::grid.arrange(p1, p2 ,p3, ncol = 3)

# correlation between movies, we can see a pattern (we rename the columns to save print space):
x <- y[, c(m_1, m_2, m_3, m_4, m_5)]
short_names <- c("Godfather", "Godfather2", "Goodfellas",
                 "You've Got", "Sleepless")
colnames(x) <- short_names
cor(x, use="pairwise.complete")

# Factors analysis
# Here is an illustration, using a simulation, of how we can use some structure to predict the r(u,i)
# Suppose our residuals r look like this:
r<-x
round(r, 1)


set.seed(1)
options(digits = 2)
Q <- matrix(c(1 , 1, 1, -1, -1), ncol=1)
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
P <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol=1)
rownames(P) <- 1:nrow(P)

X <- jitter(P%*%t(Q))
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(aling="c")

P

set.seed(1)
options(digits = 2)
m_6 <- "Scent of a Woman"
Q <- cbind(c(1 , 1, 1, -1, -1, -1), 
           c(1 , 1, -1, -1, -1, 1))
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)
P <- cbind(rep(c(2,0,-2), c(3,5,4)), 
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
rownames(P) <- 1:nrow(X)

X <- jitter(P%*%t(Q), factor=1)
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(align="c")

P

six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
tmp <- y[,six_movies]
cor(tmp, use="pairwise.complete")


###  --------------------------------
### SVD and PCA
###
# You can think of singular value decomposition (SVD) as an algorithm that finds the vectors  𝑝  and  𝑞  that permit us to write the matrix of residuals  𝑟  with  𝑚  rows and  𝑛  columns in the following way:
#   𝑟𝑢,𝑖=𝑝𝑢,1𝑞1,𝑖+𝑝𝑢,2𝑞2,𝑖+...+𝑝𝑢,𝑚𝑞𝑚,𝑖, 
# with the variability of these terms decreasing and the  𝑝 ’s uncorrelated to each other.
# SVD also computes the variabilities so that we can know how much of the matrix’s total variability is explained as we add new terms.
# The vectors q are called the principal components and the vectors p  are the user effects. 
# By using principal components analysis (PCA), matrix factorization can capture structure in the data determined by user opinions about movies.

miniy<-matrix(c(3 , 1, 1, 2), nrow = 2,ncol=2)
miniy
miniy <- sweep(miniy, 2, colMeans(miniy))
miniy
minipca<-prcomp(miniy)
minipca$rotation
minipca$x
miniy %*% t(minipca$rotation) - minipca$x
miniy  - minipca$x %*% minipca$rotation

# make the residuals with NAs equal to 0
y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)

dim(y)
# principal components and they are stored in this matrix:
dim(pca$rotation)

# user effects
dim(pca$x)

# variability of each of the vectors:
plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

# the first two principal components are related to the structure in opinions about movies:
library(ggrepel)
pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))

pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)

pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)


###  --------------------------------
### Comprehension Check: Matrix Factorization
###
# In this exercise, we will see one of the ways that this decomposition can be useful. 
# To do this, we will construct a dataset that represents grade scores for 100 students in 24 different subjects. 
# he overall average has been removed so this data represents the percentage point each student received above or below the average test score. 
# So a 0 represents an average grade (C), a 25 is a high grade (A+), and a -25 represents a low grade (F). You can simulate the data like this:
  
set.seed(1987, sample.kind="Rounding")
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))
# Our goal is to describe the student performances as succinctly as possible. For example, we want to know if these test results are all just a random independent numbers. 
# Are all students just about as good? Does being good in one subject  imply you will be good in another? How does the SVD help with all this? 
# We will go step by step to show that with just three relatively small pairs of vectors we can explain much of the variability in this  100×24  dataset. 

# Q1 You can visualize the 24 test scores for the 100 students by plotting an image:

my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)
y[1:10,1:10]
      
# How would you describe the data based on this figure?
# The students that test well are at the top of the image and there seem to be three groupings by subject.

# Q2 You can examine the correlation between the test scores directly like this:

my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# Which of the following best describes what you see?
# There is correlation among all tests, but higher if the tests are in science and math and even higher within each subject.
 
# Q3 Remember that orthogonality means that  𝑈⊤𝑈  and  𝑉⊤𝑉  are equal to the identity matrix. This implies that we can also rewrite the decomposition as
# 𝑌𝑉=𝑈𝐷 or 𝑈⊤𝑌=𝐷𝑉⊤ 
# We can think of YV and UTY  as two transformations of  𝑌  that preserve the total variability of  𝑌  since  𝑈  and  𝑉  are orthogonal.

# Use the function svd() to compute the SVD of y. This function will return  𝑈 ,  𝑉 , and the diagonal entries of  𝐷 .

s <- svd(y)
names(s)
s$d
s$u[1:10,1:10]
s$v[1:10,1:10]
dim(s$u)
dim(s$v)
dim(diag(s$d))

# You can check that the SVD works by typing:
  
y_svd <- s$u %*% diag(s$d) %*% t(s$v)

max(abs(y - y_svd))

# Compute the sum of squares of the columns of  𝑌  and store them in ss_y. 
ss_y <- apply(y^2, 2, sum)
# Then compute the sum of squares of columns of the transformed  𝑌𝑉  and store them in ss_yv
yv <- y %*% s$v
ss_yv <- apply(yv^2, 2, sum )
sum(ss_y)
sum(ss_yv )
# => Confirm that sum(ss_y) is equal to sum(ss_yv).

# et aussi...
sum(apply( (s$u %*% diag(s$d))^2,2,sum) )
sum(apply(diag(s$d)^2, 2, sum))

# Q4 We see that the total sum of squares is preserved. This is because  V is orthogonal. 
# Now to start understanding how  YV  is useful, plot ss_y against the column number and then do the same for ss_yv.
# What do you observe?
plot(ss_y)  
plot(ss_yv)

# Q5 Now notice that we didn't have to compute ss_yv because we already have the answer. 
# How? Remember that  𝑌𝑉=𝑈𝐷  and because  𝑈  is orthogonal, we know that the sum of squares of the columns of  𝑈𝐷  are the diagonal entries of  𝐷  sq
# uared. Confirm this by plotting the square root of ss_yv versus the diagonal entries of  𝐷 .

data.frame(x= s$d, y = sqrt(ss_yv)) %>% ggplot(aes(x,y))+geom_point()
# => droite donc ss_yv(j) = sum (YV(i)^2) = lambas(j) ^2 

# Q6 So from the above we know that the sum of squares of the columns of  𝑌  (the total sum of squares) adds up to the sum of s$d^2 and
# that the transformation  𝑌𝑉  gives us columns with sums of squares equal to s$d^2
# Now compute the percent of the total variability that is explained by just the first three columns of  𝑌𝑉 .
# What proportion of the total variability is explained by the first three columns of  𝑌𝑉 ?

sum(apply(s$u^2, 2, sum))
sum(apply(diag(s$d)^2, 2, sum)) # = sum(ss_yv ) = sum(ss_y )
sum(apply( (s$u %*% diag(s$d))^2, 2, sum))  # = sum(ss_yv ) = sum(ss_y )

sum(ss_yv[1:3]) / sum(ss_yv)
sum(s$d[1:3]^2) / sum(s$d^2)

# Q7 Before we continue, let's show a useful computational trick to avoid creating the matrix diag(s$d). 
# To motivate this, we note that if we write  𝑈  out in its columns  [𝑈1,𝑈2,…,𝑈𝑝]  then  𝑈𝐷  is equal to
# 𝑈𝐷=[𝑈1𝑑1,1,𝑈2𝑑2,2,…,𝑈𝑝𝑑𝑝,𝑝] 
# Use the sweep function to compute  𝑈𝐷  without constructing diag(s$d) or using matrix multiplication.
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

ud <- sweep(s$u, 2, s$d, FUN = "*")

# Q8 We know that  𝑈1𝑑1,1 , the first column of  𝑈𝐷 , has the most variability of all the columns of  𝑈
# 𝐷 . Earlier we looked at an image of  𝑌  using my_image(y), in which we saw that the student to student variability is quite larg
# e and that students that are good in one subject tend to be good in all. This implies that the average (across all subjects) 
# for each student should explain a lot of the variability. 
# Compute the average score for each student, plot it against  𝑈1𝑑1,1 , and describe what you find.

data.frame(x= s$u[,1]*s$d[1], y = rowMeans(y)) %>%
  ggplot(aes(x,y))+geom_point()

ud[1:10,1]
rowMeans(y) [1:10]

# There is a linearly increasing relationship between the average score for each student and U1d1,1 (si on plot l'inverse: plot(-s$u[,1]*s$d[1], rowMeans(y)) )

# Q9 We note that the signs in SVD are arbitrary because:
# 𝑈𝐷𝑉⊤=(−𝑈)𝐷(−𝑉)⊤ 
# With this in mind we see that the first column of  𝑈𝐷  is almost identical to the average score for each student except for the sign.
# 
# This implies that multiplying  𝑌  by the first column of  𝑉  must be performing a similar operation to taking the average
# . Make an image plot of  𝑉  and describe the first column relative to others and how this relates to taking an average.
# 
# How does the first column relate to the others, and how does this relate to taking an average?

my_image(s$v)
# The first column is very close to being a constant, which implies that the first column of YV is the sum of the rows of Y multiplied by some constant, and is thus proportional to an average.

# Q10 We already saw that we can rewrite  𝑈𝐷  as
#   𝑈1𝑑1,1+𝑈2𝑑2,2+⋯+𝑈𝑝𝑑𝑝,𝑝 
# with  𝑈𝑗  the j-th column of  𝑈 . This implies that we can rewrite the entire SVD as:
#   𝑌=𝑈1𝑑1,1𝑉⊤1+𝑈2𝑑2,2𝑉⊤2+⋯+𝑈𝑝𝑑𝑝,𝑝𝑉⊤𝑝 
# Plot  𝑈1 , then plot  𝑉⊤1  using the same range for the y-axis limits
str(y[1:10,1:10, drop=FALSE])
range(s$u[,1,drop=FALSE])
range(s$v[,1,drop=FALSE])
plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))

# then make an image of  𝑈1𝑑1,1𝑉⊤1  and compare it to the image of  𝑌 . Hint: use the my_image() function defined above. Use the drop=FALSE argument to assure the subsets of matrices are matrices.
u1d1v1t <- s$u[,1,drop=FALSE] %*% t(s$v[,1,drop=FALSE]) * s$d[1]
dim( u1d1v1t)
my_image(u1d1v1t)
# or ...
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
my_image(y)


# Q11 We see that with just a vector of length 100, a scalar, and a vector of length 24, we can actually come close to reconstructing the a  100×24  matrix. 
# This is our first matrix factorization:
# 𝑌≈𝑑1,1𝑈1𝑉⊤1

# In the exercise in Q6, we saw how to calculate the percent of total variability explained. 
# However, our approximation only explains the observation that good students tend to be good in all subjects. 
# Another aspect of the original data that our approximation does not explain was the higher similarity we observed within subjects. 
# We can see this by computing the difference between our approximation and original data and then computing the correlations. 
# You can see this by running this code:
#   
resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)


# Now that we have removed the overall student effect, the correlation plot reveals that we have not yet explained the within subject correlation 
# nor the fact that math and science are closer to each other than to the arts. So let's explore the second column of the SVD.

# Repeat the previous exercise (Q10) but for the second column: Plot  𝑈2 , then plot  𝑉⊤2  using the same range for the y-axis limits
# , then make an image of  𝑈2𝑑2,2𝑉⊤2  and compare it to the image of resid.
plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))
my_image(resid)

#Q12 The second column clearly relates to a student's difference in ability in math/science versus the arts. 
#We can see this most clearly from the plot of s$v[,2]. Adding the matrix we obtain with these two columns will help with our approximation:
# 𝑌≈𝑑1,1𝑈1𝑉⊤1+𝑑2,2𝑈2𝑉⊤2 

# We know it will explain 
sum(s$d[1:2]^2)/sum(s$d^2) * 100 
# percent of the total variability. We can compute new residuals like this:
  
resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# and see that the structure that is left is driven by the differences between math and science. 
# Confirm this by first plotting  𝑈3 , then plotting  𝑉⊤3  using the same range for the y-axis limits, the
# then making an image of  𝑈3𝑑3,3𝑉⊤3  and comparing it to the image of resid.
plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])))
my_image(resid)

# Q13 
# The third column clearly relates to a student's difference in ability in math and science. 
# We can see this most clearly from the plot of s$v[,3]. 
# Adding the matrix we obtain with these two columns will help with our approximation:
# 𝑌≈𝑑1,1𝑈1𝑉⊤1+𝑑2,2𝑈2𝑉⊤2+𝑑3,3𝑈3𝑉⊤3 
# We know it will explain: 
  sum(s$d[1:3]^2)/sum(s$d^2) * 100 
# percent of the total variability. We can compute new residuals like this:
resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
# We no longer see structure in the residuals: they seem to be independent of each other. 

# This implies that we can describe the data with the following model:
#  𝑌=𝑑1,1𝑈1𝑉⊤1+𝑑2,2𝑈2𝑉⊤2+𝑑3,3𝑈3𝑉⊤3+𝜀 
# with  𝜀  a matrix of independent identically distributed errors.
# This model is useful because we summarize of  100×24  observations with  3×(100+24+1)= 375  numbers.
# Furthermore, the three components of the model have useful interpretations:
#     1 - the overall ability of a student
# 2 - the difference in ability between the math/sciences and arts
# 3 - the remaining differences between the three subjects.
# 
# The sizes  𝑑1,1,𝑑2,2  and  𝑑3,3  tell us the variability explained by each component. 
# Finally, note that the components  𝑑𝑗,𝑗𝑈𝑗𝑉⊤𝑗  are equivalent to the jth principal component.

# Finish the exercise by plotting an image of  𝑌 , an image of  𝑑1,1𝑈1𝑉⊤1+𝑑2,2𝑈2𝑉⊤2+𝑑3,3𝑈3𝑉⊤3  and an image of the residuals, all with the same zlim.

my_image(y)
modely <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(modely, zlim=range(y))
resid <- y - modely
my_image(resid, zlim=range(y))

###  --------------------------------
### CLUSTERING
### https://rafalab.github.io/dsbook/clustering.html

#construct a simple example based on movie ratings. Here we quickly construct a matrix x that has ratings for the 50 movies with the most ratings.


data("movielens")
top <- movielens %>%
  group_by(movieId) %>%
  summarize(n=n(), title = first(title)) %>%
  top_n(50, n) %>%
  pull(movieId)

x <- movielens %>% 
  filter(movieId %in% top) %>%
  group_by(userId) %>%
  filter(n() >= 25) %>%
  ungroup() %>% 
  select(title, userId, rating) %>%
  spread(userId, rating)

row_names <- str_remove(x$title, ": Episode") %>% str_trunc(20)
x <- x[,-1] %>% as.matrix()
x <- sweep(x, 2, colMeans(x, na.rm = TRUE))
x <- sweep(x, 1, rowMeans(x, na.rm = TRUE))
rownames(x) <- row_names

# We want to use these data to find out if there are clusters of movies based on the ratings from 139 movie raters. 
# A first step is to find the distance between each pair of movies using the dist function:
  
d <- dist(x)

###  --------------------------------
### Hierarchical clustering (hclust + cutree)
###

# The hclust function implements this algorithm and it takes a distance as input.

h <- hclust(d)

#We can see the resulting groups using a dendrogram.
plot(h, cex = 0.65, main = "", xlab = "")

# To generate actual groups we can do one of two things: 
# arg h: 1) decide on a minimum distance needed for observations to be in the same group or 
# arg k: 2) decide on the number of groups you want and then find the minimum distance that achieves this. 
# The function cutree can be applied to the output of hclust to perform either of these two operations and generate groups.
groups <- cutree(h, k = 10)

names(groups)[groups==4]
str(groups)
names(groups)[groups==9]

# We can change the size of the group by either making k larger or h smaller. 
cutree(h, h=6) # => 50 groupes de 1
cutree(h, h=16) # => 5 groupes

# We can also explore the data to see if there are clusters of movie raters.
h_2 <- dist(t(x)) %>% hclust()
plot(h_2)

###  --------------------------------
### K-MEANS (kmeans)
###

# The kmeans function included in R-base does not handle NAs. For illustrative purposes we will fill out the NAs with 0s. 
# In general, the choice of how to fill in missing data, or if one should do it at all, should be made with care.

x_0 <- x
x_0[is.na(x_0)] <- 0
k <- kmeans(x_0, centers = 10)
# The cluster assignments are in the cluster component:
groups <- k$cluster

# Note that because the first center is chosen at random, the final clusters are random. 
# We impose some stability by repeating the entire function several times and averaging the results. 
# The number of random starting values to use can be assigned through the nstart argument.
k <- kmeans(x_0, centers = 10, nstart = 25)
groups <- k$cluster
sapply(1:10, function(x) {names(groups)[groups==x]})


###  --------------------------------
### Heatmap (heatmap)
###
#  demonstrate this with the tissue_gene_expression dataset. We will scale the rows of the gene expression matrix.

data("tissue_gene_expression")
x <- sweep(tissue_gene_expression$x, 2, colMeans(tissue_gene_expression$x))
h_1 <- hclust(dist(x))
h_2 <- hclust(dist(t(x)))
# Now we can use the results of this clustering to order the rows and columns.
image(x[h_1$order, h_2$order])

# But there is heatmap function that does it for us:
heatmap(x, col = RColorBrewer::brewer.pal(11, "Spectral"))
dim(x)

###  --------------------------------
### Filtering features
###
# One simple approach to try to remove features with no information is to only include those with high variance
library(matrixStats)
sds <- colSds(x, na.rm = TRUE)
o <- order(sds, decreasing = TRUE)[1:25]
heatmap(x[,o], col = RColorBrewer::brewer.pal(11, "Spectral"))

###  --------------------------------
### Comprehension Check: Clustering
###

# Q1 Load the tissue_gene_expression dataset. Remove the row means and compute the distance between each observation. Store the result in d.
data("tissue_gene_expression")
x <- sweep(tissue_gene_expression$x, 1, rowMeans(tissue_gene_expression$x))
d <- dist(x)
# or...
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))


# Q2 Make a hierarchical clustering plot and add the tissue types as labels.
# You will observe multiple branches. Which tissue type is in the branch farthest to the left?
h <- hclust(d)
plot(h, cex = 0.65)


# Q3 Run a k-means clustering on the data with  𝐾=7 .
# Make a table comparing the identified clusters to the actual tissue types. 
# Run the algorithm several times to see how the answer changes.  What do you observe for the clustering of the liver tissue?
k <- kmeans(x,centers =7)
# k$size
# groups <- k$cluster
# sapply(1:7, function(x) {names(groups)[groups==x]})
# str(d)
# attr(d,which = "Size")
# dim(x)
data.frame(actual_tissue = rownames(x), cluster = k$cluster) %>% 
  separate(actual_tissue, c("tissue","index"),"_") %>% 
  group_by(tissue,cluster) %>%
  summarize(population=n())

# => solution 
cl <- kmeans(tissue_gene_expression$x, centers = 7)
table(cl$cluster, tissue_gene_expression$y)

length (cl$cluster)
length(tissue_gene_expression$y)

# Liver is split into two clusters (one large and one small) about 60% of the time. 
# The other 40% of the time it is either in a single cluster or in three clusters at roughly equal frequency.

# Q4 : Heatmap;  Select the 50 most variable genes. Make sure the observations show up in the columns, that the predictor are centered, 
# and add a color bar to show the different tissue types. Hint: use the ColSideColors argument to assign colors. 
# Also, use col = RColorBrewer::brewer.pal(11, "RdBu") for a better use of colors.

library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)

#  scale	:character indicating if the values should be centered and scaled in either the row direction or the column direction, or none. The default is "row" if symm false, and "none" otherwise.

###  --------------------------------
###  --------------------------------
###  --------------------------------
###  --------------------------------
### Breast Cancer Project Part 1
### 
###  --------------------------------
###  --------------------------------
###  --------------------------------

# The brca dataset from the dslabs package contains information about breast cancer diagnosis biopsy samples for tumors 
# that were determined to be either benign (not cancer) and malignant (cancer). 
#
# The brca object is a list consisting of:
#   
# brca$y: a vector of sample classifications ("B" = benign or "M" = malignant)
# brca$x: a matrix of numeric features describing properties of the shape and size of cell nuclei extracted from biopsy microscope images
#
# For these exercises, load the data by setting your options and loading the libraries and data as shown in the code here:
  
options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)


# Question 1: Dimensions and properties

# How many samples are in the dataset? 569
str(brca)
class(brca)
str(brca$x)
class(brca$x)
str(brca$y)
class(brca$y)
dim(brca$x)
length(brca$y)
brca$x[1:10,1:10]
head(brca$y)

# How many predictors are in the matrix? 30
# What proportion of the samples are malignant? 0.373
mean(brca$y == "M")

# Which column number has the highest mean? 24 area_worst
which.max(colMeans(brca$x))

# Which column number has the lowest standard deviation? 20 fractal_dim_se
xsd <- colSds(brca$x)
xsd[which.min(xsd)]
colnames(brca$x)[which.min(xsd)]


# Question 2: Scaling the matrix

# Use sweep() two times to scale each column: subtract the column mean, then divide by the column standard deviation.
x <- sweep(brca$x, 2, colMeans(brca$x)) 
colMeans(x)
x <- sweep(x, 2, colSds(x), FUN="/")
x_scaled <- x

# After scaling, what is the standard deviation of the first column? 1
colSds(x)
sd(x[,1])
# After scaling, what is the median value of the first column? -0.215
median(x[,1])

# Question 3: Distance
# Calculate the distance between all samples using the scaled matrix.
d <- dist(x)
# What is the average distance between the first sample, which is benign, and other benign samples? 4.4
mean(as.matrix(d)[1,brca$y=="B"])
# What is the average distance between the first sample and malignant samples? 7.12
mean(as.matrix(d)[1,brca$y=="M"])

# Question 4: Heatmap of features
# Make a heatmap of the relationship between features using the scaled matrix.

# il faut transposer pour considérer les 30 features et non pas les 569 samples
d_features <- dist(t(x))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

# Question 5: Hierarchical clustering
# Perform hierarchical clustering on the 30 features. Cut the tree into 5 groups.
h_features <-hclust(d_features)
groups_features <- cutree(h_features, k = 5)
sapply(1:5,function(kk){names(groups_features)[groups_features==kk]})
plot(h_features)
# All but one of the answer options are in the same group.
# Which is in a different group? concavity_mean
  
# Question 6: PCA: proportion of variance
# Perform a principal component analysis of the scaled matrix.
# What proportion of variance is explained by the first principal component?  0.443
# How many principal components are required to explain at least 90% of the variance? 7
pca <- prcomp(x =x)
summary(pca)
dim(pca$x)
dim(pca$rotation)
pcasds <- colSds(pca$x)
sqrt(pcasds[1])/sqrt(sum(pcasds))

# Question 7: PCA: plotting PCs
# Plot the first two principal components with color representing tumor type (benign/malignant).
# From the plot, you can see that the benign tumors tend to have smaller values of PC1 and that the malignant tumors have larger values of PC1. 
# PC2 values have a similar spread for both benign and malignant tumors.
data.frame(pca$x[,1:2], type=brca$y) %>% 
  ggplot(aes(PC1,PC2, fill = type))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)

# Question 8: PCA: PC boxplot.
# Make a boxplot of the first 10 PCs grouped by tumor type.
# Which PCs are significantly different enough by tumor type that there is no overlap in the interquartile ranges (IQRs) for benign and malignant samples?
data.frame(pca$x[,1:10], type=brca$y) %>% 
  ggplot(aes(PC1,  type))+geom_boxplot()
for(i in 1:10){
  boxplot(pca$x[,i] ~ brca$y, main = paste("PC", i))
}
data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()

# Breast Cancer Project Part 3
#
# Set the seed to 1, then create a data partition splitting brca$y and the scaled version of the brca$x matrix into a 20% test set and 80% train 
# using the following code:
  
set.seed(1, sample.kind = "Rounding")  
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]
# You will be using these training and test sets throughout the exercises in Parts 3 and 4. 
# Save your models as you go, because at the end, you'll be asked to make an ensemble prediction and to compare the accuracy of the various models!

# Question 9: Training and test sets
# Check that the training and test sets have similar proportions of benign and malignant tumors.
# What proportion of the training set is benign? 0.628
mean(train_y=="B")

# What proportion of the test set is benign? 0.626
mean(test_y=="B")

# Question 10a: K-means Clustering
# The predict_kmeans() function defined here takes two arguments - a matrix of observations x and a k-means object k - 
# and assigns each row of x to a cluster from k.

predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}
# Set the seed to 3. Perform k-means clustering on the training set with 2 centers and assign the output to k. 
# Then use the predict_kmeans() function to make predictions on the test set.
# What is the overall accuracy? 0.922
k <- kmeans(train_x,centers = 2)
k$centers
predicted <- predict_kmeans(test_x,k)
mean(predicted == as.numeric(test_y) )

# equivalent
kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")
mean(kmeans_preds == test_y)

# Question 10b: K-means Clustering
# What proportion of benign tumors are correctly identified? 0.986
# What proportion of malignant tumors are correctly identified? 0.814
sum(kmeans_preds == test_y & kmeans_preds =="B") / sum(test_y =="B")
sum(kmeans_preds == test_y & kmeans_preds =="M") / sum(test_y =="M")

# equivalent
sensitivity(factor(kmeans_preds), test_y, positive = "B")
sensitivity(factor(kmeans_preds), test_y, positive = "M")

# Question 11: Logistic regression model
# Fit a logistic regression model on the training set using all predictors. Ignore warnings about the algorithm not converging. 
glm_model <- glm(y~. , data = data.frame(train_x, y=train_y ) ,  family = "binomial")
p_hat_glm <- predict(glm_model, newdata = data.frame(test_x, y= test_y), type="response")

# Make predictions on the test set. What is the accuracy of the logistic regression model on the test set? => 0.957

y_hat_glm <- ifelse(p_hat_glm > 0.5, "M", "B") %>% factor
mean(y_hat_glm == test_y)

# ou simplement ... 
train_glm <- train(train_x, train_y, method = "glm")
glm_preds <- predict(train_glm, test_x)
mean(glm_preds == test_y)

# Question 12: LDA and QDA models
# Train an LDA model and a QDA model on the training set. Make predictions on the test set using each model.
# 

fit_lda <- train(train_x, train_y, method = "lda")
lda_preds <- predict(fit_lda, test_x)  
mean(lda_preds == test_y)

fit_qda <- train(train_x, train_y, method = "qda")
qda_preds <- predict(fit_qda, test_x)  
mean(qda_preds == test_y)

# Question 13: Loess model
# Set the seed to 5, then fit a loess model on the training set with the caret package. You will need to install the gam package if you have not yet done so. 
# Use the default tuning grid. This may take several minutes; ignore warnings. Generate predictions on the test set. What is the accuracy of the loess model on the test set?
set.seed(5, sample.kind = "Rounding")
fit_loess <- train(train_x, train_y, method="gamLoess")  
loess_predict <- predict(fit_loess, test_x)
mean(loess_predict == test_y) # 0.983

# Question 14: K-nearest neighbors model
#
# Set the seed to 7, then train a k-nearest neighbors model on the training set using the caret package. 
# Try odd values of 𝑘 from 3 to 21. Use the final model to generate predictions on the test set.
# 
set.seed(7, sample.kind = "Rounding")
fit_knn <- train(train_x, train_y, method ="knn", tuneGrid = data.frame(k=seq(3,21,1)) ) # final value k=21
fit_knn$bestTune
knn_predict <- predict(fit_knn, test_x)
mean(knn_predict== test_y) # 0.948

# Question 15a: Random forest model
#
# Set the seed to 9, then train a random forest model on the training set using the caret package. 
# Test mtry values of 3, 5, 7 and 9. Use the argument importance = TRUE so that feature importance can be extracted. 
# Generate predictions on the test set.
# 
# What value of mtry gives the highest accuracy? 3
# What is the accuracy of the random forest model on the test set? 0.983 avec importance = TRUE, 0.974 without
# What is the most important variable in the random forest model? area_worst with importance = TRUE, radius_worst else
set.seed(9, sample.kind = "Rounding")
train_rf <- train(train_x, train_y, method ="rf", tuneGrid = data.frame(mtry=seq(3,9,2)) )#, importance = TRUE) 
train_rf$bestTune
rf_pred <- predict(train_rf$finalModel, test_x)
mean(rf_pred== test_y) 
varImp(train_rf$finalModel)
varImp(train_rf)
plot(train_rf)

# Question 15b: Random forest model
#
# Consider the top 10 most important variables in the random forest model.
# Which set of features is most important for determining tumor type? => worst
varImp(train_rf)$importance %>% mutate(predictor=rownames(.), val=B+M)%>% arrange(desc(val)) %>% top_n(wt=val,n = 10) 

# Question 16a: Creating an ensemble
#
# Create an ensemble using the predictions from the 7 models created in the previous exercises: k-means, logistic regression, LDA, QDA, loess, k-nearest neighbors, and random forest. 
# Use the ensemble to generate a majority prediction of the tumor type (if most models suggest the tumor is malignant, predict malignant).
# What is the accuracy of the ensemble prediction?
#   
ensemble <- cbind(kmeans_preds, glm_preds, lda_preds, qda_preds, loess_predict, knn_predict, rf_pred )
colnames(ensemble)<- c("kmeans","logistic","lda","qda","loess","knn","rf")

ensemble <- data.frame(ensemble) %>% mutate(kmeans = ifelse(kmeans=="B","1","2") ) %>% mutate_all(as.numeric)
ensemble %>% gather(model,prediction, ) %>%   group_by(model,prediction) %>% table

majo_pred <- ensemble %>% mutate(sample= row_number()) %>% gather(model,prediction, -sample) %>%   
  group_by(sample) %>% 
  summarize(B=sum(prediction==1), M=sum(prediction==2)) %>%
  mutate(majority=ifelse(M>B,"M","B")) %>% 
  pull(majority)

mean(majo_pred == test_y)  # 0.983

# solution (pas compatible avec l historique)
# ensemble <- cbind(glm = glm_preds == "B", lda = lda_preds == "B", qda = qda_preds == "B", loess = loess_preds == "B", rf = rf_preds == "B", knn = knn_preds == "B", kmeans = kmeans_preds == "B")
# 
# ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
# mean(ensemble_preds == test_y)

# Question 16b: Creating an ensemble
# 
# Make a table of the accuracies of the 7 models and the accuracy of the ensemble model.
# Which of these models has the highest accuracy?  LDA
ensemble %>% mutate(sample= row_number()) %>% 
  gather(model,prediction, -sample) %>% 
  mutate(prediction=ifelse(prediction==1,"B","M")) %>% # spread(model, prediction)
  group_by(model) %>% 
  summarize(precision = mean(prediction == test_y))
  
  


###  --------------------------------
### 
###