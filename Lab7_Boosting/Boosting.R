#ЗАДАНИЕ 1
library(rpart)
library(mlbench)
library(adabag)
data(Vehicle)
n <- dim(Vehicle)[1]
Vehicle_rand <- Vehicle[ order(runif(n)),]
#разделим выборку на 30% теста и 70% для обучения
nt <- as.integer(n*0.7)
Vehicle_train <- Vehicle_rand[1:nt, ]
Vehicle_test <- Vehicle_rand[(nt+1):n, ]

#Зададим максимальное число итераций в алг. adaboost.M1, равным от 1 до 301 с шагом 10: 
mfinal = c()
mfinal <- append(mfinal, 1)
for(i in 1:30){
  mfinal <- append(mfinal, 1+10*i)
}
#Зададим максимальную глубину каждого дерева: 
maxdepth <- 5
err = c()

for(i in 1:length(mfinal)){
#Построим ансамбль деревьев решений  
Vehicle.adaboost <- boosting(Class ~.,data=Vehicle_train, mfinal=mfinal[i], maxdepth=maxdepth) 
#Используя построенную модель, предскажем ответы на тестовой выборке 
Vehicle.adaboost.pred <- predict.boosting(Vehicle.adaboost, Vehicle_test) 
#Вычислим ошибку предсказания: 
err <- append(err, Vehicle.adaboost.pred$error)
}
plot(mfinal, err, xlab="Число деревьев", ylab="Ошибка")
err

#ЗАДАНИЕ 2
library(rpart)
library(mlbench)
library(adabag)
data(Glass)
n <- dim(Glass)[1]
Glass_rand <- Glass[ order(runif(n)),]
#разделим выборку на 30% теста и 70% для обучения
nt <- as.integer(n*0.7)
Glass_train <- Glass_rand[1:nt, ]
Glass_test <- Glass_rand[(nt+1):n, ]
#Зададим максимальную глубину каждого дерева: 
maxdepth <- 5
#Зададим максимальное число итераций: 
mfinal = c()
mfinal <- append(mfinal, 1)
for(i in 1:20){
  mfinal <- append(mfinal, 1+10*i)
}

err = c()
for(i in 1:length(mfinal)){
  Glass.bagging <- bagging(Type ~., data=Glass_train, mfinal=mfinal[i],
                           maxdepth=maxdepth)
  Glass.bagging.pred <- predict.bagging(Glass.bagging, Glass_test)
  err <- append(err, Glass.bagging.pred$error)
}
plot(mfinal, err, xlab="Число деревьев", ylab="Ошибка", pch = 19)
err

#ЗАДАНИЕ 3
library(kknn)
library(rpart)
library(mlbench)
library(adabag)
#Vehicle
data(Vehicle)
n <- dim(Vehicle)[1]
Vehicle_rand <- Vehicle[ order(runif(n)),]
#разбиваем на обучающую и тестовую выборку
nt <- as.integer(n*0.7)
Vehicle_train <- Vehicle_rand[1:nt, ]
Vehicle_test <- Vehicle_rand[(nt+1):n, ]

#knn
Vehicle_classifier <- kknn(Class~., Vehicle_train, Vehicle_test, distance = 2, k = 5)
fit <- fitted(Vehicle_classifier)
tb <- table(fit, Vehicle_test$Class)
error.kknn <- 1-(sum(diag(tb))/sum(tb))
#использование единичного дерева
maxdepth <- 5
Vehicle.rpart <- rpart(Class ~., data=Vehicle_train, maxdepth=maxdepth)
Vehicle.rpart.pred <- predict(Vehicle.rpart, Vehicle_test, type="class")
tb <- table(Vehicle.rpart.pred, Vehicle_test$Class)
error.rpart <- 1-(sum(diag(tb))/sum(tb))

error.kknn
error.rpart

#GLASS
data(Glass)
n <- dim(Glass)[1]
Glass_rand <- Glass[ order(runif(n)),]
#делим на обучающую и тестовую
nt <- as.integer(n*0.7)
Glass_train <- Glass_rand[1:nt, ]
Glass_test <- Glass_rand[(nt+1):n, ]

#knn
Glass_classifier <- kknn(Type~., Glass_train, Glass_test, distance = 2, k = 7)
fit <- fitted(Glass_classifier)
tb <- table(fit, Glass_test$Type)
error.kknn <- 1-(sum(diag(tb))/sum(tb))
#использование единичного дерева
Glass.rpart <- rpart(Type ~., data=Glass_train, maxdepth=maxdepth)
Glass.rpart.pred <- predict(Glass.rpart, Glass_test, type="class")
tb <- table(Glass.rpart.pred, Glass_test$Type)
error.rpart <- 1-(sum(diag(tb))/sum(tb))

error.kknn
error.rpart

#ЗАДАНИЕ 3 (Бустинг с knn)
library(dplyr)

knn_w <- function(target, train, k, w) 
  return(list(target = target, train = train, 
              levels = levels(train[, target]), k = k, w = w))

knn_w_predicted <- function(clfier, testdata) {
  n <- nrow(testdata)
  pred <- rep(NA_character_, n)
  trainlabels <- clfier$train[, clfier$target]
  
  train <- clfier$train[, !(names(clfier$train) %in% clfier$target)]
  test <- testdata[, !(names(testdata) %in% clfier$target)]
  
  for (i in 1:n) {
    n_number <- order(apply(train, 1, function(x)
      sum((test[i,] - x)^2)))[1:clfier$k]
    
    myfreq <- data.frame(names = clfier$levels,
                         freq = rep(0, length(clfier$levels)))
    for (t in n_number) {
      myfreq[myfreq$names == trainlabels[t], ][2] <- myfreq[myfreq$names == trainlabels[t], ][2] + clfier$w[t]
    }
    most_frequent <- clfier$levels[myfreq$freq == max(myfreq$freq)]
    pred[i] <- sample(most_frequent, 1)
  }
  
  factor(pred, levels = levels(trainlabels))
}


knn_boosting <- function(target, data, k, mfinal) {
  #число строк в массиве данных
  n <- nrow(data)
  #инициализация одинаковых весов
  w <- rep(1/n, each = n)
  
  classifiers <- list()
  alphas <- vector()
  
  for (t in 1:mfinal) {
    #обучение слабого классификатора
    clfier <- knn_w(target, train = data, k = k, w)
    knn_predicted <- knn_w_predicted(clfier, data)
    #ошибки слабого классификатора
    error <- vector()
    
    for (i in 1:n) {
      if (data[[target]][i] != knn_predicted[i]) 
        error <- append(error, w[i])
    }
    
    if (sum(error) >= 0.5) {
      break()
    }
    
    classifiers[[t]] <- clfier
    #вес слабого классификатора
    alphas[[t]] <- log((1 - sum(error)) / sum(error)) / 2
    
    #модификация весов для следующей итерации (большие для неправельно определенных)
    for (i in 1:n) {
      if (knn_predicted[i] != data[[target]][i]) 
      {
        w[i] <- w[i]*exp(alphas[[t]])} 
      else{
        w[i] <- w[i]*exp(-alphas[[t]])}
    }
  }
  
  result <- list()
  result$classifiers <- classifiers
  result$alphas <- alphas
  result$levels <- levels(data[, target])
  return(result)
}

boosting_pred <- function(clfier, testdata) {
  n <- nrow(testdata)
  pred = rep(NA_character_, n)
  
  for (i in 1:n) {
    myfreq <- data.frame(names = clfier$levels,
                         freq = rep(0, length(clfier$levels)))
    
    for (j in 1:length(clfier$classifiers)) {
      prediction <- knn_w_predicted(clfier$classifiers[[j]], testdata[i, ])
      myfreq[myfreq$names == prediction, ][2] <- myfreq[myfreq$names == prediction, ][2] + clfier$alphas[j]
    }
    
    most_frequent = clfier$levels[myfreq$freq == max(myfreq$freq)]
    pred[i] <- sample(most_frequent, 1)
  }
  factor(pred, levels = clfier$levels)
}

#Анализ тестовых ошибок для Glass и Vehicle с использованием бустинга с knn для k = 5
Glass_boosting <- knn_boosting('Type', Glass_train, k = 7, mfinal = 4)
Glass_pred <- boosting_pred(Glass_boosting, Glass_test)
tab1 <- table(Glass_test$Type, Glass_pred)
#ошибка классификации для Glass
1 - sum(diag(tab1)) / sum(tab1)

Vehicle_train1 <- Vehicle_train[1:400,1:19]
Vehicle_train1
Vehicle_test1 <- Vehicle_test[1:150,1:19]
Vehicle_test1
Vehicle_boosting <- knn_boosting('Class', Vehicle_train1, k = 5, mfinal = 4)
Vehicle_pred <- boosting_pred(Vehicle_boosting, Vehicle_test1)
tab2 <- table(Vehicle_test1$Class, Vehicle_pred)
#ошибка классификации для Vehicle
1 - sum(diag(tab2)) / sum(tab2)

