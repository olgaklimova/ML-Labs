##КРЕСТИКИ-НОЛИКИ
library(kknn)
A_raw <- read.table("Tic_tac_toe.txt", sep = ",", stringsAsFactors = TRUE)
#Число строк в базе
n <- dim(A_raw)[1]
# Устанавливаем базу генерации случайных чисел и рандомизируем выборку
set.seed(12345)
A_rand <- A_raw[ order(runif(n)), ]
#Разделим данные на обучающие и тестирующие (90% обучающая)
nt <- as.integer(n*0.9)
A_train <- A_rand[1:nt, ]
A_test <- A_rand[(nt+1):n, ]
#Процент классов из V10 в обучающей и тестовой выборке
prop.table(table(A_train$V10))
prop.table(table(A_test$V10))
#Строим классификатор (k = 7, distance = 2, kernel = "optimal")
A_classifier <- kknn(V10~., A_train, A_test, k = 7, distance = 2, kernel = "optimal")
A_predicted <- fitted(A_classifier)
tab1 = table(A_predicted, A_test$V10)
#вычисляем точность классификации
accuracy = (tab1[1,1] + tab1[2,2]) / (tab1[1,1] + tab1[2,2] + tab1[1,2] + tab1[2,1])
tab1
accuracy

##СПАМ
library(kernlab)
library(kknn)
data(spam)
#Случайным образом выбираем 10% сообщений для тестирования
idx <- sample(1:dim(spam)[1], 4601*0.1);
spamtrain <- spam[-idx, ];
spamtest <- spam[idx, ];
#Обучаем классификатор
A_classifier <- kknn(type ~ ., spamtrain, spamtest, k = 7, distance = 2, kernel = "optimal")
predict <- fitted(A_classifier)
tab2 = table(predict, spamtest$type)
#вычисляем точность классификации
accuracy2 = (tab2[1,1] + tab2[2,2]) / (tab2[1,1] + tab2[2,2] + tab2[1,2] + tab2[2,1])
tab2
accuracy2

##GLASS
library(kknn)
library(rcompanion)
data(glass)
glass <- glass[, -1]
#Число строк в базе
n <- dim(glass)[1]
# Устанавливаем базу генерации случайных чисел и рандомизируем выборку
set.seed(12345)
Glass_rand <- glass[ order(runif(n)), ]
#Разделим данные на обучающие и тестирующие (80% обучающая)
nt <- as.integer(n*0.8)
Glass_train <- Glass_rand[1:nt, ]
Glass_test <- Glass_rand[(nt+1):n, ]

Depk = matrix(c(1:10, 1:10),nrow = 10, ncol = 2, byrow = TRUE)
#Строим классификатор для выявления зависимости ошибки от k (distance = 2, kernel = "optimal")
for (k in 1:10){
Glass_classifier <- kknn(Type ~ ., Glass_train, Glass_test, k = k, distance = 2, kernel = "optimal")
fit <- fitted(Glass_classifier)
tab3 = table(fit, Glass_test$Type)
sum1 = 0
sum2 = 0
#вычисляем суммы числителя и знаменателя для нахождения точности классификации
for(i in 1:6){
  sum1 = sum1 + tab3[i,i]
  for(j in 1:6){
    sum2 = sum2 + tab3[i,j]
  }
}
#вероятность ошибочной классификации
mistake3 = 1 - sum1/sum2
Depk[k, 1] = k
Depk[k, 2] = mistake3
}
plot(Depk[,1],Depk[,2], col="red", ylab="Ошибка классификации", xlab="k", pch = 19, type="o")


Depd = matrix(c(1:10, 1:10),nrow = 10, ncol = 2, byrow = TRUE)
#Строим классификатор для выявления зависимости ошибки от distance (k = 7, kernel = "optimal")
for (d in 1:10){
  Glass_classifier <- kknn(Type ~ ., Glass_train, Glass_test, k = 7, distance = d, kernel = "optimal")
  fit <- fitted(Glass_classifier)
  tab3 = table(fit, Glass_test$Type)
  sum1 = 0
  sum2 = 0
  #вычисляем суммы числителя и знаменателя для нахождения точности классификации
  for(i in 1:6){
    sum1 = sum1 + tab3[i,i]
    for(j in 1:6){
      sum2 = sum2 + tab3[i,j]
    }
  }
  #точность классификации
  accuracy3 = sum1/sum2
  Depd[d, 1] = d
  Depd[d, 2] = accuracy3
}
plot(Depd[,1],Depd[,2], col="blue", ylab="Точность классификации", xlab="distance", pch = 19, type="o")

#определеям тип стекла
example <- data.frame(RI=1.516, Na=11.7, Mg=1.01, Al=1.19, Si=72.59, K=0.43,
                      Ca=11.44, Ba=0.02, Fe=0.1)
Glass_classifier <- kknn(Type~., Glass_train, example,  k = 7, distance = 2, kernel = "optimal")
#выведем матрицу вероятносте прогнозируемых классов
Glass_classifier$prob

#определим наименнее значимый признак для классификации
library(dplyr)
glass
#удаляем признак RI
Newglass <- glass %>% select(-Fe) 
n <- dim(Newglass)[1]
NewGlass_rand <- Newglass[ order(runif(n)), ]
#Разделим данные на обучающие и тестирующие (80% обучающая)
nt <- as.integer(n*0.8)
NewGlass_train <- NewGlass_rand[1:nt, ]
NewGlass_test <- NewGlass_rand[(nt+1):n, ]
NewGlass_classifier <- kknn(Type ~ ., NewGlass_train, NewGlass_test, k = 7, distance = 2, kernel = "optimal")
fit <- fitted(NewGlass_classifier)
tab3 = table(fit, NewGlass_test$Type)
sum1 = 0
sum2 = 0
#вычисляем суммы числителя и знаменателя для нахождения точности классификации
for(i in 1:6){
  sum1 = sum1 + tab3[i,i]
  for(j in 1:6){
    sum2 = sum2 + tab3[i,j]
  }
}
#точность классификации
accuracy3 = sum1/sum2
accuracy3

##SVMDADA4
svmdata4_train <- read.table("svmdata4.txt", sep = "\t", stringsAsFactors = TRUE)
svmdata4_test <- read.table("svmdata4test.txt", sep = "\t", stringsAsFactors = TRUE)
fit.train <- train.kknn(Colors ~ ., svmdata4_train, kmax = 15, distance = 2,
                       kernel = "optimal")
plot(svmdata4_train$X1, svmdata4_train$X2, pch=21, xlab = "X1", ylab="X2",
     bg=c("red","blue") [unclass(svmdata4_train$Colors)], main="My train data")

#Лучшее значение k = 8
fit.train

##Титаник
library(kknn)
library(dplyr)
#Загрузка обучающей выборки
Titanic_train <- read.csv("Titanic_train.csv", header = TRUE, sep = ",", dec = ".",
                          stringsAsFactors = FALSE)
#Загрузка тестовой выборки
Titanic_test <- read.csv("Titanic_test.csv", header = TRUE, sep = ",", dec = ".",
                         stringsAsFactors = FALSE)
#Удаление столбцов, ненесущих информации для обучения
Titanic_train <- Titanic_train %>% select(-PassengerId)
Titanic_train <- Titanic_train %>% select(-Name)
Titanic_train <- Titanic_train %>% select(-Ticket)
Titanic_train <- Titanic_train %>% select(-Cabin)
Titanic_train <- Titanic_train %>% select(-Embarked)
Titanic_train

#Нахождение оптимального k
fit.train <- train.kknn(Survived ~ ., Titanic_train, kmax = 40, distance = 2,
                        kernel = "optimal")
fit.train

#Заполнение пропусков
#Нахождение средних значений
Age_mean <- summary(Titanic_test$Age)[4]
Fare_mean <- summary(Titanic_test$Fare)[4]
for(i in 1:dim(Titanic_test)[1]){
  if(is.na(Titanic_test$Age[i])){
    Titanic_test$Age[i]=Age_mean
  }
  if(is.na(Titanic_test$Fare[i])){
    Titanic_test$Fare[i]=Fare_mean
  }
}

Titanic_classifier <- kknn(Survived ~ ., Titanic_train, Titanic_test, k = 37, distance = 2, kernel = "optimal")
fit <- round(fitted(Titanic_classifier))
Gen_sub = read.csv("gender_submission.csv", header = TRUE, sep = ",", dec = ".",
                   stringsAsFactors = FALSE)

tab4 = table(fit, Gen_sub$Survived)
tab4 
accuracy4 = (tab4[1,1] + tab4[2,2]) / (tab4[1,1] + tab4[2,2] + tab4[1,2] + tab4[2,1])
accuracy4


#посчитать точность по встроенной функции: accuracy(fit, Glass_test$Type)