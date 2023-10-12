
##КРЕСТИКИ-НОЛИКИ
#install.packages("e1071")
library(e1071)
# импортируем данные в R
# установить параметр stringsAsFactors = TRUE, 
# так как все данные - категориальные
A_raw <- read.table("C:/Users/Unicorn/Desktop/Машинное Обучение/Лабы/Tic_tac_toe.txt", sep = ",", stringsAsFactors = TRUE)
# число строк в базе
n <- dim(A_raw)[1]
# Создан фрейм, который можно просмотреть, используя str(A_raw).
# Имеется 9 столбцов признаков V1-V9 и V10 (класс) и 
# все имеют один и тот же тип Factor.
# 2 #############################################################
# Создание обучающей и тестирующей выборки
# Скажем, имеем n примеров в исходной выборке, 
# используем 80% для обучения и оставшиеся - для тестирования. 
# Устанавливаем базу генерации случайных чисел и рандомизируем выборку
set.seed(12345)
A_rand <- A_raw[ order(runif(n)), ]
# разделим данные на обучающие и тестирующие
nt <- as.integer(n*0.8)
A_train <- A_rand[1:nt, ]
A_test <- A_rand[(nt+1):n, ]
# Можно убедиться, какой имеется процент каждого 
# класса V2 в обучающей и тестирующей выборке
prop.table(table(A_train$V10))
prop.table(table(A_test$V10))
# 3 ############################################################
# Используем Наивный Байесовский классификатор из пакета e1071
#	A_classifier <- naiveBayes(A_train[,-10], A_train$V10)
# Другой вариант классификатора
A_classifier <- naiveBayes(V10 ~ ., data = A_train)
# 4 ############################################################
# Теперь оценим полученную модель:
A_predicted <- predict(A_classifier, A_test)
# Используем table для сравнения прогнозируемых значений с тем, что есть
tab1 = table(A_predicted, A_test$V10)
accuracy = (tab1[1,1] + tab1[2,2]) / (tab1[1,1] + tab1[2,2] + tab1[1,2] + tab1[2,1])
tab1
accuracy

##СПАМ
library(kernlab)
library(e1071)
data(spam)
## Посмотрим, какие признаки используются и их значения
##spam[0:1,]
## Случайным образом выбираем 10% сообщений для тестирования,
## точнее индексы 10% тестов
idx <- sample(1:dim(spam)[1], 4601*0.9);
spamtrain <- spam[-idx, ];
spamtest <- spam[idx, ];
## Обучаем классификатор
model <- naiveBayes(type ~ ., data = spamtrain);
##predict(model, spamtest);
tab2 = table(predict(model, spamtest), spamtest$type)
##predict(model, spamtest, type = "raw")
accuracy2 = (tab2[1,1] + tab2[2,2]) / (tab2[1,1] + tab2[2,2] + tab2[1,2] + tab2[2,1])
tab2
accuracy2

##ГЕНЕРАЦИЯ ТОЧЕК
library(e1071)
n1 <- rnorm(50, 10, 4)
n2 <- rnorm(50, 20, 3)
n3 <- rnorm(50, 14, 4)
n4 <- rnorm(50, 18, 3)

X1 <- c(n1,n2)
X2 <- c(n3,n4)
C <- rep(c("-1" , "1") , each = 50)
T_rand <- data.frame(X1, X2, C, stringsAsFactors = TRUE)
#T
plot(X1, X2, col = rep(1:2, each = 50), pch = 19)

# разделим данные на обучающие и тестирующие
n = 100
#70% обучающая
nt <- as.integer(n*0.7)
T_train <- T_rand[1:nt, ]
T_test <- T_rand[(nt+1):n, ]

##обучение
T_classifier <- naiveBayes(C ~ ., data = T_train)
# Оценка полученной модели:
T_predicted <- predict(T_classifier, T_test)
# Используем table для сравнения прогнозируемых значений с тем, что есть
tab3 = table(T_predicted, T_test$C)
accuracy3 = (tab3[1,1] + tab3[2,2]) / (tab3[1,1] + tab3[2,2] + tab3[1,2] + tab3[2,1])
tab3
accuracy3

##ТИТАНИК
library(e1071)
#Загрузка обучающей выборки
Titanic_train <- read.csv("Titanic_train.csv", header = TRUE, sep = ",", dec = ".",
                          stringsAsFactors = FALSE)
#Загрузка тестовой выборки
Titanic_test <- read.csv("Titanic_test.csv", header = TRUE, sep = ",", dec = ".",
                         stringsAsFactors = FALSE)

#Обучение
Titanic_classifier <- naiveBayes(Survived ~ ., data = Titanic_train)
# Оценка полученной модели:
Titanic_predicted <- predict(Titanic_classifier, Titanic_test)

# Сравним полученные результаты с тестовыми
Gen_sub = read.csv("gender_submission.csv", header = TRUE, sep = ",", dec = ".",
                   stringsAsFactors = FALSE)
tab4 = table(Titanic_predicted, Gen_sub$Survived)
accuracy4 = (tab4[1,1] + tab4[2,2]) / (tab4[1,1] + tab4[2,2] + tab4[1,2] + tab4[2,1])
# Выводим таблицу с числом классифицированных элементов и точность
tab4
accuracy4

