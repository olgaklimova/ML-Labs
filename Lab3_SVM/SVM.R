##ЗАДАНИЕ 1
library(e1071) 
#окрашивание фона
area.pallete = function(n = 2) 
{ 
  cols = rainbow(n) 
  cols[1:2] = c("PaleGreen", "Pink") 
  return(cols) 
} 
#цвет символов
symbols.pallete = c("Green", "Red")
#загрузка обучающей и тестовой выборки
data1Train <- read.table("svmdata1.txt", sep = "\t", stringsAsFactors = TRUE)
data1Test <- read.table("svmdata1test.txt", sep = "\t", stringsAsFactors = TRUE)
#построение модели типа "C-classification" с параметром С = 1 и ядром "linear"
svmModel1 <- svm(Color ~., data=data1Train, type="C-classification", cost=1,
              kernel="linear")
#построение графика
plot(svmModel1, data1Train, grid=250,  symbolPalette = symbols.pallete, color.palette = area.pallete)
#определение полученных опорных векторов
svmModel1
#определение ошибки классификации на обучающей выборке
predictions1Train <- predict(svmModel1, data1Train)
table(data1Train$Color, predictions1Train)
#определение ошибки классификации на тестовой выборке
predictions1Test <- predict(svmModel1, data1Test)
table(data1Test$Color, predictions1Test)



##ЗАДАНИЕ 2
library(e1071) 
#загрузка обучающей и тестовой выборки
data2Train <- read.table("svmdata2.txt", sep = "\t", stringsAsFactors = TRUE)
data2Test <- read.table("svmdata2test.txt", sep = "\t", stringsAsFactors = TRUE)
#построение первой модели типа "C-classification" с параметром С = 183 и ядром "linear"
svmModel2 <- svm(Colors ~., data=data2Train, type="C-classification", cost=183,
                 kernel="linear")

#определение точности классификации на обучающей выборке
predictions2Train <- predict(svmModel2, data2Train)
tab1 = table(data2Train$Colors, predictions2Train)
tab1
accuracy1 = (tab1[1,1] + tab1[2,2]) / (tab1[1,1] + tab1[2,2] + tab1[1,2] + tab1[2,1])
accuracy1

#построение второй модели типа "C-classification" с параметром С = 71  и ядром "linear"
svmModel2 <- svm(Colors ~., data=data2Train, type="C-classification", cost=71,
                 kernel="linear")
#определение точности классификации на тестовой выборке
predictions2Test <- predict(svmModel2, data2Test)
tab2 = table(data2Test$Colors, predictions2Test)
accuracy2 = (tab2[1,1] + tab2[2,2]) / (tab2[1,1] + tab2[2,2] + tab2[1,2] + tab2[2,1])
accuracy2


##ЗАДАНИЕ 3
library(e1071) 
#считываем данные и разделяем на тестовую и обучающую выборки
data3_raw <- read.table("svmdata3.txt", sep = "\t", stringsAsFactors = TRUE)
n <- dim(data3_raw)[1]
data3_rand <- data3_raw[ order(runif(n)),]
#80% для обучения
nt <- as.integer(n*0.8)
data3Train <- data3_rand[1:nt, ]
data3Test <- data3_rand[(nt+1):n, ]


#построение модели типа "C-classification" с параметром С = 1 и ядром "polynomial"
svmModel3 <- svm(Colors ~., data=data3Train, type="C-classification", cost=1,
                 kernel="polynomial")
#определение точности классификации на тестовой выборке
predictions3Test <- predict(svmModel3, data3Test)
tab2 = table(data3Test$Colors, predictions3Test)
accuracy2 = (tab2[1,1] + tab2[2,2]) / (tab2[1,1] + tab2[2,2] + tab2[1,2] + tab2[2,1])
tab2
accuracy2

#построение модели типа "C-classification" с параметром С = 1 и ядром "radial"
svmModel3 <- svm(Colors ~., data=data3Train, type="C-classification", cost=1,
                 kernel="radial")
#определение точности классификации на тестовой выборке
predictions3Test <- predict(svmModel3, data3Test)
tab2 = table(data3Test$Colors, predictions3Test)
accuracy2 = (tab2[1,1] + tab2[2,2]) / (tab2[1,1] + tab2[2,2] + tab2[1,2] + tab2[2,1])
tab2
accuracy2

#построение модели типа "C-classification" с параметром С = 1 и ядром "sigmoid"
svmModel3 <- svm(Colors ~., data=data3Train, type="C-classification", cost=1,
                 kernel="sigmoid")
#определение точности классификации на тестовой выборке
predictions3Test <- predict(svmModel3, data3Test)
tab2 = table(data3Test$Colors, predictions3Test)
accuracy2 = (tab2[1,1] + tab2[2,2]) / (tab2[1,1] + tab2[2,2] + tab2[1,2] + tab2[2,1])
tab2
accuracy2

#определение ошибки при различных значениях degree
Depdeg = matrix(c(1:10, 1:10),nrow = 10, ncol = 2, byrow = TRUE)
for (i in 1:10){
#построение модели типа "C-classification" с параметром С = 1 и ядром "polynomial"
svmModel3 <- svm(Colors ~., data=data3Train, type="C-classification", cost=1,
                 kernel="polynomial", degree = i)
#определение точности классификации на тестовой выборке
predictions3Test <- predict(svmModel3, data3Test)
tab2 = table(data3Test$Colors, predictions3Test)
accuracy2 = (tab2[1,1] + tab2[2,2]) / (tab2[1,1] + tab2[2,2] + tab2[1,2] + tab2[2,1])
Depdeg[i, 1] = i
Depdeg[i, 2] = 1 - accuracy2
}
plot(Depdeg[,1],Depdeg[,2], col="blue", ylab="Ошибка классификации", xlab="degree", pch = 19, type="o")
Depdeg

##Задание 4
data4Train <- read.table("svmdata4.txt", sep = "\t", stringsAsFactors = TRUE)
data4Test <- read.table("svmdata4test.txt", sep = "\t", stringsAsFactors = TRUE)

#построение модели типа "C-classification" с параметром С = 1 и ядром "polynomial"
svmModel4 <- svm(Colors ~., data=data4Train, type="C-classification", cost=1,
                 kernel="polynomial")
#определение точности классификации на тестовой выборке
predictions4Test <- predict(svmModel4, data4Test)
tab2 = table(data4Test$Colors, predictions4Test)
accuracy2 = (tab2[1,1] + tab2[2,2]) / (tab2[1,1] + tab2[2,2] + tab2[1,2] + tab2[2,1])
tab2
1 - accuracy2


#построение модели типа "C-classification" с параметром С = 1 и ядром "radial"
svmModel4 <- svm(Colors ~., data=data4Train, type="C-classification", cost=1,
                 kernel="radial")
#определение точности классификации на тестовой выборке
predictions4Test <- predict(svmModel4, data4Test)
tab2 = table(data4Test$Colors, predictions4Test)
accuracy2 = (tab2[1,1] + tab2[2,2]) / (tab2[1,1] + tab2[2,2] + tab2[1,2] + tab2[2,1])
tab2
1 - accuracy2

#построение модели типа "C-classification" с параметром С = 1 и ядром "sigmoid"
svmModel4 <- svm(Colors ~., data=data4Train, type="C-classification", cost=1,
                 kernel="sigmoid")
#определение точности классификации на тестовой выборке
predictions4Test <- predict(svmModel4, data4Test)
tab2 = table(data4Test$Colors, predictions4Test)
accuracy2 = (tab2[1,1] + tab2[2,2]) / (tab2[1,1] + tab2[2,2] + tab2[1,2] + tab2[2,1])
tab2
1 - accuracy2

##ЗАДАНИЕ 5
data5Train <- read.table("svmdata5.txt", sep = "\t", stringsAsFactors = TRUE)
data5Test <- read.table("svmdata5test.txt", sep = "\t", stringsAsFactors = TRUE)

#построение модели типа "C-classification" с параметром С = 1 и ядром "polynomial"
svmModel5 <- svm(Colors ~., data=data5Train, type="C-classification", cost=1,
                 kernel="polynomial", degree = 2)
#определение точности классификации на тестовой выборке
predictions5Test <- predict(svmModel5, data5Test)
tab2 = table(data5Test$Colors, predictions5Test)
accuracy2 = (tab2[1,1] + tab2[2,2]) / (tab2[1,1] + tab2[2,2] + tab2[1,2] + tab2[2,1])
tab2
1 - accuracy2


#построение модели типа "C-classification" с параметром С = 1 и ядром "radial"
svmModel5 <- svm(Colors ~., data=data5Train, type="C-classification", cost=1,
                 kernel="radial")
#определение точности классификации на тестовой выборке
predictions5Test <- predict(svmModel5, data5Test)
tab2 = table(data5Test$Colors, predictions5Test)
accuracy2 = (tab2[1,1] + tab2[2,2]) / (tab2[1,1] + tab2[2,2] + tab2[1,2] + tab2[2,1])
tab2
1 - accuracy2

#построение модели типа "C-classification" с параметром С = 1 и ядром "sigmoid"
svmModel5 <- svm(Colors ~., data=data5Train, type="C-classification", cost=1,
                 kernel="sigmoid")
#определение точности классификации на тестовой выборке
predictions5Test <- predict(svmModel5, data5Test)
tab2 = table(data5Test$Colors, predictions5Test)
accuracy2 = (tab2[1,1] + tab2[2,2]) / (tab2[1,1] + tab2[2,2] + tab2[1,2] + tab2[2,1])
tab2
1 - accuracy2

##демонстрация переобучения при изменении gamma
#определение ошибки при различных значениях degree
Depgam = matrix(c(1:10, 1:10),nrow = 10, ncol = 2, byrow = TRUE)
for (i in 1:10){
  #построение модели типа "C-classification" с параметром С = 1 и ядром "radial"
  svmModel5 <- svm(Colors ~., data=data5Train, type="C-classification", cost=1,
                   kernel="radial", gamma = i)
  #определение точности классификации на обучающей выборке
  predictions5Train <- predict(svmModel5, data5Train)
  tab2 = table(data5Train$Colors, predictions5Train)
  accuracy2 = (tab2[1,1] + tab2[2,2]) / (tab2[1,1] + tab2[2,2] + tab2[1,2] + tab2[2,1])
  Depgam[i, 1] = i
  Depgam[i, 2] = 1 - accuracy2
}
plot(Depgam[,1],Depgam[,2], col="blue", ylab="Ошибка классификации", xlab="gamma", pch = 19, type="o")
Depgam[,2]

area.pallete = function(n = 2) 
{ 
  cols = rainbow(n) 
  cols[1:2] = c("PaleGreen", "Pink") 
  return(cols) 
} 
#цвет символов
symbols.pallete = c("Green", "Red")
#построение модели типа "C-classification" с параметром С = 1 и ядром "linear"
svmModel5 <- svm(Colors ~., data=data5Train, type="C-classification", cost=1,
                    kernel="radial", gamma = 9)
#построение графика
plot(svmModel5, data5Train, grid=250,  symbolPalette = symbols.pallete, color.palette = area.pallete)

##ЗАДАНИЕ 6
data6 <- read.table("svmdata6.txt", sep = "\t", stringsAsFactors = TRUE)
#построение регрессионной модели
regression_model <- svm(data6$X, data6$Y, type = "eps-regression", cost=1,
                        kernel="radial")
Eps = c(0.01,0.05,0.1,0.15,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
DepEps = c()
for(i in 1:length(Eps)){
  regression_model <- svm(data6$X, data6$Y, type = "eps-regression", cost=1,
                          epsilon=Eps[i], kernel="radial")
  predictions = predict(regression_model, data6$X)
#подсчет среднеквадратичной ошибки
  DepEps <- append(DepEps, sd(data6$Y-predictions))
}
plot(Eps, DepEps,col="red", pch = 19, xlab="Epsilon", ylab="Среднеквадратичная ошибка", type = "o")