#ЗАДАНИЕ 1
library(datasets)
reglab1 <- read.table("C:/Users/Unicorn/Desktop/Машинное Обучение/Лабы/reglab1.txt", sep = "\t", header = TRUE)
n <- dim(reglab1)[1]
reglab1_rand <- reglab1[ order(runif(n)),]
nt <- as.integer(n*0.8)
reglab1_train <- reglab1_rand[1:nt, ]
reglab1_test <- reglab1_rand[(nt+1):n, ]
#регрессия с использованием 1-й модели
f1 = lm(z ~ ., reglab1_train)
pred1 <- predict(f1, reglab1_test)
#ошибка 1
mist1 = sd(reglab1_test$z-pred1)
f1
mist1
#регрессия с использованием 2-й модели
f2 = lm(x ~ ., reglab1_train)
pred2 <- predict(f2, reglab1_test)
#ошибка 2
mist2 = sd(reglab1_test$z-pred2)
f2
mist2
#регрессия с использованием 3-й модели
f3 = lm(y ~ ., reglab1_train)
pred3 <- predict(f3, reglab1_test)
#ошибка
mist3 = sd(reglab1_test$z-pred3)
f3
mist3

#ЗАДАНИЕ 2
library(datasets)
reglab2 <- read.table("C:/Users/Unicorn/Desktop/Машинное Обучение/Лабы/reglab2.txt", sep = "\t", header = TRUE)
n <- dim(reglab2)[1]
reglab2_rand <- reglab2[ order(runif(n)),]
#80% для обучения
nt <- as.integer(n*0.8)
reglab2_train <- reglab2_rand[1:nt, ]
reglab2_test <- reglab2_rand[(nt+1):n, ]
x <- c("x1", "x2", "x3", "x4")
res <- list(x = NULL, rss = NULL)
#среднее значение y по обучающей выборке записываем в вектор предсказаний
pred <- rep(summary(reglab2_train$y)[4],n-nt)
#значение максимальной остаточной суммы квадратов при пустом подмножестве
rss = summary((reglab2_test$y-pred)^2)[6]
res$x <- append(res$x,summary(reglab2_train$y)[4])
res$rss <- append(res$rss, rss)
for (k in 1:4) {
  combs <- combn(x, k)
  combs
  for (i in 1:dim(combs)[2]) {
    f <- lm(as.formula(paste("y", paste(combs[,i], collapse = " + "),
                                 sep = " ~ ")),reglab2_train)
    pred <- predict(f, reglab2_test)
    #остаточная сумма квадратов
    rss = summary((reglab2_test$y-pred)^2)[6]
    res$x <- append(res$x, paste(combs[,i], collapse = " "))
    res$rss <- append(res$rss, rss)
  }
}
res

#ЗАДАНИЕ 3
library(datasets)
cygage <- read.table("C:/Users/Unicorn/Desktop/Машинное Обучение/Лабы/cygage.txt", sep = "\t", header = TRUE)
n <- dim(cygage)[1]
cygage_rand <- cygage[ order(runif(n)),]
#разделим множество на обучающее и тестовое (80% для обучения)
nt <- as.integer(n*0.8)
cygage_train <- cygage_rand[1:nt, ]
cygage_test <- cygage_rand[(nt+1):n, ]
f = lm(calAge ~ Depth, cygage_train, weights=cygage_train$Weight)
f
pred <- predict(f, cygage_test)
f_summary <- summary(f)
MSE = mean(f_summary$residuals^2)
MSE

#ЗАДАНИЕ 4
library(datasets)
library(dplyr)
library(MASS)
library(glmnet)
#загрузка данных
data(longley)
#исключение переменной "population"
longley <- longley %>% select(-Population)
n <- dim(longley)[1]
longley_rand <- longley[ order(runif(n)),]
#разделяем датасет на обучающую и тестовую выборки по 50%
nt <- as.integer(n*0.5)
longley_train <- longley_rand[1:nt, ]
longley_test <- longley_rand[(nt+1):n, ]
#записываем значения лямбд
lambdas = c()
for(i in 0:25){
  lambdas <- append(lambdas, 10^(-3+0.2*i))
}

#вектора ошибок
train_mistake = c()
test_mistake = c()

pred <- function(f, longley) {
  result <- NULL
  for (i in 1:dim(longley)[1]) {
    sum <- 0
    for (k in names(f)) 
      if (k == "(Intercept)" | k == "") sum <- sum + as.numeric(f[k]) 
      else sum <- sum + longley[i, k] * as.numeric(f[k])
      result <- c(result, sum)
  }
  return(result)
}

#строим гребневую регрессию
f <- lm.ridge(Employed ~ ., longley_train, lambda = lambdas)
for(i in 1:length(lambdas)){
  #строим гребневую регрессию
  #f <- lm.ridge(Employed ~ ., longley_train, lambda = lambdas[i])
  train_mistake <- append(train_mistake, sd(longley_train$Employed - pred(f$coef[,i],longley_train)))
  test_mistake <- append(test_mistake, sd(longley_test$Employed - pred(f$coef[,i],longley_test)))
}

plot(lambdas, train_mistake, pch=20, type = "o", col="black",
     main = "Зависимость ошибок от lambda",
     xlab="lambda", ylab="Ошибки")
lines(lambdas, test_mistake, pch=20, type = "o", col="blue")

##Способ предсказания(pred) - предсказывает среднее*
##f <- glmnet(longley_train, longley_train$Employed, lambda = lambdas[i])
##pred1 <- predict(f, newX1, s = lambdas[i])
##pred1
##newX2 <- model.matrix(~.,data=longley_test[-6])
##pred2 <- predict(f, newX2, s = lambdas[i])
##pred2

#ЗАДАНИЕ 5
library(datasets)
data(EuStockMarkets)
#построение котировок на одном графике
plot(EuStockMarkets[,1], type = "l",
     main = "Кривые изменения котеровок во времени",
     xlab="Время", ylab="Котировка",  col="red")
lines(EuStockMarkets[,2], type = "l", col="blue")
lines(EuStockMarkets[,3], type = "l", col="black")
lines(EuStockMarkets[,4], type = "l", col="brown")

#построение регрессий по-отдельности
DAX = lm(EuStockMarkets[,1] ~ time(EuStockMarkets), EuStockMarkets)
SMI = lm(EuStockMarkets[,2] ~ time(EuStockMarkets), EuStockMarkets)
CAC = lm(EuStockMarkets[,3] ~ time(EuStockMarkets), EuStockMarkets)
FTSE = lm(EuStockMarkets[,4] ~ time(EuStockMarkets), EuStockMarkets)
#построение регрессии для всех котировок
all = lm(EuStockMarkets[,1]+EuStockMarkets[,2]+EuStockMarkets[,3]+EuStockMarkets[,4]~time(EuStockMarkets),
         EuStockMarkets)
#вывод коэффициентов
DAX
SMI
CAC
FTSE
all

#ЗАДАНИЕ 6
library(datasets)
data(JohnsonJohnson)
length(JohnsonJohnson)
time(JohnsonJohnson)
#построение графиков
plot(time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
     JohnsonJohnson[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
     type = "l", main = "Кривые изменения прибыли", xlab="Время",
     ylab="Кривая", col="red")
lines(time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
      JohnsonJohnson[seq(from = 2, to = length(JohnsonJohnson), by = 4)],
      type = "l", col="blue")
lines(time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
      JohnsonJohnson[seq(from = 3, to = length(JohnsonJohnson), by = 4)],
      type = "l", col="black")
lines(time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
      JohnsonJohnson[seq(from = 4, to = length(JohnsonJohnson), by = 4)],
      type = "l", col="brown")


#построение моделей
qtr1 = lm(JohnsonJohnson[seq(from = 1, to = length(JohnsonJohnson), by = 4)] ~
            time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson),
                                     by = 4)], JohnsonJohnson)
qtr2 = lm(JohnsonJohnson[seq(from = 2, to = length(JohnsonJohnson), by = 4)] ~
            time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson),
                                     by = 4)], JohnsonJohnson)
qtr3 = lm(JohnsonJohnson[seq(from = 3, to = length(JohnsonJohnson), by = 4)] ~
            time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson),
                                     by = 4)], JohnsonJohnson)
qtr4 = lm(JohnsonJohnson[seq(from = 4, to = length(JohnsonJohnson), by = 4)] ~
            time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson),
                                     by = 4)], JohnsonJohnson)

all = lm(JohnsonJohnson[seq(from = 1, to = length(JohnsonJohnson), by = 4)]+
           JohnsonJohnson[seq(from = 2, to = length(JohnsonJohnson), by = 4)]+
           JohnsonJohnson[seq(from = 3, to = length(JohnsonJohnson), by = 4)]+
           JohnsonJohnson[seq(from = 4, to = length(JohnsonJohnson), by = 4)]~
           time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
           JohnsonJohnson)
#вывод параметров моделей:
qtr1
qtr2
qtr3
qtr4
all
#предсказание на 2016 год
year=2016
pred1 = coef(qtr1)[1]+coef(qtr1)[2]*year
pred2 = coef(qtr2)[1]+coef(qtr2)[2]*year
pred3 = coef(qtr3)[1]+coef(qtr3)[2]*year
pred4 = coef(qtr4)[1]+coef(qtr4)[2]*year
#прогноз на год в среднем
pred_all = (coef(all)[1]+coef(all)[2]*year)/4
pred1
pred2
pred3
pred4
pred_all

#ЗАДАНИЕ 7
library(datasets)
data(sunspot.year)
sunspot.year
#построим таблицу на полученных данных (где будут года и пятна)
sunspot = data.frame(year = seq(1700,1988,1), 
                     spots=sunspot.year[seq(1,length(sunspot.year),1)])
plot(sunspot$year, sunspot$spots, type = "l", col="blue",
     main = "Изменение числа солнечных пятен", xlab="Время",
     ylab="Число пятен")
#линейная регрессия
f = lm(spots ~ year, sunspot)
f

#ЗАДАНИЕ 8
library(datasets)
gas <- read.csv("C:/Users/Unicorn/Desktop/Машинное Обучение/Лабы/UKgas.csv", header = TRUE, sep = ",", dec = ".",
                stringsAsFactors = FALSE)
#построение регрессии
qtr1 = lm(UKgas[seq(from = 1, to = dim(gas)[1], by = 4)] ~
            time[seq(from = 1, to = dim(gas)[1], by = 4)], gas)
qtr2 = lm(UKgas[seq(from = 2, to = dim(gas)[1], by = 4)] ~
            time[seq(from = 2, to = dim(gas)[1], by = 4)], gas)
qtr3 = lm(UKgas[seq(from = 3, to = dim(gas)[1], by = 4)] ~
            time[seq(from = 3, to = dim(gas)[1], by = 4)], gas)
qtr4 = lm(UKgas[seq(from = 4, to = dim(gas)[1], by = 4)] ~
            time[seq(from = 4, to = dim(gas)[1], by = 4)], gas)
#по всем
all = lm(UKgas[seq(from = 1, to = dim(gas)[1], by = 4)]+
           UKgas[seq(from = 2, to = dim(gas)[1], by = 4)]+
           UKgas[seq(from = 3, to = dim(gas)[1], by = 4)]+
           UKgas[seq(from = 4, to = dim(gas)[1], by = 4)] ~
           time[seq(from = 1, to = dim(gas)[1], by = 4)]+
           time[seq(from = 2, to = dim(gas)[1], by = 4)]+
           time[seq(from = 3, to = dim(gas)[1], by = 4)]+
           time[seq(from = 4, to = dim(gas)[1], by = 4)], gas)
#вывод параметров моделей
qtr1
qtr2
qtr3
qtr4
all
#предсказание по кварталам
coef(qtr1)[1]+coef(qtr1)[2]*year
coef(qtr2)[1]+coef(qtr2)[2]*year
coef(qtr3)[1]+coef(qtr3)[2]*year
coef(qtr4)[1]+coef(qtr4)[2]*year
#в среднем по году
(coef(all)[1]+coef(all)[2]*year)/4

#ЗАДАНИЕ 9
library(datasets)
data(cars)
cars
#регрессионная модель
f = lm(dist ~ speed, cars)
f
#предсказание
predict(f, data.frame(speed=40,dist=0))