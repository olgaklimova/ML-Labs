#ЗАДАНИЕ 1
library(cluster)
data(pluton)
pluton
#кластеризация на три класса методом kmeans
cl_pluton <- kmeans(pluton, 3)
plot(pluton, col = cl_pluton$cluster)

#создаем вектор максимального количества итераций (от 1 до 100)
iter_max = c()
for(i in 1:100){
  iter_max <- append(iter_max, i)
}
cl_pluton
#общая вариация внутри кластера (сумма cl_pluton$withinss)
cl_pluton$tot.withinss

#вектор общих вариаций внутри кластера при различных iter.max
totwithinss = c()
for(i in iter_max){
  cl_pluton <- kmeans(pluton, 3, iter.max = iter_max[i])
  totwithinss <- append(totwithinss, cl_pluton$tot.withinss)
}
#построение графика
plot(iter_max, totwithinss, pch = 20, xlab="Максимальное число итераций",
     ylab="Общая вариация",
     main = "")
totwithinss

#ЗАДАНИЕ 2
#генерация трех кластеров, сильно вытянутых вдоль оси (с помощью норм. распр-я)
data <- data.frame(x = c(rnorm(100, 0, 1), rnorm(100, 20, 5), rnorm(100, -20, 5)),
                  y = c(rnorm(100, 20, 10), rnorm(100, 0, 2), rnorm(100, 0, 2)))

col = c(rep("red", 100), rep("blue", 100), rep("green", 100))

#объединение вектора чисел и вектора светов
data_col <- cbind(data, col)
plot(data_col$x, data_col$y, xlab="x", ylab="y", col = data_col$col)

#использование стандартизации
#используется
cl_eucl1 = clara(data, k = 3, metric = "euclidean", stand = TRUE)
plot(data, col = cl_eucl1$clustering, xlab = "x", ylab = "y")
#не используется
cl_eucl2 = clara(data, k = 3, metric = "euclidean", stand = FALSE)
plot(data, col = cl_eucl2$clustering, xlab = "x", ylab = "y")

#использование метрик
#euclidean
cl_eucl2 = clara(data, k = 3, metric = "euclidean", stand = FALSE)
plot(data, col = cl_eucl2$clustering, xlab = "x", ylab = "y")

#manhattan
cl_manh1 = clara(data, k = 3, metric = "manhattan", stand = FALSE)
plot(data, col = cl_manh1$clustering, xlab = "x", ylab = "y")

#manhattan
cl_manh2 = clara(data, k = 3, metric = "manhattan", stand = TRUE)
plot(data, col = cl_manh2$clustering, xlab = "x", ylab = "y")

cl_eucl1$silinfo$avg.width
cl_eucl2$silinfo$avg.width
cl_manh1$silinfo$avg.width
cl_manh2$silinfo$avg.width

#ЗАДАНИЕ 3
library(cluster)
data("votes.repub")
votes.repub
#построение дэндрограммы
plot(agnes(votes.repub))


#ЗАДАНИЕ 4
library(cluster)
data("animals")
animals
plot(agnes(animals))

#ЗАДАНИЕ 5
library(cluster)
seeds <- read.table("C:/Users/Unicorn/Desktop/Машинное Обучение/Лабы/seeds_dataset.txt", sep = "", stringsAsFactors = TRUE)
seeds
#слишком большой получается
plot(agnes(seeds))
#кластеризация (на 3 класстера)
cl_clara1 = clara(seeds, k = 3, metric = "manhattan", stand = FALSE)
cl_clara1
plot(seeds, col = cl_clara1$clustering)
cl_clara1$silinfo$avg.width

#кластеризация на три класса методом kmeans
cl_kmeans1 <- kmeans(seeds, 3)
plot(seeds, col = cl_kmeans1$cluster)
cl_kmeans1$tot.withinss