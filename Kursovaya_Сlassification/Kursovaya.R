#Загрузка данных и разбиение на обучающую и тестовую выборки
library(e1071)
library(kknn)
library(Rtsne)
library(dplyr)
library(adabag)
library(cluster)
library(glmnet)
library(autoencoder)

masses <- read.table("C:/Users/Unicorn/Desktop/Машинное Обучение/mammographic_masses.data", sep = ",")

#ПОДГОТОВКА
#______________________________
#Заполнение пропусков
#Нахождение средних значений по столбцам V1, V2, V3, V4, V5
sum1 = 0
sum2 = 0
sum3 = 0
sum4 = 0
sum5 = 0
for(i in 1:961){
  if(masses$V1[i] == "?"){
  }
  else{
    sum1 = sum1 + as.numeric(masses$V1[i])
  }
}
sum1
V1_mean <- round(sum1/961)

for(i in 1:961){
  if(masses$V2[i] == "?"){
  }
  else{
    sum2 = sum2 + as.numeric(masses$V2[i])
  }
}
sum2
V2_mean <- round(sum2/961)

for(i in 1:961){
  if(masses$V3[i] == "?"){
  }
  else{
  sum3 = sum3 + as.numeric(masses$V3[i])
  }
}
sum3
V3_mean <- round(sum3/961)
V3_mean
for(i in 1:961){
  if(masses$V4[i] == "?"){
  }
  else{
    sum4 = sum4 + as.numeric(masses$V4[i])
  }
}
sum4
V4_mean <- round(sum4/961)
V4_mean
for(i in 1:961){
  if(masses$V5[i] == "?"){
  }
  else{
    sum5 = sum5 + as.numeric(masses$V5[i])
  }
}
sum5
V5_mean <- round(sum5/961)
V5_mean
#Заполнение
for(i in 1:961){
  if(masses$V1[i] == "?"){
    masses$V1[i]=V1_mean
  }
  if(masses$V2[i] == "?"){
    masses$V2[i]=V2_mean
  }
  if(masses$V3[i] == "?"){
    masses$V3[i]=V3_mean
  }
  if(masses$V4[i] == "?"){
    masses$V4[i]=V4_mean
  }
  if(masses$V5[i] == "?"){
    masses$V5[i]=V5_mean
  }
}

#masses$V6 = as.factor(masses$V6)
#Сделать остальные параметры в виде чисел
for(i in 1:961){
  masses$V1[i] = as.integer(masses$V1[i])
}
for(i in 1:961){
  masses$V2[i] = as.integer(masses$V2[i])
}
for(i in 1:961){
  masses$V3[i] = as.integer(masses$V3[i])
}
for(i in 1:961){
  masses$V4[i] = as.integer(masses$V4[i])
}
for(i in 1:961){
  masses$V5[i] = as.integer(masses$V5[i])
}
for(i in 1:961){
  masses$V6[i] = as.integer(masses$V6[i])
}
masses$V6 = as.integer(masses$V6)
masses$V1 = as.integer(masses$V1)
masses$V2 = as.integer(masses$V2)
masses$V3 = as.integer(masses$V3)
masses$V4 = as.integer(masses$V4)
masses$V5 = as.integer(masses$V5)
masses

n<-dim(masses)[1]
#Разделим данные случайным образом на обучающую и тестовую выборки
set.seed(12345)
#Для обучения возьмем 80%
n.train<- as.integer(n*0.8)
n.test<- n-n.train
rand <- masses[ order(runif(n)), ]
masses_train <- rand[1:n.train, ]
masses_test <- rand[(n.train+1):n, ]

#TSNE
#______________________________
#Данные для tsne
tsne_masses <- masses
#Убираем неуникальные строки
tsne_masses = distinct(tsne_masses)
#Визуализация данных при помощи метода Rtsne
#Добавляем в данные цвета
for(i in 1:length(tsne_masses$V6)){
  if(tsne_masses$V6[i]== 0){
    tsne_masses$V6[i] = "green"
  }
  else{
    tsne_masses$V6[i] = "red"
  }
}
tsne_masses
#TSNE
tsne<-Rtsne(tsne_masses, pca = TRUE, dim = 2)
plot(tsne$Y, pch=19, col = tsne_masses$V6, xlab="Y[,1]", ylab="Y[,2]")
tsne_masses$V6

masses_train$V6 = as.factor(masses_train$V6)
masses_test$V6 = as.factor(masses_test$V6)

#КЛАССИФИКАТОРЫ
#______________________________
#Наивный Байесовский классификатор
bayes <- naiveBayes(V6~.,data = masses_train)
bayes_predict <- predict(bayes,masses_test)
bayes_tab <- table(bayes_predict, masses_test$V6)
#Точность классификации
bayes_accuracy <- sum(diag(bayes_tab))/sum(bayes_tab)
#Ошибка классификации
bayes_error <- 1-bayes_accuracy
bayes_tab
bayes_accuracy

#k-ближайших соседей
#Нахождение оптимального k
fit.train <- train.kknn(V6 ~ ., masses_train, kmax = 50, distance = 1,
                        kernel = "triangular")
fit.train
#Нахождение оптимального dis
knn_dis = seq(1,50,1)
knn_err = NULL
for(i in knn_dis) {
  knn <- kknn(V6 ~ ., masses_train, masses_test, k = 3, kernel="triangular", distance=i)
  tab = table(fitted(knn), masses_test$V6)
  knn_err <- c(knn_err, 1-(sum(diag(tab))/sum(tab)))
}
res = data.frame(knn_dis, knn_err)   
plot(res, xlab="Расстояние (dis)", ylab="Ошибка", type = "o", col="blue")
res

#Нахождение оптимального ядра
knn_kernel = c("rectangular", "triangular", "epanechnikov","biweight","triweight", "cos", "inv", "gaussian", "rank", "optimal")
knn_err = NULL
for(i in knn_kernel) {
  knn <- kknn(V6 ~ ., masses_train, masses_test, k = 3, kernel=i, distance=1)
  tab = table(fitted(knn), masses_test$V6)
  knn_err <- c(knn_err, 1-(sum(diag(tab))/sum(tab)))
}
res = data.frame(knn_kernel,knn_err)
res

#Классификатор k-ближайших соседей для k = 3, kernel="cos", distance=1
knn<-kknn(as.factor(V6)~., masses_train, masses_test, k = 3, kernel="cos", distance=1)
knn_tab = table(fitted(knn),masses_test$V6)
knn_accuracy <- sum(diag(knn_tab))/sum(knn_tab)
knn_err <- 1 - knn_accuracy
knn_accuracy
knn_tab

#Бэггинг
#глубина дерева
depths = seq(1, 15, 1)
bagging_err = c()
for(i in 1:length(depths)){
  bagging <- bagging(V6 ~., data=masses_train, mfinal=1,
                     maxdepth=i)
  bagging.pred <- predict.bagging(bagging, masses_test)
  bagging_err <- append(bagging_err, bagging.pred$error)
}
plot(depths, bagging_err, xlab="Глубина дерева", ylab="Ошибка на тесте", pch=19)
bagging_err

#максимальной число итераций
mfinal = seq(1, 15, 1)
maxdepth <- 3
bagging_err = c()
for(i in 1:length(mfinal)){
  bagging <- bagging(V6 ~., data=masses_train, mfinal=mfinal[i],
                          maxdepth = maxdepth)
  bagging.pred <- predict.bagging(bagging, masses_test)
  bagging_err <- append(bagging_err, bagging.pred$error)
}
plot(mfinal, bagging_err, xlab="Максимальное число итераций", ylab ="Ошибка на тесте", pch=19)

#Бэггинг при maxdepth = 4 и mfinal = 12
bagging_err = c()
bagging <- bagging(V6 ~., data=masses_train, mfinal = 12,
                   maxdepth = 4)
bagging.pred <- predict.bagging(bagging, masses_test)
bagging_err <- append(bagging_err, bagging.pred$error)
#точность
bagging_acuracy = 1 - bagging_err
bagging_acuracy

#КЛАСТЕРИЗАЦИЯ
#______________________________
#убираем метки
cluster_masses <- masses[,-6]
#нахождение оптимального сочетания параметров
cl_eu1 = clara(cluster_masses, k = 2, metric = "euclidean", stand = FALSE)
tb <- table(cl_eu1$clustering, masses$V6)
cl_eu1.err = sum(diag(tb))/sum(tb)
cl_eu1.err
cl_eu2 = clara(cluster_masses, k = 2, metric = "euclidean", stand = TRUE)
tb <- table(cl_eu2$clustering, masses$V6)
cl_eu2.err = sum(diag(tb))/sum(tb)
cl_eu2.err
cl_ma1 = clara(cluster_masses, k = 2, metric = "manhattan", stand = FALSE)
tb <- table(cl_ma1$clustering, masses$V6)
cl_ma1.err = sum(diag(tb))/sum(tb)
cl_ma1.err
cl_ma2 = clara(cluster_masses, k = 2, metric = "manhattan", stand = TRUE)
tb <- table(cl_ma2$clustering, masses$V6)
cl_ma2.err = sum(diag(tb))/sum(tb)
cl_ma2.err

#кластеризация с оптимальными параметрами
cl_eu2 = clara(cluster_masses, k = 2, metric = "euclidean", stand = TRUE)
plot(cluster_masses, col = cl_eu2$clustering)

#построение дендрограммы
dend_masses <- cluster_masses[1:100,1:5]
plot(agnes(dend_masses))
#сравнение полученных результатов с реальными метками данных
dend <- agnes(dend_masses)
dend$order.lab
#элементы по дентрограмме в одном кластере
indexes_cl1 = as.integer(dend$order.lab[1:69])
#этот класс 1 (злокачественная) - определено по 1-му элементу в masses
masses
#реальные метки
claster1 <- masses[indexes_cl1,]
ones = rep(1, dim(claster1)[1])
claster1$V6
tab1 <- table(claster1$V6, ones)
tab1
indexes_cl2 = as.integer(dend$order.lab[69:100])
claster2 <- masses[indexes_cl2,]
nulls = rep(0, dim(claster2)[1])
tab2 <- table(claster2$V6, nulls)
tab2
#ошибка
cluster_err = (tab1[1,1]+tab2[2,1])/100
cluster_err

#ОПРЕДЕЛЕНИЕ НАИБОЛЕЕ ЗНАЧИМЫХ ПРИЗНАКОВ
x = as.matrix(masses[,-6])
y = as.matrix(masses[,6])
lambda_seq <- seq(0.001, 10, 0.001)
cv <- cv.glmnet(x, y, alpha = 1, lambda = lambda_seq, nfolds = 5)
plot(cv)
best_lam <- cv$lambda.min
best_lam
lasso_best <- glmnet(x, y, alpha = 1, lambda = best_lam, label=TRUE)
coef(lasso_best)

#АВТОКОДЕР
#использование автокодера
#число слоев – 3; число нейронов в скрытом слое – 4; функц. актив. – tanh; eps = 0
masses[,-6]
encoder<-autoencode(as.matrix(masses[,-6]),
                         nl=3,
                         N.hidden = 4,
                         epsilon = 0,
                         unit.type = "tanh",
                         lambda = 0.0002,
                         beta = 6,
                         rho = 0.1,
                         max.iterations = 20000,
                         rescale.flag = TRUE)
encoder
decomposition <- predict.autoencoder(encoder, as.matrix(masses[,-6]),hidden.output = TRUE)
decomposition

#добавляем к полученным данным столбец с метками
decomposition.frame <- as.data.frame(decomposition$X.output)
V5 <- as.numeric(masses$V6)
decomposition.encode.frame <- tibble::add_column(decomposition.frame,V5)
decomposition.encode.frame

#Убираем неуникальные строки
tsne_frame <- distinct(decomposition.encode.frame)

#Визуализация данных при помощи метода Rtsne
tsne <- Rtsne(as.matrix(tsne_frame),pca = TRUE, dim = 2)
plot(tsne$Y, pch=21, bg=c("green","red"), xlab="Y[,1]", ylab="Y[,2]")

#Разделим данные случайным образом на обучающую и тестовую выборки
set.seed(12345)
#Для обучения возьмем 80%
train<- as.integer(n*0.8)
test<- n-train
rand <- decomposition.encode.frame[ order(runif(n)), ]
encode_train <- rand[1:train, ]
encode_test <- rand[(train+1):n, ]
encode_train
#knn
#Нахождение оптимального k
fit.train <- train.kknn(V5~., encode_train, kmax = 50, distance = 1,
                        kernel = "rectangular")
fit.train
#классификация
knn<-kknn(V5~., encode_train, encode_test, k=4, kernel="rectangular", distance=1)
tab <- table(fitted(knn),encode_test$V5)
acuracy <- (sum(diag(tab))/sum(tab))
acuracy
err = 1-(sum(diag(tab))/sum(tab))
err

#разряженная размерность: N.hidden = 20
encoder<-autoencode(as.matrix(masses[,-6]),
                    nl=3,
                    N.hidden = 20,
                    epsilon = 0.5,
                    lambda = 0.0002,
                    beta = 6,
                    rho = 0.9,
                    unit.type = "tanh",
                    max.iterations = 20000,
                    rescale.flag = TRUE)
reconstraction<-predict.autoencoder(encoder,as.matrix(masses[,-6]),hidden.output = FALSE)
decomposition<-predict.autoencoder(encoder,as.matrix(masses[,-6]),hidden.output = TRUE)
decomposition
#добавляем к полученным данным столбец с метками
decomposition.frame <- as.data.frame(decomposition$X.output)
V21 <- as.numeric(masses$V6)
decomposition.encode.frame <- tibble::add_column(decomposition.frame,V21)
decomposition.encode.frame
#Разделим данные случайным образом на обучающую и тестовую выборки
set.seed(12345)
#Для обучения возьмем 80%
train<- as.integer(n*0.8)
test<- n-train
rand <- decomposition.encode.frame[ order(runif(n)), ]
encode_train <- rand[1:train, ]
encode_test <- rand[(train+1):n, ]
encode_train
#knn
#Нахождение оптимального k
fit.train <- train.kknn(V21~., encode_train, kmax = 50, distance = 1,
                        kernel = "rectangular")
fit.train
encode_train
#классификация
knn<-kknn(V21~., encode_train, encode_test, k=3, kernel="rectangular", distance=1)
tab <- table(fitted(knn),encode_test$V21)
acuracy <- (sum(diag(tab))/sum(tab))
acuracy
err = 1-(sum(diag(tab))/sum(tab))
err

