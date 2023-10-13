#ЗАДАНИЕ 1
library(tree)
library(maptree)
library(mlbench)
#загружаем данные Glass
data(Glass)
#определяем общее количество примеров в обучающей выборке
Glass <- Glass[,-1]
n <- dim(Glass)[1]
#делим данные на тестовую и обучающую выборку (80% обучающая)
glass_rand <- Glass[ order(runif(n)),]
nt <- as.integer(n*0.8)
glass_train <- glass_rand[1:nt, ]
glass_test <- glass_rand[(nt+1):n, ]

#построим дерево классификации для данных Glass:
glass.tr <- tree(Type ~., glass_train)
#построение дерева решений
plot(glass.tr, type = "uniform")
text(glass.tr,cex=0.7)
#красивое изображение дает функция draw.tree из библиотеки maptree:
draw.tree(glass.tr, cex=0.7)
glass.tr
#обрежем дерево
glass.tr_opt <- snip.tree(glass.tr, nodes = c(5,103))
draw.tree(glass.tr_opt, cex=0.7)
#пример
example <- data.frame(RI=1.516, Na=11.7, Mg=1.01, Al=1.19, Si=72.59, K=0.43,
                      Ca=11.44, Ba=0.02, Fe=0.1)
predict(glass.tr_opt,example)

#ЗАДАНИЕ 2
library(tree)
library(maptree)
library(DAAG)
data(spam7)
n <- dim(spam7)[1]
#разделяем на обучающую и тестовую выборку (обучающая 80%)
spam7_rand <- spam7[ order(runif(n)),]
nt <- as.integer(n*0.8)
spam7_train <- spam7_rand[1:nt, ]
spam7_test <- spam7_rand[(nt+1):n, ]
#дерево для spam7
spam7.tr <- tree(yesno ~., spam7_train)
draw.tree(spam7.tr, cex=0.7)
#процедура “cost-complexity prunning”
newspam7.tr <- prune.tree(spam7.tr, method = "misclass")
newspam7.tr$k
draw.tree(prune.tree(spam7.tr, k = 0), cex=0.7)
draw.tree(prune.tree(spam7.tr, k = 12), cex=0.7)
draw.tree(prune.tree(spam7.tr, k = 63), cex=0.7)
draw.tree(prune.tree(spam7.tr, k = 83.5), cex=0.7)
draw.tree(prune.tree(spam7.tr, k = 673), cex=0.7)

#ЗАДАНИЕ 3
library(tree)
library(maptree)
library(DAAG)
library(e1071) 
data(nsw74psid1)
n <- dim(nsw74psid1)[1]
#разделяем на тестовую и обучающую
nsw_rand <- nsw74psid1[order(runif(n)),]
nt <- as.integer(n*0.8)
nsw_train <- nsw_rand[1:nt, ]
nsw_test <- nsw_rand[(nt+1):n, ]
#строим дерево решений для nsw74psid1
nsw.tr <- tree(re78 ~., nsw_train)
draw.tree(nsw.tr, cex=0.7)
predictions_tree <- predict(nsw.tr, nsw_test[-10])
#регрессионная модель на методе опорных векторов
svmModel <- svm(nsw_train[-10], nsw_train$re78, type = "eps-regression", cost=1,kernel="radial")
predictions_svm <- predict(svmModel, nsw_test[-10])
tab2 = table(nsw_test$re78, predictions_svm)
#вычислим ошибки (tree_mistake > svm_mistake)
tree_mistake <- sd(nsw_test$re78 - predictions_tree)
svm_mistake <- sd(nsw_test$re78 - predictions_svm)
tree_mistake
svm_mistake

#ЗАДАНИЕ 4
library(tree)
library(maptree)
lenses_raw <- read.table("C:/Users/Unicorn/Desktop/Машинное Обучение/Лабы/Lenses.txt", sep = "", stringsAsFactors = TRUE)
lenses_raw <- lenses_raw[,-1]
n <- dim(lenses_raw)[1]
lenses_rand <- lenses_raw[ order(runif(n)),]
#для обучения возьмем 90%
nt <- as.integer(n*0.8)
lenses_train <- lenses_rand[1:nt, ]
lenses_test <- lenses_rand[(nt+1):n, ]
#дерево решений
lenses.tr <- tree(V6 ~., lenses_train)
draw.tree(lenses.tr, cex=0.7)
#пример
example <- data.frame(V2=2, V3=1, V4=2, V5=1)
predict(lenses.tr,example)

#ЗАДАНИЕ 5
svmdata4 <- read.table("C:/Users/Unicorn/Desktop/Машинное Обучение/Лабы/svmdata4.txt", sep = "\t", stringsAsFactors = TRUE)
svmdata4test <- read.table("C:/Users/Unicorn/Desktop/Машинное Обучение/Лабы/svmdata4test.txt", sep = "\t", stringsAsFactors = TRUE)
#построение дерева решений
svmdata4.tr <- tree(Colors ~., svmdata4)
draw.tree(svmdata4.tr, cex=0.7)
pred_svmdata4 <- predict(svmdata4.tr, svmdata4test)

#ЗАДАНИЕ 6
library(dplyr)
Titanic_train <- read.csv("C:/Users/Unicorn/Desktop/Машинное Обучение/Лабы/Titanic_train.csv", header = TRUE, sep = ",", dec = ".",
                    stringsAsFactors = TRUE)
Titanic_test <- read.csv("C:/Users/Unicorn/Desktop/Машинное Обучение/Лабы/Titanic_test.csv", header = TRUE, sep = ",", dec = ".",
                   stringsAsFactors = TRUE)
#построение дерева решений
Titanic.tr <- tree(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, Titanic_train)
draw.tree(Titanic.tr, cex=0.7)
fit <- round(predict(Titanic.tr,Titanic_test))
Gen_sub = read.csv("C:/Users/Unicorn/Desktop/Машинное Обучение/Лабы/gender_submission.csv", header = TRUE, sep = ",", dec = ".",
                   stringsAsFactors = FALSE)
tab = table(fit, Gen_sub$Survived)
accuracy = (tab[1,1] + tab[2,2]) / (tab[1,1] + tab[2,2] + tab[1,2] + tab[2,1])
tab 
accuracy