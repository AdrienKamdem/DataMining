library(neuralnet) # library de reseaux de neurones

#EXO5
XOR = c(0,1,1,0) # creation d'un vecteur 

xor.data = data.frame(expand.grid(c(0,1),c(0,1)),XOR) #matrice avec 2 variables et var1 et var2 et vecteur xor
print(xor.data)

net.xor = neuralnet(XOR~Var1+Var2,xor.data,hidden=2, rep=5) # 2 neurones sur la couche cachée, 5 reseaux et choisit le meilleur selon l'erreur => ressort l'erreur ainsi que le nombre d'étapes
print(net.xor)#toutes les caractéristiques du reseaux de neurones de xor
plot(net.xor, rep="best")#affichage du reseau de neurones

net.xor$act.fct #fonction d'activation
net.xor$err.fct #affichage de la fonction erreur
net.xor$data #affichage de la matrice xor
net.xor$net.result #résultats


#EXO6

# Caractéristiques
dim(iris)
names(iris)
str(iris)
attributes(iris)

#Ajout de trois variables booléennes
iris$setosa <- iris$Species=="setosa"
iris$virginica <- iris$Species == "virginica"
iris$versicolor <- iris$Species == "versicolor"

#Centrer et réduire les données
mean=apply(iris[,1:4],2,mean)
sd=sapply(iris[,1:4], sd)
data.scaled = scale(iris[,1:4], center = mean, scale = sd) # Scale the data centrée les variable
iris[,1:4]=data.scaled

# Devision en train et test
iris.train.idx <- sample(x = nrow(iris), size = nrow(iris)*0.75)
iris.train <- iris[iris.train.idx,]
iris.valid <- iris[-iris.train.idx,]

#Construction d'un réseau de neuone
iris.net <- neuralnet(setosa+versicolor+virginica ~
                        Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                      data=iris.train, hidden=5, rep = 5,
                      linear.output = F)
plot(iris.net, rep="best")

#COnstruction d'un réseau de neurone
iris.net <- neuralnet(setosa+versicolor+virginica ~
                        Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                      data=iris.train, hidden=c(5,2), rep = 5,
                      linear.output = F)
plot(iris.net, rep="best")

#Prédictions et matrices de confusion
iris.prediction <- compute(iris.net, iris.valid[1:4])
idx <- apply(iris.prediction$net.result, 1, which.max)
predicted <- c("setosa", "versicolor", "virginica")[idx]
table(predicted, iris.valid$Species)

