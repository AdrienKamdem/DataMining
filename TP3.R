dim(iris) # matrice de donn�es
names(iris) # titre des colonnes
str(iris) # caract�ristiques de notre dataset
attributes(iris) # 

set.seed(1234) #m�thode de tirage al�atoire en fonction de 1234 
ind = sample(2, nrow(iris), replace=TRUE, prob=c(0.7,0.3))
trainData = iris[ind==1,] #0.7
testData = iris[ind==2,] #0.3

myFormula = Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width #formulaton du probl�me
library(rpart)
iris_part = rpart(myFormula, data=trainData, control=rpart.control(minsplit=5)) # formation d'un classifieur arbre de d�cision
attributes(iris_part)  
print(iris_part)

library(rpart.plot)
prp(iris_part,extra=1) # plot

trainPred = predict(iris_part, newdata = trainData, type="class")
table(trainPred, trainData$Species) #matrice de confusion de l'arbre de d�cision

testPred = predict(iris_part, newdata = testData, type="class")
table(testPred, testData$Species)


# A revoir
iris_part$control$cp
plotcp(iris_part)

iris_part = rpart(myFormula, data=trainData, control = rpart.control(minsplit = 5, cp=0.2))

library(randomForest)
rf = randomForest(Species~., data=trainData, ntree=100)
print(rf)
