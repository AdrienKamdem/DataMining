install.packages("dplyr")
install.packages("funModelling")
install.packages("mlbench")

library(dplyr)
library(funModeling)
library(mlbench)

#Suppression des données manquantes
d = data("PimaIndiansDiabetes2", package = "mlbench")
print(PimaIndiansDiabetes2)
PimaIndiansDiabetes2 = na.omit(PimaIndiansDiabetes2)
print(PimaIndiansDiabetes2)

#Division en train et test set 
set.seed(123)
training.samples = sample(x = nrow(PimaIndiansDiabetes2),
                           size = nrow(PimaIndiansDiabetes2)*0.8)
train.data = PimaIndiansDiabetes2[training.samples,]
test.data = PimaIndiansDiabetes2[-training.samples,]
print(train.data)
print(test.data)


#EXERCICE 1 

#regression logistic => affichage coeff de la regression 
model = glm( diabetes ~ glucose, data = train.data, family = binomial)
summary(model)$coef

#la regression logistique est P(Diabète|Glucose)=1/(1+e^z) 
#la fonction logit z=intercept+coeff1*x1 = ln(p/1-p)


#EXERCICE 2
#Vizualisation du modèle
train.data %>%
  mutate(prob = ifelse(diabetes == "pos", 1, 0)) %>%
  ggplot(aes(glucose, prob)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model",
    x = "Plasma Glucose Concentration",
    y = "Probability of being diabete-pos"
  )

#Prédiction de la classe à partir de deux valeurs du glucose
newdata = data.frame(glucose = c(20, 180))
logit = predict(model, newdata)
logit
probabilities = predict(model, newdata, type = "response")
probabilities
predicted.classes = ifelse(probabilities > 0.5, "pos", "neg")
predicted.classes


