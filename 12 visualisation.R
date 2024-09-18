#Librairies
library(randomForest)
library(randomForestExplainer)
library(tibble)
library(ggplot2)
#library(rpart)
#library(rpart.plot)


#Import des résultats du Random Forest
modeleRF01 = get(load("modeleRF01-100-11-005.RData"))

#Visualisation des taux d'erreur OOB et de la matrice de confusion
plot(modeleRF01,main ="Taux d'erreur par nombre d'arbres")
plot(modeleRF01,main ="Taux d'erreur par nombre d'arbres", ylim = c(0.2378,0.244))
modeleRF01

#Hiérarchisation des termes discriminants par coefficient de Gini
round(importance(modeleRF01), 2)
class(round(importance(modeleRF01), 2))
impRF01 = importance(modeleRF01)
#impRF01
nomsVar = rownames(impRF01)
impRF01 = impRF01[impRF01[,1]>0,1]
resultatsRF01 = tibble(variable = names(impRF01), coeffGini = impRF01)
#resultatsRF01
resultatsRF01$variable = factor(resultatsRF01$variable, levels = resultatsRF01$variable[order(resultatsRF01$coeffGini, decreasing=FALSE)])
resultatsRF02 = resultatsRF01[order(resultatsRF01$coeffGini, decreasing=FALSE),]
#Visualisation des termes discriminants hiérarchisés par le coefficient de Gini
print(resultatsRF02,n=20)
ggplot(resultatsRF02) + 
  geom_col(aes(x = variable, y = coeffGini)) + 
  coord_flip()
#Zoom sur le coefficient de Gini
print(length(resultatsRF02[["coeffGini"]]))
resultatsRF03 = subset(resultatsRF02, coeffGini < 0.5)
print(length(resultatsRF03[["coeffGini"]]))
#Zoom sur les résultats de meilleure qualité
resultatsRF04 = subset(resultatsRF03, coeffGini < 0.1)
print(resultatsRF04,n=20)
print(length(resultatsRF04[["coeffGini"]]))
write.csv(resultatsRF04,"resultatsRF04-300-11.csv",row.names = FALSE)

#Visualisation des pourcentages de mots rares
plot_min_depth_distribution(modeleRF01)

#Visualisation de la précision des prédictions
accuracies <- read.csv("accuracies-400-20.csv", header = TRUE, encoding = "UTF-8")
colnames(accuracies) <- c("validation_croisee", "accuracy")
accuracies

ggplot(accuracies) + 
  geom_col(aes(x=validation_croisee, y = accuracy))
