#Librairies
library(randomForest)

#Import des données nettoyées et paramétrées
dtmD <- read.table(file="dtmd-010.txt", as.is = TRUE, header = TRUE, sep = "\t", row.names = NULL)
options(max.print=10) #limitations du nombre des résultats pour les impressions de test

#Ajout à la matrice de la colonne des profils
data <- read.csv2("traitement-2024-2.csv", header = TRUE, encoding = "UTF-8")
colnames(data)[1] <- "doc_id"
data_sexe = subset(data, select = c(doc_id, sexe))

dtmD[1]

dtmD = transform(dtmD, maPrediction = data_sexe$sexe)
dtmD$maPrediction = factor(dtmD$maPrediction)
#is.factor(dtmD$maPrediction)

#dim(dtmD)
#dtmD = dtmD[1:15811,1913:2013]#test sur les 100 derniers enregistrements => TEST


#Paramétrage du prototype
K = 10 #validation croisée
DO = dim(dtmD)[1]
accuracies = array(NA, K)

#Lancement de la boucle de Random forest
for (i in 1:K){ 
  #Segmentation des données pour l'entraînement et le test de la prédiction
  set.seed(i) #indice de reproductibilité
  indicesTrain = sample(1:DO, size = floor(0.7*DO)) #70% des données pour l'entraînement
  indicesTest = setdiff(1:DO, indicesTrain) #30% des données pour le test
  
  #Lancement de l'entraînement d'apprentissage et des prédictions
  modeleRF01 = randomForest(maPrediction ~ ., data=dtmD[indicesTrain,], ntree = 100, nPerme=11) #Paramètre à optimiser : nombre d'arbres, et nPerme
  predTestRF01 = predict(modeleRF01, newdata = dtmD[indicesTest,])
  
  #Evaluation des tests de la prédiction
  matConf = table(predTestRF01, dtmD$maPrediction[indicesTest])
  accuracies[i] = sum(diag(matConf)) / sum(matConf)
  print(c(i,accuracies[i])) # le calibrage augmente à mesure qu'on séloigne de 0.5
}


#Visualisation des résultats
modeleRF01 #caractéristiques de la matrice dense

class(modeleRF01)
write.csv(accuracies,"accuracies-100-11-010.csv")
save(modeleRF01,file = "modeleRF01-100-11-010.RData")



