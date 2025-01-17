#Librairies
library(randomForest)

#Import des donn�es nettoy�es et param�tr�es
dtmD <- read.table(file="dtmd-010.txt", as.is = TRUE, header = TRUE, sep = "\t", row.names = NULL)
options(max.print=10) #limitations du nombre des r�sultats pour les impressions de test

#Ajout � la matrice de la colonne des profils
data <- read.csv2("traitement-2024-2.csv", header = TRUE, encoding = "UTF-8")
colnames(data)[1] <- "doc_id"
data_sexe = subset(data, select = c(doc_id, sexe))

dtmD[1]

dtmD = transform(dtmD, maPrediction = data_sexe$sexe)
dtmD$maPrediction = factor(dtmD$maPrediction)
#is.factor(dtmD$maPrediction)

#dim(dtmD)
#dtmD = dtmD[1:15811,1913:2013]#test sur les 100 derniers enregistrements => TEST


#Param�trage du prototype
K = 10 #validation crois�e
DO = dim(dtmD)[1]
accuracies = array(NA, K)

#Lancement de la boucle de Random forest
for (i in 1:K){ 
  #Segmentation des donn�es pour l'entra�nement et le test de la pr�diction
  set.seed(i) #indice de reproductibilit�
  indicesTrain = sample(1:DO, size = floor(0.7*DO)) #70% des donn�es pour l'entra�nement
  indicesTest = setdiff(1:DO, indicesTrain) #30% des donn�es pour le test
  
  #Lancement de l'entra�nement d'apprentissage et des pr�dictions
  modeleRF01 = randomForest(maPrediction ~ ., data=dtmD[indicesTrain,], ntree = 100, nPerme=11) #Param�tre � optimiser : nombre d'arbres, et nPerme
  predTestRF01 = predict(modeleRF01, newdata = dtmD[indicesTest,])
  
  #Evaluation des tests de la pr�diction
  matConf = table(predTestRF01, dtmD$maPrediction[indicesTest])
  accuracies[i] = sum(diag(matConf)) / sum(matConf)
  print(c(i,accuracies[i])) # le calibrage augmente � mesure qu'on s�loigne de 0.5
}


#Visualisation des r�sultats
modeleRF01 #caract�ristiques de la matrice dense

class(modeleRF01)
write.csv(accuracies,"accuracies-100-11-010.csv")
save(modeleRF01,file = "modeleRF01-100-11-010.RData")



