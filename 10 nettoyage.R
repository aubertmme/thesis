#Chargement des librairies
library(stringr) #nettoyage des cha�nes de caract�re
library(NLP) #nettoyage du dataframe (n�cessaire � la librairie tm)
library(tm) #nettoyage du dataframe
library(SnowballC) #cr�ation de matrice

#Import des donn�es
data <- read.csv2("traitement-2024-2.csv", header = TRUE, encoding = "UTF-8")
options(max.print=2) #limitations du nombre des r�sultats pour les impressions de test
#data

#Concat�nation des cha�nes de caract�re dans un dataframe
colnames(data)[1] <- "doc_id"
data$text = paste(data$titre, data$chapeau, data$objectif, data$programme, data$pre_requis_liste, data$pre_requis_libre, data$competences_requises, data$remarques)
#data$text
data2 = subset(data, select = c(doc_id, text))
data_sexe = subset(data, select = c(doc_id, sexe))

class(data2) #v�rification du format des donn�es
options(max.print=2) #limitations du nombre des r�sultats pour les impressions de test
print(data2[1]) #v�rification de la transformation en dataframe
print(data2[2]) #v�rification de la transformation en dataframe

#Nettoyage des cha�nes de caract�re concat�n�es dans le dataframe
##supprimer le HTML et les caract�res parasites pour la suite du traitement
data2$text = str_replace_all(data2$text,"</?[a-z]+/?>"," ") 
##supprimer le bruit
data2$text = str_replace_all(data2$text,"\\\n"," ") #sauts de ligne
#data2$text = str_replace_all(data2$text,"[^ ]*[<>/][^ ]*"," ") #HTML
data2$text = str_replace_all(data2$text,"[^'[:^punct:]]"," ")#ponctuation
data2$text <- iconv(data2$text, from="UTF-8", to="ASCII//TRANSLIT")#supprimer les accents
#data2$text = str_replace_all(data2$text,"<br />"," ") #correction apr�s premi�re visualisation des donn�es : supprimer les tags html restants
print(data2$text)

##Export du texte pour la lemmatisation
print(data2)
write.csv(data2,"data_clean.csv")

nb_doc_id <- length(unique(data2[["doc_id"]]))
nb_doc_id
