#Chargement des librairies
library(stringr) #nettoyage des cha�nes de caract�re
library(NLP) #nettoyage du dataframe (n�cessaire � la librairie tm)
library(tm) #nettoyage du dataframe
library(SnowballC) #cr�ation de matrice

#Import des donn�es
data <- read.csv2("traitement-2024-2.csv", header = TRUE, encoding = "UTF-8")
options(max.print=2) #limitations du nombre des r�sultats pour les impressions de test
data

#Concat�nation des cha�nes de caract�re dans un dataframe
colnames(data)[1] <- "doc_id"
data$text = paste(data$titre, data$chapeau, data$objectif, data$programme, data$pre_requis_liste, data$pre_requis_libre, data$competences_requises, data$remarques)
data$text
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
data2$text = str_replace_all(data2$text,"[^ ]*[<>/][^ ]*"," ") #HTML
data2$text = str_replace_all(data2$text,"[^'[:^punct:]]"," ")#ponctuation
data2$text <- iconv(data2$text, from="UTF-8", to="ASCII//TRANSLIT")#supprimer les accents
data2$text = str_replace_all(data2$text,"<br />"," ") #correction apr�s premi�re visualisation des donn�es : supprimer les tags html restants
print(data2$text)

##Export du texte pour la lemmatisation
print(data2)
write.csv(data2,"data_clean.csv")

#Cr�ation d'un corpus de documents � partir du dataframe
(ds <- DataframeSource(data2))
corpus_fiches <- Corpus(ds)
inspect(corpus_fiches)
meta(corpus_fiches) #imprime uniquement les metadata

#Nettoyage du corpus
corpus_fiches <- tm_map(corpus_fiches, content_transformer(tolower))
corpus_fiches <- tm_map(corpus_fiches, removeWords, stopwords("french"))
corpus_fiches <- tm_map(corpus_fiches, removePunctuation)
corpus_fiches <- tm_map(corpus_fiches, stripWhitespace)

inspect(corpus_fiches)
meta(corpus_fiches)
options(max.print=10) #adaptation deslimitations du nombre des r�sultats pour les impressions de test
summary(corpus_fiches) #v�rification des formats repris par le corpus

#Import des fiches nettoy�es dans une matrice termes-documents
dtm <- DocumentTermMatrix(corpus_fiches)
inspect(dtm)

##Param�trage de la raret� des mots
removeCommonTerms <- function (x, pct) 
{
  stopifnot(inherits(x, c("DocumentTermMatrix", "TermDocumentMatrix")), 
            is.numeric(pct), pct > 0, pct < 1)
  m <- if (inherits(x, "DocumentTermMatrix")) 
    t(x)
  else x
  t <- table(m$i) < m$ncol * (pct)
  termIndex <- as.numeric(names(t[t]))
  if (inherits(x, "DocumentTermMatrix")) 
    x[, termIndex]
  else x[termIndex, ]
}
inspect(dtm)
dtm <- removeCommonTerms(dtm, 0.1) #Pourcentage � adapter pour l'optimisation
inspect(dtm)

#Transformation de la matrice dense en matrice creuse
dtmM = as.matrix(dtm)
dtmD = as.data.frame.matrix(dtmM)

#Export des donn�es pour stockage
write.table(dtmD, file="dtmd.txt", row.names=FALSE, sep="\t", quote=FALSE)
print(dtmD[1:1])
class(dtmD)

