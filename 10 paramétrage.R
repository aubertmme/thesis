#Chargement des librairies
library(stringr) #nettoyage des cha�nes de caract�re
library(NLP) #nettoyage du dataframe (n�cessaire � la librairie tm)
library(tm) #nettoyage du dataframe
library(SnowballC) #cr�ation de matrice

#Import des donn�es
#sans lemmatisation
data2 <- read.csv("data_clean.csv", header = TRUE, encoding = "UTF-8")
# OU
# avec lemmatisation
#data2 <- read.csv("lem2.csv", header = TRUE, encoding = "UTF-8")

#Cr�ation d'un corpus de documents � partir du dataframe
(ds <- DataframeSource(data2))
corpus_fiches <- Corpus(ds)
#inspect(corpus_fiches)
#meta(corpus_fiches) #imprime uniquement les metadata

#Nettoyage du corpus
corpus_fiches <- tm_map(corpus_fiches, content_transformer(tolower))
corpus_fiches <- tm_map(corpus_fiches, removeWords, stopwords("french"))
corpus_fiches <- tm_map(corpus_fiches, removePunctuation)
corpus_fiches <- tm_map(corpus_fiches, stripWhitespace)

#inspect(corpus_fiches)
#meta(corpus_fiches)
#options(max.print=10) #adaptation deslimitations du nombre des r�sultats pour les impressions de test
#summary(corpus_fiches) #v�rification des formats repris par le corpus

#Import des fiches nettoy�es dans une matrice termes-documents
dtm <- DocumentTermMatrix(corpus_fiches)
#inspect(dtm)

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
#inspect(dtm)
dtm <- removeCommonTerms(dtm, 0.1) #Pourcentage � adapter pour l'optimisation
#inspect(dtm)

#Transformation de la matrice dense en matrice creuse
dtmM = as.matrix(dtm)
dtmD = as.data.frame.matrix(dtmM)

#Export des donn�es pour stockage
write.table(dtmD, file="dtmd-010.txt", row.names=FALSE, sep="\t", quote=FALSE)
#print(dtmD[1:1])
#class(dtmD)

