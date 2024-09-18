#Chargement des librairies
library(stringr) #nettoyage des chaînes de caractère

#Import des données
options(max.print=1000)
voc_fh <- read.csv2("voc-fh-2.csv", header = TRUE, encoding = "UTF-8")
class(voc_fh)
results <- read.csv("resultatsRF04-300-11.csv", header = TRUE, encoding = "UTF-8")
print(results)

#nettoyage
##supprimer les accents
voc_fh$X.U.FEFF.vocf <- iconv(voc_fh$X.U.FEFF.vocf, from="UTF-8", to="ASCII//TRANSLIT")
voc_fh$voch <- iconv(voc_fh$voch, from="UTF-8", to="ASCII//TRANSLIT")
##supprimer les majuscules
voc_fh$X.U.FEFF.vocf <- tolower(voc_fh$X.U.FEFF.vocf)
voc_fh$voch <- tolower(voc_fh$voch)
print(voc_fh)
##renommer les colonnes
colnames(voc_fh)[1] <- "vocf"
colnames(voc_fh)[2] <- "voch"
voc_fh


#comparer les listes de termes
## Fonction pour trouver des mots similaires entre deux colonnes
find_similar_words <- function(col1, col2) {
  similar_words <- character()
  
  for (word1 in col1) {
    for (word2 in col2) {
      if (str_detect(word2, paste0("^", word1, ".*"))) {
        similar_words <- c(similar_words, word1)
        break
      }
    }
  }
## Créer un dataframe avec les résultats
  result_df <- data.frame(similar_words = unique(similar_words))
  return(result_df)
}
## Appel de la fonction
voc_f <- find_similar_words(voc_fh$vocf, results$variable)
voc_h <- find_similar_words(voc_fh$voch, results$variable)
print(voc_f)
print(voc_h)
