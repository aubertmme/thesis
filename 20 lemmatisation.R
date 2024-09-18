#LIBRARIES
library(udpipe)
udpipe_download_model("french-gsd")
library(dplyr)

#ETAPE 0
##IMPORTER LES DONNEES
data <- read.csv("data_clean.csv", 
                 header = TRUE,encoding = "UTF-8")

##LEMMATISER 
texteAnalyse = udpipe(x = data, "french-gsd", trace = FALSE) #/!\ gros job
View(texteAnalyse)

##EXPORTER LES RESULTATS
tb <- data.frame(texteAnalyse)
write.csv(tb, "lem.csv" )

###pour éviter une énième lémmatisation
texteAnalyse <- read.csv("lem.csv", header = TRUE, sep = ",", quote = "\"", fill = TRUE, comment.char = "",encoding = "UTF-8")
View(texteAnalyse)

rownames(texteAnalyse) <- NULL #réindexer les lignes

##EXPORTER LES RESULTATS NETTOYES
tb <- data.frame(texteAnalyse)
write.csv(tb, "lem_clean.csv" )

##CONSOLIDER POUR LE PROTOTYPE
data_lem <- read.csv("lem_clean.csv", 
                 header = TRUE,encoding = "UTF-8")
data_lem2 <- subset(data_lem, select= c(doc_id,lemma))

nb_doc_id <- length(unique(data_lem2[["doc_id"]]))
doc_id_df <- c(unique(data_lem$doc_id))
nb_doc_id

data_lem3 <- data_lem2 %>%
  group_by(doc_id) %>%
  summarise(text = paste(c(lemma), collapse = " "))
data_lem3

##EXPORTER POUR LE PROTOTYPE
tb <- data.frame(data_lem3)
write.csv(tb, "lem2.csv" )

  
