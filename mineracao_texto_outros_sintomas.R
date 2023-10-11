
#library(gridExtra)
library(tidyverse)
library(magrittr)
library(dplyr)
library(readr)
library(ggthemes)
library(ggplot2)

install.packages("C:\\Users\\luiza\\Downloads\\wordcloud_2.6.zip", repos = NULL, type = "source")

#Mineração

#install.packages("wordcloud")
#install.packages("tm")

library(wordcloud)
library(tm)

#dados de 2020
dados_gerais <- read_csv("dados/dadosGerais2020.csv")
#dados de 2021
dados_gerais2021 <- read_csv("dados/dadosGerais2021.csv")

df <- rbind(dados_gerais, dados_gerais2021)

view(df)
glimpse(df)

df$OUTROS <- gsub("ASSINTOMATICA", "ASSINTOMATICO", df$OUTROS)
df$OUTROS <- gsub("ASSINTOMÁTICA", "ASSINTOMATICO", df$OUTROS)
df$OUTROS <- gsub("Assintomatica", "ASSINTOMATICO", df$OUTROS)
df$OUTROS <- gsub("assintomatica", "ASSINTOMATICO", df$OUTROS)
df$OUTROS <- gsub("Assintomática", "ASSINTOMATICO", df$OUTROS)
df$OUTROS <- gsub("assintomática", "ASSINTOMATICO", df$OUTROS)
sintomas <- df$OUTROS

view(sintomas)

print(sintomas)

#Produzir corpus
sintomas_text <- VCorpus(VectorSource(sintomas))
print(sintomas_text)

#Tratamento dos caracteres especiais

sintomas_text <- tm_map(sintomas_text,
                        content_transformer(function(x) iconv(x, to = 'UTF-8', sub = 'byte')))

#tratamento cara caixa baixa
sintomas_text <- tm_map(sintomas_text, content_transformer(tolower))

#tirar pontuação
sintomas_text <- tm_map(sintomas_text, removePunctuation)

# tirar stop words
sintomas_text <- tm_map(sintomas_text,removeWords, stopwords("portuguese"))

#nuvem de palavras




#Primeira visualiza?ao
wordcloud(sintomas_text,min.freq=2,max.words=100)
formatacao <- brewer.pal(8,"Dark2")
wordcloud(sintomas_text,min.freq=2,max.words=100, random.order=T, colors=formatacao)




library(RColorBrewer)
library(cluster)   
library(fpc)

#####
#Limpeza do texto com a Document Term Matrix
sintomas_dtm <- DocumentTermMatrix(sintomas_text)   
print(sintomas_dtm)

#sintomas_frequencia <- colSums(as.matrix(sintomas_dtm))   
#length(sintomas_frequencia) 
#tail(bitcoin_frequencia,10)

#Removendo termos espar?os
sintomas_dtms <- removeSparseTerms(sintomas_dtm, 0.98) 
sintomas_dtm

sintomas_frequencia <- colSums(as.matrix(sintomas_dtms))   
length(sintomas_frequencia) 

sintomas_frequencia <- sort(colSums(as.matrix(sintomas_dtms)), decreasing=TRUE) 
sintomas_frequencia

#Convertendo a matriz de frequencia em dataframe para o plot
sintomas_plot <- data.frame(word=names(sintomas_frequencia), freq=sintomas_frequencia)  
view(sintomas_plot)

#Criando o grafico
grafico <- ggplot(subset(sintomas_plot, sintomas_frequencia>800), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Grafico de barras com os termos mais frequentes") +
  labs(y="Frequencia", x = "Termos")
grafico  


#Removendo palavras especificas e limpando novamente o corpus
sintomas_text <- tm_map(sintomas_text, removeWords, c("dor"))
sintomas_dtms <- removeSparseTerms(DocumentTermMatrix(sintomas_text) , 0.98) 


sintomas_frequencia <- colSums(as.matrix(sintomas_dtms))   
length(sintomas_frequencia) 

sintomas_frequencia <- sort(colSums(as.matrix(sintomas_dtms)), decreasing=TRUE) 
sintomas_frequencia

#Convertendo a matriz de frequencia em dataframe para o plot
sintomas_plot <- data.frame(word=names(sintomas_frequencia), freq=sintomas_frequencia)  
sintomas_plot
sintomas_plot$word <- gsub("paladar", "perda ou diminuição do paladar", sintomas_plot$word)
sintomas_plot$word <- gsub("corpo", "dor no corpo", sintomas_plot$word)

#Criando o grafico
grafico <- ggplot(subset(sintomas_plot, sintomas_frequencia>800), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Outros sintomas mais frequentes") +
  labs(y="Frequencia", x = "Sintomas")
grafico   

#wordcloud(names(sintomas_frequencia),sintomas_frequencia,min.freq=2,max.words=150, random.order=T, colors=formatacao)
