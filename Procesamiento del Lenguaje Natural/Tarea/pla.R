
############### Se cargan librerías ##################

#install.packages("RWeka") # install RWeka package
#install.packages("tidyverse")
#install.packages("tm")
#install.packages("RColorBrewer")

library(tidyverse) # data manipulation
library(tm) # text mining
library(wordcloud) # word cloud generator
library(wordcloud2) # word cloud generator
library(tidytext) # text mining for word processing and sentiment analysis
library(reshape2) # reshapes a data frame
library(radarchart) # drawing the radar chart from a data frame
library(RWeka) # data mining tasks
library(knitr) # dynamic report generation

ep4 <- read.table("data/Text_files/SW_EpisodeIV.txt")
ep5 <- read.table("data/Text_files/SW_EpisodeV.txt")
ep6 <- read.table("data/Text_files/SW_EpisodeVI.txt")


#Se transforman los df a factores
ep4[sapply(ep4, is.character)] <- lapply(ep4[sapply(ep4, is.character)], 
                                         as.factor)

ep5[sapply(ep5, is.character)] <- lapply(ep5[sapply(ep5, is.character)], 
                                         as.factor)


ep6[sapply(ep6, is.character)] <- lapply(ep6[sapply(ep6, is.character)], 
                                       as.factor)



bing <- read_csv("data/lexi/Bing.csv")
nrc <- read_csv("data/lexi/NRC.csv")
afinn <- read_csv("data/lexi/Afinn.csv")

# Text transformations, la entrada tiene que ser un corpus
cleanCorpus <- function(corpus){
  #Las funciones tm_map son del paquete tm
  #Función para remover la puntuación del corpus
  corpus.tmp <- tm_map(corpus, removePunctuation)
  #Remueve los espacios en blanco extras
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  #Transforma las mayusculas a minusculas
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
  #stopwords("english") es un paquete con muchas palabras en ingles,
  #además se agregan otras palabras sin significado por si solas
  v_stopwords <- c(stopwords("english"), c("thats","weve","hes","theres","ive","im",
                                           "will","can","cant","dont","youve","us",
                                           "youre","youll","theyre","whats","didnt"))
  #Se remueven las palabras escritas en el array v_stopwords
  corpus.tmp <- tm_map(corpus.tmp, removeWords, v_stopwords)
  #Se remueven los números
  corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
  #Se retorna el corpus limpio
  return(corpus.tmp)
  
}



# Most frequent terms 
frequentTerms <- function(text){
  #La funcion Corpus del paquete tm crea  y computa el corpus
  #Mientras que la función VectorSource interpreta cada elemento de 
  #text como un doccumento
  s.cor <- Corpus(VectorSource(text))
  #Se aplica la primera función creada cleanCorpus
  s.cor.cl <- cleanCorpus(s.cor)
  #Del paquete tm transforma el documento a una matriz
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  #Remueve elementos que aparecen en pequeñas cantidades
  s.tdm <- removeSparseTerms(s.tdm, 0.999)
  #Transforma los datos a una matriz de R
  m <- as.matrix(s.tdm)
  #Se suma por fila para obtener la frecuencia
  #Como está ordenado como matriz se ordena según la frecuencia más alta
  word_freqs <- sort(rowSums(m), decreasing=TRUE)
  #Se crea un dataframe con estas frecuencias
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  return(dm)
  
}


# Define bigram tokenizer 
tokenizer  <- function(x){
  #Del pack RWeka, para tokenizar, se da el parámetro 2 para
  #crear los bigramas
  NGramTokenizer(x, Weka_control(min=2, max=2))
  
}


# Most frequent bigrams 
frequentBigrams2 <- function(text){
  #Se crea un volatile corpus, que se elimina al final de la ejecución
  s.cor <- VCorpus(VectorSource(text))
  #Se utiliza la primera función que se creó
  s.cor.cl <- cleanCorpus(s.cor)
  #Se crea la matriz, pero con los tokens que ordena que sean bigramas
  s.tdm <- TermDocumentMatrix(s.cor.cl, control=list(tokenize=tokenizer))
  #Se remueven los bigramas pocos frecuentes
  s.tdm <- removeSparseTerms(s.tdm, 0.999)
  #Se transforma a matriz de R
  m <- as.matrix(s.tdm)
  #Se ordenan por frecuencia
  word_freqs <- sort(rowSums(m), decreasing=TRUE)
  #Se crea el dataframe con la información
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  return(dm)
}

####################EPISODIO 4#######################
library(devtools)
devtools::install_github("lchiffon/wordcloud2")


#Para contar la cantidad de dialogos 
length(ep4$dialogue)
#Para contar la cantidad de personajes
length(levels(ep4$character))

# Top 20 characters with more dialogues 
top.ep4.chars <- as.data.frame(sort(table(ep4$character), decreasing=TRUE))[1:20,]

# Visualization 
ggplot(data=top.ep4.chars, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="#56B4E9", colour="black") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x="Character", y="Number of dialogues")

wordcloud2(frequentTerms(ep4$dialogue), size=0.5, figPath ="../Imagen4.png")

# Most frequent bigrams
ep4.bigrams <- frequentBigrams(ep4$dialogue)[1:20,]
ggplot(data=ep4.bigrams, aes(x=reorder(word, -freq), y=freq)) +  
  geom_bar(stat="identity", fill="chocolate2", colour="black") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x="Bigram", y="Frequency")

########### Analisis de sentimientos ############
trilogy <- rbind(ep4, ep5, ep6)

#Se crea un dataframe ordenado donde en la primera columna son
#los personajes y en la segunda la palabra tokenizada
tokens <- trilogy %>%  
  mutate(dialogue=as.character(trilogy$dialogue)) %>%
  unnest_tokens(word, dialogue)

# Positive and negative words
tokens %>%
  inner_join(get_sentiments("bing")) %>% #Se agrega si son palabras positivas o negativas
  count(word, sentiment, sort=TRUE) %>% #Se realiza el conteo de las palabras
  acast(word ~ sentiment, value.var="n", fill=0) %>% #Separa las columnas según es positivo o negativo
  comparison.cloud(colors=c("#F8766D", "#00BFC4"), max.words=100) #Se crea la nube de palabras


############# Analisis de sentimientos por frecuencia #########



# Sentiments and frequency associated with each word  
sentiments <- tokens %>% 
  inner_join(nrc, "word") %>%
  count(word, sentiment, sort=TRUE) 

# Frequency of each sentiment
ggplot(data=sentiments, aes(x=reorder(sentiment, -n, sum), y=n)) + 
  geom_bar(stat="identity", aes(fill=sentiment), show.legend=FALSE) +
  labs(x="Sentiment", y="Frequency") +
  theme_bw() 

# Top 10 terms for each sentiment
sentiments %>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free_y") +
  labs(y="Frequency", x="Terms") +
  coord_flip() +
  theme_bw() 


# Sentiment analysis for the Top 10 characters with more dialogues
tokens %>%
  filter(character %in% c("LUKE","HAN","THREEPIO","LEIA","VADER",
                          "BEN","LANDO","YODA","EMPEROR","RED LEADER")) %>%
  inner_join(nrc, "word") %>%
  count(character, sentiment, sort=TRUE) %>%
  ggplot(aes(x=sentiment, y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  facet_wrap(~character, scales="free_x") +
  labs(x="Sentiment", y="Frequency") +
  coord_flip() +
  theme_bw() 


# Most relevant words for each character
top.chars.tokens %>%
  count(character, word) %>%
  bind_tf_idf(word, character, n) %>%
  group_by(character) %>% 
  arrange(desc(tf_idf)) %>%
  slice(1:10) %>%
  ungroup() %>%
  mutate(word2=factor(paste(word, character, sep="__"), 
                      levels=rev(paste(word, character, sep="__"))))%>%
  ggplot(aes(x=word2, y=tf_idf)) +
  geom_col(aes(fill=character), show.legend=FALSE) +
  facet_wrap(~character, scales="free_y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(y="tf–idf", x="Sentiment") +
  scale_x_discrete(labels=function(x) gsub("__.+$", "", x)) +
  coord_flip() +
  theme_bw()