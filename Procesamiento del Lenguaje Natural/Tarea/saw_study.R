#Se cargan las librerías necesarias
#install.packages("readtext")

library(tidyverse) # data manipulation
library(tm) # text mining
library(wordcloud) # word cloud generator
library(wordcloud2) # word cloud generator
library(tidytext) # text mining for word processing and sentiment analysis
library(reshape2) # reshapes a data frame
library(radarchart) # drawing the radar chart from a data frame
library(RWeka) # data mining tasks
library(knitr) # dynamic report generation
library(readtext)

#Se cargan los archivos
FILEDIR = "data_holo/PDF/"
filenames <- list.files(FILEDIR)
filenames <- gsub(".txt$", "", filenames)
txts <- readtext(FILEDIR)
order_levels <- c("Spice and Wolf  Vol1","Spice and Wolf  Vol2","Spice and Wolf  Vol3",
                  "Spice and Wolf  Vol4","Spice and Wolf  Vol5","Spice and Wolf  Vol6",
                  "Spice and Wolf  Vol7","Spice and Wolf  Vol8","Spice and Wolf  Vol9",
                  "Spice and Wolf  Vol10","Spice and Wolf  Vol11","Spice and Wolf  Vol12",
                  "Spice and Wolf  Vol13","Spice and Wolf  Vol14","Spice and Wolf  Vol15",
                  "Spice and Wolf  Vol16","Spice and Wolf  Vol17","Spice and Wolf  Vol18",
                  "Spice and Wolf  Vol19","Spice and Wolf  Vol20","Spice and Wolf  Vol21",
                  "Spice and Wolf s2 Vol1","Spice and Wolf s2 Vol2"
                  )
txts$doc_id <- ordered(txts$doc_id, levels = order_levels)
tryfunc <- function(x) gsub("â€™s", "", x)


#Se cargan los lexicom
bing <- read_csv("data/lexi/Bing.csv")
nrc <- read_csv("data/lexi/NRC.csv")
afinn <- read_csv("data/lexi/Afinn.csv")

#Limpieza del texto, se remueven todos los carácteres que no se quieren utilizar
cleanCorpus <- function(corpus){
  s.cor <- Corpus(VectorSource(corpus))
  corpus.tmp <- tm_map(s.cor, function(x) gsub("\\t", "  ", x))
  corpus.tmp <- tm_map(corpus.tmp, function(x) gsub("\\n", " ", x))
  corpus.tmp <- tm_map(corpus.tmp, function(x) gsub("—", " ", x))
  corpus.tmp <- tm_map(corpus.tmp, function(x) gsub("-", "", x))
  corpus.tmp <- tm_map(corpus.tmp, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
  v_stopwords <- c(stopwords("english"), c("thats","weve","hes","theres","ive","im",
                                           "will","can","cant","dont","youve","us",
                                           "youre","youll","theyre","whats","didnt",
                                           "chapter"))
  corpus.tmp <- tm_map(corpus.tmp, removeWords, v_stopwords)
  corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
  corpus.tmp <- tm_map(corpus.tmp, PlainTextDocument)
  return(corpus.tmp[[1]][1])}

for(i in 1:length(txts$doc_id)){
  txts[i,2] <- tryfunc(txts[i,2])
}

txts$text<- iconv(txts$text, 'utf-8', 'ascii', sub='')

for(i in 1:length(txts$doc_id)){
  txts[i,2] <- cleanCorpus(txts[i,2])
}

txts <- txts %>% arrange(doc_id)
####################Analisis de data#######################

frequentTerms <- function(text){
  
  s.cor.cl <- Corpus(VectorSource(text))
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm, 0.999)
  m <- as.matrix(s.tdm)
  word_freqs <- sort(rowSums(m), decreasing=TRUE)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  return(dm)
  
}

tokens <- txts %>%  
  mutate(text=as.character(txts$text)) %>%
  unnest_tokens(word, text)
jj <- frequentTerms(txts$text)
jj[1,2] = 5100
jj[2,2] = 5000
wordcloud2(jj, size = 0.8, figPath = "../Imagen4.png")



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
  scale_fill_brewer(palette="Paired") +
  labs(x="Sentiment", y="Frequency") +
  theme_bw() 

# Top 10 terms for each sentiment
sentiments %>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE)  +
  scale_fill_manual(values=replicate(10,"#CC6600")) +
  facet_wrap(~sentiment, scales="free_y") +
  labs(y="Frequency", x="Terms") +
  coord_flip() +
  theme_bw() 

# Sentiment analysis for the Top 10 characters with more dialogues
tokens %>%
  inner_join(nrc, "word") %>%
  count(doc_id, sentiment, sort=TRUE) %>%
  ggplot(aes(x=sentiment, y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  scale_fill_manual(values=replicate(10,"#CC6600")) +
  facet_wrap(~doc_id, scales="free_y") +
  labs(x="Sentiment", y="Frequency") +
  coord_flip() +
  theme_bw() 



############### CARGAR AHORA ###############

plot <- tokens %>% 
  inner_join(afinn, "word") %>% 
  count(value, sort=T) %>%
  ggplot(aes(x=value, y=n)) +
  geom_bar(stat="identity", aes(fill=n), show.legend=FALSE, width=0.5) +
  geom_label(aes(label=n)) +
  scale_fill_gradient(low="thistle1", high="thistle4") +
  scale_x_continuous(breaks=seq(-3, 4, 1)) +
  labs(x="Score", y="Frequency", title="Word distribution (AFINN lexicon)") +
  theme_bw() 
plot


plot <- tokens %>% 
  inner_join(afinn, "word") %>% 
  count(word, value, sort=T) %>% 
  mutate(contribution=n*value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  ggplot(aes(x=reorder(word, contribution), y=contribution, fill=n*value>0)) +
  geom_col(show.legend=FALSE) +
  labs(x="Words", y="Sentiment score * Number of ocurrences") +
  coord_flip() +
  theme_bw() +
  theme(legend.position="none")



tokens %>% 
  inner_join(afinn, "word") %>% 
  count(value, sort=T) 

# Most relevant words for each character
mystopwords <- data_frame(word=c(stopwords("english"), 
                                 c("thats","weve","hes","theres","ive","im",
                                   "will","can","cant","dont","youve","us",
                                   "youre","youll","theyre","whats","didnt")))

# Tokens without stopwords
top.chars.tokens <- txts %>%
  mutate(text=as.character(txts$text)) %>%
  unnest_tokens(word, text) %>%
  anti_join(mystopwords, by="word")

# Most relevant words for each novel
fff <- top.chars.tokens %>%
  count(doc_id, word) %>%
  bind_tf_idf(word, doc_id, n) %>%
  group_by(doc_id) %>% 
  arrange(desc(tf_idf)) %>%
  slice(1:10) %>%
  ungroup() %>%
  mutate(word2=factor(paste(word, doc_id, sep="__"), 
                      levels=rev(paste(word, doc_id, sep="__"))))%>%
  ggplot(aes(x=word2, y=tf_idf)) +
  geom_col(aes(fill=doc_id), show.legend=FALSE) +
  facet_wrap(~doc_id, scales="free_y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(y="tf–idf", x="Sentiment") +
  scale_x_discrete(labels=function(x) gsub("__.+$", "", x)) +
  coord_flip() +
  theme_bw()


# Most relevant words for each character
mystopwords <- data_frame(word=c(stopwords("english"), 
                                 c("thats","weve","hes","theres","ive","im",
                                   "will","can","cant","dont","youve","us",
                                   "youre","youll","theyre","whats","didnt")))

# Tokens without stopwords
top.chars.tokens <- txts[5,2,1:2] %>%
  arrange(doc_id) %>%
  slice_head(n = 10) %>%
  mutate(text=as.character(txts[1:10,1:2]$text)) %>%
  NGramTokenizer(Weka_control(min = 1, max = 1))

top.chars.tokens2 <- txts %>%
  arrange(doc_id) %>%
  slice_head(n = 10) %>%
  mutate(text=as.character(txts[1:10,1:2]$text)) %>%
  unnest_tokens(ngrams, text) %>% #############REVISAR
  filter(ngrams == "ars")

%>%
  anti_join(mystopwords, by="word") 

# Most relevant words for each novel
fff <- top.chars.tokens %>%
  count(doc_id, word) 

%>%
  bind_tf_idf(word, doc_id, n) %>%
  group_by(doc_id) %>% 
  arrange(desc(tf_idf)) %>%
  slice(1:10) %>%
  ungroup() %>%
  mutate(word2=factor(paste(word, doc_id, sep="__"), 
                      levels=rev(paste(word, doc_id, sep="__"))))

%>%
  ggplot(aes(x=word2, y=tf_idf)) +
  geom_col(aes(fill=doc_id), show.legend=FALSE) +
  facet_wrap(~doc_id, scales="free_y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(y="tf–idf", x="Sentiment") +
  scale_x_discrete(labels=function(x) gsub("__.+$", "", x)) +
  coord_flip() +
  theme_bw()


# Tokens without stopwords
top.chars.tokens <- txts %>%
  arrange(doc_id) %>%
  slice_tail(n = 13) %>%
  mutate(text=as.character(txts[10:23,1:2]$text))
asdasd <- txts[10:23,1:2]

top.chars.tokens <- txts[10:23,1:2] %>%
  mutate(text=as.character(txts[10:23,1:2]$text)) %>%
  unnest_tokens(word, text) %>%
  anti_join(mystopwords, by="word")

# Most relevant words for each novel
top.chars.tokens %>%
  count(doc_id, word) %>%
  bind_tf_idf(word, doc_id, n) %>%
  group_by(doc_id) %>% 
  arrange(desc(tf_idf)) %>%
  slice(1:10) %>%
  ungroup() %>%
  mutate(word2=factor(paste(word, doc_id, sep="__"), 
                      levels=rev(paste(word, doc_id, sep="__"))))%>%
  ggplot(aes(x=word2, y=tf_idf)) +
  geom_col(aes(fill=doc_id), show.legend=FALSE) +
  facet_wrap(~doc_id, scales="free_y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(y="tf–idf", x="Sentiment") +
  scale_x_discrete(labels=function(x) gsub("__.+$", "", x)) +
  coord_flip() +
  theme_bw()





cleanCorpus2 <- function(corpus){
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
  s.cor.cl <- cleanCorpus2(s.cor)
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


wordcloud2(frequentTerms(txts$text), size=0.5, figPath ="../Imagen4.png")






# Most relevant words for each character
mystopwords <- data_frame(word=c(stopwords("english"), 
                                 c("thats","weve","hes","theres","ive","im",
                                   "will","can","cant","dont","youve","us",
                                   "youre","youll","theyre","whats","didnt")))

# Tokens without stopwords
top.chars.tokens <- txts %>%
  arrange(doc_id) %>%
  slice_head(n = 10) %>%
  mutate(text=as.character(txts[1:10,1:2]$text)) %>%
  unnest_tokens(word, text) %>%
  anti_join(mystopwords, by="word")

# Most relevant words for each novel
top.chars.tokens %>%
  count(doc_id, word) %>%
  group_by(doc_id) %>% 
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ungroup() %>%
  mutate(word2=factor(paste(word, doc_id, sep="__"), 
                      levels=rev(paste(word, doc_id, sep="__"))))%>%
  ggplot(aes(x=word2, y=n)) +
  geom_col(aes(fill=doc_id), show.legend=FALSE) +
  facet_wrap(~doc_id, scales="free_y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(y="tf–idf", x="Sentiment") +
  scale_x_discrete(labels=function(x) gsub("__.+$", "", x)) +
  coord_flip() +
  theme_bw()



```{r message=FALSE, warning=FALSE,fig.dim = c(10, 8), out.width = "150%", out.height = "150%"}

mystopwords <- data_frame(word=c(stopwords("english"), 
                                 c("thats","weve","hes","theres","ive","im",
                                   "will","can","cant","dont","youve","us",
                                   "youre","youll","theyre","whats","didnt",
                                   "klass","aryes")))
top.chars.tokens2 <- txts %>%
  arrange(doc_id) %>%
  slice_head(n = 13) %>%
  mutate(text=as.character(txts[1:13,1:2]$text)) %>%
  unnest_tokens(ngrams, text) %>% #############REVISAR
  filter(ngrams == "ars")

top.chars.tokens <- txts %>%
  slice_tail(n = 11) %>%
  arrange(doc_id) %>%
  mutate(text=as.character(txts[13:23,1:2]$text)) %>%
  unnest_tokens(word, text) %>%
  filter(word == "ars")

%>%
  anti_join(mystopwords, by="word")


top.chars.tokens %>%
  count(doc_id, word) %>%
  bind_tf_idf(word, doc_id, n) %>%
  group_by(doc_id) %>% 
  arrange(desc(tf_idf)) %>%
  slice(1:10) %>%
  ungroup() %>%
  mutate(word2=factor(paste(word, doc_id, sep="__"), 
                      levels=rev(paste(word, doc_id, sep="__"))))%>%
  ggplot(aes(x=word2, y=tf_idf)) +
  geom_col(aes(fill=doc_id), show.legend=FALSE) +
  facet_wrap(~doc_id, scales="free_y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(y="tf–idf", x="Sentiment") +
  scale_x_discrete(labels=function(x) gsub("__.+$", "", x)) +
  coord_flip() +
  theme_bw()



