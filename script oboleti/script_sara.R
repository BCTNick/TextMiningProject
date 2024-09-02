library(tidyverse)
library(readtext)
library(tm)
library(topicmodels)
library(udpipe)

#normativa 1914
norm_1900 <- readtext("C:\\Users\\saran\\OneDrive\\Desktop\\progetto-text-mining\\Normativa\\normative.txt\\1900.txt")
View(norm_1900)
print(norm_1900$text)

#trasformo in un corpus
corpus_1900 <- Corpus(VectorSource(norm_1900$text))
inspect(corpus_1900)

#tolower
corpus_1900 <- tm_map(corpus_1900, content_transformer(tolower))
inspect(corpus_1900)

#togli a capo
rimuovi_acapo <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus_1900 <- tm_map(corpus_1900, rimuovi_acapo,"\n")
inspect(corpus_1900)

#rimuovo stopword
corpus_1900 <- tm_map(corpus_1900, removeWords, stopwords('italian'))
inspect(corpus_1900)

#rimuovo punteggiuatura
corpus_1900 <- tm_map(corpus_1900, removePunctuation)
inspect(corpus_1900)

#rimuovo spazi bianchi
corpus_1900 <- tm_map(corpus_1900, stripWhitespace)
inspect(corpus_1900)

#creo tdm


tdm_corpus_ns_1900 <- TermDocumentMatrix(corpus_1900)
inspect(tdm_corpus_ns_1900)

#lemmatizzazione

ud_model <- udpipe_download_model(language = "italian")
ud_model <- udpipe_load_model(ud_model$file_model)


lemm_1900<- udpipe_annotate(ud_model, x = as.character(norm_1900$text))
lemm_1900 <- as.data.frame(lemm_1900)
lemm_1900


#calcolo frequenze

# Trasformo la Term Document Matrix in un dataframe
tdm_corpus_1900<- as.data.frame(as.matrix(tdm_corpus_ns_1900))

# Calcola la frequenza dei termini
frequencies_1900 <- rowSums(tdm_corpus_1900)

# Ordina le frequenze in ordine decrescente
sorted_frequencies_1900 <- sort(frequencies_1900, decreasing = TRUE)

# Visualizza le prime N parole più frequenti
N <- 20
head(sorted_frequencies_1900, N)
head(sorted_frequencies_1900)


#lda
library(topicmodels)

# Creazione della Document-Term Matrix
dtm_1900 <- DocumentTermMatrix(corpus_1900, control = list(weighting = weightTf))

# Impostiamo il numero di topic 
num_topics <- 3
lda_model_1900 <- LDA(dtm_1900, k = 3, control = list(seed = 1234))
# Visualizzazione dei risultati
topics_1900<- terms(lda_model_1900, 6) # Mostra i primi 6 termini per topic
print(topics_1900)
# Puoi anche esaminare la distribuzione dei topic nei documenti
topic_distribution_1900<- posterior(lda_model_1900)$topics_1900
print(topic_distribution_1900)
# Estrarre i topic probabili per ciascun documento
topic_probabilities_1900 <- topics(lda_model_1900, 1)
print(topic_probabilities_1900)
topics_1900


#rappresentazione grafica

library(ggplot2)
library(dplyr)
# Ottenere i termini per ciascun topic
topic_terms_1900 <- as.data.frame(terms(lda_model_1900, 6))
names(topic_terms_1900) <- paste("Topic", 1:num_topics)
# Convertire in formato lungo per ggplot
topic_terms_long_1900<- pivot_longer(topic_terms_1900, cols = everything(), 
                                     names_to = "Topic", values_to = "Term")
# Calcolare le frequenze di ciascun termine nel corpus
term_frequencies_1900 <- colSums(as.matrix(dtm_1900))
topic_terms_long_1900 <- topic_terms_long_1900 %>%
  mutate(Frequency = sapply(Term, function(x) term_frequencies_1900[x]))
# Creare il grafico
ggplot(topic_terms_long_1900, aes(x = reorder(Term, -Frequency), y = Frequency, fill = Topic)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Top Terms in Each Topic 1900", x = "Terms", y = "Frequency") +
  facet_wrap(~ Topic, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))