# librerie ----------------------------------------------------------------
## @knitr carico-le-librerie

library(tidyverse)
library(dplyr)
library(tidytext)
library(readtext)
library(koRpus)
library(koRpus.lang.it)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(hunspell)
library(ggplot2)
library(udpipe)
library(topicmodels)
library(seededlda)


## @knitr importazione
# IMPORTAZIONE -----------------------------------------------------------------

# Lettura dei documenti
df <- readtext('data/1866-72/*.rtf')



# LEMMATIZZAZIONE ---------------------------------------------------------


# Dividere il dataframe in una lista di dataframes basata sulla colonna `doc_id`
df_list <- split(df, df$doc_id)

# Ciclo for per lemmatizzare ciascun dataframe e aggiungere i lemmi al dataframe finale
lemmatized_df_list <- list()  # Lista per i dataframe lemmatizzati

for (doc in names(df_list)) {
  # Lemmatizzazione del testo
  tagged.sep <- treetag(
    file = df_list[[doc]]$text, 
    format = "obj",                 
    lang = "it",
    doc_id = df_list[[doc]]$doc_id,
    sentc.end = c(".", "!", "?"),
    treetagger = "manual",
    TT.options = list(
      path = "C:/TreeTagger",         
      preset = "it"
    )
  )
  
  # Estrazione del dataframe lemma_singolo
  lemma_singolo <- tagged.sep@tokens
  
  lemmatized_df_list[[doc]] <- lemma_singolo
}

# Unire tutti i dataframe dei lemmi singoli in un unico dataframe
lemmatized_df <- do.call(rbind, lemmatized_df_list)


df_lemma <- lemmatized_df |> 
  group_by(doc_id)  |> 
  summarize(text = paste(lemma, collapse = " "))  

## @knitr correzione-ortografia
# CORREZIONE ORTOGRAFICA --------------------------------------------------

parole_sbagliate <- df_lemma$text |> 
  hunspell(dict = dictionary(lang = "it_IT")) |> 
  unlist()

frequenze_parole_sbagliate <- table(parole_sbagliate) |> 
  sort(decreasing = T) 

head(frequenze_parole_sbagliate, 20) 



## @knitr corpus
# CORPUS QUANTEDA ---------------------------------------------------------

corpus <- corpus(df_lemma)

head(summary(corpus))


## @knitr parsing
# PARSING -----------------------------------------------------------------

quanteda_options(language_stemmer = "italian")


## segmentazione dei testi con quanteda

quanteda_sentences <- corpus_reshape(corpus)


## @knitr tokenizzazione-con-quanteda

quanteda_tokens<-corpus |> 
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_symbols = TRUE)


## @knitr stopwords 

##rimuovi stopwords con quanteda

quanteda_nosw_tokens <- quanteda_tokens |> 
  tokens_remove(stopwords("it")) 


## @knitr stopwords2 

#rimuovo parole sbagliate
quanteda_nosw_tokens <- quanteda_nosw_tokens |> 
  tokens_remove(c(parole_sbagliate,
                "@card",
                "essere",
                "fare",
                "ogni",
                "suddetto",
                "ciascuno",
                "ciascun",
                "essi",
                "medesimo"))

#rimuovo parole con meno di 4 lettere
quanteda_nosw_tokens <- tokens_keep(quanteda_nosw_tokens, min_nchar = 4)


## @knitr sottrazione 

print(sum(ntoken(quanteda_tokens))-sum(ntoken(quanteda_nosw_tokens))) ##quante sw sono state eliminate


## @knitr dfm 
# ESPLORAZIONE CORPUS -----------------------------------------------------

##document-feature matrix
quanteda_dfm <- dfm(quanteda_nosw_tokens)

head(quanteda_dfm)


## @knitr vocabolario 

quanteda_vocabolario <- quanteda_dfm |> 
  textstat_frequency() 

head(quanteda_vocabolario, 30)

## @knitr fcm 

##feature co-occurrence matrix
quanteda_fcm <- fcm(quanteda_dfm)
dim(quanteda_fcm)
quanteda_fcm

## @knitr feature-network 

feat <- names(topfeatures(quanteda_dfm, 30))
fcm50 <- fcm_select(quanteda_fcm, pattern = feat, selection = "keep")

textplot_network(fcm50, 
                 min_freq = 0.8)


## @knitr ANALISI DELLE CORRISPONDENZE ----------------------------------------------------------

tmod_ca <- textmodel_ca(quanteda_dfm)
textplot_scale1d(tmod_ca)


dat_ca <- data.frame(dim1 = coef(tmod_ca, doc_dim = 1)$coef_document, 
                     dim2 = coef(tmod_ca, doc_dim = 2)$coef_document)
dat_ca

plot(1, xlim = c(-1.5, 3.5), ylim = c(-2, 2.4), type = "n", xlab = "Dimension 1", ylab = "Dimension 2")
grid()

color_66_67 <- colorRampPalette(c("lightblue", "darkblue"))(nrow(dat_ca) - 2)
color_72 <- colorRampPalette(c("lightgreen", "green"))(2)

for (i in 1:(nrow(dat_ca) - 2)) {
  points(dat_ca$dim1[i], dat_ca$dim2[i], col = color_66_67[i], pch = 16)
}

for (i in (nrow(dat_ca) - 2 + 1):nrow(dat_ca)) {
  points(dat_ca$dim1[i], dat_ca$dim2[i], col = color_72[i - (nrow(dat_ca) - 2)], pch = 16)
}

legend("topright", legend = c("1866-67", "1872"),
       fill = c(color_66_67[5], color_72[1]), bty = "n",
       title = "Anni")


# TOPIC MODELING ----------------------------------------------------------

top20dfm <- quanteda_dfm |> 
  dfm_trim(min_termfreq = 0.7, 
           max_docfreq = 0.5,
           termfreq_type = "quantile", docfreq_type = "prop")


tmod_lda <- textmodel_lda(top20dfm, k = 4)

terms(tmod_lda, 10)

head(topics(tmod_lda), 20)

# assign topic as a new document-level variable
top20dfm$topic <- topics(tmod_lda)

# cross-table of the topic frequency
table(top20dfm$topic)


##grafici

top10 <- terms(tmod_lda, n = 10) |>
  as_tibble() |>
  pivot_longer(cols=starts_with("t"),
               names_to="topic", values_to="word")


phi <- tmod_lda$phi |>
  as_tibble(rownames="topic")  |>
  pivot_longer(cols=c(-topic))

top10phi <- top10 |>
  left_join(y=phi, by=c("topic", "word"="name")) 

sort_facets <- function(df, cat_a, cat_b, cat_out, ranking_var){
  res <- df |>
    mutate({{cat_out}}:=factor(paste({{cat_a}}, {{cat_b}}))) |>
    mutate({{cat_out}}:=reorder({{cat_out}}, rank({{ranking_var}})))
  
  return(res)  
}

dd2 <- sort_facets(top10phi, topic, word, category2, value)

gpl <- ggplot(dd2, aes(y=category2, x=value)) +
  geom_bar(stat = "identity") +
  facet_wrap(. ~ topic, scales = "free_y", nrow=3) +
  scale_y_discrete(labels=dd2$word, breaks=dd2$category2,
  )+
  xlab("Probability")+
  ylab(NULL)

gpl 

# GRAFICI -----------------------------------------------------------------

## @knitr grafico-tokens-types

summary(corpus) |> 
  ggplot(aes(x = Text, y = Tokens, group = 1)) +
  geom_line(aes(lty = "Tokens")) +
  geom_line(aes(y = Types, lty = "Types"))+
  geom_line(aes(y = Sentences, lty = "Sentences"))+
  scale_x_discrete(labels = NULL) +
  labs(x = "Leggi 1800", y = "", lty =NULL)
##aggiungere colori alle linee


## @knitr barplot 

quanteda_dfm |> 
  topfeatures() |> 
  as_tibble(rownames = "Forme") |> 
  ggplot(aes(reorder(Forme, -value), value)) +
  geom_col(fill = "darkorange") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs (x = NULL, y = NULL,
        caption = "quanteda")

## @knitr wordcloud 

quanteda_dfm |> 
  textplot_wordcloud(rotation = 0.25, 
                     color = rev(RColorBrewer::brewer.pal(4, "RdBu")))



# CLASSIFICATION ----------------------------------------------------------

library(caret)
