## @knitr sostituzione-apostrofi

df <- df |> 
  mutate(text = str_replace_all(text, "[\'’](?!\\s)", "' "))








# LEMMATIZZAZIONE ---------------------------------------------------------

##trasformuamo i tokens in dataframe
df_pulito <- vapply(quanteda_nosw_tokens, paste, FUN.VALUE = character(1), collapse = " ")  |> 
  corpus() |> 
  tidy()

df_pulito <- data.frame(
  doc_id = docnames(corpus),
  text = df_pulito$text
)

##usiamo udpipe
ud_model <- udpipe_download_model(language = "italian")
ud_model <- udpipe_load_model(ud_model$file_model)

df_pieno<- udpipe_annotate(ud_model, x = as.character(df_pulito$text)) |> 
  as.data.frame()

##ritrasformiamo il dataframe in tokens solo con i lemmi
doc_ids <- unique(df_pieno$doc_id)
name_mapping <- setNames(docnames(corpus), doc_ids)

df_pieno <- df_pieno  |> 
  mutate(document_name = name_mapping[doc_id])

lemma <- df_pieno |> 
  group_by(document_name)  |> 
  summarize(text = paste(lemma, collapse = " "))  

quanteda_nosw_tokens <- lemma |> 
  corpus() |> 
  tokens()

quanteda_nosw_tokens <- quanteda_nosw_tokens |> 
  tokens_remove("na")





###topic modeling con topic models

library(topicmodels)

# Ora puoi eseguire il modello LDA
num_topics <- 4
lda_model_1900 <- LDA(quanteda_dfm, k = num_topics, control = list(seed = 1234))



# Per visualizzare i termini principali di ciascun topic
terms(lda_model_1900, 15)  # Mostra i 10 termini principali per ciascun topic

# Per visualizzare la distribuzione dei topic nei documenti
topics(lda_model_1900)  # Mostra il topic principale per ciascun documento

# Per visualizzare la distribuzione dei termini nei topic
library(tidytext)
lda_topics <- tidy(lda_model_1900, matrix = "beta")
lda_topics
