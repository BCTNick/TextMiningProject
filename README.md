# Text Mining Analysis of Italian Laws during the Third War of Independence

## Project Overview
This project focuses on text mining and natural language processing (NLP) to analyze Italian laws from the Third War of Independence (1866). The analysis uncovers linguistic patterns, and topics of interest from the period through various text mining techniques, including topic modeling and corrispondence analysis.

## Analysis Overview
* Text Preprocessing: The text of Italian laws was cleaned, tokenized, and lemmatized for consistency in analysis. Tokenization and lemmatization were performed using the koRpus library along with TreeTagger, while spelling consistency was checked using hunspell.
* Visualization and Statistical Analysis: Word frequencies and topic distributions were visualized using ggplot2, providing graphical interpretations of the findings.
* Topic Modeling: The topicmodels library was used to perform Latent Dirichlet Allocation (LDA) and seeded LDA, identifying key themes and topics within the legal texts.
* Correspondence Analysis: The relationships between terms and documents were investigated using correspondence analysis with the quanteda.textmodels library. This method revealed co-occurrence patterns and how terms clustered within the legal documents, offering insights into the semantic structures.
