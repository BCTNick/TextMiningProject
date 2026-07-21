# The Language of the War Economy

This project applies text mining techniques to Italian legal texts about loans to citizens during and after the Third War of Independence. The corpus includes decrees and legal acts issued between 1866 and 1872.

The analysis explores how the administrative and financial language of the newly unified Kingdom of Italy reflected the strategies used to fund public and military expenditure: national loans, bonds, taxation, revenue collection, and the role of the National Bank of the Kingdom of Italy.

## Objective

The goal is to identify the most frequent terms, the relationships between words and documents, and the main topics emerging from the legal corpus. The project combines quantitative text analysis with historical interpretation, linking linguistic patterns to the financial construction of post-unification Italy.

## Corpus

The corpus consists of 11 legal documents in RTF format, stored in `data/1866-72`. The documents mainly include royal and ministerial decrees related to loans, government bonds, taxation, revenue collection, and public finance instruments.

## Method

The analysis pipeline is implemented in R:

1. importing the documents with `readtext`;
2. lemmatizing the Italian texts with TreeTagger;
3. checking spelling consistency with `hunspell`;
4. building the corpus with `quanteda`;
5. tokenizing the texts and removing punctuation, numbers, symbols, and stopwords;
6. creating a document-feature matrix;
7. producing frequency analysis, co-occurrence analysis, wordclouds, correspondence analysis, and topic modeling.

## Main Results

### Most Frequent Vocabulary

The most frequent lemmas reveal the central vocabulary of the corpus: `prestito` (loan), `decreto` (decree), `comune` (municipality), `lira`, `titolo` (security/bond), `rendita` (annuity/government bond), `banca` (bank), and other terms connected to public debt and administrative management.

![Vocabulary barplot](assets/vocabulary-barplot.png)

### Wordcloud

The wordcloud provides a visual summary of the most prominent terms. The centrality of words such as `prestito`, `decreto`, and `comune` confirms the financial and administrative nature of the corpus.

![Corpus wordcloud](assets/wordcloud.png)

### Co-occurrence Network

The co-occurrence network highlights relationships among the most frequent lemmas. These links help show which concepts appear together in the decrees, such as taxation, bonds, public offices, revenue collection, and banking institutions.

![Co-occurrence network](assets/cooccurrence-network.png)

### Correspondence Analysis

Correspondence analysis compares the documents according to their vocabulary. The decrees from 1866-1867 and those from 1872 show thematic differences: the earlier texts are more closely connected to the national loan and fiscal allocation, while the 1872 documents focus more on banks, bond certificates, annuities, and public debt.

![Correspondence analysis](assets/correspondence-analysis.png)

### Topic Modeling

Topic modeling identifies four main lexical groups. The first topics are associated with the 1866 decrees on loans, taxation, and collection procedures; the fourth topic is closer to the 1872 ministerial decree, with terms such as `banca` (bank), `cartella` (bond certificate), `rendita` (annuity), `consolidato` (consolidated debt), and `portatore` (bearer).

![Topic modeling](assets/topic-modeling.png)

## Main Files

- `pre-analisi.R`: main script for importing, preprocessing, analyzing, and visualizing the corpus.
- `ca3d.R`: correspondence analysis and 3D visualization.
- `Report.qmd`: Quarto report of the analysis.
- `data/1866-72`: corpus of legal documents.

## Presentation Materials

The local PowerPoint file `Presentazione.pptx` is not published on GitHub. The essential content of the presentation is summarized in this README, together with the exported charts stored in `assets`.
