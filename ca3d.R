library(ca)
# Esegui l'analisi delle corrispondenze
ca_result <- ca(convert(quanteda_dfm, to = "matrix"))

plot(ca_result)

##3d
library(rgl)

# Ottenere le coordinate delle righe e delle colonne
row_coords <- ca_result$rowcoord
col_coords <- ca_result$colcoord

# Selezionare i termini più frequenti
term_frequencies <- colSums(convert(quanteda_dfm, to = "matrix"))
top_terms <- names(sort(term_frequencies, decreasing = TRUE)[1:30]) 
filtered_col_coords <- col_coords[colnames(convert(quanteda_dfm, to = "matrix")) %in% top_terms, , drop = FALSE]

# Visualizzare in 3D
modello3d <- plot3d(row_coords[,1], row_coords[,2], row_coords[,3], 
                    col = "lightblue", size = 1, type = "s", xlab = "Dim 1", ylab = "Dim 2", zlab = "Dim 3", 
                    main = "Analisi delle corrispondenze")
points3d(filtered_col_coords[,1], filtered_col_coords[,2], filtered_col_coords[,3], 
         col = "orange", size = 5) 

# Aggiungere le etichette dei documenti
text3d(row_coords[,1], row_coords[,2], row_coords[,3], 
                    texts = rownames(convert(quanteda_dfm, to = "matrix")), col = "blue", cex = 0.7)

# Aggiungere le etichette dei termini più frequenti
text3d(filtered_col_coords[,1], filtered_col_coords[,2], filtered_col_coords[,3], 
       texts = rownames(filtered_col_coords), col = "red", cex = 0.7)
