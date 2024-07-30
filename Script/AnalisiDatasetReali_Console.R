#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# Controlla se il nome del file è stato fornito
if(length(args)==0) {
  stop("Devi fornire il nome del file come argomento", call.=FALSE)
}

library(readr)


source("~/Calinski_Harabasz_Thesis/Script/FunzioniDatasetReali.R")

# Lettura nome del dataset
nome <- nomeDataset(args[1])
cat(sprintf("\nIl dataset analizzato è : %s\n", nome))
print()

# Lettura e pulizia del dataset da dati NA
dataset <- na.omit(read_csv(args[1]))

print("\nAnalisi dei risultati di clustering del dataset reale passato tramite la metrica di Calinski-Harabasz\n")
print("\nVengono utilizzati K-means, DBSCAN e Hierarchical-clustering come algoritmi di clustering\n")

# Chiama la funzione per l'analisi del dataset
calcoloTabellaCH(dataset, nome)