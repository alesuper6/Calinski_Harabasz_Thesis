#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# Controlla se il nome del file è stato fornito
if(length(args)==0) {
  stop("Devi fornire il nome del file come argomento", call.=FALSE)
}

library(readr)


source("~/Calinski_Harabasz_Thesis/Script/FunzioniDatasetReali.R")

# Lettura e pulizia del dataset da dati NA
dataset <- na.omit(read_csv(args[1]))

# Chiama la funzione per l'analisi del dataset
AnalisiDatasetReale(dataset)