#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# args è ora una lista di stringhe contenente i parametri
n <- as.integer(args[1])  # converte il primo parametro in un intero


source("~/Calinski_Harabasz_Thesis/Script/FunzioniMatrice.R")

# Chiamata funzione per l'analisi del dataset artificiale
file_name <- AnalisiMatrice(n, TRUE)

# Percorso del file in formato Unix
unix_path <- file_name

# Converti il percorso del file in formato Windows
windows_path <- normalizePath(unix_path, winslash = "\\")

# Apri il file PDF 
system2("open" , windows_path)
