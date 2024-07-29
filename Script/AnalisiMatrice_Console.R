#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# args è ora una lista di stringhe contenente i parametri
n <- as.integer(args[1])  # converte il primo parametro in un intero


source("~/Calinski_Harabasz_Thesis/Script/FunzioniMatrice.R")

AnalisiMatrice(n)

