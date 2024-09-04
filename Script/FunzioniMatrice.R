library(ggplot2)
library(fpc)


## Funzione che legge da tastiera il numero passato

readInteger <- function(){
  # Richiesta valore
  cat("Inserisci un valore intero: ")
  
  # Legge il valore e lo converte in intero
  valore <- scan(what = integer(), nmax = 1)
  
  return(valore)
}

##---------------------------------------------------------------------------##

## Funzione che crea la matrice parametrica [2n, n]
## Con la prima metà di righe di soli 0 e l'altra metà di righe di soli 1

creaMatrice <- function(n) {
  # Crea una matrice vuota di dimensione 2n*n
  matr <- matrix(nrow = 2*n, ncol = n)
  
  # Riempie la prima metà delle righe con 0
  matr[1:n, ] <- 0
  
  # Riempie la seconda metà delle righe con 1
  matr[(n+1):(2*n), ] <- 1
  
  return(matr)
}

##---------------------------------------------------------------------------##


## Funzione che modifica la matrice passata
## Sostituisce le righe selezionate con valori compresi tra 0 e 1

modificaRigaSelezionata <- function(matrice, riga) {
  
  # Ottieni le dimensioni della matrice
  nrighe <- nrow(matrice)
  ncolonne <- ncol(matrice)
  
  # Sostituisci le righe selezionate con numeri casuali tra 0 e 1
  matrice[riga, ] <- matrix(runif(length(riga) * ncolonne), 
                             nrow = length(riga))
  
  return(matrice)
}

##---------------------------------------------------------------------------##



# Funzione per iterazione dei passaggi di modifica della matrice e validazione attraverso CH
# Il parametro console viene settato a False di base per le chiamate eseguite all'interno di Rstudio

analisiMatrice <- function(n, console=FALSE){
  
  # CASO BASE: prima metà delle righe composta di soli 0 e la seconda metà 
  # composta di soli 1.
  
  # Abilita/disabilita stampa
  verbose <- FALSE
  
  # controlli del parametro n per la matrice
  if(missing(n))
    stop("Missing input argument. A parametric value is required")
  if(!is.numeric(n))
    stop("Argument 'n' must be numeric")
  if(n<=1)
    warning("The minimum value for consideration should be higher than 1",immediate. = TRUE)
  
  if(verbose){
    print(n)
  }
  # Creazione e visualizzazione matrice 
  matrice <-creaMatrice(n)
  if(verbose){
    matrice
  }
  
  
  # Clustering con K-means
  cl <- kmeans(matrice, 2, , 100)
  
  # Calcolo dell'indice di Calinski-Harabasz con calinhara
  # Valore atteso -> inf
  CH <- calinhara(matrice, cl$cluster)
  cat(sprintf("\nValore indice Calinski-Harabasz: %f\n", CH))
  
  
  # CASO ITERATIVO: viene presa la matrice del caso precedente ed una riga scelta 
  #                 casualmente viene sostituita con valori compresi tra 0 e 1.
  
  
  
  # Creazione dataframe iterazioni/valore
  risultati <- data.frame(iterazione = integer(), valore = numeric(),
                          stringsAsFactors = FALSE)
  
  
  # Creazione pool da cui estrarre la riga da modificare
  m <- nrow(matrice) # Numero di righe della matrice
  pool <- sample(1:m) # Crea un pool di numeri da 1 a n ordinato in modo casuale
  
  
  # Ciclo iterativo per modifica di una riga alla volta
  for (i in 1:nrow(matrice)) {
    if(verbose){
      cat(sprintf("\nIterazione %d:\n", i))
    }
    
    # Pesca un numero casualmente senza reimmissione dal pool creato
    selezione <- pool[i] 
    if(verbose){
      cat(sprintf("\nRiga modificata: %d\n", selezione))
    }
    
    # Modifica riga della matrice
    matrice <- modificaRigaSelezionata(matrice, selezione)
    
    # Visualizzazione della matrice
    if(verbose){
      print(matrice)
    }
    
    
    # Clustering con K-means
    cl <- kmeans(matrice, 2, , 100)
    
    # Calcolo dell'indice di Calinski-Harabasz con calinhara
    CH <- calinhara(matrice, cl$cluster)
    cat(sprintf("\nValore indice Calinski-Harabasz: %f\n", CH))
    
    # Aggiornamento dataframe iterazioni/valore
    risultati <- rbind(risultati, data.frame(iterazione = i, valore = CH))
    
  }
  
  
  # Visualizzazione dei valori di Calinski-Harabasz per ogni matrice
  print(risultati)
  
  
  
  # Creazione del grafico a linee
  grafico <- ggplot(data = risultati, aes(x = risultati[,1] , y = risultati[,2])) +
    geom_line(color = "blue", linewidth = 1) +
    ylim(0, max(risultati$valore)) +
    xlim(0, 2*n) +
    theme(panel.grid.major = element_line(color = "grey"), # Aggiunge la griglia principale
          panel.grid.minor = element_blank(), # Rimuove la griglia secondaria
          panel.background = element_blank()) +
    scale_x_continuous(breaks = round(seq(min(risultati$iterazione), max(risultati$iterazione), by = 1), 1)) +
    labs(title = "Grafico valore CH", x = "# righe manipolate", y = "Calinski-Harabasz index")
  
  
  # Visualizzazione del grafico
  visualizzaGrafico(grafico, n, console)
  
  
}


##---------------------------------------------------------------------------##


#Visualizzazione grafico della manipolazione della matrice

visualizzaGrafico <- function(grafico, n, console) {
  
  # Percorso della directory di download
  download_dir <- "~/Calinski_Harabasz_Thesis/Risultati/GraficiDatasetArtificiali" 
  
  # Flag per la stampa del nome file
  flag=FALSE
  
  # Nome base del file
  base_name <- "graficoN="
  
  # Estensione del file
  ext <- ".pdf"
  
  # Indice per il nome del file
  i <- 1
  
  # Nome del file completo
  file_name <- file.path(download_dir, paste0(base_name, n, ext))
  
  # Controlla se il file esiste già
  while(file.exists(file_name)) {
    # Se il file esiste, incrementa l'indice e crea un nuovo nome del file
    i <- i + 1
    file_name <- file.path(download_dir, paste0(base_name, n, '(', i, ')', ext))
    flag= TRUE
  }
  
  # Salva il grafico in un file PDF con il nuovo nome
  if(flag){
    ggsave(paste0(base_name, n, '(', i, ')', ext), plot = grafico, device = NULL, path = "~/Calinski_Harabasz_Thesis/Risultati/GraficiDatasetArtificiali")
  } else{
    ggsave(paste0(base_name, n, ext), plot = grafico, device = NULL, path = "~/Calinski_Harabasz_Thesis/Risultati/GraficiDatasetArtificiali")
  }
  
  # Se la funzione viene chiamata da console restituisce il percorso del file 
  if(console){
    return(file_name)
  }
  else{
    grafico
  }
  
}
