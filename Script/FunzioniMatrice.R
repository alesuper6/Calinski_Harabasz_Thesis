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
## Sostituisce casualmente un numero x di righe con valori compresi tra 0 e 1

rimpiazzaCasualmenteRighe <- function(matr, x) {
  # Ottieni le dimensioni della matrice
  nrighe <- nrow(matr)
  ncolonne <- ncol(matr)
  
  # Controlla se x è valido
  if (x > nrighe) {
    stop("x non può essere maggiore del numero di righe della matrice")
  }
  
  # Seleziona x righe casuali
  righe_casuali <- sample(1:nrighe, x)
  
  # Sostituisci le righe casuali con numeri casuali tra 0 e 1
  matr[righe_casuali, ] <- matrix(runif(x * ncolonne), nrow = x)
  
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

# Definizione di una funzione per leggere un intero da tastiera
readInteger <- function() {
  n <- readline(prompt="Inserisci un intero: ")
  return(as.integer(n))
}

##---------------------------------------------------------------------------##

