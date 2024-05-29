## Funzione che crea la matrice parametrica [2n, n]
## Con la prima metà di righe di soli 0 e l'altra metà di righe di soli 1

crea_matrice <- function(n) {
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

rimpiazza_casualmente_righe <- function(matr, x) {
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

rimpiazza_righe_selezionate <- function(matrice, righe) {
  # Ottieni le dimensioni della matrice
  nrighe <- nrow(matrice)
  ncolonne <- ncol(matrice)
  
  # Controlla se tutte le righe selezionate sono valide
  if (any(righe > nrighe)) {
    stop("Alcune righe selezionate non sono valide")
  }
  
  # Sostituisci le righe selezionate con numeri casuali tra 0 e 1
  matrice[righe, ] <- matrix(runif(length(righe) * ncolonne), 
                             nrow = length(righe))
  
  return(matrice)
}

