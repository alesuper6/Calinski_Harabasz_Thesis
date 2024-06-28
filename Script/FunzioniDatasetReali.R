library(dbscan)
library(fpc) 

AnalisiDatasetReale <- function(dataset){
  
  # K-means
  # Creazione lista con i valori di k
  k <- c(2,3,5)
   
  # Creazione lista per i valori di Calinski-Harabasz
  CH <- c()
  
  for (i in k) {
    
    # Clustering con K-means
    cl <- kmeans(dataset, i, , 100)
    
    # Validazione con Calinski-Harabasz tramite calinhara
    valore <- calinhara(dataset, cl$cluster)
    
    # Aggiungiamo il valore alla lista
    CH <- c(CH, valore)
    
  }
  
  # Creazione dataframe iterazione/valore
  risultati <- data.frame(k = k, val.CH = CH)
  
  # Stampa dataframe dei risultati
  print(risultati)
  
  
  # DBSCAN
  # Creazione dataframe iterazioni/valore
  risultati <- data.frame(caso = integer(), valore = numeric(),
                          stringsAsFactors = FALSE)
  
  # Creazione lista con i valori di eps
  list_eps <- c( , , )
  
  # Creazione lista con i valori di minPts
  list_minPts <- c( , , )
  
  # Controllo lunghezza liste
  if(length(list_eps)==length(list_minPts)){
    
    # Ciclo per la validazione del clustering
    for (i in 1:length(list_eps)) { 
      eps <- list_eps[i]          #estrae il valore di eps
      minPts <- list_minPts[i]    #estrae il valore di minPts
      
      # Clustering con DBSCAN 
      clustering <- dbscan(dataset, eps, minPts)
      
      # Aggiornamento dataframe coi risultati
      risultati <- rbind(risultati, data.frame(caso=1, valore=calinhara(dataset, clustering$cluster)))
      
      # Stampa risultato
      cat(sprintf("\nIndice Calinski-Harabasz per eps=%g e minPts=%g: %f\n",eps , minPts, risultati[i,2] ))
    }
  } else {
    print("Le liste devono avere la stessa lunghezza")  
  }
  
  
  #Hierarchical-clustering
  # Clustering con complete-linkage e validazione del clustering con CH.IDX
  cmplt_link <- CH.IDX(dataset, 10, 2, "hclust_complete")
  
  # Stampa risultati
  cat(sprintf("\nIndice Calinski-Harabasz per complete-linkage: \n"))
  print(cmplt_link)
  
  # Clustering con average-linkage e validazione del clustering con CH.IDX
  avrg_link <- CH.IDX(dataset, 10, 2, "hclust_average")
  
  # Stampa risultati
  cat(sprintf("\nIndice Calinski-Harabasz per average-linkage: \n"))
  avrg_link
  
  # Clustering con single-linkage e validazione del clustering con CH.IDX
  sngl_link <- CH.IDX(dataset, 10, 2, "hclust_single")
  
  # Stampa risultati
  cat(sprintf("\nIndice Calinski-Harabasz per single-linkage: \n"))
  sngl_link
  
}
