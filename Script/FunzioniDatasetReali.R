library(dbscan)
library(fpc) 

AnalisiDatasetReale <- function(dataset){
  
  # K-means
  
  # Creazione lista con i valori di k
  k <- c(2,3,5)
  
  # Creazione lista per i valori di Calinski-Harabasz
  CH.kmeans <- c()
  
  for (i in k) {
    
    # Clustering con K-means
    cl <- kmeans(dataset, i, , 100)
    
    # Validazione con Calinski-Harabasz tramite calinhara
    valore <- calinhara(dataset, cl$cluster)
    
    # Aggiungiamo il valore alla lista
    CH.kmeans <- c(CH.kmeans, valore)
    
  }
  
  # Creazione dataframe /valore
  risultati.kmeans <- data.frame(k = k, val.CH = CH.kmeans)
  
  # Stampa risultati
  cat(print("Risultati ottenuti tramite l'algoritmo K-means"))
  print(risultati.kmeans)
  
  
  
  
  # DBSCAN
  
  # Creazione lista con i valori di eps
  list_eps <- c(3, 3.3, 3)
  
  # Creazione lista con i valori di minPts
  list_minPts <- c(10, 10, 11)
  
  # Creazione lista per i valori di Calinski-Harabasz
  CH.dbscan <- c()
  
  # Controllo lunghezza liste
  if(length(list_eps)==length(list_minPts)){
    
    # Ciclo per la validazione del clustering
    for (i in 1:length(list_eps)) { 
      eps <- list_eps[i]          #estrae il valore di eps
      minPts <- list_minPts[i]    #estrae il valore di minPts
      
      # Clustering con DBSCAN 
      clustering <- dbscan(dataset, eps, minPts)
      
      # Aggiornamento lista coi risultati
      CH.dbscan <- c(CH.dbscan, calinhara(dataset, clustering$cluster))
    }
    
    # Creazione dataframe iterazione\valore
    risultati.dbscan <- data.frame(eps = list_eps, minPts = list_minPts, val.CH = CH.dbscan)
    
    # Stampa dataframe 
    cat(print("Risultati ottenuti tramite l'algoritmo DBSCAN"))
    print(risultati.dbscan)
    
  } else {
    print("Le liste devono avere la stessa lunghezza")  
  }
  
  
  
  
  #Hierarchical-clustering
  
  # Creazione lista con i valori di method
  method <- c("complete", "average", "single")
  
  # Creazione dataframe per i risultati
  risultati.hier <- data.frame(k = 2:10)
  
  # Ciclo per clustering tramite hierarchical clustering e validazione con calinhara
  for(i in method){
    
    # Creazione lista per i valori di Calinski-Harabasz 
    CH.hier<- c()
    
    # Clustering con complete-linkage
    H.model <- hclust(dist(dataset), i)
    
    # Ciclo per la validazione del clustering con calinhara
    for (j in 2:10) {
      cluster <- cutree(H.model, k = j)  #Taglio dell'albero risultante dal clustering
      CH.hier <- c(CH.hier, calinhara(dataset, cluster))  #Aggiornamento lista coi risultati
    }
    
    # Aggiornamento dataframe risultati
    if(i == "complete"){
      risultati.hier$complete <- CH.hier 
    }
    if(i == "average"){
      risultati.hier$average <- CH.hier 
    }
    if(i == "single"){
      risultati.hier$single <- CH.hier 
    }
  }
  
  # Stampa del dataframe dei risultati
  cat(print("Risultati ottenuti tramite l'algoritmo Hierarchical-clustering"))
  print(risultati.hier)
  
}

##---------------------------------------------------------------------------##

localOpt <- function(list){
  max <- 0
  for (i in list) {
    if(max<i){
      max <- i
    }else{
      return(max)
    }
  }
  
  
}