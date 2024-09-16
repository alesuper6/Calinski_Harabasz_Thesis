library(fpc) 
library(cluster)
library(readr)



# Funzione per la lettura del dataset, interfaccia per l'analisi

analisiDatasetReali <- function(data){
  
  # Lettura nome del dataset
  nome <- nomeDataset(data)
  cat(sprintf("Il dataset analizzato è : %s", nome))
  
  # Lettura e pulizia del dataset da dati NA
  dataset <- na.omit(read_csv(data))
  
  print("Analisi dei risultati di clustering del dataset reale passato tramite la metrica di Calinski-Harabasz")
  print("Vengono utilizzati K-means, DBSCAN e Hierarchical-clustering come algoritmi di clustering")
  
  # Chiama la funzione per l'analisi del dataset
  calcoloTabellaCH(dataset, nome)
}

##---------------------------------------------------------------------------##



# Funzione che itera le combinazioni di iperp. per i diversi algoritmi per il calcolo della tabella 

calcoloTabellaCH <- function(dataset, nome){
  
  # Creazione dataframe per i risultati
  risultati <- data.frame(matrix(ncol = 9, nrow = 0))
  colnames(risultati) <- c("metodo" , "k" , "distance" , "nstart" , "eps" , "minPts" , "k_out" , "linkage" , "CH")
  
  
  
  
  
  # K-means

  # Creazione lista con i valori di k
  list_k <- c(2,3,5)
  
  # Creazione lista con valori di distance
  list_distance <- c("euclidean", "manhattan")
  
  # Creazione lista con valori di nstart
  list_nstart <- c(2,20,100)
  
  
  
  for (i in 1:length(list_k)) {
    for (j in 1:length(list_distance)){
      for (n in 1:length(list_nstart)) {
        
        # Calcolo della matrice di distanza dei dati
        diss <- dist(dataset, method = list_distance[j])
        
        # Clustering con K-means
        obj <- pam(diss, list_k[i], nstart = list_nstart[n])
        
        # Validazione con Calinski-Harabasz tramite calinhara
        valore <- calinhara(dataset, obj$cluster)
        
        # Creazione riga della tabella risultati 
        new_row <- data.frame(metodo = "K-means", k = list_k[i], distance = list_distance[j], nstart = list_nstart[n], eps = NA, minPts = NA, k_out = NA , linkage = NA, CH = valore)
        
        # Aggiunta della riga al dataframe risultati
        risultati <- rbind(risultati, new_row)
      }
    }
  }
  
  
  
  
  
  # DBSCAN

  # Creazione lista con i valori di eps
  list_eps <- valoriEps(nome)
  
  # Creazione lista con i valori di minPts
  list_minPts <- valoriMinPts(nome)
  
  # Ciclo per la validazione del clustering
  for (i in 1:length(list_eps)) { 
    for (j in 1:length(list_minPts)) {
      
      # Clustering con DBSCAN 
      clustering <- dbscan(dataset, list_eps[i], list_minPts[j])
      
      # Validazione con Calinski-Harabasz tramite calinhara
      valore <- calinhara(dataset, clustering$cluster)
      
      # Creazione riga della tabella risultati 
      new_row <- data.frame(metodo = "DBSCAN", k = NA, distance = NA, nstart = NA, eps = list_eps[i], minPts = list_minPts[j], k_out = max(clustering$cluster), linkage = NA, CH = valore)
      
      # Aggiunta della riga al dataframe risultati
      risultati <- rbind(risultati, new_row)
    }
  }
  
  
  
  
  #Hierarchical-clustering
  
  # Creazione lista con i valori di method
  list_linkage <- c("complete", "average", "single")
  
  # Creazione dataframe per i risultati
  risultati.hier <- data.frame(k = 2:10)
  
  # Ciclo per clustering tramite hierarchical clustering e validazione con calinhara
  for(i in list_linkage){
    for (j in list_distance) {
      
      # Creazione lista per i valori di Calinski-Harabasz 
      CH.hier<- c()
      
      # Clustering con complete-linkage
      H.model <- hclust(dist(dataset, method = j), i)
      
      # Ciclo per la validazione del clustering con calinhara
      for (x in 2:10) {
        
        #Taglio dell'albero risultante dal clustering
        cluster <- cutree(H.model, k = x) 
        
        #Aggiornamento lista coi risultati
        CH.hier <- c(CH.hier, calinhara(dataset, cluster))  
      }
      
      # Aggiornamento dataframe risultati
      if(i == "complete"){
        
        # Aggiornamento dataframe Hierarchical-clustering
        risultati.hier$complete <- CH.hier 
        
        # Calcolo ottimo locale per complete-link
        cmplt <- localOpt(risultati.hier$complete)    
        
        # Selezione riga con ottimo locale
        row_sel <- risultati.hier[risultati.hier$complete==cmplt , ]
        
        # Creazione riga della tabella risultati 
        new_row <- data.frame(metodo = "Hierarchical-clustering", k = row_sel$k, distance = j, nstart = NA, eps = NA, minPts = NA, k_out = NA, linkage = "complete", CH = row_sel$complete)
        
        # Aggiunta della riga al dataframe risultati
        risultati <- rbind(risultati, new_row)
      }
      
      if(i == "average"){
        
        # Aggiornamento dataframe Hierarchical-clustering
        risultati.hier$average <- CH.hier 
        
        # Calcolo ottimo locale per average-link
        avrg <- localOpt(risultati.hier$average)    
        
        # Selezione riga con ottimo locale
        row_sel <- risultati.hier[risultati.hier$average==avrg , ]
        
        # Creazione riga della tabella risultati 
        new_row <- data.frame(metodo = "Hierarchical-clustering", k = row_sel$k, distance = j, nstart = NA, eps = NA, minPts = NA, k_out = NA, linkage = "average", CH = row_sel$average)
        
        # Aggiunta della riga al dataframe risultati
        risultati <- rbind(risultati, new_row)
      }
      
      if(i == "single"){
        
        # Aggiornamento dataframe Hierarchical-clustering
        risultati.hier$single <- CH.hier 
        
        # Calcolo ottimo locale per single-link
        sngl <- localOpt(risultati.hier$single) 
        
        # Selezione riga con ottimo locale
        row_sel <- risultati.hier[risultati.hier$single==sngl , ]
        
        # Creazione riga della tabella risultati 
        new_row <- data.frame(metodo = "Hierarchical-clustering", k = row_sel$k, distance = j, nstart = NA, eps = NA, minPts = NA, k_out = NA, linkage = "single", CH = row_sel$single)
        
        # Aggiunta della riga al dataframe risultati
        risultati <- rbind(risultati, new_row)
      }
    }
  }
  
  
  
  
  
  
  # Confronto finale
  
  # Stampa nome dataset
  cat(sprintf("\nDataset analizzato : %s\n",nome))
  
  # Stampa risultati
  print(risultati)
  
  # Ordina il dataframe per valore di Calinski-Harabasz decrescente
  risultati_decr <- risultati[order(risultati$CH, decreasing = TRUE),]
  
  # Stampa del dataframe dei risultati ordinato per valore di Calinski-Harabasz decrescente
  cat(sprintf("\nTabella riassuntiva ordinata per valore dell'indice di Calinski-Harabasz decrescente:\n" ))
  print(risultati_decr)
}



##---------------------------------------------------------------------------##



# Funzione per il calcolo dell'ottimo locale con valore di k minimo per hier. clustering

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




##---------------------------------------------------------------------------##



# Funzione che assegna i valori di eps in base al nome del dataset

valoriEps <- function(nome) {
  
  if(nome=="neuroblastoma.csv" || nome=="Spain_cardiac_arrest.csv"){
    return(c(3, 3.3, 3.6))
  }
  
  if(nome=="Sepsis_SIRS.csv"){
    return(c(18, 19.8, 21.6))
  }
  
  if(nome=="depression_heart_failure.csv"){
    return(c(80, 88, 96))
  }
  
  if(nome=="diabetes_type1.csv"){
    return(c(20))
  }
  
}



##---------------------------------------------------------------------------##



# Funzione che assegna i valori di minPts in base al nome del dataset

valoriMinPts <- function(nome){
  
  if(nome=="neuroblastoma.csv"){
    return(c(10, 11, 12))
  }
  
  if(nome=="Spain_cardiac_arrest.csv"){
    return(c(6, 7, 8))
  }
  
  if(nome=="Sepsis_SIRS.csv"){
    return(c(8, 9, 10))
  }
  
  if(nome=="depression_heart_failure.csv"){
    return(c(14, 15, 16))
  }
  
  if(nome=="diabetes_type1.csv"){
    return(c(2, 5))
  }
  
}



##---------------------------------------------------------------------------##



# Funzione che restituisce il nome del dataset passato alla funzione

nomeDataset <- function(percorso_completo){
  
  nome_file <- basename(percorso_completo)
  return(nome_file)
  
}