library(ggplot2)
library(readr)
library(UniversalCVI)

load("~/Calinski-Harabasz-Thesis/Data/Dataset_Check.RData")

# Esempio 1: 
# dataset con due cluster ben separati tra loro

# Visualizzazione dei dati
ggplot(cl_separati, aes(x = x, y = y, color = cluster)) +
  geom_point() +
  theme_minimal()
View(cl_separati)

# Calcolo Calinski-Harabasz con CH.IDX
# Valore atteso -> inf.
CH.IDX(cl_separati[,1:2], 2)


# Esempio 2: 
# dataset con due cluster sovrapposti tra loro

# Visualizzazione dei dati
ggplot(cl_sovrapposti, aes(x = x, y = y, color = cluster)) +
  geom_point() +
  theme_minimal()
View(cl_sovrapposti)

# Calcolo Calinski-Harabasz con CH.IDX
# Valore atteso -> 0
CH.IDX(cl_sovrapposti[,1:2], 2)

