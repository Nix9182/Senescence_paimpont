{
  library(readxl)
  library(vegan)
  library(rasterdiv)
}

setwd("C:/Users/cobod/OneDrive/Bureau/Master BEE MNHN/Paimpont")

data_insectes <- read_excel("Donnees_Shannon.xlsx", sheet="InsecteShannon", col_names = T)
data_insectes[is.na(data_insectes)] <-0
data_insectes2 <- data_insectes[,-1]
data_index <- data_insectes[,1]

data_index$Shannon<- diversity(data_insectes2)
data_index$Simpson<- diversity(data_insectes2, "simpson")
data_index$ID <- data_insectes[,1]
data_index$Pielou <-#Calculer en faisant shannon/richness
  
data_ilot <-data_index[data_index$ID == c("Ilot 1", "Ilot 2", "Ilot 3", "Ilot 4", "Ilot 5"),]
data_geree <- data_index[data_index$ID == c("Gérée 1", "Gérée 2", "Gérée 3", "Gérée 4", "Gérée 5"),]

mean_shannon_ilot <- mean(data_ilot$Shannon)
mean_shannon_geree <- mean(data_geree$Shannon)

shapiro.test(data_ilot$Shannon)
shapiro.test(data_geree$Shannon)

#distribution non-normale donc test de Mann-Whitney
wilcox.test(data_ilot$Shannon,data_geree$Shannon)

#p-value > alpha=0.05 donc les deux zones ont la même distribution et ont un indice de Shannon moyen semblable

jaccard_similarity <- function(A, B) { 
  intersection = length(intersect(A, B)) 
  union = length(A) + length(B) - intersection 
  return (intersection/union) 
} 

Jaccard_Similarity <- jaccard_similarity(data_insectes2[1:5,], data_insectes2[6:10,])

Jaccard_Similarity 

# Jaccard Dissimilarity/Distance between sets, A and B  
Jaccard_Distance = 1 - Jaccard_Similarity 
Jaccard_Distance
