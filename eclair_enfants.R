# importation du dataset

eclair <- read.csv("Donnees_enfants2.csv", sep = ";", 
                   header = TRUE, stringsAsFactors = T)

eclair$Int.Chocolat = as.factor(eclair$Int.Chocolat)
eclair$Sucre = as.factor(eclair$Sucre)
eclair$Lait = as.factor(eclair$Lait)

library(ade4)
#acm.disjonctif renvoie le TDC quand on a un tableau de facteurs


TDC = acm.disjonctif(eclair[,c("Int.Chocolat", "Sucre", "Lait")])
data = data.frame (TDC, Conso = eclair$Conso, Eclair =  eclair$Eclair, Liking = eclair$Liking)
summary(data)
# data = data[, -c(3,8,13)]

data.agr <- data[,c("Conso", "Eclair", "Liking")]

data.agr$Int.Chocolat.manque <- (data$Int.Chocolat.1+data$Int.Chocolat.2)
data.agr$Int.Chocolat.trop <- (data$Int.Chocolat.4+data$Int.Chocolat.5)

data.agr$Sucre.manque <- (data$Sucre.1+data$Sucre.2)
data.agr$Sucre.trop <- (data$Sucre.4+data$Sucre.5)

data.agr$Lait.manque <- (data$Lait.1+data$Lait.2)
data.agr$Lait.trop <- (data$Lait.4+data$Lait.5)

summary(data.agr)

for (k in 4:9){
  data.agr[,k] = as.numeric(data.agr[,k])
  
  
  
}

# data.agr$Int.Chocolat.ok <- as.factor(data$Int.Chocolat.3)
# data.agr$Sucre.ok <- as.factor(data$Sucre.3)
# data.agr$Lait.ok <- as.factor(data$Lait.3)



library(FactoMineR)
mod <- LinearModel(Liking ~., data = data.agr, selection = "bic") 
mod2 <- LinearModel(Liking ~., data = data.agr) 
