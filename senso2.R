setwd("~/Bureau/ACO/stat/cours/sensometrie")

adults <- read.csv("./donnees_perception/Donnees_adultes2.csv", sep=";", header=TRUE)
enfants <- read.csv("./donnees_perception/Donnees_enfants2.csv", sep=";", header=TRUE)

names(adults)
names(enfants)

summary(adults)
summary(enfants)

factors_commun <- c("Glacage", "Fourrage", "Eclair", "Attentes.gustatives", "Ressenti")

adults$Glacage <- factor(adults$Glacage)
adults$Fourrage <- factor(adults$Fourrage)
adults$Eclair <- factor(adults$Eclair)
adults$Attentes.gustatives <- factor(adults$Attentes.gustatives)
adults$Ressenti <- factor(adults$Ressenti)


adults$Int.Chocolat <- factor(adults$Int.Chocolat)
adults$Sucre <- factor(adults$Sucre)
adults$Lait <- factor(adults$Lait)
adults$Amer <- factor(adults$Amer)

# enfants$Glacage <- factor(enfants$Glacage)
# enfants$Fourrage <- factor(enfants$Fourrage)
# enfants$Eclair <- factor(enfants$Eclair)
# enfants$Attentes.gustatives <- factor(enfants$Attentes.gustatives)
# enfants$Ressenti <- factor(enfants$Ressenti)

library(ade4)
disj <- acm.disjonctif(adults[,c("Int.Chocolat", "Sucre", "Lait", "Amer")])
dataComp <- data.frame(adults["Liking"], disj, adults[,c("Conso", "Eclair")])

dataComp <- dataComp[, -c(4, 9, 14, 19)]

dataComp$BasChoc <- (dataComp$Int.Chocolat.1+dataComp$Int.Chocolat.2)
dataComp$TropChoc <- (dataComp$Int.Chocolat.4+dataComp$Int.Chocolat.5)

dataComp$BasSucre <- (dataComp$Sucre.1+dataComp$Sucre.2)
dataComp$TropSucre <- (dataComp$Sucre.4+dataComp$Sucre.5)

dataComp$BasLait <- (dataComp$Lait.1+dataComp$Lait.2)
dataComp$TropLait <- (dataComp$Lait.4+dataComp$Lait.5)

dataComp$BasAmer <- (dataComp$Amer.1+dataComp$Amer.2)
dataComp$TropAmer <- (dataComp$Amer.4+dataComp$Amer.5)

dataComp <- dataComp[, -c(2:17)]

library(FactoMineR)
lm <- LinearModel(Liking~., data=dataComp, selection = "bic")
lmBrut <- LinearModel(Liking~., data=dataComp)


