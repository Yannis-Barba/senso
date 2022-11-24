rm(list=ls())
setwd("~/Bureau/ACO/stat/cours/sensometrie")

adults <- read.csv("./donnees_perception/Donnees_adultes2.csv", sep=";", header=TRUE)

names(adults)

summary(adults)

## Adults

adults$Glacage <- factor(adults$Glacage)
adults$Fourrage <- factor(adults$Fourrage)
adults$Eclair <- factor(adults$Eclair)
adults$Attentes.gustatives <- factor(adults$Attentes.gustatives)
adults$Ressenti <- factor(adults$Ressenti)


adults$Int.Chocolat <- factor(adults$Int.Chocolat)
adults$Sucre <- factor(adults$Sucre)
adults$Lait <- factor(adults$Lait)
adults$Amer <- factor(adults$Amer)

library(ade4)
disj <- acm.disjonctif(adults[,c("Int.Chocolat", "Sucre", "Lait", "Amer")])
dataComp <- data.frame(adults["Liking"], disj, adults[,c("Conso", "Eclair")])

dataComp$Conso <- as.factor(dataComp$Conso)

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

## Enfants

# importation du dataset

enfants <- read.csv("./donnees_perception/Donnees_enfants2.csv", sep = ";", 
                   header = TRUE, stringsAsFactors = T)

enfants$Int.Chocolat = as.factor(enfants$Int.Chocolat)
enfants$Sucre = as.factor(enfants$Sucre)
enfants$Lait = as.factor(enfants$Lait)

library(ade4)
#acm.disjonctif renvoie le TDC quand on a un tableau de facteurs


TDC = acm.disjonctif(enfants[,c("Int.Chocolat", "Sucre", "Lait")])
data = data.frame (TDC, Conso = enfants$Conso, Eclair =  enfants$Eclair, Liking = enfants$Liking)
summary(data)


data.agr <- data[,c("Conso", "Eclair", "Liking")]

data.agr$BasChoc <- (data$Int.Chocolat.1+data$Int.Chocolat.2)
data.agr$TropChoc <- (data$Int.Chocolat.4+data$Int.Chocolat.5)

data.agr$BasSucre <- (data$Sucre.1+data$Sucre.2)
data.agr$TropSucre <- (data$Sucre.4+data$Sucre.5)

data.agr$BasLait <- (data$Lait.1+data$Lait.2)
data.agr$TropLait <- (data$Lait.4+data$Lait.5)

summary(data.agr)

library(FactoMineR)
mod <- LinearModel(Liking ~., data = data.agr, selection = "bic") 
mod2 <- LinearModel(Liking ~., data = data.agr) 



# Représentation graphique
modAdults <- lmBrut
modEnfants <- mod2

coefAdults <- abs(modAdults$Ttest[71:76,1])
coefEnfants <- abs(modEnfants$Ttest[49:54,1])

library(ggplot2)
library(tidyverse)

data <- data.frame(coefAdults, coefEnfants)

data %>% ggplot()+
  geom_point(aes(x=coefAdults, y=coefEnfants))+
  geom_text(aes(x=coefAdults, y=coefEnfants, label=rownames(data)),
            hjust=-0.15, vjust=0)+
  geom_abline(intercept = 0, slope = 1)+
  scale_x_continuous(limits = c(0,2.5))+
  scale_y_continuous(limits = c(0,2.5))+
  coord_fixed(ratio=1)

