rm(list=ls())

setwd("~/Bureau/ACO/stat/cours/sensometrie")

# Importation du dataset
data <- read.csv("trih.txt", sep = "\t", stringsAsFactors = TRUE, header = TRUE)
str(data)
rownames(data) <- data$Stimuli


#tableau pour un seul individu 
data_E82 <- data.frame(data[1],data[4:6], data[4:6], data[4:6], fact[,-1])

descr <- read.csv("Descriptive.csv", header = TRUE, sep = ";", fileEncoding = "utf-8")
str(descr)

nbpart <- descr$Nbdehiérarchies # nb de hiérarchies par indiv

library(FactoMineR)

fact <- read.csv("./Design.csv", header = T, sep = ";", fileEncoding = "utf-8")


MFA <- FactoMineR::MFA(data2[,-1], group = c(nbpart, 429), graph = T, num.group.sup = c(171))
#data2[431]

MFA_E82 <- FactoMineR::MFA(data_E82[,-1], 
                           group = c(3,3,rep(1,10)), 
                           type = c(rep("s",5), rep("n", 7)), 
                           name.group = c("E82", "E82", "H1", "H2", "H3", colnames(fact[-1])),
                           num.group.sup = c(3:12))


F1 = MFA_E82$quanti.var.sup$coord[,1]
F2 = MFA_E82$quanti.var.sup$coord[,2]

dim(data)

# Généralisation 

# Tout en double  
data <- read.csv("trih.txt", sep = "\t", stringsAsFactors = TRUE, header = TRUE)
rownames(data) <- data$Stimuli

data2 <- data.frame(data, data[-1], fact[,-1])
dim(data2)

nfact.sup = dim(data)[2] - 1
noms_ind <- as.character(descr$Ind)

MFA_tot <- FactoMineR::MFA(data2[,-1], 
                           group = c(nbpart,rep(1, nfact.sup + 7)), 
                           type = c(rep("s", length(noms_ind) + nfact.sup), rep("n", 7)), 
                           # name.group = c(noms_ind, names(data[,-1]), colnames(fact[-1])),
                           num.group.sup = c((length(noms_ind)+1):606))







