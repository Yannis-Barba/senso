setwd("~/Bureau/ACO/stat/cours/sensometrie")

rm(list=ls())

senso <- read.table("./senso.txt", header=TRUE )

# on voit qu'il y a des hiéarchies et des numéros. 
# il faut voir à quel point les hiérarchies chez les enfants. 

ind <- names(senso)

test1 <- senso["E158.H1"]
test2 <- senso["E158.H2"]

# Nombre d'individus 

unique_name <- apply(ind[1:length(ind)], FUN=function(i) strsplit(i, "[.]"))

# 


### à partir du fichier xls

senso_ind <- read.csv("./Descriptive.csv", sep=";", header=TRUE)
nbre_ind <- length(senso_ind$Ind)
nbre_hierarchie <- senso_ind$Nbdehiérarchies

require(FactoMineR)

afm <- FactoMineR::MFA(senso_ind[-1], group = nbre_hierarchie)
