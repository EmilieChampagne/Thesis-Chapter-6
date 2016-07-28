PetitesO <-read.table(file="D:\\Documents\\Doctorat\\Chap 6 Outaouais\\PetitesO_clean.txt", header= TRUE, na.string= ".", stringsAsFactors = TRUE)
PetitesO$Count <- 1
PetitesO$Haut <- as.numeric(PetitesO$Haut)

GAB <- as.data.frame(1:106)
colnames(GAB) <- c("ID")

#Liste de toutes les espèces présentes: AMsp; BOJ*; BOP, CAC, CEP*; CET; CHG*; CHR; CHsp;
#COS; COV; DIM; EPB; EPD*; ERE; ERR; ERS; HEG; INC; MEL; NOL; OSV; PEB*; PEF; PEG; PIB;
#PIR; SAB; SALIX*; SUV*; THC; TIA; TIA; VAC
#*: < 10 individus

#######Ajout du nombre de tige par espèce########
#Sans classe de taille

#Aggrégation pour avoir une ligne par individu
a <- aggregate (PetitesO$Count, by= data.frame(PetitesO$ID, PetitesO$Esp, PetitesO$Dist, PetitesO$Haut, PetitesO$Diam, 
                          PetitesO$Fleche, PetitesO$Arbre), FUN= "sum", na.rm= TRUE)
a$x <- NULL
colnames(a)[1:7] <- c("ID", "Esp", "Dist", "Haut", "Diam", "Fleche", "Arbre")

#Création d'une variable dont la valeur est toujours de 1
a$Count <- 1 

#On garde les arbres morts (très peu d'individus et il y a du broutement dessus)
#On enlève les arbres <25 cm qui étaient probablement sous la neige
logic <- which(a$Haut >= 25)
a <- a[logic, ]

#Aggrégation du fichier pour obtenir le nombre d'arbre par variables nommées dans le data.frame
b <- aggregate (a$Count, by= data.frame(a$ID, a$Esp), FUN= "sum")
colnames(b)[1:3] <- c("ID", "Esp", "Nb")

#Création d'une fonction qui ajoute automatiquement les données dans PLL.analyse pour une espèce et un état de l'arbre
Creation.GAB <- function(Esp, Source, Name, Out){
  x <-Source[Source$Esp == Esp, ]
  Out <- merge(Out, x, by = "ID", all.x = TRUE)
  Out$Esp <- NULL
  Out[is.na(Out)] <- 0
  colnames(Out)[ncol(Out)] <- c(Name)
  return(Out)
}

#Liste de toutes les espèces présentes: AMsp; BOJ*; BOP, CAC, CEP*; CET; CHG*; CHR; CHsp;
#COS; COV; DIM; EPB; EPD*; ERE; ERR; ERS; HEG; INC; MEL; NOL; OSV; PEB*; PEF; PEG; PIB;
#PIR; SAB; SALIX*; SUV*; THC; TIA; TIA; VAC
#*: < 10 individus
GAB <- Creation.GAB(Esp= "AMsp", Source= b, Name= "AMsp", Out= GAB)
GAB <- Creation.GAB(Esp= "BOJ", Source= b, Name= "BOJ", Out= GAB)
GAB <- Creation.GAB(Esp= "BOP", Source= b, Name= "BOP", Out= GAB)
GAB <- Creation.GAB(Esp= "CAC", Source= b, Name= "CAC", Out= GAB)
GAB <- Creation.GAB(Esp= "CEP", Source= b, Name= "CEP", Out= GAB)
GAB <- Creation.GAB(Esp= "CET", Source= b, Name= "CET", Out= GAB)
GAB <- Creation.GAB(Esp= "CHG", Source= b, Name= "CHG", Out= GAB)
GAB <- Creation.GAB(Esp= "CHR", Source= b, Name= "CHR", Out= GAB)
GAB <- Creation.GAB(Esp= "CHsp", Source= b, Name= "CHsp", Out= GAB)
GAB <- Creation.GAB(Esp= "COS", Source= b, Name= "COS", Out= GAB)
GAB <- Creation.GAB(Esp= "COV", Source= b, Name= "COV", Out= GAB)
GAB <- Creation.GAB(Esp= "DIM", Source= b, Name= "DIM", Out= GAB)
GAB <- Creation.GAB(Esp= "EPB", Source= b, Name= "EPB", Out= GAB)
GAB <- Creation.GAB(Esp= "ERE", Source= b, Name= "ERE", Out= GAB)
GAB <- Creation.GAB(Esp= "ERR", Source= b, Name= "ERR", Out= GAB)
GAB <- Creation.GAB(Esp= "ERS", Source= b, Name= "ERS", Out= GAB)
GAB <- Creation.GAB(Esp= "HEG", Source= b, Name= "HEG", Out= GAB)
GAB <- Creation.GAB(Esp= "INC", Source= b, Name= "INC", Out= GAB)
GAB <- Creation.GAB(Esp= "MEL", Source= b, Name= "MEL", Out= GAB)
GAB <- Creation.GAB(Esp= "NOL", Source= b, Name= "NOL", Out= GAB)
GAB <- Creation.GAB(Esp= "OSV", Source= b, Name= "OSV", Out= GAB)
GAB <- Creation.GAB(Esp= "PEB", Source= b, Name= "PEB", Out= GAB)
GAB <- Creation.GAB(Esp= "PEF", Source= b, Name= "PEF", Out= GAB)
GAB <- Creation.GAB(Esp= "PEG", Source= b, Name= "PEG", Out= GAB)
GAB <- Creation.GAB(Esp= "PIB", Source= b, Name= "PIB", Out= GAB)
GAB <- Creation.GAB(Esp= "PIR", Source= b, Name= "PIR", Out= GAB)
GAB <- Creation.GAB(Esp= "SAB", Source= b, Name= "SAB", Out= GAB)
GAB <- Creation.GAB(Esp= "SALIX", Source= b, Name= "SALIX", Out= GAB)
GAB <- Creation.GAB(Esp= "SUV", Source= b, Name= "SUV", Out= GAB)
GAB <- Creation.GAB(Esp= "THC", Source= b, Name= "THC", Out= GAB)
GAB <- Creation.GAB(Esp= "TIA", Source= b, Name= "TIA", Out= GAB)
GAB <- Creation.GAB(Esp= "VAC", Source= b, Name= "VAC", Out= GAB)

#####Ajout valeur broutement PIB central, all PIB, all species#####

#Broutement sur le PIB central
NB.PIB <- PetitesO[PetitesO$Esp == "PIB" & PetitesO$Dist == 0 & PetitesO$Classe == "NB",  ]
C.PIB <- PetitesO[PetitesO$Esp == "PIB" & PetitesO$Dist == 0 & PetitesO$Classe == "C",  ]
L.PIB <- PetitesO[PetitesO$Esp == "PIB" & PetitesO$Dist == 0 & PetitesO$Classe == "L",  ]
NB.PIB$Classe <- NULL
C.PIB$Classe <- NULL
L.PIB$Classe <- NULL
NB.PIB$Count <- NULL
C.PIB$Count <- NULL
L.PIB$Count <- NULL
colnames(NB.PIB)[7] <- c("NB")
colnames(C.PIB)[7] <- c("C")
colnames(L.PIB)[7] <- c("L")

a <- merge(NB.PIB,  C.PIB, by = c("ID", "Esp", "Incert", "Dist", "Haut", "Diam", "Fleche", "Arbre"), all = TRUE)
Central <- merge(a,  L.PIB, by = c("ID", "Esp", "Incert", "Dist", "Haut", "Diam", "Fleche", "Arbre"), all = TRUE)
Central[is.na(Central)] <- 0

#Ajout au fichier central
Central$Esp <- NULL 
Central$Dist <- NULL
Central$Fleche <- NULL
Central$Arbre <- NULL
Central$Diam <- NULL
Central$Incert <- NULL
Central$Haut <- NULL
colnames(Central)[2:4] <- c("NB.cent", "C.cent", "L.cent")

GAB <- merge (GAB, Central, by = "ID", all.x = FALSE, all.y = TRUE)
GAB[is.na(GAB)] <- 0
GAB$Dispo.cent <- GAB$NB.cent + GAB$C.cent + GAB$L.cent

#Broutement sur TOUS les PIB
NB.PIB <- PetitesO[PetitesO$Esp == "PIB" & PetitesO$Classe == "NB",  ]
NB.PIB$Classe <- NULL
colnames(NB.PIB)[7] <- c("NB.pib")

C.PIB <- PetitesO[PetitesO$Esp == "PIB" & PetitesO$Classe == "C",  ]
C.PIB$Classe <- NULL
colnames(C.PIB)[7] <- c("C.pib")

L.PIB <- PetitesO[PetitesO$Esp == "PIB" & PetitesO$Classe == "L",  ]
L.PIB$Classe <- NULL
colnames(L.PIB)[7] <- c("L.pib")

#Aggréger pour avoir une ligne par parcelle
NB.PIB <- aggregate (NB.PIB$NB.pib, by= data.frame(NB.PIB$ID), FUN= "sum", na.rm= TRUE)
colnames(NB.PIB)[1:2] <- c("ID", "NB.pib")

C.PIB <- aggregate (C.PIB$C.pib, by= data.frame(C.PIB$ID), FUN= "sum", na.rm= TRUE)
colnames(C.PIB)[1:2] <- c("ID", "C.pib")

L.PIB <- aggregate (L.PIB$L.pib, by= data.frame(L.PIB$ID), FUN= "sum", na.rm= TRUE)
colnames(L.PIB)[1:2] <- c("ID", "L.pib")

#Ajout au fichier global
GAB <- merge (GAB, NB.PIB, by = "ID", all = TRUE)
GAB <- merge (GAB, C.PIB, by = "ID", all = TRUE)
GAB <- merge (GAB, L.PIB, by = "ID", all = TRUE)

GAB[is.na(GAB)] <- 0
GAB$Dispo.pib <- GAB$NB.pib + GAB$L.pib + GAB$C.pib

#Broutement sur TOUS les arbres
NB <- PetitesO[PetitesO$Classe == "NB",  ]
NB$Classe <- NULL
colnames(NB)[7] <- c("NB.tot")

C <- PetitesO[PetitesO$Classe == "C",  ]
C$Classe <- NULL
colnames(C)[7] <- c("C.tot")

L <- PetitesO[PetitesO$Classe == "L",  ]
L$Classe <- NULL
colnames(L)[7] <- c("L.tot")

#Aggréger pour avoir une ligne par parcelle
NB <- aggregate (NB$NB.tot, by= data.frame(NB$ID), FUN= "sum", na.rm= TRUE)
colnames(NB)[1:2] <- c("ID", "NB.tot")

C <- aggregate (C$C.tot, by= data.frame(C$ID), FUN= "sum", na.rm= TRUE)
colnames(C)[1:2] <- c("ID", "C.tot")

L <- aggregate (L$L.tot, by= data.frame(L$ID), FUN= "sum", na.rm= TRUE)
colnames(L)[1:2] <- c("ID", "L.tot")

#Ajout au fichier global
GAB <- merge (GAB, NB, by = "ID", all.x = TRUE, all.y = FALSE)
GAB <- merge (GAB, C, by = "ID", all.x = TRUE, all.y = FALSE)
GAB <- merge (GAB, L, by = "ID", all.x = TRUE, all.y = FALSE)

GAB[is.na(GAB)] <- 0
GAB$Dispo.tot <- GAB$NB.tot + GAB$L.tot + GAB$C.tot

#####Indices#####
library(vegan)

#Colonnes 2 à 34 sont les valeurs d'espèces
GAB$shannon <- diversity(GAB[,c(2:33)], "shannon")
GAB$simpson <- diversity(GAB[,c(2:33)], "simpson")
GAB$richness <- specnumber(GAB[,c(2:33)])

####Data exploration####
mat <- as.data.frame(cor(GAB[, 2:48]))
mat2 <- mat^2
round(mat2, digits = 2)


####Analyses simple####
library(AICcmodavg)
library(MASS)
GAB$Consom.cent <- GAB$C.cent/GAB$Dispo.cent
GAB$Consom.pib <- GAB$C.pib/GAB$Dispo.pib
GAB$Consom.tot <- GAB$C.tot/GAB$Dispo.tot

GAB$Densit <- GAB$AMsp + GAB$BOJ + GAB$BOP + GAB$CAC + GAB$CEP + GAB$CET + GAB$CHG + GAB$CHR + 
  GAB$CHsp + GAB$COS + GAB$COV + GAB$DIM + GAB$EPB + GAB$ERE + GAB$ERR + GAB$ERS + GAB$HEG + 
  GAB$INC + GAB$MEL + GAB$NOL + GAB$OSV + GAB$PEB + GAB$PEF + GAB$PEG + GAB$PIB + GAB$PIR + 
  GAB$SAB + GAB$SALIX + GAB$SUV + GAB$THC + GAB$TIA + GAB$VAC

#Avec une régression logistique succès/essais
test <- glm(Consom.cent ~ shannon, weights = Dispo.cent, family=binomial, data=GAB)
c_hat(test) #valeur = 4.017109. Sous 4, on peut utiliser le modèle.
test <- glm(Consom.pib ~ shannon, weights = Dispo.pib, family=binomial, data=GAB)
c_hat(test) #4.463042
test <- glm(Consom.tot ~ shannon, weights = Dispo.tot, family=binomial, data=GAB)
c_hat(test) #21.33204

test <- glm(Consom.cent ~ simpson, weights = Dispo.cent, family=binomial, data=GAB)
c_hat(test) #3.954771
test <- glm(Consom.pib ~ simpson, weights = Dispo.pib, family=binomial, data=GAB)
c_hat(test) #4.422942
test <- glm(Consom.tot ~ simpson, weights = Dispo.tot, family=binomial, data=GAB)
c_hat(test) #20.84924

test <- glm(Consom.cent ~ richness, weights = Dispo.cent, family=binomial, data=GAB)
c_hat(test) #4.050104
test <- glm(Consom.pib ~ richness, weights = Dispo.pib, family=binomial, data=GAB)
c_hat(test) #4.441381
test <- glm(Consom.tot ~ richness, weights = Dispo.tot, family=binomial, data=GAB)
c_hat(test) #22.43207

#GLM de poisson avec offset
test <- glm(C.cent ~ shannon + offset(log(Dispo.cent)), family=poisson, data=GAB)
c_hat(test) #3.194066
test <- glm(C.pib ~ shannon + offset(log(Dispo.pib)), family=poisson, data=GAB)
c_hat(test) #3.692504
test <- glm(C.tot ~ shannon + offset(log(Dispo.tot)), family=poisson, data=GAB)
c_hat(test) #16.13834, doit aller avec la négative binomiale

test <- glm(C.cent ~ simpson + offset(log(Dispo.cent)), family=poisson, data=GAB)
c_hat(test) #3.143594
test <- glm(C.pib ~ simpson + offset(log(Dispo.pib)), family=poisson, data=GAB)
c_hat(test) #3.677164
test <- glm(C.tot ~ simpson + offset(log(Dispo.tot)), family=poisson, data=GAB)
c_hat(test) #15.82413, doit aller avec la négative binomiale

test <- glm(C.cent ~ richness + offset(log(Dispo.cent)), family=poisson, data=GAB)
c_hat(test) #3.242525
test <- glm(C.pib ~ richness + offset(log(Dispo.pib)), family=poisson, data=GAB)
c_hat(test) #3.66726
test <- glm(C.tot ~ richness + offset(log(Dispo.tot)), family=poisson, data=GAB)
c_hat(test) #17.18688, doit aller avec la négative binomiale

#Modèles

#PIB central/Shannon
Cent.S <- glm(C.cent ~ shannon + offset(log(Dispo.cent)), family=poisson, data=GAB)
c_hat(Cent.S)
summary(Cent.S, dispersion = 3.194066) #No significant effects
  #Résidus
plot(rstudent(Cent.S) ~ fitted(Cent.S), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
  #Influence
plot(cooks.distance(Cent.S), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)#68 et 85 très influentes
identify(cooks.distance(Cent.S))
temp <- GAB[c(1:67, 69:84, 86:103), ]
Cent.S2 <- glm(C.cent ~ shannon + offset(log(Dispo.cent)), family=poisson, data=temp)
c_hat(Cent.S2)
summary(Cent.S2, dispersion = 2.943308) #No significant effects, no changes without outlier

  #Adding density
  Cent.S3 <- glm(C.cent ~ shannon + Densit + offset(log(Dispo.cent)), family=poisson, data=GAB)
  c_hat(Cent.S3)
  summary(Cent.S3, dispersion = 3.228639)#No significant effects

#PIB central/Simpson
Cent.Si <- glm(C.cent ~ simpson + offset(log(Dispo.cent)), family=poisson, data=GAB)
c_hat(Cent.Si)
summary(Cent.Si, dispersion = 3.143594) #p = 0.08 Tendance vers un effet négatif
#Résidus
plot(rstudent(Cent.Si) ~ fitted(Cent.Si), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(Cent.Si), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)#68 et 85 très influentes
identify(cooks.distance(Cent.Si))
temp <- GAB[c(1:67, 69:84, 86:103), ]
Cent.Si2 <- glm(C.cent ~ simpson + offset(log(Dispo.cent)), family=poisson, data=temp)
c_hat(Cent.Si2)
summary(Cent.Si2, dispersion = 2.961774) #No significant effects

plot(Consom.cent ~ simpson, data = GAB)#Effet pas très visuel et dépend simplement de deux outliers

  #Adding density
  Cent.Si3 <- glm(C.cent ~ simpson + Densit + offset(log(Dispo.cent)), family=poisson, data=GAB)
  c_hat(Cent.Si3)
  summary(Cent.Si3, dispersion = 3.182304) #p = 0.12 for simpson 


#PIB central/Richness
Cent.r <- glm(C.cent ~ richness + offset(log(Dispo.cent)), family=poisson, data=GAB)
c_hat(Cent.r)
summary(Cent.r, dispersion = 3.239678) #No significant effects
#Résidus
plot(rstudent(Cent.r) ~ fitted(Cent.r), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(Cent.r), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)#68
identify(cooks.distance(Cent.r))
temp <- GAB[c(1:67, 69:103), ]
Cent.r2 <- glm(C.cent ~ richness + offset(log(Dispo.cent)), family=poisson, data=temp)
c_hat(Cent.r2)
summary(Cent.r2, dispersion = 2.883034) #Significant effect of richness
temp <- GAB[c(1:67, 69:84, 86:103), ]
Cent.r2 <- glm(C.cent ~ richness + offset(log(Dispo.cent)), family=poisson, data=temp)
c_hat(Cent.r2)
summary(Cent.r2, dispersion = 2.883034) #Significant effect of richness

plot(Consom.cent ~ richness, data = GAB)#Belle relation linéaire, mais pas significative avec obs. 68

  #Adding density
  Cent.r3 <- glm(C.cent ~ richness + Densit + offset(log(Dispo.cent)), family=poisson, data=GAB)
  c_hat(Cent.r3)
  summary(Cent.r3, dispersion = 3.265033) #Richness no longer significant
  temp <- GAB[c(1:67, 69:84, 86:103), ]
  Cent.r4 <- glm(C.cent ~ richness + Densit + offset(log(Dispo.cent)), family=poisson, data=temp)
  c_hat(Cent.r4)
  summary(Cent.r4, dispersion = 2.743154) #Richness significant. Not density

#Central/Density
  Cent.d <- glm(C.cent ~ Densit + offset(log(Dispo.cent)), family=poisson, data=GAB)
  c_hat(Cent.d)
summary(Cent.d, dispersion = 3.207527) #No significant effects
#Résidus
plot(rstudent(Cent.d) ~ fitted(Cent.d), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(Cent.d), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)#68, 85
identify(cooks.distance(Cent.d))
temp <- GAB[c(1:67, 69:84, 86:103), ]
Cent.d2 <- glm(C.cent ~ Densit + offset(log(Dispo.cent)), family=poisson, data=temp)
c_hat(Cent.d2)
summary(Cent.d2, dispersion = 2.964096) #No significant effects

#PIB/Shannon
pib.S <- glm(C.pib ~ shannon + offset(log(Dispo.pib)), family=poisson, data=GAB)
c_hat(pib.S)
summary(pib.S, dispersion = 3.692649) #No significant effects
#Résidus
plot(rstudent(pib.S) ~ fitted(pib.S), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(pib.S), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)
identify(cooks.distance(pib.S))#68,70,85
temp <- GAB[c(1:67, 69, 71:84, 86:103), ]
pib.S2 <- glm(C.pib ~ shannon + offset(log(Dispo.pib)), family=poisson, data=temp)
c_hat(pib.S2)
summary(pib.S2, dispersion = 3.447879) #No significant effects

#PIB/Simpson
pib.Si <- glm(C.pib ~ simpson + offset(log(Dispo.pib)), family=poisson, data=GAB)
c_hat(pib.Si)
summary(pib.Si, dispersion = 3.677164) #p = 0.03 Effet négatif
#Résidus
plot(rstudent(pib.Si) ~ fitted(pib.Si), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(pib.Si), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)
identify(cooks.distance(pib.Si))#1, 68,70,85
temp <- GAB[c(2:67, 69, 71:84, 86:103), ]
pib.Si2 <- glm(C.pib ~ simpson + offset(log(Dispo.pib)), family=poisson, data=temp)
c_hat(pib.Si2)
summary(pib.Si2, dispersion = 3.356899) #No significant effects

plot(Consom.pib ~ simpson, data = GAB)#Effet pas très visuel et dépendant des outliers

  #Adding density
  pib.Si3 <- glm(C.pib ~ simpson + Densit + offset(log(Dispo.pib)), family=poisson, data=GAB)
  c_hat(pib.Si3)
  summary(pib.Si3, dispersion = 3.672716) #p = 0.03 Effet négatif

#PIB/Richness
pib.r <- glm(C.pib ~ richness + offset(log(Dispo.pib)), family=poisson, data=GAB)
c_hat(pib.r)
summary(pib.r, dispersion = 3.66726) #No significant effects
#Résidus
plot(rstudent(pib.r) ~ fitted(pib.r), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(pib.r), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)
identify(cooks.distance(pib.r))#68,70
temp <- GAB[c(1:67, 69, 71:103), ]
pib.r2 <- glm(C.pib ~ richness + offset(log(Dispo.pib)), family=poisson, data=temp)
c_hat(pib.r2)
summary(pib.r2, dispersion = 3.480454) #No significant effects

plot(Consom.pib ~ richness, data = GAB)

  #Adding density
  pib.r3 <- glm(C.pib ~ richness + Densit + offset(log(Dispo.pib)), family=poisson, data=GAB)
  c_hat(pib.r3)
  summary(pib.r3, dispersion = 3.69471) #No significant effects
  
#PIB/Density
pib.d <- glm(C.pib ~ Densit + offset(log(Dispo.pib)), family=poisson, data=GAB)
c_hat(pib.d)
summary(pib.d, dispersion = 3.700316) #No significant effects
#Résidus
plot(rstudent(pib.d) ~ fitted(pib.d), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(pib.d), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)


#All/Shannon
all.s <- glm.nb(C.tot ~ shannon + offset(log(Dispo.tot)), data=GAB)
summary(all.s)
#Résidus
plot(rstudent(all.s) ~ fitted(all.s), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(all.s), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)

  #Adding density
  all.s2 <- glm.nb(C.tot ~ shannon + Densit + offset(log(Dispo.tot)), data=GAB)
  summary(all.s2)


#All/Simpson
all.si <- glm.nb(C.tot ~ simpson + offset(log(Dispo.tot)), data=GAB)
summary(all.si)
#Résidus
plot(rstudent(all.si) ~ fitted(all.si), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(all.si), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)

  #Adding density
  all.si2 <- glm.nb(C.tot ~ simpson + Densit + offset(log(Dispo.tot)), data=GAB)
  summary(all.si2)


#All/Richness
all.r <- glm.nb(C.tot ~ richness + offset(log(Dispo.tot)), data=GAB)
summary(all.r)
#Résidus
plot(rstudent(all.r) ~ fitted(all.r), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(all.r), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)

  #Adding density
all.r2 <- glm.nb(C.tot ~ richness + Densit + offset(log(Dispo.tot)), data=GAB)
summary(all.r2)


####Broutement 0/1#####
GAB$Prob.cent <- ifelse(GAB$Consom.cent > 0, 1, 0)
GAB$Prob.pib <- ifelse(GAB$Consom.pib > 0, 1, 0)
GAB$Prob.tot <- ifelse(GAB$Consom.tot > 0, 1, 0)

test <- glm(Prob.cent ~ shannon, family=binomial, data=GAB)
summary(test)
test <- glm(Prob.pib ~ shannon, family=binomial, data=GAB)
summary(test)
test <- glm(Prob.tot ~ shannon, family=binomial, data=GAB)
summary(test)

test <- glm(Prob.cent ~ simpson, family=binomial, data=GAB)
summary(test)
test <- glm(Prob.pib ~ simpson, family=binomial, data=GAB)
summary(test)
test <- glm(Prob.tot ~ simpson, family=binomial, data=GAB)
summary(test)

test <- glm(Prob.cent ~ richness, family=binomial, data=GAB)
summary(test) #significant
test <- glm(Prob.pib ~ richness, family=binomial, data=GAB)
summary(test)
test <- glm(Prob.tot ~ richness, family=binomial, data=GAB)
summary(test)#tendency


#####PCA#####
#PCA pour les espèces pour remplacer par une ou deux variables.

pca <- princomp(GAB[, c(2:18, 20:33)], cor = FALSE)
summary(pca)
pca$loadings
biplot(pca)
#Pas de regroupement clairs, variabilité expliqué par chaque axe faible (axe 1 = 0.265).

#####PCA avec transformation de Hellinger####

#Transformation recommandé pour données d'abondance avec bcp de 0 (Legendre & Gallagher 2001)
a<- decostand(GAB[, c(2:18, 20:33)], "hellinger")
pca <- princomp(a, cor = FALSE)
summary(pca)
pca$loadings
biplot(pca)

#Significativité des axes
nsims <- 1000
##create object to hold data
output <- matrix(NA, nrow = nsims, ncol = 4)

##initiate loop
for(i in 1:nsims) {
  ##randomize data and combine in a dataset
  combo <- as.data.frame(apply(a, 2, function(x) sample(x, size = length(a[, 1]), replace = FALSE)))
  
  ##run PCA on randomized data
  rand_pca <- princomp(combo, cor = FALSE)
  
  ##extract eigenvalues
  output[i, 1] <- rand_pca$sdev[1]^2
  output[i, 2] <- rand_pca$sdev[2]^2
  output[i, 3] <- rand_pca$sdev[3]^2
  output[i, 4] <- rand_pca$sdev[4]^2
}

layout(matrix(1:4, nrow = 2, ncol = 2))
hist(output[, 1], main = expression(paste("Histogramme de freuences de ", italic(lambda), " randomises pour axe 1")),
      xlim = c(0, 0.2), xlab = "Axe 1 de PCA", cex.lab = 1.2)
abline(v = pca$sdev[1]^2, col = "red", lwd = 2)
hist(output[, 2], main = expression(paste("Histogramme de frequences de ", italic(lambda), " randomises pour axe 2")),
     xlim = c(0, 0.2), xlab = "Axe 2 de PCA", cex.lab = 1.2)
abline(v = pca$sdev[2]^2, col = "red", lwd = 2)
hist(output[, 3], main = expression(paste("Histogramme de frequences de ", italic(lambda), " randomises pour axe 3")),
     xlim = c(0, 0.2), xlab = "Axe 3 de PCA", cex.lab = 1.2)
abline(v = pca$sdev[3]^2, col = "red", lwd = 2)
hist(output[, 4], main = expression(paste("Histogramme de frequences de ", italic(lambda), " randomises pour axe 4")),
     xlim = c(0, 0.2), xlab = "Axe 4 de PCA", cex.lab = 1.2)
abline(v = pca$sdev[4]^2, col = "red", lwd = 2)
p1 <- length(which(output[, 1] >= pca$sdev[1]^2))/nsims  #< 0.001 - first axis is important
p2 <- length(which(output[, 2] >= pca$sdev[2]^2))/nsims
p3 <- length(which(output[, 3] >= pca$sdev[3]^2))/nsims
p4 <- length(which(output[, 4] >= pca$sdev[4]^2))/nsims

#The three first axis explain more variability then randomized variables
b <- as.data.frame(pca$scores[, 1:3])
GAB <- cbind(GAB, b)

##Analyse with axis of PCA
#Axis 1, strong + of vac, less strong - of maples
#Axis 2, strong - of NOL, + of maples
#Axis 3, neg of CHsp, + of maples and NOL

layout(matrix(1, nrow = 1, ncol = 1))
#Central pib
Cent.pca <- glm(C.cent ~ Comp.1 + Comp.2 + Comp.3 + offset(log(Dispo.cent)), family=poisson, data=GAB)
c_hat(Cent.pca)
summary(Cent.pca, dispersion = 2.977971) #Effect of axis 1 and axis 3
#Résidus
plot(rstudent(Cent.pca) ~ fitted(Cent.pca), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(Cent.pca), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)#68 et 85 très influentes
identify(cooks.distance(Cent.pca))
temp <- GAB[c(1:67, 69:84, 86:103), ]
Cent.pca2 <- glm(C.cent ~ Comp.1 + Comp.2 + Comp.3 + offset(log(Dispo.cent)), family=poisson, data=temp)
c_hat(Cent.pca2)
summary(Cent.pca2, dispersion = 2.857457) #Effect of axis 1

plot(C.cent ~ Comp.1, data = GAB)#Effet pas très visuel

#pib
PIB.pca <- glm(C.pib ~ Comp.1 + Comp.2 + Comp.3 + offset(log(Dispo.pib)), family=poisson, data=GAB)
c_hat(PIB.pca)
summary(PIB.pca, dispersion = 3.468568) #Effect of axis 3
#Résidus
plot(rstudent(PIB.pca) ~ fitted(PIB.pca), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(PIB.pca), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)#68 et 70 très influentes
identify(cooks.distance(PIB.pca))
temp <- GAB[c(1:67, 69, 71:103), ]
PIB.pca2 <- glm(C.pib ~ Comp.1 + Comp.2 + Comp.3 + offset(log(Dispo.pib)), family=poisson, data=temp)
c_hat(PIB.pca2)
summary(PIB.pca2, dispersion = 3.29488) #Effect of axis 3

plot(C.pib ~ Comp.3, data = GAB)#Effet pas très visuel

#All browsing
all.pca <- glm.nb(C.tot ~ Comp.1 + Comp.2 + Comp.3 + offset(log(Dispo.tot)), data=GAB)
summary(all.pca)#Nothing
#Résidus
plot(rstudent(all.pca) ~ fitted(all.pca), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(all.pca), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)

#####PCA with less species####

#Removing species that are in 5 or less plots
colSums(GAB != 0)
#AMsp, BOJ, CAC, CEP, CET, CHG, INC, MEL, PEB, SALIX, SUV, THC, TIA
#Transformation recommandé pour données d'abondance avec bcp de 0 (Legendre & Gallagher 2001)
a<- decostand(GAB[, c(4, 9:18, 21:22, 24:28, 33)], "hellinger")
pca <- princomp(a, cor = FALSE)
summary(pca)
pca$loadings
biplot(pca)

#Significativité des axes
nsims <- 1000
##create object to hold data
output <- matrix(NA, nrow = nsims, ncol = 4)

##initiate loop
for(i in 1:nsims) {
  ##randomize data and combine in a dataset
  combo <- as.data.frame(apply(a, 2, function(x) sample(x, size = length(a[, 1]), replace = FALSE)))
  
  ##run PCA on randomized data
  rand_pca <- princomp(combo, cor = FALSE)
  
  ##extract eigenvalues
  output[i, 1] <- rand_pca$sdev[1]^2
  output[i, 2] <- rand_pca$sdev[2]^2
  output[i, 3] <- rand_pca$sdev[3]^2
  output[i, 4] <- rand_pca$sdev[4]^2
}

layout(matrix(1:4, nrow = 2, ncol = 2))
hist(output[, 1], main = expression(paste("Histogramme de freuences de ", italic(lambda), " randomises pour axe 1")),
     xlim = c(0, 0.2), xlab = "Axe 1 de PCA", cex.lab = 1.2)
abline(v = pca$sdev[1]^2, col = "red", lwd = 2)
hist(output[, 2], main = expression(paste("Histogramme de frequences de ", italic(lambda), " randomises pour axe 2")),
     xlim = c(0, 0.2), xlab = "Axe 2 de PCA", cex.lab = 1.2)
abline(v = pca$sdev[2]^2, col = "red", lwd = 2)
hist(output[, 3], main = expression(paste("Histogramme de frequences de ", italic(lambda), " randomises pour axe 3")),
     xlim = c(0, 0.2), xlab = "Axe 3 de PCA", cex.lab = 1.2)
abline(v = pca$sdev[3]^2, col = "red", lwd = 2)
hist(output[, 4], main = expression(paste("Histogramme de frequences de ", italic(lambda), " randomises pour axe 4")),
     xlim = c(0, 0.2), xlab = "Axe 4 de PCA", cex.lab = 1.2)
abline(v = pca$sdev[4]^2, col = "red", lwd = 2)
p1 <- length(which(output[, 1] >= pca$sdev[1]^2))/nsims  #< 0.001 - first axis is important
p2 <- length(which(output[, 2] >= pca$sdev[2]^2))/nsims
p3 <- length(which(output[, 3] >= pca$sdev[3]^2))/nsims
p4 <- length(which(output[, 4] >= pca$sdev[4]^2))/nsims
layout(matrix(1:1, nrow = 1, ncol = 1))
#The three first axis explain more variability then randomized variables
GAB$Comp.1 <- NULL
GAB$Comp.2 <- NULL
GAB$Comp.3 <- NULL
b <- as.data.frame(pca$scores[, 1:3])
GAB <- cbind(GAB, b)

##Analyse with axis of PCA
#Axis 1, strong + of vac, less strong - of maples
#Axis 2, strong - of NOL, + of maples
#Axis 3, neg of CHsp, + of maples and NOL

layout(matrix(1, nrow = 1, ncol = 1))

#Central pib
Cent.pca <- glm(C.cent ~ Comp.1 + Comp.2 + Comp.3 + offset(log(Dispo.cent)), family=poisson, data=GAB)
c_hat(Cent.pca)
summary(Cent.pca, dispersion = 2.963171) #Effect of axis 1 and axis 3
#Résidus
plot(rstudent(Cent.pca) ~ fitted(Cent.pca), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(Cent.pca), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)#68 et 85 très influentes
identify(cooks.distance(Cent.pca))
temp <- GAB[c(1:67, 69:84, 86:103), ]
Cent.pca2 <- glm(C.cent ~ Comp.1 + Comp.2 + Comp.3 + offset(log(Dispo.cent)), family=poisson, data=temp)
c_hat(Cent.pca2)
summary(Cent.pca2, dispersion = 2.857457) #Effect of axis 1

plot(C.cent ~ Comp.1, data = GAB)#Effet pas très visuel

#pib
PIB.pca <- glm(C.pib ~ Comp.1 + Comp.2 + Comp.3 + offset(log(Dispo.pib)), family=poisson, data=GAB)
c_hat(PIB.pca)
summary(PIB.pca, dispersion = 3.458244) #Effect of axis 3
#Résidus
plot(rstudent(PIB.pca) ~ fitted(PIB.pca), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(PIB.pca), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)#70 très influentes
identify(cooks.distance(PIB.pca))
temp <- GAB[c(1:69, 71:103), ]
PIB.pca2 <- glm(C.pib ~ Comp.1 + Comp.2 + Comp.3 + offset(log(Dispo.pib)), family=poisson, data=temp)
c_hat(PIB.pca2)
summary(PIB.pca2, dispersion = 3.423375) #Effect of axis 3

plot(C.pib ~ Comp.3, data = GAB)#Effet plus visuel

#All browsing
all.pca <- glm.nb(C.tot ~ Comp.1 + Comp.2 + Comp.3 + offset(log(Dispo.tot)), data=GAB)
summary(all.pca)#Nothing
#Résidus
plot(rstudent(all.pca) ~ fitted(all.pca), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(all.pca), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)


#Removing species that are in <=10  plots
colSums(GAB != 0)
#AMsp, BOJ, CAC, CEP, CET, CHG, COS, EPB INC, MEL, PEB, PIR, SALIX, SUV, THC, TIA
#Transformation recommandé pour données d'abondance avec bcp de 0 (Legendre & Gallagher 2001)
a<- decostand(GAB[, c(4, 9:10, 12:13, 15:18, 21:22, 24:26, 28, 33)], "hellinger")
pca <- princomp(a, cor = FALSE)
summary(pca)
pca$loadings
biplot(pca)
#more variability explained by axis than in the 2 previous PCA

#Significativité des axes
nsims <- 1000
##create object to hold data
output <- matrix(NA, nrow = nsims, ncol = 4)

##initiate loop
for(i in 1:nsims) {
  ##randomize data and combine in a dataset
  combo <- as.data.frame(apply(a, 2, function(x) sample(x, size = length(a[, 1]), replace = FALSE)))
  
  ##run PCA on randomized data
  rand_pca <- princomp(combo, cor = FALSE)
  
  ##extract eigenvalues
  output[i, 1] <- rand_pca$sdev[1]^2
  output[i, 2] <- rand_pca$sdev[2]^2
  output[i, 3] <- rand_pca$sdev[3]^2
  output[i, 4] <- rand_pca$sdev[4]^2
}

layout(matrix(1:4, nrow = 2, ncol = 2))
hist(output[, 1], main = expression(paste("Histogramme de freuences de ", italic(lambda), " randomises pour axe 1")),
     xlim = c(0, 0.2), xlab = "Axe 1 de PCA", cex.lab = 1.2)
abline(v = pca$sdev[1]^2, col = "red", lwd = 2)
hist(output[, 2], main = expression(paste("Histogramme de frequences de ", italic(lambda), " randomises pour axe 2")),
     xlim = c(0, 0.2), xlab = "Axe 2 de PCA", cex.lab = 1.2)
abline(v = pca$sdev[2]^2, col = "red", lwd = 2)
hist(output[, 3], main = expression(paste("Histogramme de frequences de ", italic(lambda), " randomises pour axe 3")),
     xlim = c(0, 0.2), xlab = "Axe 3 de PCA", cex.lab = 1.2)
abline(v = pca$sdev[3]^2, col = "red", lwd = 2)
hist(output[, 4], main = expression(paste("Histogramme de frequences de ", italic(lambda), " randomises pour axe 4")),
     xlim = c(0, 0.2), xlab = "Axe 4 de PCA", cex.lab = 1.2)
abline(v = pca$sdev[4]^2, col = "red", lwd = 2)
p1 <- length(which(output[, 1] >= pca$sdev[1]^2))/nsims  #< 0.001 - first axis is important
p2 <- length(which(output[, 2] >= pca$sdev[2]^2))/nsims
p3 <- length(which(output[, 3] >= pca$sdev[3]^2))/nsims
p4 <- length(which(output[, 4] >= pca$sdev[4]^2))/nsims

#The three first axis explain more variability then randomized variables
GAB$Comp.1 <- NULL
GAB$Comp.2 <- NULL
GAB$Comp.3 <- NULL
b <- as.data.frame(pca$scores[, 1:3])
GAB <- cbind(GAB, b)

##Analyse with axis of PCA
layout(matrix(1, nrow = 1, ncol = 1))

#Central pib
Cent.pca <- glm(C.cent ~ Comp.1 + Comp.2 + Comp.3 + offset(log(Dispo.cent)), family=poisson, data=GAB)
c_hat(Cent.pca)
summary(Cent.pca, dispersion = 2.90379) #Effect of axis 1 and axis 3
#Résidus
plot(rstudent(Cent.pca) ~ fitted(Cent.pca), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(Cent.pca), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)#68 et 85 très influentes
identify(cooks.distance(Cent.pca))
temp <- GAB[c(1:67, 69:84, 86:103), ]
Cent.pca2 <- glm(C.cent ~ Comp.1 + Comp.2 + Comp.3 + offset(log(Dispo.cent)), family=poisson, data=temp)
c_hat(Cent.pca2)
summary(Cent.pca2, dispersion = 2.819679) #Effect of axis 1

plot(C.cent ~ Comp.1, data = GAB)#Effet pas très visuel

#pib
PIB.pca <- glm(C.pib ~ Comp.1 + Comp.2 + Comp.3 + offset(log(Dispo.pib)), family=poisson, data=GAB)
c_hat(PIB.pca)
summary(PIB.pca, dispersion = 3.409321) #Effect of axis 2 and 3
#Résidus
plot(rstudent(PIB.pca) ~ fitted(PIB.pca), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(PIB.pca), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)#70 très influentes
identify(cooks.distance(PIB.pca))
temp <- GAB[c(1:69, 71:103), ]
PIB.pca2 <- glm(C.pib ~ Comp.1 + Comp.2 + Comp.3 + offset(log(Dispo.pib)), family=poisson, data=temp)
c_hat(PIB.pca2)
summary(PIB.pca2, dispersion = 3.289649) #Effect of axis 2, tendency

plot(C.pib ~ Comp.2, data = GAB)#Effet pas visuel

#All browsing
all.pca <- glm.nb(C.tot ~ Comp.1 + Comp.2 + Comp.3 + offset(log(Dispo.tot)), data=GAB)
summary(all.pca)#Nothing
#Résidus
plot(rstudent(all.pca) ~ fitted(all.pca), ylab = "Résidus de Student", xlab = "Valeurs prédites",main = "Résidus de Student vs valeurs prédites")
#Influence
plot(cooks.distance(all.pca), ylab = "Distance de Cook", xlab = "Observations", 
     main = "Influence des observations")
abline(h = 4/(103-2), lty = 2)

####Adding environmental factors####
Env <-read.table(file="D:\\Documents\\Doctorat\\Chap 6 Outaouais\\Attributs_parcelles.txt", header= TRUE, na.string= ".", stringsAsFactors = TRUE)

GAB <- merge(GAB, Env, by.all = "ID", all.x = TRUE, all.y = TRUE, sort = TRUE)

#Analyse simple des facteurs environnementaux sur broutement
#Central
test <- glm(C.cent ~ Transect + offset(log(Dispo.cent)), family=poisson, data=GAB)
c_hat(test)
summary(test, dispersion = 2.171012) #No significant effects

test <- glm(C.cent ~ Perturb + offset(log(Dispo.cent)), family=poisson, data=GAB)
c_hat(test)
summary(test, dispersion = 3.147642) #No significant effects

test <- glm(C.cent ~ An_pert + offset(log(Dispo.cent)), family=poisson, data=GAB)
c_hat(test)
summary(test, dispersion = 3.29139)
plot(GAB$C.cent ~ GAB$An_pert)#On a pas vraiment la distribution pour tester ça

test <- glm(C.cent ~ Plant + offset(log(Dispo.cent)), family=poisson, data=GAB)
c_hat(test)
summary(test, dispersion = 3.251456)
boxplot(GAB$C.cent ~ GAB$Plant)#Browsing higher in plantations

#All pib
test <- glm(C.pib ~ Transect + offset(log(Dispo.pib)), family=poisson, data=GAB)
c_hat(test)
summary(test, dispersion = 2.672063) #Some different from others
boxplot(GAB$C.pib ~ GAB$Transect)

test <- glm(C.pib ~ Perturb + offset(log(Dispo.pib)), family=poisson, data=GAB)
c_hat(test)
summary(test, dispersion = 3.216604) #Tendency for some differences
boxplot(GAB$C.pib ~ GAB$Perturb)#Higher in degagement, logical since it's also linked with plantation

test <- glm(C.pib ~ An_pert + offset(log(Dispo.pib)), family=poisson, data=GAB)
c_hat(test)
summary(test, dispersion = 3.963787)
plot(GAB$C.pib ~ GAB$An_pert)#On a pas vraiment la distribution pour tester ça

test <- glm(C.pib ~ Plant + offset(log(Dispo.pib)), family=poisson, data=GAB)
c_hat(test)
summary(test, dispersion = 3.378257)
boxplot(GAB$C.pib ~ GAB$Plant)#Browsing higher in plantations

####RDA####
species<- decostand(GAB[, c(2:18, 20:33)], "hellinger")
rda <- rda(species ~ GAB$Perturb + GAB$An_pert + GAB$Transect + GAB$Plant, na.action = na.exclude)
rda
summary(rda)
plot(rda)
