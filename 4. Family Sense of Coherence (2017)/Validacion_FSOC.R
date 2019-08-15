setwd("/Users/home/Documents/Dropbox_260118/Papers futuros/1. Family SOC/Valdacion F-SOC")
getwd()

list.files()
#rm(list=ls())

library(haven)
library(lavaan)
library(psych)
library(GPArotation)
library(MVN)
library(dplyr)

data_FSOC <- read_sav("Base_estudio f-soc_adaptacion_familiar.sav")

head(data.frame(data_FSOC))
names(data_FSOC)

########### ########### ########### ########### ########### 
########### ########### ########### ########### ########### 
########### ########### ########### ########### ########### 
# Normalidad multivariada

data_FSOC_mvn <- na.omit(data_FSOC)

Mardia <- mvn(data_FSOC_mvn[1:12], mvnTest = "mardia")
Mardia$multivariateNormality
HZ <- mvn(data_FSOC_mvn[1:12], mvnTest = "hz",)
HZ$multivariateNormality
########### ########### ########### ########### ########### 
########### ########### ########### ########### ########### 
########### ########### ########### ########### ########### 
# CFA

# Sacar el ítem 1. 

modelcfa1 <- 'f=~ FSOC1_INVERSO + FSOC2_INVERSO+ FSOC3_INVERSO+ FSOC4+
FSOC5+ FSOC6+ FSOC7+ FSOC8_INVERSO+ FSOC9+ FSOC10_INVERSO+ FSOC11+ FSOC12_INVERSO'

fitcfa1 <- cfa(modelcfa1, data = data_FSOC,
               estimator = "WLSMV", mimic = "Mplus")
summary(fitcfa1, fit.measures = T, standardized = T)

########### ########### ########### ########### ########### 

# Sin el ítem 1. 

modelcfa2 <- 'f=~ FSOC2_INVERSO+ FSOC3_INVERSO+ FSOC4+
FSOC5+ FSOC6+ FSOC7+ FSOC8_INVERSO+ FSOC9+ FSOC10_INVERSO+ FSOC11+ FSOC12_INVERSO'

fitcfa2 <- cfa(modelcfa2, data = data_FSOC,
               estimator = "WLSMV", mimic = "Mplus")
summary(fitcfa2, fit.measures = T, standardized = T)

mi <- inspect(fitcfa2, "mi")
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]

########### ########### ########### ########### ########### 
########### ########### ########### ########### ########### 
########### ########### ########### ########### ########### 
# Correlacion entre items. 

modelcfa3 <- 'FSOC=~ FSOC2_INVERSO+ FSOC3_INVERSO+ FSOC4+
FSOC5+ FSOC6+ FSOC7+ FSOC8_INVERSO+ FSOC9+ FSOC10_INVERSO+ FSOC11+ FSOC12_INVERSO
FSOC7 ~~ FSOC11
FSOC5 ~~ FSOC11'

fitcfa3 <- cfa(modelcfa3, data = data_FSOC,
               estimator = "WLSMV", mimic = "Mplus")
summary(fitcfa3, fit.measures = T, standardized = T)

mi <- inspect(fitcfa3, "mi")
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]

########### ########### ########### ########### ########### 
########### ########### ########### ########### ########### 
########### ########### ########### ########### ########### 
# Omega y Alpha

names(data_FSOC)

omegaSem(data_FSOC[,c(2:12)],
         fm = "ml", flip = T, nfactors = 1) 

alpha(data_FSOC[,c(2:12)])
dim(data_FSOC)
########### ########### ########### ########### ########### 
########### ########### ########### ########### ########### 
########### ########### ########### ########### ########### 
# Composite reliability y AVE


#Funciones
# Composite reliability

comp.rel <- function(x,y) {
  operacion <- (sum(x) * sum(x))/(sum(x) * sum(x) + sum(y))
  return(operacion)
}


# AVE

ave <- function(x) {
  operacion <- sum(x^2)/length(x)
  return(operacion)
}



########### ########### ########### ########### ########### 
# Extraer las cargas y los errores

conf.comp <- parameterEstimates(fitcfa3, standardized = T)
#conf.comp
# Cargas factoriales
comp_FSOC <- conf.comp$std.all[c(1:11)]


#conf.comp$std.all
# Errores de las cargas
errores_FSOC <- conf.comp$std.all[c(14:24)]

########### ########### ########### ########### ########### 
# Calculo AVE y Comp

# Composite reliability

comp.rel(comp_FSOC,errores_FSOC)

# AVE

ave(comp_FSOC)

########### ########### ########### ########### ########### 
########### ########### ########### ########### ########### 
########### ########### ########### ########### ########### 
#list.files()
# Multigrupo

# Recoger la data original

data_mg <- read_sav("Base_estudio adaptacion_familiar_estres_esperanza.sav") 
names(data_mg)
data_mg <- data_mg[,c(1,3,12:23)]
dim(data_mg)

data_mg$FSOC2 <- as.numeric(as.character(data_mg$FSOC2))
data_mg$FSOC3 <- as.numeric(as.character(data_mg$FSOC3))
data_mg$FSOC4 <- as.numeric(as.character(data_mg$FSOC4))
data_mg$FSOC5 <- as.numeric(as.character(data_mg$FSOC5))
data_mg$FSOC6 <- as.numeric(as.character(data_mg$FSOC6))
data_mg$FSOC7 <- as.numeric(as.character(data_mg$FSOC7))
data_mg$FSOC8 <- as.numeric(as.character(data_mg$FSOC8))
data_mg$FSOC9 <- as.numeric(as.character(data_mg$FSOC9))
data_mg$FSOC10 <- as.numeric(as.character(data_mg$FSOC10))
data_mg$FSOC11 <- as.numeric(as.character(data_mg$FSOC11))
data_mg$FSOC12 <- as.numeric(as.character(data_mg$FSOC12))


data_mg$FSOC2_INV <- recode(data_mg$FSOC2, "1" = "7", "2" = "6", "3" = "5",
                            "4" = "4", "5" = "3", "6" = "2", "7" = "1") 
data_mg$FSOC3_INV <- recode(data_mg$FSOC3, "1" = "7", "2" = "6", "3" = "5",
                            "4" = "4", "5" = "3", "6" = "2", "7" = "1") 
data_mg$FSOC8_INV <- recode(data_mg$FSOC8, "1" = "7", "2" = "6", "3" = "5",
                            "4" = "4", "5" = "3", "6" = "2", "7" = "1") 
data_mg$FSOC10_INV <- recode(data_mg$FSOC10, "1" = "7", "2" = "6", "3" = "5",
                            "4" = "4", "5" = "3", "6" = "2", "7" = "1") 
data_mg$FSOC12_INV <- recode(data_mg$FSOC12, "1" = "7", "2" = "6", "3" = "5",
                            "4" = "4", "5" = "3", "6" = "2", "7" = "1") 

# Analisis multigrupo

modelmg <- 'FSOC=~ FSOC2_INV + FSOC3_INV + FSOC4 + FSOC5 + FSOC6 + FSOC7 +
FSOC8_INV + FSOC9 + FSOC10_INV + FSOC12_INV'

# configural equivalence
fitmg1<-cfa(modelmg,data=data_mg,group="Sexo",
            estimator = "WLSMV", mimic = "Mplus")
summary(fitmg1, fit.measures = T, standardized = T)

# metric equivalence: set the factor loadings equal across groups
fitmg2<-cfa(modelmg,data=data_mg,group="Sexo",
            group.equal=c("loadings"), estimator = "WLSMV",
            mimic = "Mplus")
summary(fitmg2, fit.measures = T, standardized = T)           

# scalar equivalence: set the factor loadings and the intercepts equal across groups
fitmg3<-cfa(modelmg,data=data_mg,group="Sexo", estimator = "WLSMV",
            mimic = "Mplus",
            group.equal=c("loadings","intercepts"))
summary(fitmg3, fit.measures = T, standardized = T) 

# Intercept invariance 

fitmg4<-cfa(modelmg,data=data_mg,group="Sexo", estimator = "WLSMV",
            mimic = "Mplus",
            group.equal=c("intercepts"))
summary(fitmg4, fit.measures = T, standardized = T) 


# Complete invariance test
fitmg6<-cfa(modelmg,data=data_mg,group="Sexo", estimator = "WLSMV",
            mimic = "Mplus",
            group.equal=c("loadings","intercepts", "residuals"))
summary(fitmg6, fit.measures = T, standardized = T) 


anova(fitmg1, fitmg2)
anova(fitmg1, fitmg3)
anova(fitmg1, fitmg4)
anova(fitmg1, fitmg6)
########### ########### ########### ########### ########### 
########### ########### ########### ########### ########### 
########### ########### ########### ########### ########### 
# Graficar el modelo

library(semPlot)
semPaths(fitcfa3,"model","stand",style="LISREL",rotation=1,
         edge.color="black", residuals = T,
         edge.label.cex=1,mar=c(10,1,8,1))

