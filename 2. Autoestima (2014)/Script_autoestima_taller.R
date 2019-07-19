#Analisis de la escala de autoestima

library(foreign)
getwd()
setwd("C:/Users/jsaravia/Dropbox/Papers futuros/Escala autoestima")

AUT <- read.spss("BD_autoestima_2009.sav", to.data.frame =T)
AUT_limpio <- na.omit(AUT)
head(AUT_limpio)

library("MVN")


AUTBD <- c("AUA1","AUA2","AUA3","AUA4","AUA5","AUA6","AUA7","AUA8","AUA9","AUA10") 
AUTn <- AUT_limpio[AUTBD] # creating a new data set from the big one
head(AUTn)

AUTmuestra <- AUTn[sample(nrow(AUTn), 1000),] 
head(AUTmuestra)
#Probando nornalidad multivariada


MN <- mardiaTest(AUTmuestra, qqplot = TRUE)
MN

MN_HZ <- hzTest(AUTmuestra, qqplot = TRUE)
MN_HZ


library(psych)
describe(AUTn) # Conseguir los descriptivos de los ítems. 

library(lavaan)
library(semPlot) 

##Con variables ordinales

AUT_mds <- c("AUA1","AUA2","AUA3","AUA4","AUA5","AUA6","AUA7","AUA8","AUA9","AUA10")
AUTmdsf <- AUT_limpio[AUT_mds]
head(AUTmdsf)
class(AUTmdsf)

AUT_limpio[,c("AUA1","AUA2","AUA3","AUA4","AUA5","AUA6","AUA7","AUA8","AUA9","AUA10")] <- 
  lapply(AUT_limpio[,c("AUA1","AUA2","AUA3","AUA4","AUA5","AUA6","AUA7","AUA8","AUA9","AUA10")], ordered)

modelaut <- 'f1 =~  AUA1 + AUA2 + AUA3 + AUA4 + AUA5 + AUA6 + AUA7 + AUA8 + AUA9 + AUA10' #creating the model to fit it
mautcfa <- cfa(modelaut, AUT_limpio, estimator = "MLM")  # fit the model
summary(mautcfa,fit.measures=T,standardized=T)


semPaths(mautcfa,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model


modelaut2 <- 'f1 =~  AUA1 + AUA2 + AUA3 + AUA4 + AUA5 
f2 =~ AUA6 + AUA7 + AUA8 + AUA9 + AUA10' #creating the model to fit it
mautcfa2 <- cfa(modelaut2, AUT_limpio, estimator = "MLM")  # fit the model
summary(mautcfa2,fit.measures=T,standardized=T)

semPaths(mautcfa2,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

mi<-inspect(mautcfa2,"mi") # modification indices
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]

anova(mautcfa,mautcfa2) # Comparacion de modelos

modelaut3 <- 'f1 =~  AUA1 + AUA2 + AUA3 + AUA4 + AUA5
f2 =~ AUA6 + AUA7 + AUA8 + AUA9 + AUA10
AUA7 ~~ AUA9' #creating the model to fit it
mautcfa3 <- cfa(modelaut3, AUT_limpio, estimator = "MLM")  # fit the model
summary(mautcfa3,fit.measures=T,standardized=T)

semPaths(mautcfa3,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

mi<-inspect(mautcfa3,"mi") # modification indices
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]

modelaut4 <- 'Aprot =~  AUA1 + AUA2 + AUA3 + AUA4 + AUA5
Aries =~ AUA6 + AUA7 + AUA8 + AUA9 + AUA10
AUA7 ~~ AUA9
AUA9 ~~ AUA10' #creating the model to fit it
mautcfa4 <- cfa(modelaut4, AUT_limpio, estimator = "MLM")  # fit the model
summary(mautcfa4,fit.measures=T,standardized=T)

semPaths(mautcfa4,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

mi<-inspect(mautcfa4,"mi") # modification indices
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]

### Es muy forzado este modelo, solo usar el 4###

modelaut5 <- 'f1 =~  AUA1 + AUA2 + AUA3 + AUA4 + AUA5
f2 =~ AUA6 + AUA7 + AUA8 + AUA9 + AUA10
AUA7 ~~ AUA9
AUA9 ~~ AUA10
AUA7 ~~ AUA10' #creating the model to fit it
mautcfa5 <- cfa(modelaut5, AUT_limpio, estimator = "MLM")  # fit the model
summary(mautcfa5,fit.measures=T,standardized=T)
predict(mautcfa5)

semPaths(mautcfa5,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

mi<-inspect(mautcfa5,"mi") # modification indices
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]

lavPredict(mautcfa5)

# Escalamiento multidimensional

library(smacof)

cor.autest <- cor(AUTmdsf, method = "pearson") # Crear una matriz de correlaciones
cor.autest # ver si la matriz ya se creó. 
cor.autestsv <- abs(cor.autest) # Generar una matriz con valores absolutos
cor.autestsv # revisar si el objeto tiene datos. 



dis.aut <- sim2diss(cor.autestsv, method = "corr", to.dist = T) # Hacer una matriz de disimilaridades
dis.aut # matriz de disimilaridades

mds1aut <- mds(dis.aut, ndim = 2, type = "ordinal", verbose = TRUE) # correr el modelo. 
mds1aut # ver el ajuste del modelo
summary(mds1aut) # ver el stress por punto
head(mds1aut)
mds1aut$spp # Stress por cada punto para ver qué ítem desajusta. 

plot(mds1aut, plot.dim = c(1,2))


plot(mds1aut, plot.type = "stressplot") # graficar la descomposición de los ítems que peor ajustan
plot(mds1aut, plot.type = "Shepard") # Diagrama de Shepard

# Correr el OMEGA

install.packages("GPArotation", dependencies = TRUE)
library(psych)
library(GPArotation)

# Intervalo de OMEGA con psych. Leer más la diferencia entre omega jerárquico y el normal. 
omegaSem(AUTmdsf, fm = "ml", flip = T, nfactors = 2) 

# Código para crear composite realibity de la escala de autoestima. 

#factor 1


cargas1 <- abs(c(0.73,0.84,0.82,0.83,0.79)) # Aquí tendrías que escribir las cargas factoriales de todos tus items
errorv1 <- abs(c(0.47,0.30,0.32,0.31,0.38)) # Aquí tienes que poner la varianza del error (error variance de cada item) 


# La funcion creada hara la operacion para sacar composite reliabiltiies
# La formula es suma de factor loadings al cuadrado / factor loadings al cuadrado + error variance

comp.rel <- function(x,y) {
  operacion <- (sum(x) * sum(x))/(sum(x) * sum(x) + sum(y))
  return(operacion)
}

comp.rel(cargas1,errorv1) # Se pone primero la funcion creada y luego en parentesis los nombres de los objetos

#Factor 2


cargas2 <- abs(c(0.71,0.69,0.63,0.64,0.69)) # Aquí tendrías que escribir las cargas factoriales de todos tus items
errorv2 <- abs(c(0.50,0.53,0.61,0.59,0.52)) # Aquí tienes que poner la varianza del error (error variance de cada item) 


# La funcion creada hara la operacion para sacar composite reliabiltiies
# La formula es suma de factor loadings al cuadrado / factor loadings al cuadrado + error variance

comp.rel <- function(x,y) {
  operacion <- (sum(x) * sum(x))/(sum(x) * sum(x) + sum(y))
  return(operacion)
}

comp.rel(cargas2,errorv2) 

# Factor 1

# Código para crear AVE

cargas1 <- abs(c(0.73,0.84,0.82,0.83,0.79))  # Aquí solo es necesario poner las cargas factoriales de los items

# Crear funcion para calcular AVE
# Formula AVE es la suma de las cargas factoriales al cuadrado / el numero de items. 

ave <- function(x) {
  operacion <- sum(x^2)/length(x)
  return(operacion)
}

ave(cargas1) # Se pone la funcion creada y luego entre parentesis las cargas factoriales.

#factor 2

cargas2 <- abs(c(0.71,0.69,0.63,0.64,0.69))

# Crear funcion para calcular AVE
# Formula AVE es la suma de las cargas factoriales al cuadrado / el numero de items. 

ave <- function(x) {
  operacion <- sum(x^2)/length(x)
  return(operacion)
}

ave(cargas2)


