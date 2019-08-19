# Analisis SOC 13 Mexico

wd <- "/Users/home/Documents/Dropbox_260118/Papers futuros/3. Soc mexico"
setwd(wd)
getwd()

library(haven)
library(lavaan)
library(semPlot)
library(psych)
library(mirt)

SOCm13 <- read_sav("BD_SOC13_MEXICO.sav")

head(SOCm13)
names(SOCm13)
dim(SOCm13)
SOCm13_limpio <- na.omit(SOCm13)
head(SOCm13_limpio)
dim(SOCm13_limpio)
SOC13BD <- c("SOC131","SOC132","SOC133","SOC134","SOC135","SOC136","SOC137","SOC138","SOC139","SOC1310", "SOC1311", "SOC1312", "SOC1313") 
SOC131n <- SOCm13_limpio[SOC13BD] # creating a new data set from the big one
SOC131nf <- na.omit(SOC131n) # base de datos sin missings
head(SOC131nf)

# descriptivos

describe(SOC131n)

#Probando nornalidad multivariada

install.packages("MVN")
library(MVN)
MN <- mardiaTest(SOC131nf, qqplot = FALSE)
MN

MN_HZ <- hzTest(SOC131nf, qqplot = TRUE)
MN_HZ

#Modelo unidimensional

modelSOC13.2sb <- 'f =~  SOC131 + SOC132 + SOC133 + SOC134 + SOC135 + SOC136 + SOC137 + SOC138 + SOC139 + SOC1310 + SOC1311 + SOC1312 + SOC1313' #creating the model to fit it
mSOC131.2sb <- cfa(modelSOC13.2sb, SOC131nf, estimator = "MLM", test = "satorra.bentler")  # fit the model
summary(mSOC131.2sb,fit.measures=T,standardized=T)

semPaths(mSOC131.2sb,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

mi<-inspect(mSOC131,"mi") # modification indices
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]

#Modelo tridimensional

modelSOC131.1sb <- 'Comp =~  SOC132 +  SOC136 + SOC138 + SOC139 + SOC1311  
Manage =~ SOC133 + SOC135 + SOC1310 + SOC1313
Meaning =~ SOC131 + SOC134 + SOC137 + SOC1312' #creating the model to fit it
mSOC131.1sb <- cfa(modelSOC131.1sb, SOC131nf, estimator ="MLM", test="satorra.bentler")  # fit the model
summary(mSOC131.1sb,fit.measures=T,standardized=T)

semPaths(mSOC131.1sb,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

mi<-inspect(mSOC131,"mi") # modification indices
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]


#Modelo tridimensional cambiado

modelSOC131.2sb <- 'Comp =~  SOC132 +  SOC136 + SOC138 + SOC139 + SOC1311  
Manage =~ SOC133 + SOC135 + SOC1310 + SOC1313
Meaning =~ SOC134 + SOC137 + SOC1312' #creating the model to fit it
mSOC131.2sb <- cfa(modelSOC131.2sb, SOC131nf, estimator ="MLM", test="satorra.bentler")  # fit the model
summary(mSOC131.2sb,fit.measures=T,standardized=T)

semPaths(mSOC131.2sb,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

modelSOC131.3sb <- 'Comp =~  SOC132 +  SOC136 + SOC138 + SOC139 + SOC1311  
Manage =~ SOC133 + SOC135 + SOC1313
Meaning =~ SOC134 + SOC137 + SOC1312' #creating the model to fit it
mSOC131.3sb <- cfa(modelSOC131.3sb, SOC131nf, estimator ="MLM", test="satorra.bentler")  # fit the model
summary(mSOC131.3sb,fit.measures=T,standardized=T)


semPaths(mSOC131.3sb,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

modelSOC131.4sb <- 'Comp =~  SOC136 + SOC138 + SOC139 + SOC1311  
Manage =~ SOC133 + SOC135 + SOC1313
Meaning =~ SOC134 + SOC137 + SOC1312' #creating the model to fit it
mSOC131.4sb <- cfa(modelSOC131.4sb, SOC131nf, estimator ="MLM", test="satorra.bentler")  # fit the model
summary(mSOC131.4sb,fit.measures=T,standardized=T)

semPaths(mSOC131.4sb,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

modelSOC131.5sb <- 'Comp =~  SOC136 + SOC138 + SOC139 + SOC1311  
Manage =~ SOC133 + SOC135 + SOC1310 + SOC1313
Meaning =~ SOC134 + SOC137 + SOC1312' #creating the model to fit it
mSOC131.5sb <- cfa(modelSOC131.5sb, SOC131nf, estimator ="MLM", test="satorra.bentler")  # fit the model
summary(mSOC131.5sb,fit.measures=T,standardized=T)

semPaths(mSOC131.5sb,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

mi<-inspect(mSOC131.5sb,"mi") # modification indices
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]

modelSOC131.6sb <- 'Comp =~  SOC136 + SOC138 + SOC139 + SOC1311  
Manage =~ SOC133 + SOC135 + SOC1310 + SOC1313
Meaning =~ SOC134 + SOC137 + SOC1312
SOC138 ~~  SOC139' #creating the model to fit it
mSOC131.6sb <- cfa(modelSOC131.6sb, SOC131nf, estimator ="MLM", test="satorra.bentler")  # fit the model
summary(mSOC131.6sb,fit.measures=T,standardized=T)

semPaths(mSOC131.6sb,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

mi<-inspect(mSOC131.6sb,"mi") # modification indices
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]

# ESTE MODELO NO TIENEN LOS ?TEMS 1,2 y 10

modelSOC131.7sb <- 'Comp =~  SOC136 + SOC138 + SOC139 + SOC1311  
Manage =~ SOC133 + SOC135 + SOC1313
Meaning =~ SOC134 + SOC137 + SOC1312
' #creating the model to fit it
mSOC131.7sb <- cfa(modelSOC131.7sb, SOC131nf, estimator ="MLM", test="satorra.bentler")  # fit the model
summary(mSOC131.7sb,fit.measures=T,standardized=T)

semPaths(mSOC131.7sb,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

mi<-inspect(mSOC131.7sb,"mi") # modification indices
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]

##### Modelo final!!! ######

modelSOC131.8sb <- 'Comp =~  SOC136 + SOC138 + SOC139 + SOC1311  
Manage =~ SOC133 + SOC135 + SOC1313
Meaning =~ SOC134 + SOC137 + SOC1312
SOC138 ~~  SOC139' #creating the model to fit it
mSOC131.8sb <- cfa(modelSOC131.8sb, SOC131nf, estimator ="MLM", test="satorra.bentler")  # fit the model
summary(mSOC131.8sb,fit.measures=T,standardized=T)

semPaths(mSOC131.8sb,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,8,1)) # Graphic of the model

mi<-inspect(mSOC131.8sb,"mi") # modification indices
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]

############################################################
############################################################
############################################################

library(dplyr) 
library(tidyr)
library(knitr)
parameterEstimates(mSOC131.8sb, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

anova(mSOC131.7sb,mSOC131.8sb)

prueba <- lavPredict(mSOC131.7sb, type = "lv")

write.csv(prueba, "C:/Users/jsaravia/Dropbox/Soc mexico/prueba1.csv", row.names=FALSE, na="")


######################################################
#########################################################
######################################################
# Escalamiento multidimensional (Revisar multiples permutaciones)


install.packages("smacof")
library(smacof)

cor.soc <- cor(SOC131nf, method = "pearson") # Crear una matriz de correlaciones
cor.soc # ver si la matriz ya se cre?. 
cor.socsv <- abs(cor.soc) # Generar una matriz con valores absolutos
cor.socsv # revisar si el objeto tiene datos. 



dis.soc <- sim2diss(cor.socsv, method = "corr", to.dist = T) # Hacer una matriz de disimilaridades
dis.soc # matriz de disimilaridades



mds1 <- mds(dis.soc, ndim = 3, type = "ordinal", verbose = TRUE) # correr el modelo. 
mds1 # ver el ajuste del modelo
summary(mds1) # ver el stress por punto
head(mds1)
mds1$spp # Stress por cada punto para ver qu? ?tem desajusta. 
plot(mds1, plot.dim = c(1,2))
plot(mds1, plot.dim = c(1,3))
plot(mds1, plot.dim = c(2,3))

plot(mds1, plot.type = "stressplot") # graficar la descomposici?n de los ?tems que peor ajustan
plot(mds1, plot.type = "Shepard") # Diagrama de Shepard

res.perm <- permtest(mds1, nrep = 100, verbose = FALSE) # Correr el modelo con la cantidad de iteraciones que quieras
res.perm # test de permutaciones

op <- par(mfrow = c(1,2))
hist(res.perm$stressvec, xlab = "Stress Values", main = "Histogram Permutations")
abline(v = quantile(res.perm$stressvec, c(0.025, 0.975)), col = "red")
abline(v = res.perm$stress, col = "blue", lwd = 2)
plot(res.perm)
par(op)

# Omega (confiabilidad)

SOC13omega <- c("SOC133","SOC134","SOC135","SOC136","SOC137","SOC138","SOC139", "SOC1311", "SOC1312", "SOC1313") 
SOC131nomega <- SOCm13_limpio[SOC13omega] # creating a new data set from the big one
SOC131nfomega <- na.omit(SOC131nomega) # base de datos sin missings
head(SOC131nfomega)

install.packages("GPArotation")
library(GPArotation)

library(psych)
omegaSem(SOC131nfomega, nfactors = 3, fm = "ml", flip = TRUE) 

############Composite relaibility and AVE #############

# C?digo para crear composite realibity de la escala de autoestima. 

#factor 1


cargas1 <- abs(c(0.67,0.66,0.69,0.59)) # Aqu? tendr?as que escribir las cargas factoriales de todos tus items
errorv1 <- abs(c(0.55,0.57,0.52,0.66)) # Aqu? tienes que poner la varianza del error (error variance de cada item) 


# La funcion creada hara la operacion para sacar composite reliabiltiies
# La formula es suma de factor loadings al cuadrado / factor loadings al cuadrado + error variance

comp.rel <- function(x,y) {
  operacion <- (sum(x) * sum(x))/(sum(x) * sum(x) + sum(y))
  return(operacion)
}

comp.rel(cargas1,errorv1) # Se pone primero la funcion creada y luego en parentesis los nombres de los objetos

ave <- function(x) {
  operacion <- sum(x^2)/length(x)
  return(operacion)
}

ave(cargas1) 

#Factor 2


cargas2 <- abs(c(0.30,0.54,0.54)) # Aqu? tendr?as que escribir las cargas factoriales de todos tus items
errorv2 <- abs(c(0.91,0.71,0.71)) # Aqu? tienes que poner la varianza del error (error variance de cada item) 


comp.rel(cargas2,errorv2) 

# Crear funcion para calcular AVE
# Formula AVE es la suma de las cargas factoriales al cuadrado / el numero de items. 

ave(cargas2) # Se pone la funcion creada y luego entre parentesis las cargas factoriales.

#Factor 3

cargas3 <- abs(c(0.64,0.49,0.78)) # Aqu? tendr?as que escribir las cargas factoriales de todos tus items
errorv3 <- abs(c(0.59,0.76,0.40)) # Aqu? tienes que poner la varianza del error (error variance de cada item) 


comp.rel(cargas3,errorv3) 

ave(cargas3)

#############################################################################
#############################################################################
#############################################################################



