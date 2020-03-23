rm(list=ls())

COVID_PERU <- "https://www.dropbox.com/s/kkgu43s31nckrn7/PERU_COVID19.csv?dl=1"

library(readxl)
library(lubridate)
library(ggplot2)
library(data.table)

#### Carga de datos y Manipulacion de variables ####

data_COVID <-fread(COVID_PERU,
                   quote = "",
                   fill = T)

head(data.frame(data_COVID))
str(data_COVID)

data_COVID$DIA <- dmy(data_COVID$DIA) #formatear fecha

### Creacion de casos en provincia
data_COVID$Positivos_Provincias <- data_COVID$NUMERO_CASOS - data_COVID$Positivos_Lima

### Creacion casos por dia 

data_COVID$CASOS_diarios <- NA # Linea vacia

data_COVID$CASOS_diarios[1] <- data_COVID$NUMERO_CASOS[1] # Primera linea
casos <- data_COVID$NUMERO_CASOS[2:nrow(data_COVID)] # Vector de la segunda hacia abajo
casos <- c(casos, 0) # Agregar un 0 al final para tener misma longitud
dif_casos <- casos - data_COVID$NUMERO_CASOS # Calcular resta
dif_casos <- dif_casos[-length(dif_casos)] # Sacar el ultimo valor
data_COVID$CASOS_diarios[2:nrow(data_COVID)] <-dif_casos # Poner los valores en la columna vacía

### Creacion proporcion testeados positivos diarios

data_COVID$Prop_Indidencia_diaria <- (data_COVID$CASOS_diarios/data_COVID$Testeados)*100
    
  

#### Graficas ####

#### Casos acumulados totales

ggplot(data_COVID, aes(x=DIA, y =NUMERO_CASOS))+
  geom_line(color = "lightblue")+
  geom_point(color = "blue")+
  geom_smooth(method = "loess", formula = 'y ~ x')+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Casos acumulados COVID-19")+
  scale_x_date(breaks='1 day', date_labels = "%e-%m-%Y")+
  scale_y_continuous(breaks = seq(0, 400, by = 20), 
                     limits = c(0,400))

#### Casos acumulados Lima

ggplot(data_COVID, aes(x=DIA, y =Positivos_Lima))+
  geom_line(color = "lightblue")+
  geom_point(color = "blue")+
  geom_smooth(method = "loess", formula = 'y ~ x')+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Casos acumulados Lima COVID-19")+
  scale_x_date(breaks='1 day', date_labels = "%e-%m-%Y")+
  scale_y_continuous(breaks = seq(0, 350, by = 20), 
                     limits = c(0,350))

#### Casos acumulados provincia

ggplot(data_COVID, aes(x=DIA, y =Positivos_Provincias))+
  geom_line(color = "lightblue")+
  geom_point(color = "blue")+
  geom_smooth(method = "loess", formula = 'y ~ x')+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Casos acumulados Provincias COVID-19")+
  scale_x_date(breaks='1 day', date_labels = "%e-%m-%Y")+
  scale_y_continuous(breaks = seq(0, 100, by = 20), 
                     limits = c(0,100))

#### Cantidad de tests por día

ggplot(data_COVID, aes(x=DIA, y =Testeados))+
  geom_line(color = "lightblue")+
  geom_point(color = "blue")+
  geom_smooth(method = "loess", formula = 'y ~ x')+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Tests realizados acumulados COVID-19")+
  scale_x_date(breaks='1 day', date_labels = "%e-%m-%Y")+
  scale_y_continuous(breaks = seq(0, 1200, by = 50), 
                     limits = c(0,1200))


#### Crecimiento diario

ggplot(data_COVID, aes(x=DIA, y =CASOS_diarios))+
  geom_line(color = "black")+
  geom_point(color = "red")+
  geom_smooth(method = "loess", formula = 'y ~ x')+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Crecimiento diario COVID-19")+
  scale_x_date(breaks='1 day', date_labels = "%e-%m-%Y")+
  scale_y_continuous(breaks = seq(0, 100, by = 5), 
                     limits = c(0,100))

#### Proporción de positivos con respecto a casos testeados por día

ggplot(data_COVID, aes(x=DIA, y =Prop_Indidencia_diaria))+
  geom_line(color = "black")+
  geom_point(color = "red")+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Crecimiento diario COVID-19")+
  scale_x_date(breaks='1 day', date_labels = "%e-%m-%Y")+
  scale_y_continuous(breaks = seq(0, 100, by = 5), 
                     limits = c(0,100))


