rm(list=ls())

COVID_PERU <- "https://www.dropbox.com/s/z3u3svqer7ekxpl/PERU_COVID19.csv?dl=1"

library(readxl)
library(lubridate)
library(ggplot2)
library(data.table)
library(ggpubr)

#### Carga de datos y Manipulacion de variables ####

data_COVID <-fread(COVID_PERU,
                   quote = "",
                   fill = T)

head(data.frame(data_COVID))
#View(data_COVID)
str(data_COVID)

data_COVID$DIA <- dmy(data_COVID$DIA) #formatear fecha
### Creacion de casos en Lima
data_COVID$Positivos_Lima <- cumsum(data_COVID$Positivos_Lima_Diarios)

### Creacion de casos en provincia
data_COVID$Positivos_Provincias <- data_COVID$NUMERO_CASOS - data_COVID$Positivos_Lima

### Creacion casos por dia 

data_COVID$CASOS_diarios <- data_COVID$Positivos_Lima_Diarios + data_COVID$Positivos_Provincia_diarios

### Creacion de testeados

data_COVID$Testeados <- data_COVID$NUMERO_CASOS + data_COVID$Descartados

### Creacion proporcion testeados positivos 

data_COVID$Prop_Indidencia_diaria <- (data_COVID$NUMERO_CASOS/data_COVID$Testeados)*100

### Creacion proporcion diaria (por hacer)

# data_COVID$Prop_Indidencia_diaria <- NA # Linea vacia
# 
# data_COVID$CASOS_diarios[1] <- data_COVID$NUMERO_CASOS[1] # Primera linea
# casos <- data_COVID$NUMERO_CASOS[2:nrow(data_COVID)] # Vector de la segunda hacia abajo
# casos <- c(casos, 0) # Agregar un 0 al final para tener misma longitud
# dif_casos <- casos - data_COVID$NUMERO_CASOS # Calcular resta
# dif_casos <- dif_casos[-length(dif_casos)] # Sacar el ultimo valor
# data_COVID$CASOS_diarios[2:nrow(data_COVID)] <-dif_casos # Poner los valores en la columna vacía
# 
    
  

#### Graficas ####

#### Casos acumulados totales

C_total <- ggplot(data_COVID, aes(x=DIA, y =NUMERO_CASOS))+
  geom_line(color = "lightblue")+
  geom_point(color = "blue")+
  geom_smooth(method = "loess", formula = 'y ~ x')+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Casos COVID-19")+
  scale_x_date(breaks='1 day', date_labels = "%e-%m-%Y")+
  scale_y_continuous(breaks = seq(0, 400, by = 50), 
                     limits = c(0,400))

#### Casos acumulados Lima

C_Lima <- ggplot(data_COVID, aes(x=DIA, y =Positivos_Lima))+
  geom_line(color = "lightblue")+
  geom_point(color = "blue")+
  geom_smooth(method = "loess", formula = 'y ~ x')+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Casos Lima COVID-19")+
  scale_x_date(breaks='1 day', date_labels = "%e-%m-%Y")+
  scale_y_continuous(breaks = seq(0, 350, by = 50), 
                     limits = c(0,350))

#### Casos acumulados provincia

C_Provincia <- ggplot(data_COVID, aes(x=DIA, y =Positivos_Provincias))+
  geom_line(color = "lightblue")+
  geom_point(color = "blue")+
  geom_smooth(method = "loess", formula = 'y ~ x')+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Casos Provincias COVID-19")+
  scale_x_date(breaks='1 day', date_labels = "%e-%m-%Y")+
  scale_y_continuous(breaks = seq(0, 100, by = 20), 
                     limits = c(0,100))

#### Crecimiento diario

Crec_diario <- ggplot(data_COVID, aes(x=DIA, y =CASOS_diarios))+
  geom_line(color = "black")+
  geom_point(color = "red")+
  geom_smooth(method = "loess", formula = 'y ~ x')+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Crecimiento diario COVID-19")+
  scale_x_date(breaks='1 day', date_labels = "%e-%m-%Y")+
  scale_y_continuous(breaks = seq(0, 100, by = 20), 
                     limits = c(0,100))

#### Cantidad de tests aculumulado

C_Tests_acumulado <- ggplot(data_COVID, aes(x=DIA, y =Testeados))+
  geom_line(color = "lightblue")+
  geom_point(color = "blue")+
  geom_smooth(method = "loess", formula = 'y ~ x')+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Tests realizados acumulados COVID-19")+
  scale_x_date(breaks='1 day', date_labels = "%e-%m-%Y")+
  scale_y_continuous(breaks = seq(0, 7000, by = 400), 
                     limits = c(0,7000))




#### Proporción de positivos con respecto a casos testeados por día

Prop_pos <- ggplot(data_COVID, aes(x=DIA, y =Prop_Indidencia_diaria))+
  geom_line(color = "black")+
  geom_point(color = "red")+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Crecimiento diario COVID-19")+
  scale_x_date(breaks='1 day', date_labels = "%e-%m-%Y")+
  scale_y_continuous(breaks = seq(0, 100, by = 5), 
                     limits = c(0,100))

#### Juntar todos los graficos en 1 ####
figure <- ggarrange(C_total, C_Lima, C_Provincia, Crec_diario,
                    labels = c(" ", " ",
                               " "),
                    ncol = 2, nrow = 2)
figure

ggsave("COVID.png", plot =figure,
       width = 10, height = 10, 
       limitsize = F)
