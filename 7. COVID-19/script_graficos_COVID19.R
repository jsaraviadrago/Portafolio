rm(list=ls())

COVID_PERU <- "https://www.dropbox.com/s/z3u3svqer7ekxpl/PERU_COVID19.csv?dl=1"

library(readxl)
library(lubridate)
library(ggplot2)
library(data.table)
library(ggpubr)
library(tidyr)

#### Carga de datos y Manipulacion de variables ####

data_COVID <-read.csv2(COVID_PERU, sep = ",",
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

### Creacion cantidad tests diaria (por hacer)

 data_COVID$Testeo_diaria <- NA # Linea vacia
 
data_COVID$Testeo_diaria[1] <- data_COVID$Testeados[1] # Primera linea
tests <- data_COVID$Testeados  [2:nrow(data_COVID)] # Vector de la segunda hacia abajo
tests <- c(tests, 0) # Agregar un 0 al final para tener misma longitud
dif_tests <- tests - data_COVID$Testeados # Calcular resta
dif_tests <- dif_tests[-length(dif_tests)] # Sacar el ultimo valor
data_COVID$Testeo_diaria[2:nrow(data_COVID)] <-dif_tests # Poner los valores en la columna vacía

#### Creacion de proporcion diaria

data_COVID$Prop_diaria_test_caso <- (data_COVID$CASOS_diarios/data_COVID$Testeo_diaria)*100 
 
### Relacion de testeados y casos positivos
   
cor.test(data_COVID$Testeo_diaria, 
         data_COVID$CASOS_diarios)

m1 <- lm(Testeo_diaria ~ -1 + CASOS_diarios, data_COVID)
summary(m1)

Cor_Casos_tests <- ggplot(data_COVID, aes(x=Testeo_diaria, y=CASOS_diarios))+
  geom_point()+
  geom_smooth(method = "lm", formula = 'y ~ x')+
  theme(panel.background = element_blank())+
  xlab("Cantidad de tests administrados diariamente")+
  ylab("Casos positivos diarios") +
  scale_y_continuous(breaks = seq(0, 300, by = 20), 
                     limits = c(0,300)) +
  scale_x_continuous(breaks = seq(0, 1700, by = 100), 
                     limits = c(0,1700))

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
  scale_y_continuous(breaks = seq(0, 1500, by = 50), 
                     limits = c(0,1500))

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
  scale_y_continuous(breaks = seq(0, 1000, by = 50), 
                     limits = c(0,1000))

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
  scale_y_continuous(breaks = seq(0, 400, by = 20), 
                     limits = c(0,400))

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
  scale_y_continuous(breaks = seq(0, 200, by = 20), 
                     limits = c(0,200))

# Casos diarios en Lima

Casos_diarios_Lima <- ggplot(data_COVID, aes(x=DIA, y =Positivos_Lima_Diarios))+
  geom_line(color = "black")+
  geom_point(color = "red")+
  geom_smooth(method = "loess", formula = 'y ~ x')+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Crecimiento diario en Lima COVID-19")+
  scale_x_date(breaks='1 day', date_labels = "%e-%m-%Y")+
  scale_y_continuous(breaks = seq(0, 200, by = 20), 
                     limits = c(0,200))

### Casos provincia diarios

Casos_diarios_Provincia <- ggplot(data_COVID, aes(x=DIA, y =Positivos_Provincia_diarios))+
  geom_line(color = "black")+
  geom_point(color = "red")+
  geom_smooth(method = "loess", formula = 'y ~ x')+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Crecimiento diario en provincia COVID-19")+
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
  scale_y_continuous(breaks = seq(0, 16000, by = 400), 
                     limits = c(0,16000))

#### Cantidad de tests negativos

C_Tests_negativos <- ggplot(data_COVID, aes(x=DIA, y =Descartados))+
  geom_line(color = "lightblue")+
  geom_point(color = "blue")+
  geom_smooth(method = "loess", formula = 'y ~ x')+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Tests negativos acumulados COVID-19")+
  scale_x_date(breaks='1 day', date_labels = "%e-%m-%Y")+
  scale_y_continuous(breaks = seq(0, 15000, by = 400), 
                     limits = c(0,15000))




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

### Casos testeados por día

C_Tests_diarios <- ggplot(data_COVID, aes(x=DIA, y =Testeo_diaria))+
  geom_line(color = "lightblue")+
  geom_point(color = "blue")+
  geom_smooth(method = "loess", formula = 'y ~ x')+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Testeo diario de COVID-19")+
  scale_x_date(breaks='1 day', date_labels = "%e-%m-%Y")+
  scale_y_continuous(breaks = seq(0, 2000, by = 200), 
                     limits = c(0,2000))

### Proporcion casos testeados con cantidad de positivos diarios

Prop_test_caso_dia <- ggplot(data_COVID, aes(x=DIA, y =data_COVID$Prop_diaria_test_caso))+
  geom_line(color = "black")+
  geom_point(color = "red")+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Proporcion positivos testeados diariamente COVID-19")+
  scale_x_date(breaks='1 day', date_labels = "%e-%m-%Y")+
  scale_y_continuous(breaks = seq(0, 100, by = 5), 
                     limits = c(0,100))

#### 23 Marzo

#### Juntar todos los graficos en 1 ####
figure <- ggarrange(C_total, C_Lima, C_Provincia, Crec_diario,
                    labels = c("1", "2",
                               "3", "4"),
                    ncol = 2, nrow = 2)
figure

ggsave("COVID.png", plot =figure,
       width = 10, height = 10, 
       limitsize = F)

#### Muestras procesadas

figure2 <- ggarrange(C_Tests_acumulado, C_Tests_negativos, C_total, Prop_pos,
                     labels = c("1", "2",
                                "3", "4"),
                     ncol = 2, nrow = 2)

ggsave("COVID_test_procesados.png", plot =figure2,
       width = 10, height = 10, 
       limitsize = F)

### Cantidad de testeados

figure3 <- ggarrange(Casos_diarios_Lima, Casos_diarios_Provincia,
                     C_Tests_diarios, Prop_test_caso_dia,
                     labels = c("1", "2",
                                "3", "4"),
                     ncol = 2, nrow = 2)

ggsave("COVID_test_diarios.png", plot =figure3,
       width = 10, height = 10, 
       limitsize = F)


#### 24 de Marzo

figure4 <- ggarrange(C_total, Casos_diarios_Lima,
                     Casos_diarios_Provincia, Cor_Casos_tests,
                     labels = c("1", "2",
                                "3", "4"),
                     ncol = 2, nrow = 2)

ggsave("COVID19_24marzo.png", plot =figure4,
       width = 10, height = 10, 
       limitsize = F)

#### 25 de Marzo

figure5 <- ggarrange(C_total, Casos_diarios_Lima,
                     Casos_diarios_Provincia, C_Tests_diarios,
                     labels = c("1", "2",
                                "3", "4"),
                     ncol = 2, nrow = 2)

ggsave("COVID19_25marzo.png", plot =figure5,
       width = 10, height = 10, 
       limitsize = F)

#### 26 de Marzo

figure6 <- ggarrange(C_total, Casos_diarios_Lima,
                     Casos_diarios_Provincia, C_Tests_diarios,
                     labels = c("1", "2",
                                "3", "4"),
                     ncol = 2, nrow = 2)

ggsave("COVID19_26marzo.png", plot =figure6,
       width = 10, height = 10, 
       limitsize = F)

#### 27 de Marzo


figure7 <- ggarrange(C_total, Casos_diarios_Lima,
                     Casos_diarios_Provincia, C_Tests_diarios,
                     labels = c("1", "2",
                                "3", "4"),
                     ncol = 2, nrow = 2)

ggsave("COVID19_27marzo.png", plot =figure7,
       width = 10, height = 10, 
       limitsize = F)

##### 1 de Abril

figure8 <- ggarrange(C_total, Casos_diarios_Lima,
                     Casos_diarios_Provincia, C_Tests_diarios,
                     labels = c("1", "2",
                                "3", "4"),
                     ncol = 2, nrow = 2)

ggsave("COVID19_01abril.png", plot =figure8,
       width = 10, height = 10, 
       limitsize = F)


#### Consolidado crecimiento Lima, provincia y total ####

data_COVID_consolidado <- data_COVID %>% 
  select(DIA, Casos_Totales = NUMERO_CASOS,
         Positivos_Lima,
         Positivos_Provincias)


data_COVID_consolidado <- data_COVID_consolidado %>%
  pivot_longer(-DIA, 
               names_to = "Casos", 
               values_to = "count")

consolidado <- ggplot(data_COVID_consolidado, aes(x=DIA, y =count,
                                   color = Casos, shape = Casos) )+
  geom_line(color = "lightblue")+
  geom_point()+
  geom_smooth(method = "loess", formula = 'y ~ x')+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank())+
  xlab(" ")+
  ylab("Casos COVID-19")+
  scale_x_date(breaks='1 day', date_labels = "%e-%m-%Y")+
  scale_y_continuous(breaks = seq(0, 1500, by = 50), 
                     limits = c(0,1500))


###### Example #####

View(gapminder)
head(data.frame(gapminder))

p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p

p + transition_time(year) +
  labs(title = "Year: {frame_time}")
