wd <- "/Users/home/Downloads/IF - Mapas de calor mensual"

setwd(wd)
list.files()

library(readxl)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(rayshader)

data1 <- read_excel("Mapa de Calor - 17.03.xlsx", sheet = 2)
names(data1)
data2 <- read_excel("Mapa de Calor - 17.04.xlsx", sheet = 2)
names(data2)
data3 <- read_excel("Mapa de Calor - 17.05.xlsx", sheet = 2)
names(data3)
data4 <- read_excel("Mapa de Calor - 17.06.xlsx", sheet = 2)
names(data4)
data5 <- read_excel("Mapa de Calor - 17.07.xlsx", sheet = 2)
names(data5)
data6 <- read_excel("Mapa de Calor - 17.08.xlsx", sheet = 2)
names(data6)
data7 <- read_excel("Mapa de Calor - 17.09.xlsx", sheet = 2)
names(data7)
data8 <- read_excel("Mapa de Calor - 17.10.xlsx", sheet = 2)
names(data8)

data_completa <- bind_rows(data1,data2,data3,data4,data5,data6,data7,data8)

head(data.frame(data_completa))
table(data_completa$hora)

data_agregada <- group_by(data_completa, dia, hora) %>% 
  summarise(Personas = n())

Heat_map <- ggplot(data_agregada, aes(dia,hora, fill= Personas)) + 
  geom_tile() +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90))+
  scale_fill_viridis(discrete=FALSE) +
  scale_x_continuous(breaks = seq(0,31, by = 1))+
  scale_y_continuous(breaks = seq(0,23, by = 1))
  plot_gg(Heat_map, scale = 250) 
render_movie(filename = "Heat_map.mp4",
             theta = -45, phi = 30,zoom = 0.5,fov = 130)
  
  

