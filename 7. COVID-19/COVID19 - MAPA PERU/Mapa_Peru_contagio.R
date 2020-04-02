library(ggplot2)
library(sp)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(dplyr)
library(rayshader)
library(magick)
library(av)
library(viridis)
library(ggrepel)
library(lubridate)
library(gganimate)
library(gapminder)
library(transformr)

peru_dpto<-getData('GADM',country='PER',level=1) #Nivel departamento

#Mapas generales: Convertir con fortify
fperu_dpto<-fortify(peru_dpto)

##Departamentos (getting names)
# ind_dpto<-data.frame(seq(1:26),peru_dpto$NAME_1)
# colnames(ind_dpto) <- c("id","Departamento") ;ind_dpto

COVID_PERU29marzo <- "https://www.dropbox.com/s/k0jtazoe2mmzk6g/Positivos290320.csv?dl=1"


ind_dpto <-read.csv(COVID_PERU29marzo, sep = ",",
                       fill = T)

names(ind_dpto)
ind_dpto <- ind_dpto %>% 
  dplyr::select(id, Departamento = Departamento2,
         Casos_29Marzo_acumulado,
         Casos_30Marzo_acumulado,
         Casos_31Marzo_acumulado,
         Casos_01Abril_acumulado)
ind_dpto$id <- as.character(ind_dpto$id)
# Datos al 01 de Abril

#Mapa de departamento nombres
fperu_dpto <- left_join(fperu_dpto, ind_dpto,
                        by = "id")

fperu_dpto <- fperu_dpto %>%  
  mutate(
    Casos_29Marzo_cat = case_when(
      Casos_29Marzo_acumulado == 0 ~ "Sin COVID19",
      Casos_29Marzo_acumulado <= 10 ~ "Menos de 10 COVID19",
      Casos_29Marzo_acumulado <= 20 ~ "Menos de 20 COVID19",
      Casos_29Marzo_acumulado <= 30 ~ "Menos de 30 COVID19",
      Casos_29Marzo_acumulado <= 100 ~ "Menos de 50 COVID19",
      Casos_29Marzo_acumulado > 100 ~ "Mas de 100 casos COVID19"))

fperu_dpto$Casos_29Marzo_cat <- factor(fperu_dpto$Casos_29Marzo_cat, 
                                       levels = c("Sin COVID19", "Menos de 10 COVID19",
                                                  "Menos de 20 COVID19", "Menos de 30 COVID19",
                                                  "Menos de 50 COVID19", "Mas de 100 casos COVID19"))



fperu_dpto <- fperu_dpto %>%  
  mutate(
    Casos_30Marzo_cat = case_when(
      Casos_30Marzo_acumulado == 0 ~ "Sin COVID19",
      Casos_30Marzo_acumulado <= 10 ~ "Menos de 10 COVID19",
      Casos_30Marzo_acumulado <= 20 ~ "Menos de 20 COVID19",
      Casos_30Marzo_acumulado <= 30 ~ "Menos de 30 COVID19",
      Casos_30Marzo_acumulado <= 100 ~ "Menos de 50 COVID19",
      Casos_30Marzo_acumulado > 100 ~ "Mas de 100 casos COVID19"))

fperu_dpto$Casos_30Marzo_cat <- factor(fperu_dpto$Casos_30Marzo_cat, 
                                       levels = c("Sin COVID19", "Menos de 10 COVID19",
                                                  "Menos de 20 COVID19", "Menos de 30 COVID19",
                                                  "Menos de 50 COVID19", "Mas de 100 casos COVID19"))



fperu_dpto <- fperu_dpto %>%  
  mutate(
    Casos_31Marzo_cat = case_when(
      Casos_31Marzo_acumulado == 0 ~ "Sin COVID19",
      Casos_31Marzo_acumulado <= 10 ~ "Menos de 10 COVID19",
      Casos_31Marzo_acumulado <= 20 ~ "Menos de 20 COVID19",
      Casos_31Marzo_acumulado <= 30 ~ "Menos de 30 COVID19",
      Casos_31Marzo_acumulado <= 100 ~ "Menos de 50 COVID19",
      Casos_31Marzo_acumulado > 100 ~ "Mas de 100 casos COVID19"))

fperu_dpto$Casos_31Marzo_cat <- factor(fperu_dpto$Casos_31Marzo_cat, 
                                       levels = c("Sin COVID19", "Menos de 10 COVID19",
                                                  "Menos de 20 COVID19", "Menos de 30 COVID19",
                                                  "Menos de 50 COVID19", "Mas de 100 casos COVID19"))




fperu_dpto <- fperu_dpto %>%  
  mutate(
  Casos_01Abril_cat = case_when(
    Casos_01Abril_acumulado == 0 ~ "Sin COVID19",
    Casos_01Abril_acumulado <= 10 ~ "Menos de 10 COVID19",
    Casos_01Abril_acumulado <= 20 ~ "Menos de 20 COVID19",
    Casos_01Abril_acumulado <= 30 ~ "Menos de 30 COVID19",
    Casos_01Abril_acumulado <= 100 ~ "Menos de 50 COVID19",
    Casos_01Abril_acumulado > 100 ~ "Mas de 100 casos COVID19"))

fperu_dpto$Casos_01Abril_cat <- factor(fperu_dpto$Casos_01Abril_cat, 
                     levels = c("Sin COVID19", "Menos de 10 COVID19",
                                "Menos de 20 COVID19", "Menos de 30 COVID19",
                                "Menos de 50 COVID19", "Mas de 100 casos COVID19"))



dpto_geometry <- fperu_dpto %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(id) %>%
  summarise(geometry = st_combine(geometry),
            Departamento = first(Departamento),
            Casos_29Marzo_acumulado = mean(Casos_29Marzo_acumulado),
            Casos_29Marzo_cat = first(Casos_29Marzo_cat),
            Casos_30Marzo_acumulado = mean(Casos_30Marzo_acumulado),
            Casos_30Marzo_cat = first(Casos_30Marzo_cat),
            Casos_31Marzo_acumulado = mean(Casos_31Marzo_acumulado),
            Casos_31Marzo_cat = first(Casos_31Marzo_cat),
            Casos_01Abril_acumulado = mean(Casos_01Abril_acumulado),
            Casos_01Abril_cat = first(Casos_01Abril_cat)) %>%
  st_cast("POLYGON") 



### Mapa en 2D

dpto_geometry_points <- st_centroid(dpto_geometry)
dpto_geometry_points <- cbind(dpto_geometry, 
                              st_coordinates(st_centroid(dpto_geometry$geometry)))

### Mapa 29 de Marzo

Mapa1 <- ggplot(data = dpto_geometry)+
  geom_sf(aes(fill = Casos_29Marzo_cat)) +
  geom_text_repel(data= dpto_geometry_points, 
                  aes(x=X, y=Y, label=paste("(",Departamento,",",Casos_29Marzo_acumulado,")"),),
                  color = "black", fontface = "bold")+
  annotate(geom = "text", x = -78, y =-16, label = "Peru COVID19 29/03",
           fontface = "italic", color = "grey22", size = 5)+
  #coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97),expand = F)+
  theme(panel.background = element_blank(),
        legend.title = element_blank(), legend.position="none") +
  scale_fill_manual(breaks = c("Sin COVID19", "Menos de 10 COVID19",
                               "Menos de 20 COVID19", "Menos de 30 COVID19",
                               "Menos de 50 COVID19", "Mas de 100 casos COVID19"), 
                    values=c("#FCFCFC", "#FCC7C7", "#FA9391",
                             "#F97270", "#FC5755", "#FC0502"))+
  xlab("") +
  ylab("")

### Mapa 30 de Marzo

Mapa2 <- ggplot(data = dpto_geometry)+
  geom_sf(aes(fill = Casos_30Marzo_cat)) +
  geom_text_repel(data= dpto_geometry_points, 
                  aes(x=X, y=Y, label=paste("(",Departamento,",",Casos_30Marzo_acumulado,")"),),
                  color = "black", fontface = "bold")+
  annotate(geom = "text", x = -78, y =-16, label = "Peru COVID19 30/03",
           fontface = "italic", color = "grey22", size = 5)+
  #coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97),expand = F)+
  theme(panel.background = element_blank(),
        legend.title = element_blank(), legend.position="none") +
  scale_fill_manual(breaks = c("Sin COVID19", "Menos de 10 COVID19",
                               "Menos de 20 COVID19", "Menos de 30 COVID19",
                               "Menos de 50 COVID19", "Mas de 100 casos COVID19"), 
                    values=c("#FCFCFC", "#FCC7C7", "#FA9391",
                             "#F97270", "#FC5755", "#FC0502"))+
  xlab("") +
  ylab("")

### Mapa 31 de Marzo

Mapa3 <- ggplot(data = dpto_geometry)+
  geom_sf(aes(fill = Casos_31Marzo_cat)) +
  geom_text_repel(data= dpto_geometry_points, 
                  aes(x=X, y=Y, label=paste("(",Departamento,",",Casos_31Marzo_acumulado,")"),),
                  color = "black", fontface = "bold")+
  annotate(geom = "text", x = -78, y =-16, label = "Peru COVID19 31/03",
           fontface = "italic", color = "grey22", size = 5)+
  #coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97),expand = F)+
  theme(panel.background = element_blank(),
        legend.title = element_blank(), legend.position="none") +
  scale_fill_manual(breaks = c("Sin COVID19", "Menos de 10 COVID19",
                               "Menos de 20 COVID19", "Menos de 30 COVID19",
                               "Menos de 50 COVID19", "Mas de 100 casos COVID19"), 
                    values=c("#FCFCFC", "#FCC7C7", "#FA9391",
                             "#F97270", "#FC5755", "#FC0502"))+
  xlab("") +
  ylab("")

### Mapa 1 de Abril


Mapa4 <- ggplot(data = dpto_geometry)+
  geom_sf(aes(fill = Casos_01Abril_cat)) +
  geom_text_repel(data= dpto_geometry_points, 
                  aes(x=X, y=Y, label=paste("(",Departamento,",",Casos_01Abril_acumulado,")"),),
            color = "black", fontface = "bold")+
  annotate(geom = "text", x = -78, y =-16, label = "Peru COVID19 01/04",
           fontface = "italic", color = "grey22", size = 5)+
  #coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97),expand = F)+
  theme(panel.background = element_blank(),
        legend.title = element_blank(), legend.position="none") +
   scale_fill_manual(breaks = c("Sin COVID19", "Menos de 10 COVID19",
                                "Menos de 20 COVID19", "Menos de 30 COVID19",
                                "Menos de 50 COVID19", "Mas de 100 casos COVID19"), 
                     values=c("#FCFCFC", "#FCC7C7", "#FA9391",
                              "#F97270", "#FC5755", "#FC0502"))+
  xlab("") +
  ylab("")

ggsave("Mapa_Peru.png", plot =Mapa1,
         width = 10, height = 10, 
         limitsize = F)

# Mapa en 3D  

dpto_geometry$Casos_29MarzoZ <- scale(dpto_geometry$Casos_29Marzo, 
                                      center = T, scale = T)

Mapa1.1 <- ggplot(data = dpto_geometry)+
  geom_sf(aes(fill = Casos_29MarzoZ)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(), legend.position="none") +
  scale_fill_viridis_c(option =  "plasma") 
  plot_gg(Mapa1.1, scale = 250) 
  render_movie(filename = "mapa.mp4",
               theta = -45, phi = 30,zoom = 0.5,fov = 130)
  
 ###### Todo junto y animacion
  
  peru_dpto<-getData('GADM',country='PER',level=1) #Nivel departamento
  
  #Mapas generales: Convertir con fortify
  fperu_dpto<-fortify(peru_dpto)
  
  ##Departamentos (getting names)
  # ind_dpto<-data.frame(seq(1:26),peru_dpto$NAME_1)
  # colnames(ind_dpto) <- c("id","Departamento") ;ind_dpto
  
COVID_PERU29marzo <- "https://www.dropbox.com/s/k0jtazoe2mmzk6g/Positivos290320.csv?dl=1"
  
dpto_animate <-read.csv(COVID_PERU29marzo, sep = ",",
                      fill = T)


a <- dpto_animate %>% 
  dplyr::select(id, Departamento2,
              Acumulado = Casos_29Marzo_acumulado)
a$Dia <- dmy("29/3/2020")
               

b <- dpto_animate %>% 
  dplyr::select(id, Departamento2,
                Acumulado  = Casos_30Marzo_acumulado)

b$Dia <- dmy("30/3/2020")

c <- dpto_animate %>% 
  dplyr::select(id, Departamento2,
                Acumulado = Casos_31Marzo_acumulado)
c$Dia <- dmy("31/3/2020")

d <- dpto_animate %>% 
  dplyr::select(id, Departamento2,
                Acumulado = Casos_01Abril_acumulado)
d$Dia <- dmy("01/4/2020")

dpto_animate <- bind_rows(a,b,c,d)

dpto_animate <- dpto_animate %>%  
  mutate(
    Casos_cat = case_when(
      Acumulado == 0 ~ "Sin COVID19",
      Acumulado <= 10 ~ "Menos de 10 COVID19",
      Acumulado <= 20 ~ "Menos de 20 COVID19",
      Acumulado <= 30 ~ "Menos de 30 COVID19",
      Acumulado <= 100 ~ "Menos de 50 COVID19",
      Acumulado > 100 ~ "Mas de 100 casos COVID19"))

dpto_animate$Casos_cat <- factor(dpto_animate$Casos_cat, 
                                       levels = c("Sin COVID19", "Menos de 10 COVID19",
                                                  "Menos de 20 COVID19", "Menos de 30 COVID19",
                                                  "Menos de 50 COVID19", "Mas de 100 casos COVID19"))
dpto_animate$id <- as.character(dpto_animate$id)

fperu_dpto_animate <- left_join(fperu_dpto, dpto_animate,
                        by = "id")

dpto_geometry_animate <- fperu_dpto_animate %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(id,Dia) %>%
  summarise(geometry = st_combine(geometry),
            Departamento = first(Departamento2),
            Acumulado = mean(Acumulado),
            Casos_cat = first(Casos_cat)) %>%
  st_cast("POLYGON") 

### Mapa en 2D

dpto_geometry_points_animate <- st_centroid(dpto_geometry_animate)
dpto_geometry_points_animate <- cbind(dpto_geometry_animate, 
                              st_coordinates(st_centroid(dpto_geometry_animate$geometry)))

### Mapa 4 days

Mapa <- ggplot(data = dpto_geometry_animate)+
  geom_sf(aes(fill = Casos_cat)) +
  geom_text_repel(data= dpto_geometry_points_animate, 
                  aes(x=X, y=Y, label=paste("(",Departamento,",",Acumulado,")"),),
                  color = "black", fontface = "bold")+
  annotate(geom = "text", x = -78, y =-16, label = "Peru COVID19",
           fontface = "italic", color = "grey22", size = 5)+
  #coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97),expand = F)+
  theme(panel.background = element_blank(),
        legend.title = element_blank(), legend.position="none") +
  scale_fill_manual(breaks = c("Sin COVID19", "Menos de 10 COVID19",
                               "Menos de 20 COVID19", "Menos de 30 COVID19",
                               "Menos de 50 COVID19", "Mas de 100 casos COVID19"), 
                    values=c("#FCFCFC", "#FCC7C7", "#FA9391",
                             "#F97270", "#FC5755", "#FC0502"))+
  xlab("") +
  ylab("")

Mapa + transition_time(Dia) +
  labs(title = "Dia: {frame_time}")

