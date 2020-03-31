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

peru_dpto<-getData('GADM',country='PER',level=1) #Nivel departamento

#Mapas generales: Convertir con fortify
fperu_dpto<-fortify(peru_dpto)

##Departamentos (getting names)
# ind_dpto<-data.frame(seq(1:26),peru_dpto$NAME_1)
# colnames(ind_dpto) <- c("id","Departamento") ;ind_dpto

COVID_PERU29marzo <- "https://www.dropbox.com/s/k0jtazoe2mmzk6g/Positivos290320.csv?dl=1"


ind_dpto <-read.csv(COVID_PERU29marzo, sep = ",",
                       fill = T)
ind_dpto$id <- as.character(ind_dpto$id)
names(ind_dpto)
ind_dpto <- ind_dpto %>% 
  select(id, Departamento = Departamento2,
         Casos_29Marzo_acumulado,
         Casos_30Marzo_acumulado)

# Datos al 30 de Marzo

#Mapa de departamento nombres
fperu_dpto <- left_join(fperu_dpto, ind_dpto,
                        by = "id")

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

dpto_geometry <- fperu_dpto %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(id) %>%
  summarise(geometry = st_combine(geometry),
            Departamento = first(Departamento),
            Casos_30Marzo_acumulado = mean(Casos_30Marzo_acumulado),
            Casos_30Marzo_cat = first(Casos_30Marzo_cat)) %>%
  st_cast("POLYGON") 



### Mapa en 2D

dpto_geometry_points <- st_centroid(dpto_geometry)
dpto_geometry_points <- cbind(dpto_geometry, 
                              st_coordinates(st_centroid(dpto_geometry$geometry)))


Mapa1 <- ggplot(data = dpto_geometry)+
  geom_sf(aes(fill = Casos_30Marzo_cat)) +
  geom_text(data= dpto_geometry_points, aes(x=X, y=Y, label=Departamento),
            color = "black", fontface = "bold", check_overlap = F)+
  annotate(geom = "text", x = -78, y =-16, label = "Peru COVID19",
           fontface = "italic", color = "grey22", size = 6)+
  #coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97),expand = F)+
  theme(panel.background = element_blank(),
        legend.title = element_blank()) +
  scale_fill_manual(breaks = c("Sin COVID19", "Menos de 10 COVID19",
                               "Menos de 20 COVID19", "Menos de 30 COVID19",
                               "Menos de 50 COVID19", "Mas de 100 casos COVID19"), 
                    values=c("#FDFCFC", "#F59B9B", "#F07E7E",
                             "#EE5E5E", "#F74343", "#F50101"))+
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
  

  
  

