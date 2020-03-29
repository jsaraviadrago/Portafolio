library(ggplot2)
library(sp)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(dplyr)

peru_dpto<-getData('GADM',country='PER',level=1) #Nivel departamento

#Mapas generales: Convertir con fortify
fperu_dpto<-fortify(peru_dpto)

##Departamentos
ind_dpto<-data.frame(seq(1:26),peru_dpto$NAME_1)
colnames(ind_dpto) <- c("id","Departamento") ;ind_dpto
ind_dpto$id <- as.character(ind_dpto$id)
#Mapa de departamento nombres
fperu_dpto <- left_join(fperu_dpto, ind_dpto,
                        by = "id")



dpto_geometry <- fperu_dpto %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(id) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") 

Mapa1 <- ggplot(data = dpto_geometry)+
  geom_sf() +
  theme(panel.background = element_blank())

ggsave("Mapa_Peru.png", plot =Mapa1,
       width = 10, height = 10, 
       limitsize = F)
