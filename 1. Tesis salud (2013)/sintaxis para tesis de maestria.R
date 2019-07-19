# Estadísticos para tesis de maestría

#Base de datos sin outliers

library(foreign, pos=4)
TesisM1 <- 
  read.spss("D:/Documentos/Maestria/Tesis de maestría SOC y practicas/Analisis estadisticos/base_datos_maestria_tesis_en_limpio_tesis - sin outliers.sav",
   use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)
colnames(TesisM1) <- tolower(colnames(TesisM1))

#Show class

class(TesisM1)

#Show variable names

names(TesisM1)

# Cambiar los settings de las graficas para agrandar

par(cex = 2, lwd = 2, col.axis = 200, col.lab = 200, col.main = 200, col.sub = 200, fg = 200)

# Separar variables en grupos

hombre <- subset(TesisM1, TesisM1$sexo == "hombre")
mujer <- subset(TesisM1, TesisM1$sexo == "mujer")

edadmenor <- subset(TesisM1, TesisM1$edad_rango == "18-20")
edadmedia <- subset(TesisM1, TesisM1$edad_rango == "21-22")
edadmayor <- subset(TesisM1, TesisM1$edad_rango == "23-29")

ciclo1 <- subset(TesisM1, TesisM1$ciclo_cursa_rango == "5-6")
ciclo2 <- subset(TesisM1, TesisM1$ciclo_cursa_rango == "7-8")
ciclo3 <- subset(TesisM1, TesisM1$ciclo_cursa_rango == "9-12")

delgadez.normal <- subset(TesisM1, TesisM1$rangos_de_imc == "Delgadez y Normal")
preobeso.obeso <- subset(TesisM1, TesisM1$rangos_de_imc == "Pre-Obeso y Obeso")

ciencias <- subset(TesisM1, TesisM1$ciencias_letras == "Ciencias")
letras <- subset(TesisM1, TesisM1$ciencias_letras == "Letras")

#Diagrama de cajas para ver outliers

layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),2,6, byrow = TRUE))
Boxplot( ~ total_soc, data=TesisM1, id.method="y")
Boxplot( ~ total_sp36, data=TesisM1, id.method="y")
Boxplot( ~ total_af, data=TesisM1, id.method="y")
Boxplot( ~ total_tiempo_libre, data=TesisM1, id.method="y")
Boxplot( ~ total_habitos_alimenticios, data=TesisM1, id.method="y")
Boxplot( ~ total_sueño, data=TesisM1, id.method="y")
Boxplot( ~ total_alcohol_general, data=TesisM1, id.method="y")
Boxplot( ~ autocuidado_prueba, data=TesisM1, id.method="y", col="green")



# Para la variable eliminar sexo o condición

hombre$sexo <- NULL
mujer$sexo <- NULL

edadmenor$edad_rango <- NULL
edadmedia$edad_rango <- NULL
edadmayor$edad_rango <- NULL

ciclo1$ciclo_cursa_rango <- NULL
ciclo2$ciclo_cursa_rango <- NULL
ciclo3$ciclo_cursa_rango <- NULL

delgadez.normal$rangos_de_imc <- NULL
preobeso.obeso$rangos_de_imc <- NULL

ciencias$ciencias_letras <- NULL
letras$ciencias_letras <- NULL

summary(TesisM1$edad)
describe(TesisM1$edad)
summary(TesisM1$ciclo_cursa)
describe(TesisM1$estatura)
summary(TesisM1$imc)
describe(TesisM1$imc)
summary(TesisM1$trabajo)
summary(TesisM1$trabajo_si_no)
summary(TesisM1$promedio_horas_trabajo)
describe(TesisM1$promedio_horas_trabajo)
summary(TesisM1$medico_enfermedad)
summary(TesisM1$pastillas_enfermedad)
summary(TesisM1$enfermo_7_dias)
describe(TesisM1$peso)



#Hacer el histograma para las variables

layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),2,6, byrow = TRUE))
hist(TesisM1$total_soc,xlab = "Total SOC puntajes", col = "blue", freq = FALSE)
lines(density(TesisM1$total_soc), col = "red", lwd = 2)

hist(TesisM1$total_sp36_sm,xlab = "Total SF36 puntajes", col = "orange", freq = FALSE)
lines(density(TesisM1$total_sp36), col = "red", lwd = 2)

hist(TesisM1$total_af,xlab = "Total Actividad fisica", col = "green", freq = FALSE)
lines(density(TesisM1$total_af), col = "red", lwd = 2)

hist(TesisM1$total_tiempo_libre,xlab = "Total tiempo libre", col = "green", freq = FALSE)
lines(density(TesisM1$total_tiempo_libre), col = "red", lwd = 2)

hist(mujer$total_autocuidado_f,xlab = "Total Autocuidado mujer", col = "green", freq = FALSE )
lines(density(mujer$total_autocuidado_f), col = "red", lwd = 2)

hist(hombre$total_autocuidado_m,xlab = "Total Autocuidado hombre", col = "green", freq = FALSE )
lines(density(hombre$total_autocuidado_m), col = "red", lwd = 2)

hist(TesisM1$total_habitos_alimenticios,xlab = "Total habitos alimenticios", col = "green", freq = FALSE)
lines(density(TesisM1$total_habitos_alimenticios), col = "red", lwd = 2)

hist(mujer$total_consumo_tabaco_alcohol_drogas_f,xlab = "Total tabaco y alcohol mujer", col = "green", freq = FALSE)
lines(density(mujer$total_consumo_tabaco_alcohol_drogas_f), col = "red", lwd = 2)

hist(hombre$total_consumo_tabaco_alcohol_drogas_m,xlab = "Total tabaco y alcohol hombre", col = "green", freq = FALSE)
lines(density(hombre$total_consumo_tabaco_alcohol_drogas_m), col = "red", lwd = 2)

hist(TesisM1$total_sueño,xlab = "Total sueño", col = "green", freq = FALSE)
lines(density(TesisM1$total_sueño), col = "red", lwd = 2)

# Histogramas para comparar variables principales con sociodemográficas

layout(matrix(c(1,2,3,4,5,6,7,8,9,10), 2, 5, byrow = TRUE))
hist(hombre$total_soc, xlab = "Soc de hombres", col = "blue", freq = TRUE)
hist(mujer$total_soc, xlab = "Soc de mujeres", col = "red", freq = TRUE)
hist(edadmenor$total_soc, xlab = "Soc 18-20", freq = TRUE)
hist(edadmedia$total_soc, xlab = "Soc 21-22", freq = TRUE)
hist(edadmayor$total_soc, xlab = "Soc 23-29", freq = TRUE)
hist(ciclo1$total_soc, xlab = "Soc ciclo 5 y 6", freq = TRUE)
hist(ciclo2$total_soc, xlab = "Soc ciclo 7 y 8", freq = TRUE)
hist(ciclo3$total_soc, xlab = "Soc ciclo 9 y 12", freq = TRUE)
hist(letras$total_soc, xlab = "Soc letras", freq = TRUE)
hist(ciencias$total_soc, xlab = "Soc ciencias")
hist(delgadez.normal$total_soc, xlab = "Soc imc normal", freq = TRUE)
hist(preobeso.obeso$total_soc, xlab = "Soc imc normal", freq = TRUE)

hist(hombre$total_af, xlab = "Actvidad física hombres", col = "green", freq = TRUE)
hist(mujer$total_af, xlab = "Actvidad física mujeres", col = "orange", freq = TRUE)
hist(delgadez.normal $total_af, xlab = "Actvidad física imc normal", col = "orange", freq = TRUE)
hist(preobeso.obeso $total_af, xlab = "Actvidad física imc normal", col = "red", freq = TRUE)

hist(ciclo1$total_af, xlab = "Actvidad física 5-6 ciclo", col = "blue", freq = TRUE)
hist(ciclo2$total_af, xlab = "Actvidad física 7-8 ciclo", col = "black", freq = TRUE)
hist(ciclo3$total_af, xlab = "Actvidad física 9-12 ciclo", col = "yellow", freq = TRUE)

hist(edadmenor$total_af, xlab = "Actividad fisica menor rango edad", freq = TRUE)
hist(edadmedia$total_af, xlab = "Actividad fisica medio rango edad", freq = TRUE)
hist(edadmayor$total_af, xlab = "Actividad fisica mayor rango edad", freq = TRUE)

hist(ciencias$total_af, xlab = "Actividad física ciencias", freq = TRUE)
hist(letras$total_af, xlab = "Actividad física letras", freq = TRUE)

# Comparación de medias o medianas SOC por sexo, rango de edad, ciencias y letras, rangos imc, ciclo cursa rango 

# Descriptivos de variables sociodemográficas y SOC



describeBy(TesisM1$total_soc, TesisM1$sexo)
describeBy(TesisM1$total_soc, TesisM1$ciencias_letras)
describeBy(TesisM1$total_soc, TesisM1$ciclo_cursa_rango)
describeBy(TesisM1$total_soc, TesisM1$rangos_de_imc)
describeBy(TesisM1$total_soc, TesisM1$edad_rango)

leveneTest(TesisM1$total_soc~TesisM1$sexo, center = "mean")
t.test(TesisM1$total_soc~TesisM1$sexo, var.equal = T)

# Kruskal-wallis y ANOVA para diferencia entre rangos de edad, ciclo SOC

aov.soc = aov(TesisM1$total_soc~TesisM1$edad_rango)
summary(aov.soc)

kruskal.test(total_soc ~ ciclo_cursa_rango, data=TesisM1)

# Mann whitney entre los tres grupos de edades, imc, ciencias y letras)
wilcox.test(edadmenor$total_soc, edadmedia$total_soc, alternative="two.sided", 
  data=TesisM1)

wilcox.test(edadmenor$total_soc, edadmayor$total_soc, alternative="two.sided", 
  data=TesisM1)

wilcox.test(edadmedia$total_soc, edadmayor$total_soc, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$total_soc ~ ciencias_letras, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$total_soc ~ rangos_de_imc, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$total_soc ~ rangos_de_imc, alternative="greater", 
  data=TesisM1)

wilcox.test(ciclo1$total_soc, ciclo2$total_soc, alternative="two.sided", 
  data=TesisM1)

wilcox.test(ciclo1$total_soc, ciclo3$total_soc, alternative="two.sided", 
  data=TesisM1)

wilcox.test(ciclo2$total_soc, ciclo3$total_soc, alternative="two.sided", 
  data=TesisM1)


qnorm(p-value/2)

# Comparación entre variables sociodemográficas de Salud percibida

describeBy(TesisM1$total_sp36_sm, TesisM1$sexo)
describeBy(TesisM1$total_sp36_sm, TesisM1$ciencias_letras)
describeBy(TesisM1$total_sp36_sm, TesisM1$ciclo_cursa_rango)
describeBy(TesisM1$total_sp36_sm, TesisM1$rangos_de_imc)
describeBy(TesisM1$total_sp36_sm, TesisM1$edad_rango)

describeBy(TesisM1$total_sp36_sf, TesisM1$sexo)
describeBy(TesisM1$total_sp36_sf, TesisM1$ciencias_letras)
describeBy(TesisM1$total_sp36_sf, TesisM1$ciclo_cursa_rango)
describeBy(TesisM1$total_sp36_sf, TesisM1$rangos_de_imc)
describeBy(TesisM1$total_sp36_sf, TesisM1$edad_rango)

wilcox.test(TesisM1$total_sp36_sf ~ sexo, alternative="two.sided", 
  data=TesisM1)

qnorm(0.0004398/2)


wilcox.test(TesisM1$total_sp36_sm ~ sexo, alternative="two.sided", 
  data=TesisM1)

qnorm(0.001877/2)

wilcox.test(TesisM1$total_sp36_sf ~ rangos_de_imc, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$total_sp36_sm ~ rangos_de_imc, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$total_sp36_sf ~ ciencias_letras, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$total_sp36_sm ~ ciencias_letras, alternative="two.sided", 
  data=TesisM1)

kruskal.test(total_sp36_sm ~ ciclo_cursa_rango, data=TesisM1)
kruskal.test(total_sp36_sf ~ ciclo_cursa_rango, data=TesisM1)

wilcox.test(ciclo1$total_sp36, ciclo2$total_sp36, alternative="two.sided", 
  data=TesisM1)

wilcox.test(ciclo1$total_sp36, ciclo3$total_sp36, alternative="two.sided", 
  data=TesisM1)

wilcox.test(ciclo2$total_sp36, ciclo3$total_sp36, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$total_sp36 ~ ciencias_letras, alternative="two.sided", 
 data=TesisM1)

kruskal.test(total_sp36_sm ~ edad_rango, data=TesisM1)
kruskal.test(total_sp36_sf ~ edad_rango, data=TesisM1)


wilcox.test(edadmenor$total_sp36, edadmedia$total_sp36, alternative="two.sided", 
  data=TesisM1)

wilcox.test(edadmenor$total_sp36, edadmayor$total_sp36, alternative="two.sided", 
  data=TesisM1)

wilcox.test(edadmedia$total_sp36, edadmayor$total_sp36, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$total_sp36 ~ rangos_de_imc, alternative="greater", 
  data=TesisM1)


# Comparación entre variables sociodemograficas de actividad física

describeBy(TesisM1$total_af, TesisM1$sexo)
describeBy(TesisM1$total_af, TesisM1$ciencias_letras)
describeBy(TesisM1$total_af, TesisM1$ciclo_cursa_rango)
describeBy(TesisM1$total_af, TesisM1$rangos_de_imc)
describeBy(TesisM1$total_af, TesisM1$edad_rango)

wilcox.test(TesisM1$total_af ~ sexo, alternative="two.sided", 
  data=TesisM1)

summary(hombre$total_af)
summary(mujer$total_af)

describeBy(TesisM1$total_af, TesisM1$sexo)

wilcox.test(TesisM1$total_af ~ rangos_de_imc, alternative="greater", 
  data=TesisM1)



describeBy(TesisM1$total_af, TesisM1$rangos_de_imc)

kruskal.test(total_af ~ edad_rango, data=TesisM1)

wilcox.test(edadmenor$total_af, edadmedia$total_af, alternative="two.sided", 
  data=TesisM1)

wilcox.test(edadmenor$total_af, edadmayor$total_af, alternative="two.sided", 
  data=TesisM1)

wilcox.test(edadmedia$total_af, edadmayor$total_af, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$total_af ~ ciencias_letras, alternative="greater", 
  data=TesisM1)

describeBy(TesisM1$total_af, TesisM1$ciencias_letras)

wilcox.test(TesisM1$total_af ~ sexo, alternative="two.sided", 
  data=TesisM1)

qnorm(0.001999/2)



# Para conseguir puntaje Z en pruebas no parametricas 

qnorm(p-value/2)

kruskal.test(total_af ~ ciclo_cursa_rango, data=TesisM1)

wilcox.test(ciclo1$total_af, ciclo2$total_af, alternative="two.sided", 
  data=TesisM1)

wilcox.test(ciclo1$total_af, ciclo3$total_af, alternative="two.sided", 
  data=TesisM1)

wilcox.test(ciclo2$total_af, ciclo3$total_af, alternative="two.sided", 
  data=TesisM1)

# Comparaciones sueño

describeBy(TesisM1$total_sueño, TesisM1$sexo)
describeBy(TesisM1$total_sueño, TesisM1$ciencias_letras)
describeBy(TesisM1$total_sueño, TesisM1$ciclo_cursa_rango)
describeBy(TesisM1$total_sueño, TesisM1$rangos_de_imc)
describeBy(TesisM1$total_sueño, TesisM1$edad_rango)


kruskal.test(total_sueño ~ ciclo_cursa_rango, data=TesisM1)

kruskal.test(total_sueño ~ edad_rango, data=TesisM1)

wilcox.test(TesisM1$total_sueño ~ sexo, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$total_sueño ~ ciencias_letras, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$total_sueño ~ rangos_de_imc, alternative="two.sided", 
  data=TesisM1)

# Comparaciones autocuidado

describeBy(TesisM1$autocuidado_prueba, TesisM1$sexo)
describeBy(TesisM1$autocuidado_prueba, TesisM1$ciencias_letras)
describeBy(TesisM1$autocuidado_prueba, TesisM1$ciclo_cursa_rango)
describeBy(TesisM1$autocuidado_prueba, TesisM1$rangos_de_imc)
describeBy(TesisM1$autocuidado_prueba, TesisM1$edad_rango)

kruskal.test(autocuidado_prueba ~ ciclo_cursa_rango, data=TesisM1)

wilcox.test(ciclo1$autocuidado_prueba, ciclo2$autocuidado_prueba, alternative="two.sided", 
  data=TesisM1)

wilcox.test(ciclo1$autocuidado_prueba, ciclo3$autocuidado_prueba, alternative="two.sided", 
  data=TesisM1)

qnorm(0.0179/2)

wilcox.test(ciclo2$autocuidado_prueba, ciclo3$autocuidado_prueba, alternative="two.sided",  data=TesisM1)

kruskal.test(autocuidado_prueba ~ edad_rango, data=TesisM1)

wilcox.test(TesisM1$autocuidado_prueba ~ sexo, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$autocuidado_prueba ~ ciencias_letras, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$autocuidado_prueba ~ rangos_de_imc, alternative="two.sided", 
  data=TesisM1)

# Comparaciones habitos alimenticios

describeBy(TesisM1$total_habitos_alimenticios, TesisM1$sexo)
describeBy(TesisM1$total_habitos_alimenticios, TesisM1$ciencias_letras)
describeBy(TesisM1$total_habitos_alimenticios, TesisM1$ciclo_cursa_rango)
describeBy(TesisM1$total_habitos_alimenticios, TesisM1$rangos_de_imc)
describeBy(TesisM1$total_habitos_alimenticios, TesisM1$edad_rango)

kruskal.test(total_habitos_alimenticios ~ ciclo_cursa_rango, data=TesisM1)
 
kruskal.test(total_habitos_alimenticios ~ edad_rango, data=TesisM1)

wilcox.test(TesisM1$total_habitos_alimenticios ~ sexo, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$total_habitos_alimenticios ~ ciencias_letras, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$total_habitos_alimenticios ~ rangos_de_imc, alternative="two.sided", 
  data=TesisM1)

# Diferencias OH

describeBy(TesisM1$total_alcohol_general, TesisM1$sexo)
describeBy(TesisM1$total_alcohol_general, TesisM1$ciencias_letras)
describeBy(TesisM1$total_alcohol_general, TesisM1$ciclo_cursa_rango)
describeBy(TesisM1$total_alcohol_general, TesisM1$rangos_de_imc)
describeBy(TesisM1$total_alcohol_general, TesisM1$edad_rango)

kruskal.test(total_alcohol_general ~ ciclo_cursa_rango, data=TesisM1)
 
kruskal.test(total_alcohol_general ~ edad_rango, data=TesisM1)

wilcox.test(TesisM1$total_alcohol_general ~ sexo, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$total_alcohol_general ~ ciencias_letras, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$total_alcohol_general ~ rangos_de_imc, alternative="two.sided", 
  data=TesisM1)

qnorm(0.06816/2)

# Comparacion de medias muestras independientes
t.test(TesisM1$total_sp36~TesisM1$sexo, var.equal = T)

# Prueba Levene

leveneTest(TesisM1$total_sp36~TesisM1$sexo, center = "mean")

# Comparacion de medias muestras relacionadas

t.test(TesisM1$total_sp36~TesisM1$sexo, paired = T)

#Anovas

aov.model = aov(TesisM1$total_sp36~TesisM1$facultad)
summary(aov.model)

# En este caso se usara Kolmogorov, pero si se quiere usar shapiro el cogido es:
shapiro.test(base$variable)

# Nota para mayor preocupacion tambien se puede correr el analisis de kolmogorov pero se debe hacer para cada una de las variables. 
# Para esto no pueden haber missings.
# Pero antes se deben calcular los parametros (ver manual de R commander) o:

fitdistr(TesisM1$total_soc, "normal")
# Luego que se obtiene el valor poner el kolmogorov y el numero que sale primero en rate que es el ajuste
ks.test(TesisM1$total_soc, "pnorm", parametro obtenido antes)

# Descriptivos y comparaciones de tiempo libre 

describeBy(TesisM1$total_tiempo_libre, TesisM1$sexo)
describeBy(TesisM1$total_tiempo_libre, TesisM1$ciencias_letras)
describeBy(TesisM1$total_tiempo_libre, TesisM1$ciclo_cursa_rango)
describeBy(TesisM1$total_tiempo_libre, TesisM1$edad_rango)
describeBy(TesisM1$total_tiempo_libre, TesisM1$rangos_de_imc)

wilcox.test(TesisM1$total_tiempo_libre ~ sexo, alternative="two.sided", 
  data=TesisM1)

wilcox.test(TesisM1$total_tiempo_libre ~ ciencias_letras, alternative="two.sided", 
  data=TesisM1)

qnorm(0.00566/2)

wilcox.test(TesisM1$total_tiempo_libre ~ rangos_de_imc, alternative="two.sided", 
  data=TesisM1)

kruskal.test(total_tiempo_libre ~ ciclo_cursa_rango, data=TesisM1)

wilcox.test(ciclo1$total_tiempo_libre, ciclo2$total_tiempo_libre, alternative="two.sided", 
  data=TesisM1)

wilcox.test(ciclo1$total_tiempo_libre, ciclo3$total_tiempo_libre, alternative="two.sided", 
  data=TesisM1)

qnorm(0.00533/2)

wilcox.test(ciclo2$total_tiempo_libre, ciclo3$total_tiempo_libre, alternative="two.sided", 
  data=TesisM1)

qnorm(0.0676/2)

kruskal.test(total_tiempo_libre ~ edad_rango, data=TesisM1)

# Diagramas de dispersión 

layout(matrix(c(1:18), 3,6, byrow = TRUE))
plot(TesisM1$total_sp36_sm~TesisM1$total_soc, main = "Correlación", ylab = "Salud percibida", xlab = "Total SOC")
abline(lm(TesisM1$total_sp36_sm~TesisM1$total_soc), col = "red") 

plot(TesisM1$total_sp36_sf~TesisM1$total_soc, main = "Correlación", ylab = "Salud percibida", xlab = "Total SOC")
abline(lm(TesisM1$total_sp36_sf~TesisM1$total_soc), col = "red") 

plot(TesisM1$total_af~TesisM1$total_soc, main = "Correlación", ylab = "Actividad fisica", xlab = "Total SOC")
abline(lm(TesisM1$total_af~TesisM1$total_soc), col = "red") 

plot(TesisM1$total_tiempo_libre~TesisM1$total_soc, main = "Correlación", ylab = "Tiempo libre", xlab = "Total SOC")
abline(lm(TesisM1$total_tiempo_libre~TesisM1$total_soc), col = "red")

plot(TesisM1$total_habitos_alimenticios~TesisM1$total_soc, main = "Correlación", ylab = "Habitos alimenticios", xlab = "Total SOC")
abline(lm(TesisM1$total_habitos_alimenticios~TesisM1$total_soc), col = "red") 

plot(TesisM1$total_alcohol_general~TesisM1$total_soc, main = "Correlación", ylab = "Habitos de consumo tabaco y alcohol mujeres", xlab = "Total SOC")
abline(lm(TesisM1$total_alcohol_general~TesisM1$total_soc), col = "red") 

plot(TesisM1$total_sueño~TesisM1$total_soc, main = "Correlación", ylab = "Total Sueño", xlab = "Total SOC")
abline(lm(TesisM1$total_sueño~TesisM1$total_soc), col = "red") 

plot(TesisM1$total_sp36_sm~TesisM1$total_af, main = "Correlacion", ylab = "Salud percibida", xlab = "Actividad fisica")
abline(lm(TesisM1$total_sp36_sm~TesisM1$total_af), col = "red") 

plot(TesisM1$total_sp36_sm~TesisM1$total_tiempo_libre, main = "Correlacion", ylab = "Salud percibida", xlab = "Tiempo libre")
abline(lm(TesisM1$total_sp36_sm~TesisM1$total_tiempo_libre), col = "red") 

plot(TesisM1$total_sp36~TesisM1$total_habitos_alimenticios, main = "Correlacion", ylab = "Salud percibida", xlab = "Habitos alimenticios")
abline(lm(TesisM1$total_sp36~TesisM1$total_habitos_alimenticios), col = "red") 

plot(TesisM1$total_sp36~TesisM1$total_sueño, main = "Correlacion", ylab = "Salud percibida", xlab = "Sueño")
abline(lm(TesisM1$total_sp36~TesisM1$total_sueño), col = "red") 

# Decripción de la muestra

summary(TesisM1$sexo)
summary(TesisM1$edad)
summary(TesisM1$estatura)
summary(TesisM1$peso)
summary(TesisM1$imc)
summary(TesisM1$ciclo_cursa)
summary(TesisM1$trabajo_si_no)
summary(TesisM1$promedio_horas_trabajo)
summary(TesisM1$medico_enfermedad)
summary(TesisM1$pastillas_enfermedad)
summary(TesisM1$enfermo_7_dias)


# Descriptivos todas las variables

describe(TesisM1$total_soc)
describe(TesisM1$total_sp36_sm)
describe(TesisM1$total_sp36_sf)
describe(TesisM1$total_af)
describe(TesisM1$total_tiempo_libre)
describe(TesisM1$autocuidado_general)
describe(TesisM1$total_habitos_alimenticios)
describe(TesisM1$total_alcohol_general)
describe(TesisM1$total_sueño)
describe(TesisM1$autocuidado_prueba)

# Descriptivos por grupos: sexo, Facultad, 

describeBy(TesisM1$total_soc, TesisM1$sexo)
describeBy(TesisM1$total_sp36_sm, TesisM1$sexo)
describeBy(TesisM1$total_sp36_sf, TesisM1$sexo)
describeBy(TesisM1$total_af, TesisM1$sexo) 
describeBy(TesisM1$total_tiempo_libre, TesisM1$sexo)
describeBy(TesisM1$total_habitos_alimenticios, TesisM1$sexo)
describeBy(TesisM1$total_consumo_tabaco_alcohol_drogas, TesisM1$sexo)
describeBy(TesisM1$total_sueño, TesisM1$sexo)

# Descriptivos Facultad

summary(TesisM1$facultad)
describeBy(TesisM1$total_af, TesisM1$ciencias_letras) 


# Correlaciones Soc y todas las variables (OJO SOC es parametrico)

cor.test(TesisM1$total_sp36_sm, TesisM1$total_soc)
cor.test(TesisM1$total_sp36_sf, TesisM1$total_soc)
cor.test(TesisM1$total_af, TesisM1$total_soc)
cor.test(TesisM1$total_tiempo_libre, TesisM1$total_soc)
cor.test(TesisM1$total_habitos_alimenticios, TesisM1$total_soc)
cor.test(TesisM1$total_sueño, TesisM1$total_soc)
cor.test(TesisM1$total_alcohol_general, TesisM1$total_soc)
cor.test(TesisM1$autocuidado_prueba, TesisM1$total_soc)



# Nota, el R bota por defecto pearson para hacer spearman y kendall, alternative es para ver si es mayor igual, diferente o menor en la hipotesis:
cor.test(TesisM1$total_sueño, TesisM1$total_soc, alternative="two.sided" method = "spearman") 
cor.test(TesisM1$total_sueño, TesisM1$total_soc, alternative="greater" method = "kendall") 

# Correlaciones Practicas de salud y salud percibida y entre prácticas

cor.test(TesisM1$total_sp36_sf, TesisM1$total_af)
cor.test(TesisM1$total_sp36_sm, TesisM1$total_af)
cor.test(TesisM1$total_sp36_sm, TesisM1$total_alcohol_general)
cor.test(TesisM1$total_sp36_sf, TesisM1$total_alcohol_general)

cor.test(TesisM1$total_sp36_sm, TesisM1$total_sueño)
cor.test(TesisM1$total_sp36_sf, TesisM1$total_sueño)

cor.test(TesisM1$total_sp36_sm, TesisM1$autocuidado_prueba)
cor.test(TesisM1$total_sp36_sf, TesisM1$autocuidado_prueba)

cor.test(TesisM1$total_sp36_sm, TesisM1$total_habitos_alimenticios)
cor.test(TesisM1$total_sp36_sf, TesisM1$total_habitos_alimenticios)

cor.test(TesisM1$total_sp36_sm, TesisM1$total_tiempo_libre)
cor.test(TesisM1$total_sp36_sf, TesisM1$total_tiempo_libre)

cor.test(TesisM1$total_sp36_sf, TesisM1$total_sp36_sm) 

cor.test(TesisM1$total_tiempo_libre, TesisM1$total_af)
cor.test(TesisM1$total_habitos_alimenticios, TesisM1$total_af)
cor.test(TesisM1$total_sueño, TesisM1$total_af)
cor.test(TesisM1$total_alcohol_general, TesisM1$total_af)
cor.test(TesisM1$autocuidado_prueba, TesisM1$total_af)


cor.test(TesisM1$total_tiempo_libre, TesisM1$total_habitos_alimenticios)
cor.test(TesisM1$total_alcohol_general, TesisM1$total_tiempo_libre)
cor.test(TesisM1$total_sueño, TesisM1$total_tiempo_libre)
cor.test(TesisM1$autocuidado_prueba, TesisM1$total_tiempo_libre)


cor.test(TesisM1$total_sueño, TesisM1$total_habitos_alimenticios)
cor.test(TesisM1$total_alcohol_general, TesisM1$total_habitos_alimenticios)
cor.test(TesisM1$autocuidado_prueba, TesisM1$total_habitos_alimenticios)


cor.test(TesisM1$total_alcohol_general, TesisM1$total_sueño)
cor.test(TesisM1$autocuidado_prueba, TesisM1$total_sueño)

cor.test(TesisM1$autocuidado_prueba, TesisM1$total_alcohol_general)


#Correlaciones SOC y variables sociodemográficas

cor.test(TesisM1$promedio_horas_trabajo, TesisM1$total_soc)
cor.test(TesisM1$ciclo_cursa, TesisM1$total_soc, method = "spearman")
cor.test(TesisM1$imc, TesisM1$total_soc)
cor.test(TesisM1$edad, TesisM1$total_soc)
cor.test(TesisM1$peso, TesisM1$total_soc)
cor.test(TesisM1$sexo_sin_tag, TesisM1$total_soc, method = "spearman")


#Correlaciones Saludmental y física y variables sociodemograficas

cor.test(TesisM1$ciclo_cursa, TesisM1$total_sp36_sf, method = "spearman")
cor.test(TesisM1$ciclo_cursa, TesisM1$total_sp36_sm, method = "spearman") 

cor.test(TesisM1$sexo_sin_tag, TesisM1$total_sp36_sf, method = "spearman")
cor.test(TesisM1$sexo_sin_tag, TesisM1$total_sp36_sm, method = "spearman")

cor.test(TesisM1$edad, TesisM1$total_sp36_sf)
cor.test(TesisM1$edad, TesisM1$total_sp36_sm)

cor.test(TesisM1$imc, TesisM1$total_sp36_sm)
cor.test(TesisM1$imc, TesisM1$total_sp36_sf)

cor.test(TesisM1$imc, TesisM1$total_af)
cor.test(TesisM1$edad, TesisM1$total_af)
cor.test(TesisM1$sexo_sin_tag, TesisM1$total_af, method = "spearman")
cor.test(TesisM1$ciclo_cursa, TesisM1$total_af, method = "spearman")


cor.test(TesisM1$sexo_sin_tag, TesisM1$total_tiempo_libre, method = "spearman")
cor.test(TesisM1$sexo_sin_tag, TesisM1$autocuidado_prueba, method = "spearman")
cor.test(TesisM1$sexo_sin_tag, TesisM1$total_habitos_alimenticios, method = "spearman")
cor.test(TesisM1$sexo_sin_tag, TesisM1$total_alcohol_general, method = "spearman")
cor.test(TesisM1$sexo_sin_tag, TesisM1$total_sueño, method = "spearman")

cor.test(TesisM1$imc, TesisM1$total_tiempo_libre)
cor.test(TesisM1$imc, TesisM1$autocuidado_prueba)
cor.test(TesisM1$imc, TesisM1$total_habitos_alimenticios)
cor.test(TesisM1$imc, TesisM1$total_alcohol_general)
cor.test(TesisM1$imc, TesisM1$total_sueño)

cor.test(TesisM1$edad, TesisM1$total_tiempo_libre)
cor.test(TesisM1$edad, TesisM1$autocuidado_prueba)
cor.test(TesisM1$edad, TesisM1$total_habitos_alimenticios)
cor.test(TesisM1$edad, TesisM1$total_alcohol_general)
cor.test(TesisM1$edad, TesisM1$total_sueño)

cor.test(TesisM1$ciclo_cursa, TesisM1$total_tiempo_libre, method = "spearman")
cor.test(TesisM1$ciclo_cursa, TesisM1$autocuidado_prueba, method = "spearman")
cor.test(TesisM1$ciclo_cursa, TesisM1$total_habitos_alimenticios, method = "spearman")
cor.test(TesisM1$ciclo_cursa, TesisM1$total_alcohol_general, method = "spearman")
cor.test(TesisM1$ciclo_cursa, TesisM1$total_sueño, method = "spearman")


# Matriz de dispersiones (no sale por el problema de los missing values)

DIS.R = abs(cor(TesisM1$total_sp36, TesisM1$total_soc))
DIS.col = dmat.color(DIS.R)
DIS.o <- order.single(DIS.R)
cpairs(TesisM1, DIS.o, panel.colors =DIS.col, gap = 0.5, main = "Correlaciones por orden y colores")

# Confiabilidad SOC

reliability(cov(TesisM1[,c("soc_1_inv","soc_2_inv","soc_3_inv","soc_4","soc_5","soc_6",
  "soc_7_inv","soc_8","soc_9","soc_10_inv","soc_11","soc_12","soc_13")], 
  use="complete.obs"))

# Confiabilidad SF-36

reliability(cov(TesisM1[,c("saludp_1r","saludp_2r","saludp_3r","saludp_4r","saludp_5r",
  "saludp_6r","saludp_7r","saludp_8r","saludp_9r","saludp_10r","saludp_11r","saludp_12r",
  "saludp_13r","saludp_14r","saludp_15r","saludp_16r","saludp_17r","saludp_18r",
  "saludp_19r","saludp_20r","saludp_21r","saludp_22r","saludp_23r","saludp_24r",
  "saludp_25r","saludp_26r","saludp_27r","saludp_28r","saludp_29r","saludp_30r",
  "saludp_31r","saludp_32r","saludp_33r","saludp_34r","saludp_35r","saludp_36r")], 
  use="complete.obs"))

# Confiabilidad SF-36 SM

reliability(cov(TesisM1[,c("saludp_17r","saludp_18r","saludp_19r","saludp_20r","saludp_23r","saludp_24r",
  "saludp_25r","saludp_26r","saludp_27r","saludp_28r","saludp_29r","saludp_30r","saludp_31r","saludp_32r")], 
  use="complete.obs"))

# Confiabilidad SF-36 SF

reliability(cov(TesisM1[,c("saludp_1r","saludp_3r","saludp_4r","saludp_5r",
  "saludp_6r","saludp_7r","saludp_8r","saludp_9r","saludp_10r","saludp_11r","saludp_12r",
  "saludp_13r","saludp_14r","saludp_15r","saludp_16r","saludp_21r","saludp_22r","saludp_33r","saludp_34r","saludp_35r","saludp_36r")], 
  use="complete.obs"))

# Confiabilidad Cevju / Actividad física

reliability(cov(TesisM1[,c("cevju1","cevju2","cevju3","cevju4",
  "cevju5","cevju6")], use="complete.obs"))

# Opcion 2 it3 fuera AF

reliability(cov(TesisM1[,c("cevju1","cevju2","cevju4",
  "cevju5","cevju6")], use="complete.obs"))

# Opción 3 it5 FUERA AF

reliability(cov(TesisM1[,c("cevju1","cevju2","cevju4",
  "cevju6")], use="complete.obs"))

Opcion 4 it2 fuera AF

reliability(cov(TesisM1[,c("cevju1","cevju4",
  "cevju6")], use="complete.obs"))

# Confiabilidad Cevju / Tiempo libre

reliability(cov(TesisM1[,c("cevju7","cevju8","cevju9","cevju_10_inv")], 
  use="complete.obs"))

# Otra opcion tiempo libre

reliability(cov(TesisM1[,c("cevju7","cevju8","cevju9")], 
  use="complete.obs"))

# Confiabilidad / Habitos alimenticios

reliability(cov(TesisM1[,c("cevju29","cevju34","cevju35","cevju39",
  "cevju40","cevju41","cevju_30_inv","cevju_31_inv","cevju_32_inv",
  "cevju_33_inv","cevju_36_inv","cevju_37_inv","cevju_38_inv","cevju_42_inv",
  "cevju_43_inv","cevju_44_inv","cevju_45_inv")], use="complete.obs"))

# Confiabilidad opcion 2 habitos alimenticios it 35

reliability(cov(TesisM1[,c("cevju29","cevju34","cevju39",
  "cevju40","cevju41","cevju_30_inv","cevju_31_inv","cevju_32_inv",
  "cevju_33_inv","cevju_36_inv","cevju_37_inv","cevju_38_inv","cevju_42_inv",
  "cevju_43_inv","cevju_44_inv","cevju_45_inv")], use="complete.obs"))

# Conf habitos alimentarios tercera opcion it 44

reliability(cov(TesisM1[,c("cevju29","cevju34","cevju39",
  "cevju40","cevju41","cevju_30_inv","cevju_31_inv","cevju_32_inv",
  "cevju_33_inv","cevju_36_inv","cevju_37_inv","cevju_38_inv","cevju_42_inv",
  "cevju_43_inv","cevju_45_inv")], use="complete.obs"))

# Conf habitos alimenticios cuarta opcion it 42

reliability(cov(TesisM1[,c("cevju29","cevju34","cevju39",
  "cevju40","cevju41","cevju_30_inv","cevju_31_inv","cevju_32_inv",
  "cevju_33_inv","cevju_36_inv","cevju_37_inv","cevju_38_inv",
  "cevju_43_inv","cevju_45_inv")], use="complete.obs"))

# Conf habitos alimenticios cuarta opcion it 30

reliability(cov(TesisM1[,c("cevju29","cevju34","cevju39",
  "cevju40","cevju41","cevju_31_inv","cevju_32_inv",
  "cevju_33_inv","cevju_36_inv","cevju_37_inv","cevju_38_inv",
  "cevju_43_inv","cevju_45_inv")], use="complete.obs"))

# Conf habitos alimenticios cuarta opcion it 37

reliability(cov(TesisM1[,c("cevju29","cevju34","cevju39",
  "cevju40","cevju41","cevju_31_inv","cevju_32_inv",
  "cevju_33_inv","cevju_36_inv","cevju_38_inv",
  "cevju_43_inv","cevju_45_inv")], use="complete.obs"))

# Conf habitos alimenticios cuarta opcion it 41

reliability(cov(TesisM1[,c("cevju29","cevju34","cevju39",
  "cevju40","cevju_31_inv","cevju_32_inv",
  "cevju_33_inv","cevju_36_inv","cevju_38_inv",
  "cevju_43_inv","cevju_45_inv")], use="complete.obs"))

# Conf habitos alimenticios cuarta opcion it 43

reliability(cov(TesisM1[,c("cevju29","cevju34","cevju39",
  "cevju40","cevju_31_inv","cevju_32_inv",
  "cevju_33_inv","cevju_36_inv","cevju_38_inv",
  "cevju_45_inv")], use="complete.obs"))

# Conf habitos alimenticios cuarta opcion it 40

reliability(cov(TesisM1[,c("cevju29","cevju34","cevju39",
  "cevju_31_inv","cevju_32_inv",
  "cevju_33_inv","cevju_36_inv","cevju_38_inv",
  "cevju_45_inv")], use="complete.obs"))

# Conf habitos alimenticios cuarta opcion it 34

reliability(cov(TesisM1[,c("cevju29","cevju39",
  "cevju_31_inv","cevju_32_inv",
  "cevju_33_inv","cevju_36_inv","cevju_38_inv",
  "cevju_45_inv")], use="complete.obs"))

# Conf habitos alimenticios cuarta opcion it 29

reliability(cov(TesisM1[,c("cevju39",
  "cevju_31_inv","cevju_32_inv",
  "cevju_33_inv","cevju_36_inv","cevju_38_inv",
  "cevju_45_inv")], use="complete.obs"))

# Item 31 por comunlidades habitos alimenticios

reliability(cov(TesisM1[,c("cevju39",
  "cevju_32_inv",
  "cevju_33_inv","cevju_36_inv","cevju_38_inv",
  "cevju_45_inv")], use="complete.obs"))

# item 39 por comunalidades

reliability(cov(TesisM1[,c("cevju_32_inv",
  "cevju_33_inv","cevju_36_inv","cevju_38_inv",
  "cevju_45_inv")], use="complete.obs"))


# Confiabilidad alcohol general

reliability(cov(TesisM1[,c("cevju48","cevju52","cevju53","cevju_46_inv","cevju_47_inv","cevju_49_inv","cevju_54_inv","cevju_55_inv","cevju_56_inv", "alcohol_h_m")], use="complete.obs"))

# Opción 2 tabaco alcohol it 55

reliability(cov(TesisM1[,c("cevju48","cevju52","cevju53","cevju_46_inv","cevju_47_inv","cevju_49_inv","cevju_54_inv","cevju_56_inv", "alcohol_h_m")], use="complete.obs"))

# Opción 3 tabaco alcohol it 52

reliability(cov(TesisM1[,c("cevju48","cevju53","cevju_46_inv","cevju_47_inv","cevju_49_inv","cevju_54_inv","cevju_56_inv", "alcohol_h_m")], use="complete.obs"))


# Confiabilidad autocuidado general

reliability(cov(TesisM1[,c("cevju11","cevju12","cevju13","cevju14",
  "cevju15","cevju16","cevju19","cevju_17_inv","cevju_18_inv", "cevju23","cevju24","cevju25","cevju28",
  "cevju_26_inv","cevju_27_inv","autocuidado_hm_general")], use="complete.obs"))

# Conf otra opción autocuidado general it26

reliability(cov(TesisM1[,c("cevju11","cevju12","cevju13","cevju14",
  "cevju15","cevju16","cevju19","cevju_17_inv","cevju_18_inv", "cevju23","cevju24","cevju25","cevju28",
  "cevju_27_inv","autocuidado_hm_general")], use="complete.obs"))

# Conf otra opción autocuidado general it27

reliability(cov(TesisM1[,c("cevju11","cevju12","cevju13","cevju14",
  "cevju15","cevju16","cevju19","cevju_17_inv","cevju_18_inv", "cevju23","cevju24","cevju25","cevju28",
  "autocuidado_hm_general")], use="complete.obs"))

# Conf otra opción autocuidado general it17

reliability(cov(TesisM1[,c("cevju11","cevju12","cevju13","cevju14",
  "cevju15","cevju16","cevju19","cevju_18_inv","cevju23","cevju24","cevju25","cevju28",
  "autocuidado_hm_general")], use="complete.obs"))

# Conf otra opción autocuidado general it18

reliability(cov(TesisM1[,c("cevju11","cevju12","cevju13","cevju14",
  "cevju15","cevju16","cevju19","cevju23","cevju24","cevju25","cevju28",
  "autocuidado_hm_general")], use="complete.obs"))

# Conf otra opción autocuidado general it28

reliability(cov(TesisM1[,c("cevju11","cevju12","cevju13","cevju14",
  "cevju15","cevju16","cevju19","cevju23","cevju24","cevju25",
  "autocuidado_hm_general")], use="complete.obs"))

# Conf otra opción autocuidado general it23

reliability(cov(TesisM1[,c("cevju11","cevju12","cevju13","cevju14",
  "cevju15","cevju16","cevju19","cevju24","cevju25",
  "autocuidado_hm_general")], use="complete.obs"))

# Conf sin auitcuidado hm-general

reliability(cov(TesisM1[,c("cevju11","cevju12","cevju13","cevju14",
  "cevju15","cevju16","cevju19","cevju24","cevju25")], use="complete.obs"))

# Conf sin it19

reliability(cov(TesisM1[,c("cevju11","cevju12","cevju13","cevju14",
  "cevju15","cevju16","cevju24","cevju25")], use="complete.obs"))

# Conf prueba autocuidado

reliability(cov(TesisM1[,c("cevju11","cevju12","cevju13","cevju14",
  "cevju15","cevju16")], use="complete.obs"))


# Confiabilidad / Sueño

reliability(cov(TesisM1[,c("cevju57","cevju58","cevju62","cevju65",
  "cevju_59_inv","cevju_60_inv","cevju_61_inv","cevju_63_inv",
  "cevju_64_inv")], use="complete.obs"))

# Conf otra opcion sueño it62

reliability(cov(TesisM1[,c("cevju57","cevju58","cevju65",
  "cevju_59_inv","cevju_60_inv","cevju_61_inv","cevju_63_inv",
  "cevju_64_inv")], use="complete.obs"))

# Conf sin it 64

reliability(cov(TesisM1[,c("cevju57","cevju58","cevju65",
  "cevju_59_inv","cevju_60_inv","cevju_61_inv","cevju_63_inv")], use="complete.obs"))

# Conf sin it 61

reliability(cov(TesisM1[,c("cevju57","cevju58","cevju65",
  "cevju_59_inv","cevju_60_inv","cevju_63_inv")], use="complete.obs"))

# Conf sin it 60

reliability(cov(TesisM1[,c("cevju57","cevju58","cevju65",
  "cevju_59_inv")], use="complete.obs"))

# Conf sin it 63

reliability(cov(TesisM1[,c("cevju57","cevju58","cevju65",
  "cevju_59_inv")], use="complete.obs"))

# Regressiones no estandarizadas (metiendo todas las variables de golpe)

modelo0 = lm(TesisM1$total_sp36_sf~TesisM1$total_sp36_sm)
summary(modelo0)
modelo0.1 = lm(TesisM1$total_sp36_sm~TesisM1$total_sp36_sf)
summary(modelo0.1)

modelo1 = lm(TesisM1$total_sp36_sf + TesisM1$total_sp36_sm~TesisM1$sexo_sin_tag + TesisM1$total_soc + TesisM1$total_tiempo_libre + TesisM1$total_sueño + TesisM1$total_af + TesisM1$autocuidado_prueba)
summary(modelo1)
modelo2 = lm(TesisM1$total_sp36_sf + TesisM1$total_sp36_sm~TesisM1$sexo_sin_tag + TesisM1$total_soc + TesisM1$total_tiempo_libre + TesisM1$total_sueño + TesisM1$total_af)
summary(modelo2)


# Regresiones metiendo variable por variable

modelo3 = lm(TesisM1$total_sp36_sf + TesisM1$total_sp36_sm~TesisM1$sexo_sin_tag)
summary(modelo3)
modelo4 = lm(TesisM1$total_sp36_sf + TesisM1$total_sp36_sm~TesisM1$sexo_sin_tag + TesisM1$total_soc)
summary(modelo4)
modelo5 = lm(TesisM1$total_sp36_sf + TesisM1$total_sp36_sm~TesisM1$sexo_sin_tag + TesisM1$total_soc + TesisM1$total_tiempo_libre) 
summary(modelo5)
modelo6 = lm(TesisM1$total_sp36_sf + TesisM1$total_sp36_sm~TesisM1$sexo_sin_tag + TesisM1$total_soc + TesisM1$total_tiempo_libre + TesisM1$total_sueño + TesisM1$total_habitos_alimenticios)
summary(modelo6)

# Comparacion de modelos

comp1 = anova(modelo1, modelo2)
comp2 = anova(modelo2, modelo6)

# Mejor modelo = modelo 2

# Modelos por separado salud mental
 
modelo1.0 = lm(TesisM1$total_sp36_sm~TesisM1$sexo_sin_tag) 
summary(modelo1.0)

modelo2.0 = lm(TesisM1$total_sp36_sm~TesisM1$sexo_sin_tag + TesisM1$total_soc) 
summary(modelo2.0)

modelo3.0 = lm(TesisM1$total_sp36_sm~TesisM1$sexo_sin_tag + TesisM1$total_soc + TesisM1$total_tiempo_libre) 
summary(modelo3.0)

modelo4.0 = lm(TesisM1$total_sp36_sm~TesisM1$sexo_sin_tag + TesisM1$total_soc + TesisM1$total_tiempo_libre + TesisM1$total_sueño) 
summary(modelo4.0)


# Comparacion de modelos

comp3 = anova(modelo4.0, modelo3.0)
comp3

# Mejor modelo = modelo 4.0

# Modelos por separado salud física

modelo1.1 = lm(TesisM1$total_sp36_sf~TesisM1$sexo_sin_tag) 
summary(modelo1.1)

modelo2.1 = lm(TesisM1$total_sp36_sf~TesisM1$sexo_sin_tag + TesisM1$total_soc) 
summary(modelo2.1)

modelo3.1 = lm(TesisM1$total_sp36_sf~TesisM1$sexo_sin_tag + TesisM1$total_soc + TesisM1$total_tiempo_libre) 
summary(modelo3.1)

modelo4.1 = lm(TesisM1$total_sp36_sf~TesisM1$sexo_sin_tag + TesisM1$total_soc + TesisM1$total_tiempo_libre + TesisM1$total_sueño) 
summary(modelo4.1)

modelo5.1 = lm(TesisM1$total_sp36_sf~TesisM1$sexo_sin_tag + TesisM1$total_soc + TesisM1$total_tiempo_libre + TesisM1$total_sueño + TesisM1$total_af)
summary(modelo5.1)

modelo6.2 = lm(TesisM1$total_sp36_sf~TesisM1$sexo_sin_tag + TesisM1$total_soc + TesisM1$total_tiempo_libre + TesisM1$total_sueño + TesisM1$total_af + TesisM1$total_habitos_alimenticios)
summary(modelo6.2)

# Prueba modelo sin sexo ni autocuidado que no sale sig

modelo6.1 = lm(TesisM1$total_sp36_sf~TesisM1$total_soc + TesisM1$total_tiempo_libre + TesisM1$total_sueño + TesisM1$total_af)
summary(modelo6.1)

comp4 = anova(modelo5.1, modelo6.2)
comp4

# Mejor modelo = modelo5.1

# Regresiones por cada sexo
# Salud mental
# Hombre

modelo0.2 = lm(hombre$total_sp36_sm~hombre$total_soc + hombre$total_tiempo_libre + hombre$total_sueño + hombre$autocuidado_prueba) 
summary(modelo0.2)

modelo1.2 = lm(hombre$total_sp36_sm~hombre$total_soc + hombre$total_tiempo_libre + hombre$total_sueño)
summary(modelo1.2)

modelo2.2 = lm(hombre$total_sp36_sm~hombre$total_soc + hombre$total_tiempo_libre) 
summary(modelo2.2)

comp5 = anova(modelo1.2, modelo2.2)
comp5

comp5.1 = anova (modelo0.2, modelo1.2)
comp5.1

mejor modelo 1.2

# Mujer salud mental

modelo1.3 = lm(mujer$total_sp36_sm~mujer$total_soc + mujer$total_tiempo_libre + mujer$total_sueño + mujer$total_af + mujer$autocuidado_prueba) 
summary(modelo1.3)

modelo2.3 = lm(mujer$total_sp36_sm~mujer$total_soc + mujer$total_tiempo_libre + mujer$total_sueño) 
summary(modelo2.3)

modelo3.3 = lm(mujer$total_sp36_sm~mujer$total_soc + mujer$total_tiempo_libre) 
summary(modelo3.3)

modelo4.3 = lm(mujer$total_sp36_sm~mujer$total_soc)
summary(modelo4.3)

compm = anova(modelo3.3, modelo4.3)
compm

modelo adecuado 4.3. 

# Regresiones por cada sexo
# Salud física
# Hombre

modelo0.4 = lm(hombre$total_sp36_sf~hombre$total_soc + hombre$total_tiempo_libre + hombre$total_sueño + hombre$total_af + hombre$autocuidado_prueba + hombre$total_habitos_alimenticios)
summary(modelo0.4)

modelo1.4 = lm(hombre$total_sp36_sf~hombre$total_soc + hombre$total_tiempo_libre + hombre$total_sueño + hombre$total_af)
summary(modelo1.4)

modelo1.41 = lm(hombre$total_sp36_sf~hombre$total_soc + hombre$total_tiempo_libre + hombre$total_sueño + hombre$total_af + hombre$total_habitos_alimenticios)
summary(modelo1.41)

modelo2.4 = lm(hombre$total_sp36_sf~hombre$total_soc + hombre$total_tiempo_libre + hombre$total_sueño)
summary(modelo2.4)

modelo3.4 = lm(hombre$total_sp36_sf~hombre$total_soc + hombre$total_tiempo_libre)
summary(modelo3.4)

comp5 = anova(modelo1.4, modelo1.41)
comp5

# Modelo adecuado 1.4. 

# Mujer salud física

modelo1.5 = lm(mujer$total_sp36_sf~mujer$total_soc + mujer$total_tiempo_libre + mujer$total_sueño + mujer$total_af)
summary(modelo1.5)

modelo1.51 = lm(mujer$total_sp36_sf~mujer$total_soc + mujer$total_tiempo_libre + mujer$total_sueño + mujer$total_af + mujer$total_habitos_alimenticios)
summary(modelo1.51)

modelo1.52 = lm(mujer$total_sp36_sf~mujer$total_soc + mujer$total_tiempo_libre + mujer$total_af + mujer$total_habitos_alimenticios)
summary(modelo1.52)

modelo2.5 = lm(mujer$total_sp36_sf~mujer$total_soc + mujer$total_tiempo_libre + mujer$total_sueño + mujer$total_habitos_alimenticios)
summary(modelo2.5)

comp6 = anova(modelo1.5, modelo1.51)
comp6

comp6.1 = anova(modelo1.5, modelo1.52)
comp6.1

# Mejor modelo 1.5
´
# Regresión SM Y SF
# HOMBRE

modelohom1 = lm(hombre$total_sp36_sf + hombre$total_sp36_sm~hombre$total_soc + hombre$total_tiempo_libre + hombre$total_sueño + hombre$total_af + hombre$autocuidado_prueba)
summary(modelohom1)

modelohom2 = lm(hombre$total_sp36_sf + hombre$total_sp36_sm~hombre$total_soc + hombre$total_tiempo_libre + hombre$total_sueño + hombre$total_af)
summary(modelohom2)

comp7 = anova(modelohom1, modelohom2)
comp7

# modelo adecuado modelohom1


# Regresión SM Y SF
# Mujer

modelomuj1 = lm(mujer$total_sp36_sf + mujer$total_sp36_sm~mujer$total_soc + mujer$total_tiempo_libre + mujer$total_sueño + mujer$total_af + mujer$autocuidado_prueba + mujer$total_habitos_alimenticios)
summary(modelomuj1)

modelomuj2 = lm(mujer$total_sp36_sf + mujer$total_sp36_sm~mujer$total_soc + mujer$total_tiempo_libre + mujer$total_sueño + mujer$total_af)
summary(modelomuj2)

modelomuj3 = lm(mujer$total_sp36_sf + mujer$total_sp36_sm~mujer$total_soc + mujer$total_tiempo_libre + mujer$total_af)
summary(modelomuj3)

comp8 = anova(modelomuj2, modelomuj3)
comp8

# Modelo más adecuado modelomuj2

# Regressiones estandarizadas (una forma)

modelo1.z = lm(scale(TesisM1$total_sp36_sf)~scale(TesisM1$sexo_sin_tag) + scale(TesisM1$total_soc) + scale(TesisM1$total_tiempo_libre) + scale(TesisM1$total_sueño) + scale(TesisM1$total_af)) 
summary(modelo1.z)

# Otra forma para conseguir beta de regresione estandarizadas

library(MASS)
modelo1.z = glm(TesisM1$total_sp36_sf~TesisM1$total_soc, data=TesisM1)
library

library(QuantPsyc)
lm.beta(modelo1.z) 

# Coefficientes Beta 

library(QuantPsyc)
# Regresión SF y SM para todos
lm.beta(modelo2) 

# Regresión SM para todos
lm.beta(modelo4.0) 

# Regresión SF para todos
lm.beta(modelo6.2) 

# Regresión para SM (HOMBRES)
lm.beta(modelo0.2) 

# Regresión para SM (MUJERES)
lm.beta(modelo4.3) 

# Regresión para SF (HOMBRES)
lm.beta(modelo1.41) 

# Regresión para SF (MUJERES)
lm.beta(modelo1.52) 

# Regresión para SF Y SM (MUJERES)
lm.beta(modelomuj2) 

# Regresión para SF Y SM (HOMBRES)
lm.beta(modelohom1) 

# Busqueda de mediadores Sobel test

library(multilevel)

mediador1.0 = lm (TesisM1$total_sp36_sf~TesisM1$total_sueño + TesisM1$total_tiempo_libre)
summary(mediador1.0)

m1 = lm (TesisM1$total_sp36_sm~TesisM1$total_tiempo_libre)
summary(m1)

m2 = lm (TesisM1$total_sp36_sm~TesisM1$total_soc)
summary(m2)

m3 = lm(TesisM1$total_sp36_sm~TesisM1$total_soc + TesisM1$total_tiempo_libre)
summary(m3)

# El orden para poner las variables es el siguiente: VI, mediador, VD
mediador1 = sobel(TesisM1$total_tiempo_libre, TesisM1$total_soc, TesisM1$total_sp36_sm)
mediador1

# Power analysis

# Diferencias de SF por sexo

pwr.t2n.test(n1 = 262 , n2= 186 , d = 0.57, sig.level =0.0004398) 

# Diferencias de SM por sexo

pwr.t2n.test(n1 = 262 , n2= 186 , d = 0.35, sig.level =0.001877) 




