COVID_regiones <- "https://www.dropbox.com/s/mk7fft88l7tnfx2/COVID_diarios_region.csv?dl=1"

data_regiones_covid19 <-read.csv(COVID_regiones, sep = ",",
                    fill = T)

library(dplyr)

data_regiones_covid19$a7<- rowSums(data_regiones_covid19[,c(3:4)])
data_regiones_covid19$a8<- rowSums(data_regiones_covid19[,c(3:5)])
data_regiones_covid19$a9 <- rowSums(data_regiones_covid19[,c(3:6)])
data_regiones_covid19$a10 <- rowSums(data_regiones_covid19[,c(3:7)])
data_regiones_covid19$a11 <- rowSums(data_regiones_covid19[,c(3:8)])
data_regiones_covid19$a12 <- rowSums(data_regiones_covid19[,c(3:9)])
data_regiones_covid19$a13 <- rowSums(data_regiones_covid19[,c(3:10)])
data_regiones_covid19$a14 <- rowSums(data_regiones_covid19[,c(3:11)])
data_regiones_covid19$a15 <- rowSums(data_regiones_covid19[,c(3:12)])
data_regiones_covid19$a16 <- rowSums(data_regiones_covid19[,c(3:13)])
data_regiones_covid19$a17 <- rowSums(data_regiones_covid19[,c(3:14)])
data_regiones_covid19$a18 <- rowSums(data_regiones_covid19[,c(3:15)])
data_regiones_covid19$a19 <- rowSums(data_regiones_covid19[,c(3:16)])
data_regiones_covid19$a20 <- rowSums(data_regiones_covid19[,c(3:17)])
data_regiones_covid19$a21 <- rowSums(data_regiones_covid19[,c(3:18)])
data_regiones_covid19$a22 <- rowSums(data_regiones_covid19[,c(3:19)])
data_regiones_covid19$a23 <- rowSums(data_regiones_covid19[,c(3:20)])
data_regiones_covid19$a24 <- rowSums(data_regiones_covid19[,c(3:21)])
data_regiones_covid19$a25 <- rowSums(data_regiones_covid19[,c(3:22)])
data_regiones_covid19$a26 <- rowSums(data_regiones_covid19[,c(3:23)])
data_regiones_covid19$a27 <- rowSums(data_regiones_covid19[,c(3:24)])
data_regiones_covid19$a28 <- rowSums(data_regiones_covid19[,c(3:25)])
data_regiones_covid19$a29 <- rowSums(data_regiones_covid19[,c(3:26)])
data_regiones_covid19$a30 <- rowSums(data_regiones_covid19[,c(3:27)])
data_regiones_covid19$a31 <- rowSums(data_regiones_covid19[,c(3:28)])



head(data_regiones_covid19)





