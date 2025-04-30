library(tidyverse)
library(FactoMineR)
library(gplots)
library(classInt)

#leer tabla original (hice algunas modificaciones agregando 0s)
tabla <- readxl::read_excel("relevamiento.xlsx")

#discretización latitud
tabla <- tabla %>% 
  mutate(LAT = LAT*(-1),
         LON = LON*(-1))

#dejé 5 clases porque con 3 quedaba toda la frecuencia en la intermedia
classIntervals(tabla$LAT , n=5, style="fisher")
tabla$LAT_dis <- 0
tabla$LAT_dis[tabla$LAT > 32 & tabla$LAT < 32.948] <- "LAT_1"
tabla$LAT_dis[tabla$LAT > 32.948 & tabla$LAT < 33.3605] <- "LAT_2"
tabla$LAT_dis[tabla$LAT >= 33.3605 & tabla$LAT < 33.6565] <- "LAT_3"
tabla$LAT_dis[tabla$LAT >= 33.6565 & tabla$LAT < 34.2075] <- "LAT_4"
tabla$LAT_dis[tabla$LAT >= 34.2075] <- "LAT_5"

#frecuencia por latitud
tabla_a <- tabla %>% 
  group_by(LAT_dis) %>% 
  summarise(colletotrechum = sum(colletotrechum),
            colonia_6 = sum(colonia_6),
            colonia_35 = sum(colonia_35),
            d_mutila = sum(d_mutila),
            d_seriata = sum(d_seriata)) %>% 
  as.data.frame()

tabla_1 <- tabla_a
rownames(tabla_1) = tabla_1$LAT_dis
tabla_1 <- tabla_1[2:6]
dt <- as.table(as.matrix(tabla_1))

jpeg("graf/proporciones1.jpeg", width = 4000, height = 3000, res = 400)
balloonplot(t(dt), main = "", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
dev.off()

# test chi cuadrado (para evaluar dependencia entre filas y columnas)
chisq <- chisq.test(tabla_1)
chisq

##### Discretización por latitud

tabla$LAT_dis <- 0
tabla$LAT_dis[tabla$LAT > 32 & tabla$LAT < 33.3605] <- "LAT_1"
tabla$LAT_dis[tabla$LAT >= 33.3605 & tabla$LAT < 34.2075] <- "LAT_2"
tabla$LAT_dis[tabla$LAT >= 34.2075] <- "LAT_3"
tabla_dis <- tabla %>% 
  mutate(LAT = LAT*-1) %>% 
  group_by(LAT_dis) %>% 
  summarise(alternaria_sp = sum(alternaria_sp), 
            colletotrechum = sum(colletotrechum),
            colonia_6 = sum(colonia_6),
            colonia_35 = sum(colonia_35),
            d_mutila = sum(d_mutila),
            d_seriata = sum(d_seriata)) %>% 
  as.data.frame()

tabla_2 <- tabla_dis
rownames(tabla_2) = tabla_2$LAT_dis
tabla_2 <- tabla_2[3:7]
dt <- as.table(as.matrix(tabla_2))
balloonplot(t(dt), main = "", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# test de correspondencia entre ubicaciones y poblaciones
res.ca <- CA(tabla_2, ncp = 5, graph = TRUE)
#elipses en columnas (poblaciones)
ellipseCA(res.ca, ellipse = "col")
#elipses en filas (columnas)
ellipseCA(res.ca, ellipse = "row")

# test de correspondencia entre ubicaciones y poblaciones
res.ca2 <- CA(tabla_2, ncp = 5, graph = TRUE)
#elipses en columnas (poblaciones) me quedaría con este gráfico para presentarlo
#muestra como determinadas poblaciones son mas comunes en varias ubicaciones
jpeg("ca1.jpeg", width = 4000, height = 4000, res = 800)
ellipseCA(res.ca2, ellipse = "col")
dev.off()
#elipses en filas (columnas) no aparece nada interesante
jpeg("ca2.jpeg", width = 4000, height = 4000, res = 800)
ellipseCA(res.ca2, ellipse = "row")
dev.off()
#hasta acá está perfecto
#formando grupos
#las ubicaciones agrupadas son similares en cuanto a lo que encontraron en ellas:
#* Grupo 1: 40, 42, 45, 47, 48
#* Grupo 2: 41, 43, 44
#* Grupo 3: 37, 38, 39 y 46
jpeg("cluster_fila.jpeg", width = 4000, height = 4000, res = 800)
HCPC(res.ca2, cluster.CA = "rows", nb.clust = 3)
dev.off()
# la agrupación por poblaciones es bien distinta, por ejemplo un grupo está 
# compuesto solo por t16
jpeg("cluster_col.jpeg", width = 4000, height = 4000, res = 800)
HCPC(res.ca2, cluster.CA = "columns", nb.clust = 3)
dev.off()
# aporte a la variación de cada fila
eig.val <- res.ca2$eig
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")

# aporte a la variación de cada columna
eig.val <- res.ca2$col$contrib
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")


# % contribuciones
eig.val <- res.ca2$row$contrib
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")

#########################
# clasificando por sintomatología
# hice 2 intentos, uno tal como estaba en la tabla y otro con sintomaticos/no sintomaticos

tabla_3 <- tabla %>% 
  group_by(nivel_manejo) %>% 
  summarise(alternaria_sp = sum(alternaria_sp), 
            colletotrechum = sum(colletotrechum),
            colonia_6 = sum(colonia_6),
            colonia_35 = sum(colonia_35),
            d_mutila = sum(d_mutila),
            d_seriata = sum(d_seriata)) %>% 
  as.data.frame()

rownames(tabla_3) <- tabla_3$sintomas
tabla_3 <- tabla_3[3:7]
dt2 <- as.table(as.matrix(tabla_3))

tabla_3
# Esta salida gráfica es interesante (podría ser un buen complemento para la última -ver al final)
jpeg("proporciones4.jpeg", width = 4000, height = 4000, res = 800)
balloonplot(t(dt2), main = "frecuencia de poblaciones", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
dev.off()

# chi cuadrado significativo
chisq <- chisq.test(tabla_3)
chisq

res.ca3 <- CA(tabla_3)
summary(res.ca2)
#poco entendible la asociación | no usaría CA para analizar estos resultados
ellipseCA(res.ca3, ellipse = "col")
ellipseCA(res.ca3, ellipse = "row")

# analisis mas sencillo dejando solo con sintomas y asintomaticos
tabla_4 <- tabla_3 %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(Sintomaticos = (A+B+C), 
         Asintomaticos = S/S) %>% 
  select(Sintomaticos, Asintomaticos)


dt3 <- as.table(as.matrix(tabla_4))

tabla_4
# presentaría esta salida gráfica 
jpeg("proporciones5.jpeg", width = 6000, height = 4000, res = 800)
balloonplot(dt3, main = "frecuencia de poblaciones", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
dev.off()
# chi cuadrado significativo
chisq <- chisq.test(tabla_4)
chisq

##### por manejo
#tabla para agregar frecuencia total
tabla_c <- tabla %>% 
  mutate(LAT = LAT*-1) %>% 
  group_by(nivel_manejo) %>% 
  summarise(alternaria_sp = sum(alternaria_sp), 
            colletotrechum = sum(colletotrechum),
            colonia_6 = sum(colonia_6),
            colonia_35 = sum(colonia_35),
            d_mutila = sum(d_mutila),
            d_seriata = sum(d_seriata)) %>% 
  as.data.frame()
View(tabla_c)
