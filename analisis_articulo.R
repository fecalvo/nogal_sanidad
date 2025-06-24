library(tidyverse)
library(FactoMineR)
library(gplots)
library(classInt)
library(emmeans)
library(multcompView)
library(multcomp)
library(reshape2)

#leer tabla original (hice algunas modificaciones agregando 0s)
tabla <- readxl::read_excel("tablas/relevamiento.xlsx")

##################
tabla_3 <- pivot_longer(tabla, cols = 11:17, names_to = "tipo_s", values_to = "conteo")

# Agrupás sumando
freq_t3 <- tabla_3 %>%
  group_by(nivel_manejo, sintoma) %>%
  filter(sintoma != "S/S") %>% 
  summarise(freq = sum(conteo), .groups = "drop")

# Convertís a tabla cruzada (matriz de contingencia)
dt_3 <- xtabs(freq ~ nivel_manejo + sintoma, data = freq_t3)

# Test chi²
test <- chisq.test(dt_3)
p_valor <- round(test$p.value, digits = 4)
print(test)


# Usás la tabla cruzada que creaste con xtabs:
jpeg("graf/balloonplot_manejo.jpeg", width = 4000, height = 3000, res = 400)

balloonplot(t(dt_3), 
            main = "Frecuencia de síntomas por nivel de manejo",
            xlab = "Sintoma", 
            ylab = "Nivel de manejo",
            label = T, 
            show.margins = FALSE)
# Agregar leyenda con resultado global
legend("topleft", 
       legend = paste0("χ² = ", chi2_val, ", p = ", p_valor),
       bty = "n", cex = 0.8)
dev.off()

######################################

# Agrupás sumando
freq_t4 <- tabla_3 %>%
  filter(sintoma != "S/S") %>% 
  group_by(estacion, sintoma) %>%
  summarise(freq = sum(conteo), .groups = "drop")

# Convertís a tabla cruzada (matriz de contingencia)
dt_4 <- xtabs(freq ~ estacion + sintoma, data = freq_t4)

# Paso 3: Test chi² global
test <- chisq.test(dt_4)
p_valor <- round(test$p.value, digits = 4)
chi2_val <- round(test$statistic, 2)

# Paso 4: Balloonplot con leyenda del test global
jpeg("graf/balloonplot_estacion.jpeg", width = 4000, height = 3000, res = 400)
balloonplot(t(dt_4), 
            main = "Frecuencia de síntomas por estación",
            xlab = "Síntoma", 
            ylab = "Estación",
            label = T, 
            show.margins = FALSE)

# Agregar leyenda con resultado global
legend("topleft", 
       legend = paste0("χ² = ", chi2_val, ", p = ", p_valor),
       bty = "n", cex = 0.8)
dev.off()

######################################

freq_t5 <- tabla %>%
  filter(sintoma != "S/S") %>% 
  mutate(nivel_manejo = as.factor(nivel_manejo)) %>% 
  group_by(nivel_manejo, sintoma) %>%
  summarise(freq = sum(`Dardos recolectados por síntoma`))

# Convertís a tabla cruzada (matriz de contingencia)
dt_5 <- xtabs(freq ~ nivel_manejo + sintoma, data = freq_t5)

# Test chi²
test <- chisq.test(dt_5)
chi2_val <- round(test$statistic, 2)
p_valor <- round(test$p.value, digits = 4)
print(test)

# Convertir tabla cruzada a data frame largo
df_heat <- as.data.frame(as.table(dt_5))

# Graficar heatmap
jpeg("graf/heatmal_estacion.jpeg", width = 4000, height = 3000, res = 800)
ggplot(df_heat, aes(x = sintoma, y = nivel_manejo, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = Freq), color = "black", size = 3) +
  labs(
    title = "Heatmap de síntomas por nivel de manejo",
    x = "Síntoma",
    y = "Nivel de manejo",
    fill = "Frecuencia"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# Lista de síntomas únicos
sintomas <- unique(freq_t5$sintoma)

# Iterar sobre cada síntoma
for (s in sintomas) {
  cat("\n\n### Síntoma:", s, "###\n")
  
  # Filtrar datos para ese síntoma
  datos_s <- freq_t5 %>% filter(sintoma == s)
  
  if (n_distinct(datos_s$nivel_manejo) > 1) {
    # Crear tabla cruzada
    tabla_s <- xtabs(freq ~ nivel_manejo, data = datos_s)
    
    # Test chi-cuadrado
    test <- suppressWarnings(chisq.test(tabla_s))  # suppressWarnings para valores esperados < 5
    chi2_val <- round(test$statistic, 2)
    p_valor <- signif(test$p.value, 3)
    
    cat(paste("Chi² =", chi2_val, "| p =", p_valor, "\n"))
    
    # Modelo GLM
    modelo <- glm(freq ~ nivel_manejo, family = "poisson", data = datos_s)
    emm <- emmeans(modelo, ~ nivel_manejo)
    cld_result <- cld(emm, Letters = letters)
    
    print(cld_result)
  } else {
    cat("No hay suficientes estaciones para comparar.\n")
  }
}

######################################
freq_t6 <- tabla %>%
  filter(sintoma != "S/S") %>% 
  group_by(estacion, sintoma) %>%
  summarise(freq = sum(`Dardos recolectados por síntoma`))

# Convertís a tabla cruzada (matriz de contingencia)
dt_6 <- xtabs(freq ~ estacion + sintoma, data = freq_t6)

# Test chi²
test <- chisq.test(dt_6)
chi2_val <- round(test$statistic, 2)
p_valor <- round(test$p.value, digits = 4)
print(test)

# Convertir tabla cruzada a data frame largo
df_heat <- as.data.frame(as.table(dt_6))

# Graficar heatmap
jpeg("graf/heatmal_estacion.jpeg", width = 4000, height = 3000, res = 800)
ggplot(df_heat, aes(x = sintoma, y = estacion, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = Freq), color = "black", size = 3) +
  labs(
    title = "Heatmap de síntomas por estación",
    x = "Síntoma",
    y = "Estación",
    fill = "Frecuencia"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# Lista de síntomas únicos
sintomas <- unique(freq_t6$sintoma)

# Iterar sobre cada síntoma
for (s in sintomas) {
  cat("\n\n### Síntoma:", s, "###\n")
  
  # Filtrar datos para ese síntoma
  datos_s <- freq_t6 %>% filter(sintoma == s)
  
  if (n_distinct(datos_s$estacion) > 1) {
    # Crear tabla cruzada
    tabla_s <- xtabs(freq ~ estacion, data = datos_s)
    
    # Test chi-cuadrado
    test <- suppressWarnings(chisq.test(tabla_s))  # suppressWarnings para valores esperados < 5
    chi2_val <- round(test$statistic, 2)
    p_valor <- signif(test$p.value, 3)
    
    cat(paste("Chi² =", chi2_val, "| p =", p_valor, "\n"))
    
    # Modelo GLM
    modelo <- glm(freq ~ estacion, family = "poisson", data = datos_s)
    emm <- emmeans(modelo, ~ estacion)
    cld_result <- cld(emm, Letters = letters)
    
    print(cld_result)
  } else {
    cat("No hay suficientes estaciones para comparar.\n")
  }
}























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
