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

# Flor, esto es una prueba

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


#### tabla 2 ####
tabla

freq_t7 <- tabla %>%
  mutate(sintoma = as.factor(sintoma)) %>% 
  group_by(sintoma) %>%
  summarise_at(vars(alternaria_sp, colletotrechum, colonia_6, colonia_35, 
                    d_mutila, d_seriata), sum) %>% 
  pivot_longer(cols = 2:7, names_to = "sp", values_to = "freq")


# Convertís a tabla cruzada (matriz de contingencia)
dt_7 <- xtabs(freq ~ sintoma + sp, data = freq_t7)


# Convertir tabla cruzada a data frame largo
df_heat <- as.data.frame(as.table(dt_7))

# Graficar heatmap
jpeg("graf/heatmal_tabla_2.jpeg", width = 4000, height = 3000, res = 800)
ggplot(df_heat, aes(x = sp, y = sintoma, fill = Freq)) +
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

# Lista de especies únicas
especies <- unique(freq_t7$sp)

# Iterar por especie
for (e in especies) {
  cat("\n\n### Especie:", e, "###\n")
  
  # Filtrar datos por especie
  datos_e <- freq_t7 %>% filter(sp == e)
  
  if (n_distinct(datos_e$sintoma) > 1) {
    # Tabla cruzada: frecuencia por síntoma
    tabla_e <- xtabs(freq ~ sintoma, data = datos_e)
    
    # Test chi-cuadrado
    test <- suppressWarnings(chisq.test(tabla_e))
    chi2_val <- round(test$statistic, 2)
    p_valor <- signif(test$p.value, 3)
    
    cat(paste("Chi² =", chi2_val, "| p =", p_valor, "\n"))
    
    # Modelo Poisson
    modelo <- glm(freq ~ sintoma, family = "poisson", data = datos_e)
    
    # Comparaciones post-hoc
    emm <- emmeans(modelo, ~ sintoma)
    cld_result <- cld(emm, Letters = letters, reversed = T)
    
    print(cld_result)
  } else {
    cat("No hay suficientes síntomas para comparar.\n")
  }
}

agregado <- tabla %>%
  filter(sintoma != "S/S") %>% 
  summarise_at(vars(alternaria_sp, colletotrechum, colonia_6, colonia_35, 
                    d_mutila, d_seriata), sum)

# Test chi²
test <- chisq.test(agregado)
chi2_val <- round(test$statistic, 2)
p_valor <- round(test$p.value, digits = 4)
print(test)


t(agregado)
