library(tidyverse)
library(sf)

#leer tabla original (hice algunas modificaciones agregando 0s)
tabla <- readxl::read_excel("data_sig/relevamiento.xlsx")

#discretización latitud
tabla <- tabla %>% 
  mutate(LAT = LAT*(-1),
         LON = LON*(-1))

#dejé 5 clases porque con 3 quedaba toda la frecuencia en la intermedia
classIntervals(tabla$LAT , n=5, style="fisher")
classIntervals(tabla$LON , n=3, style="fisher")

jpeg("graf/mapa_lat.jpeg", width = 4000, height = 4000, 
     units = "px", res = 800) 
ggplot() +
  geom_sf(data = misf, fill = "white", color = "black") +
  geom_hline(yintercept = -32.695, size = 0.3, color = "red") +
  geom_hline(yintercept = -32.984, size = 0.3, color = "red") +
  geom_hline(yintercept = -33.3605, size = 0.3, color = "red") +
  geom_hline(yintercept = -33.6565, size = 0.3, color = "red") +
  geom_hline(yintercept = -34.2075, size = 0.3, color = "red") + 
  geom_hline(yintercept = -34.618, size = 0.3, color = "red") +
  annotate(geom = "text", x = -65.8, y = -32.83, label = "LAT 1") +
  annotate(geom = "text", x = -65.8, y = -33.154, label = "LAT 2") +
  annotate(geom = "text", x = -65.8, y = -33.50, label = "LAT 3") +
  annotate(geom = "text", x = -65.8, y = -33.96, label = "LAT 4") +
  annotate(geom = "text", x = -65.8, y = -34.432, label = "LAT 5") +
  geom_vline(xintercept = -68.464, size = 0.3, color = "red") +
  geom_vline(xintercept = -68.7385, size = 0.3, color = "red") +
  geom_vline(xintercept = -69.1135, size = 0.3, color = "red") +
  geom_vline(xintercept = -69.311, size = 0.3, color = "red") +
  annotate(geom = "text", x = -69.21, y = -38, label = "LON 3", angle = 90, size = 4) +
  annotate(geom = "text", x = -68.92, y = -38, label = "LON 2", angle = 90, size = 4) +
  annotate(geom = "text", x = -68.60, y = -38, label = "LON 1", angle = 90, size = 4) +
  xlim(-71, -65) +
  ylim(-39 , -32) +
  xlab("") +
  ylab("") +
  theme_bw() 
dev.off()

help("annotate")
data <- readxl::read_excel("ARG/coordenadas.xlsx")

cbind(ubi, numero) %>% 
  arrange(desc(Latitud))

jpeg("graficos_pistacho/mapa.jpeg", width = 4000, height = 6000, 
     units = "px", res = 800) 

ggplot(ubi) +
  geom_sf(data = misf, fill = "yellow", color = "black") +
  geom_point(aes(x = Longitud_Decimal, y = Latitud_Decimal, color = numero), size = 10, alpha= 0.5) +
  scale_color_gradient(low = "yellow",high = "#008000", na.value = NA)+ 
  guides(color = guide_legend(title="Aptitud")) +
  theme_bw() +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank())

dev.off()

