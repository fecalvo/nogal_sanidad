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
tabla$LAT_dis <- 0
tabla$LAT_dis[tabla$LAT > 32 & tabla$LAT < 32.948] <- "LAT_1"
tabla$LAT_dis[tabla$LAT > 32.948 & tabla$LAT < 33.3605] <- "LAT_2"
tabla$LAT_dis[tabla$LAT >= 33.3605 & tabla$LAT < 33.6565] <- "LAT_3"
tabla$LAT_dis[tabla$LAT >= 33.6565 & tabla$LAT < 34.2075] <- "LAT_4"
tabla$LAT_dis[tabla$LAT >= 34.2075] <- "LAT_5"
misf <- sf::read_sf("provincia-de-mendoza-shp.shp")

jpeg("graf/mapa_lat.jpeg", width = 4000, height = 4000, 
     units = "px", res = 800) 
ggplot() +
  geom_sf(data = misf, fill = "white", color = "black") +
  geom_hline(yintercept = -32.695, size = 0.3) +
  geom_hline(yintercept = -32.984, size = 0.3) +
  geom_hline(yintercept = -33.3605, size = 0.3) +
  geom_hline(yintercept = -33.6565, size = 0.3) +
  geom_hline(yintercept = -34.2075, size = 0.3) + 
  geom_hline(yintercept = -34.618, size = 0.3) +
  annotate(geom = "text", x = -65.8, y = -32.83, label = "LAT 1") +
  annotate(geom = "text", x = -65.8, y = -33.154, label = "LAT 2") +
  annotate(geom = "text", x = -65.8, y = -33.50, label = "LAT 3") +
  annotate(geom = "text", x = -65.8, y = -33.96, label = "LAT 4") +
  annotate(geom = "text", x = -65.8, y = -34.432, label = "LAT 5") +
  xlim(-71, -65) +
  xlab("") +
  ylab("") +
  theme_bw() 
dev.off()

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

