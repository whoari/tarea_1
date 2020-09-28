#Tarea 1 ----
#Ariadna Lobo
# Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen=999)
# Librerias ----
pacman::p_load(sf, tidyverse, leaflet, stringr, rebus, DT, kableExtra)
library(webshot)
# Bases de Datos ----
mpios <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios2.geojson", quiet = TRUE) %>% 
  filter(CVE_ENT == "09")
edo <- st_read("https://raw.githubusercontent.com/JuveCampos/MexicoSinIslas/master/Sin_islas.geojson", quiet = TRUE)
edo <- st_read("https://raw.githubusercontent.com/JuveCampos/MexicoSinIslas/master/Sin_islas.geojson", quiet = TRUE) %>% 
  filter(ENTIDAD == "CIUDAD DE MÉXICO")
library(readr)
victimas <- read_csv("C:/Users/Ariadna/Desktop/CIDE/Estadística/Estadística Garrido/Tercer semestre/Tarea 1/Tarea 1 Ariadna Lobo/Tarea 1/01_Datos/victimas-en-carpetas-de-investigacion-pgj (2).csv")
View(victimas)
# Explorar datos ----
glimpse(mpios)
st_crs(mpios)
glimpse(victimas)
head(victimas)
class(victimas)
class(mpios)
st_geometry_type(mpios)
# Elabore un documento en HTML (en RMarkdown) que contenga al menos 4 de los siguientes 5 elementos ---- 

# 1.- Una gráfica en ggplot ----
# Número de víctimas dentro de carpetas de investigación abiertas de enero de 2020 a marzo de 2020.
vic_mes <- victimas %>% 
  group_by(Mes_inicio) %>% 
  count(Mes_inicio)
class(vic_mes)
#Primero agrupé la variable de mes de inicio, pues también está el mes de hechos, pero yo sólo quería en qué mes se abrió la carpeta y no en qué mes sucedió el hecho.
# Después, utilicé ggplot para generar una gráfica de barras con geom_col, donde designé el mes como X (variable categórica) y "n", que son el número de víctimas, como Y (variable numérica).
vic_mes %>%   
  ggplot(aes(x = Mes_inicio, y = n, fill = n)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 14, color = "black", face = "bold"),
        legend.title = element_text(size = 10, color = "black", face = "bold"),
        legend.key.size = unit(1.5, "line"),
        legend.text = element_text(size = 9, color = "black", face = "plain"),
        plot.caption = element_text(size = 10, hjust = 1, color = "black", face = "italic"),
        plot.subtitle = element_text(size = 11, color = "black", face = "plain")) +
  labs(title = "Número de víctimas de violencia\nfamiliar 2020",
       fill = "Número de víctimas\nen carpetas",
       subtitle = "De enero a marzo de 2020",
       caption = "Fuente: Datos Abiertos CDMX") +
  geom_text(aes(label = c("1871", " 2192", "2610", "2971")), position=position_dodge(width=1), hjust = 0.5, vjust = -0.4, size = 2, color= "black", fontface = 2)
# 2.- Un mapa estático en ggplot ----
# Un desmadre que hice
#Tomé el mapa de municipios, le indiqué que sólo tomara ciertas alcaldías para que el número de variables fuera el mismo que mi base de datos y utilicé "marge" para pegarlos.

mpios <- mpios %>% 
  mutate(ALC_M = str_to_upper(NOM_MUN))
vic_alc <- victimas %>% 
  group_by(AlcaldiaHechos) %>% 
  count() %>% 
  filter(AlcaldiaHechos %in% c("AZCAPOTZALCO", "CUAJIMALPA DE MORELOS", "IZTACALCO", "IZTAPALAPA", "LA MAGDALENA CONTRERAS", "MILPA ALTA", "TLALPAN", "XOCHIMILCO", "MIGUEL HIDALGO", "VENUSTIANO CARRANZA"))

  vic_por_alcaldia <- merge(vic_alc, mpios, 
                          by.x="AlcaldiaHechos", 
                          by.y = "ALC_M")

view(vic_por_alcaldia)
#tuve que buscar por qué no me lo graficaba y pues resulta que el señor R no hallaba la variable GEOMETRY
vic_por_alcaldia <- st_as_sf(vic_por_alcaldia, sf_column_name = "geometry") 
#Ahora sí, el mapa:
plot(vic_por_alcaldia,max.plot= 1)
ggplot() + 
  geom_sf(data = vic_por_alcaldia, fill = "purple", alpha = 0.7) +
  geom_sf(data = mpios, fill = "orange", alpha = 0.5) +
  labs(title = "Víctimas de violencia familar en CI en 2020", 
       subtitle = "Datos de 2020. Ciudad de México", 
       caption = "Cada punto rosa representa la coordenada de las víctimas reportadas en las carpetas de investigación.\nFuente: Datos Abiertos CDMX") + 
  theme_bw() + 
  theme(axis.text = element_blank(), 
        panel.grid = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_rect(),
        axis.ticks = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5, 
                                  colour = "purple", 
                                  family = "Arial", 
                                  face = "bold", 
                                  size = 15), 
        plot.subtitle = element_text(hjust = 0.5, 
                                     colour = "black", 
                                     family = "Arial", 
                                     face = "bold", 
                                     size = 15), 
        plot.caption = element_text(colour = "gray50", 
                                    hjust = 1))
# 3.- Mapa dinámico ----
max_ <-  max(vic_por_alcaldia$n, na.rm = TRUE)
min_ <-  min(vic_por_alcaldia$n, na.rm = TRUE)
#Seleccioné el mínimo y máximo del número de víctimas en CI por alcladía
#Designé la paleta de colores y del tipo de mapa que utilizaría
pal_vic <- colorNumeric(palette = "Greys", 
                         rev = FALSE, 
                         domain = c(min_, 
                                    max_))

leaflet(vic_por_alcaldia, options = leafletOptions(zoomControl = TRUE)) %>%
  addProviderTiles("CartoDB.DarkMatter") %>% 
  addPolygons(data=mpios, color="yellow", opacity = 0.5) %>% 
    addPolygons(data = vic_por_alcaldia, fillColor = ~pal_vic(n), color = ~pal_vic(n), fillOpacity = 0.5,label = vic_por_alcaldia$AlcaldiaHechos, popup = paste0(vic_por_alcaldia$n))
# 4.- Tabla ----
DT::datatable(vic_por_alcaldia)
