#### Carga base de datos ####
base <- read.table("soja_geo.txt", header = T)
head(base)
class(base)

#### Instalación y carga de paquetes ####
lista_paquetes <-
  c(
    "ggplot2",
    "spatialreg",
    "concaveman",
    "Rcpp",
    "ade4",
    "e1071",
    "gstat",
    "sp",
    "mapview",
    "tmap",
    "automap",
    "RColorBrewer",
    "rgdal",
    "spdep",
    "fields",
    "tcltk",
    "raster",
    "data.table",
    "MASS",
    "pdp",
    "caret",
    "SpatialPack",
    "interp",
    "akima",
    "distances",
    "fclust",
    "cluster",
    "randomForest",
    "ggpubr",
    "stringr",
    "gbm",
    "clhs",
    "leafsync",
    "leafpop",
    "spatstat"
  )

paquetes_nuevos <-
  lista_paquetes[!(lista_paquetes %in% installed.packages()[, "Package"])]
if (length(paquetes_nuevos))
  install.packages(paquetes_nuevos)

library(sf)
library(ggplot2)
library(mapview)

#### Generación objeto sf ####
class(base)
head(base)
base_t <- st_as_sf(base, coords = c("x", "y"), crs = 4326)
class(base_t)
base_t

#### Conversión de coordenadas geográficas a planas (UTM) ####
base_t <- st_transform(base_t, crs = 32720)
base_t

#### Exportar base de datos ####
st_write(base_t,
         "soja_t.csv",
         layer_options = "GEOMETRY=AS_XY",
         delete_layer = T) # .csv
st_write(base_t, "soja_t.shp", delete_layer = T) # .shp
st_write(base_t, "soja_t.gpkg", delete_layer = T) # .gpkg

#### Lectura de datos espaciales
base_sp <- st_read("soja_t.gpkg")

#### Visualización de datos espaciales ####
#### plot sf ####
plot(base_t)

plot(
  base_t,
  key.pos = 4,
  axes = TRUE,
  key.width = lcm(1.5),
  key.length = 1,
  pal = terrain.colors
)

plot(
  base_t,
  key.pos = 4,
  axes = TRUE,
  key.width = lcm(1.5),
  key.length = 1,
  pal = heat.colors
) # topo.colors cm.colors

plot(
  base_t,
  key.pos = 4,
  axes = TRUE,
  key.width = lcm(1.5),
  key.length = 1,
  breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 10)
)

#### plot ggplot2 ####
base_t %>% ggplot() +
  geom_sf(aes(color = REND)) +
  scale_color_viridis_c(direction = -1)

base_t %>% ggplot() +
  geom_sf(aes(color = REND)) +
  scale_color_gradient(low = "#fffb00", high = "#ff0000")


#### plot mapview ####
mapview(base_t)
mapview(base_t,
        alpha = 0,
        zcol = "REND",
        col.regions = topo.colors)
mapview(base_t, zcol = "REND") + mapview(base_sp)
