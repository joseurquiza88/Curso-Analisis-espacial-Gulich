# Carga de paquetes
library(gstat)
library(spdep)
library(mapview)
library(raster)
library(tmap)
library(leaflet)
library(MASS)
library(e1071)
library(stars)
library(ggplot2)
library(cowplot)
library(caret)

# Carga de base de datos ####
datos <- read.table("MO_Córdoba.txt", header = T)
datos <- st_as_sf(datos, coords = c("x", "y"), crs = 22174)
datos$x <- st_coordinates(datos)[, 1]
datos$y <- st_coordinates(datos)[, 2]

# Random Forest Kriging ####
# Ajuste de modelo de RF
fitControl <- trainControl(method = "cv",number = 10)

set.seed(7)
train_rf <- train(
  MO ~ NDVI + TWI + PPmed + Altura + x + y,
  data = datos,
  method = "rf",
  trControl = fitControl,
  verbose = FALSE,
  importance = T
  
)

# Incorporamos los residuos del RF a la base de datos
datos$residuosRF <- datos$MO - predict(train_rf, newdata = datos)

# Ajuste de semivariograma experimetal y teorico a los residuos del RF
vrfk <- variogram(residuosRF ~ 1 , datos)
plot(vrfk)

vrfk <- variogram(residuosRF ~ 1 , datos, cutoff = 150000) # Forzamos el ajuste!!!
plot(vrfk)
vrfk

v.fit_mo_rfk <-
  fit.variogram(vrfk , vgm(c("Exp", "Sph","Nug")))
plot(vrfk , v.fit_mo_rfk)

# Kriging sobre residuos del RF
grilla_covariables <- read.table("grilla_MO.txt")
head(grilla_covariables)
grilla_covariables <-
  st_as_sf(grilla_covariables,
           coords = c("x", "y"),
           crs = 22174)
grilla_covariables$x <- st_coordinates(grilla_covariables)[, 1]
grilla_covariables$y <- st_coordinates(grilla_covariables)[, 2]

kgresRF <-
  krige(residuosRF ~ 1,
        datos,
        grilla_covariables,
        model = v.fit_mo_rfk,
        nmax = 25)

pred_kg_residuos <-
  st_rasterize(kgresRF["var1.pred"],
               dx = 500,
               dy = 500,
               value = NA_real_)

plot(pred_kg_residuos,
     col = terrain.colors(20),
     reset = FALSE)

# Prediccion final
grilla_covariables$RF_pred <-
  predict(train_rf, newdata = grilla_covariables) + kgresRF$var1.pred

grilla_covariables$RF <-
  predict(train_rf, newdata = grilla_covariables) 

# Mapa Interactivo
grilla_covariables_rast2 <-
  st_rasterize(grilla_covariables, dx = 500, dy = 500)

grilla_covariables_rast2_vis <-grilla_covariables_rast2

tmap_mode('view')
mapa_prediccionRF_KO <-
  tm_basemap(
    c(
      Satelite = "Esri.WorldImagery",
      Politico = "Esri.WorldGrayCanvas",
      Topo = "Esri.WorldTopoMap"
    )
  ) +
  tm_shape(grilla_covariables_rast2) +
  tm_raster(
    col = "RF_pred",
    title = "MO (%)",
    style = "quantile",
    palette = "YlOrBr",
    contrast = c(0, 1),
    n = 10
  ) +
  tm_layout(legend.format = list(
    scientific = TRUE,
    format = "f",
    digits = 1
  ))
mapa_prediccionRF_KO


mapa_prediccionRF <-
  tm_basemap(
    c(
      Satelite = "Esri.WorldImagery",
      Politico = "Esri.WorldGrayCanvas",
      Topo = "Esri.WorldTopoMap"
    )
  ) +
  tm_shape(grilla_covariables_rast2_vis) +
  tm_raster(
    col = "RF",
    title = "MO (%)",
    style = "quantile",
    palette = "YlOrBr",
    contrast = c(0, 1),
    n = 10
  ) +
  tm_layout(legend.format = list(
    scientific = TRUE,
    format = "f",
    digits = 1
  ))

mapa_RFs <-mapa_prediccionRF_KO + mapa_prediccionRF
mapa_RFs
