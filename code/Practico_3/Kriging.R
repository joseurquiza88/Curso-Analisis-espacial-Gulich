# Carga de paquetes ####
library(gstat)
library(sf)
library(mapview)
library(ggplot2)
library(cowplot)
library(raster)
library(tmap)
library(leaflet)
library(stars)


#  Carga de base de datos ####
setwd("D:/Josefina/Cursos/curso_gulich/analisis_espacial/code/Practico_2")
datos <- st_read("soja_depurado.gpkg")
head(datos)

# limites (bordes) del lote ####
#esto lo hacemos a partir de los mismos datos, 
# concavety limites mas suavisado o mas fijo
limites <- concaveman::concaveman(datos, concavity = 10)
plot(limites)
limites2 <- concaveman::concaveman(datos, concavity =1 )

plot(limites2)
class(limites)
mapview(limites, col.regions = "red") +  mapview(datos)

# Ajuste de semivariograma experimetal, sin tendencia ####
semi_exp <-
  variogram(REND ~ 1, datos)
plot(semi_exp)

# Ajuste de semivariograma teorico ####
modelo_teorico <-
  fit.variogram(semi_exp, vgm(c("Exp", "Sph")))
modelo_teorico
plot(semi_exp , modelo_teorico)

# Generacion de grilla de prediccion ####
#2.5 metros, grilla de 2.5 x 2.5 m
grilla <- st_bbox(limites) %>%
  st_as_stars(dx = 2.5) %>%
  st_crop(limites)# la corto para que tenga la misma forma de los limites que gemera

plot(grilla)
mapview(limites) +
  mapview(grilla)

#  Kriging Ordinario ####
interp_kg <-
  krige(REND ~ 1,
        datos,
        grilla,
        model = modelo_teorico,
        nmax = 25)

plot(interp_kg)
interp_kg

# Funcio para detectar datos con coordendas repetidas ####
zd <- zerodist(as_Spatial(datos))
zd
# Eliminar duplicados
# datos2 <- datos[-zd[,2], ]

# st_equals(datos)
# unicos = st_equals(datos, retain_unique = TRUE)
# datos[-unlist(datos),]

plot(interp_kg["var1.pred"],
     main = "Kriging Ordinario: Predicciones", col = terrain.colors(10))

plot(interp_kg["var1.var"], main = "Kriging Ordinario: Varianza", col = terrain.colors(10))

interp_kg$DE_pred <- sqrt(interp_kg$var1.var)
interp_kg

plot(interp_kg["DE_pred"],
     main = "Kriging Ordinario: DE", col = terrain.colors(15))

####  Exportar predichos y DE de prediccion ####
write_stars(interp_kg,
            layer = "var1.pred",
            "pred_rinde_soja.tiff",
            delete_layer = T)

write_stars(interp_kg,
            layer = "DE_pred",
            "DE_rinde_soja.tiff",
            delete_layer = T)


interp_kg_poly <-
  st_as_sf(interp_kg, as_points = FALSE, merge = TRUE)

ggplot(interp_kg_poly) +
  geom_sf(aes(color = var1.pred))

interp_kg_ptos <-
  st_as_sf(interp_kg,
           as_points = TRUE)

st_write(interp_kg_ptos,
         "pred_soja_ptos.gpkg",
         delete_layer = TRUE)


st_write(interp_kg_poly,
         "pred_soja_poly.gpkg",
         delete_layer = TRUE)

####  Visualizacion interactiva ####
tmap_mode('view')
#tmaptools::palette_explorer() # opcion para visulizar paletas

interp_kg_DE <- interp_kg

mapa_prediccion <-
  tm_basemap(
    c(
      Satelite = "Esri.WorldImagery",
      Politico = "Esri.WorldGrayCanvas",
      Topo = "Esri.WorldTopoMap"
    )
  ) +
  tm_shape(interp_kg) +
  tm_raster(
    col = "var1.pred",
    title = "Rendimiento (t/ha)",
    style = "fixed",
    palette = "Spectral",
    contrast = c(0, 1),
    breaks = c(0.9, 1.5, 1.6, 1.7, 1.8, 2, 2.1, 2.2, 2.3, 2.4, 2.6, 3.1)
  ) +
  tm_layout(legend.format = list(
    scientific = TRUE,
    format = "f",
    digits = 1
  ))

mapa_prediccion


mapa_DE <-
  tm_basemap(
    c(
      Satelite = "Esri.WorldImagery",
      Politico = "Esri.WorldGrayCanvas",
      Topo = "Esri.WorldTopoMap"
    )
  ) +
  tm_shape(interp_kg_DE) +
  tm_raster(
    col = "DE_pred",
    title = "DE de Prediccion",
    style = "quantile",
    palette = "YlOrBr",
    n = 5
  ) +
  tm_layout(legend.format = list(
    scientific = TRUE,
    format = "f",
    digits = 3
  ))

mapa_DE

mapa_muestra <- tm_shape(datos) +
  tm_dots(
    "REND",
    title = "Rendimiento (t/ha)",
    style = "fixed",
    palette = "Spectral",
    breaks = c(0.8, 1.5, 1.6, 1.7, 1.8, 2, 2.1, 2.2, 2.3, 2.4, 2.6, 3.3),
    alpha = 0.7,
    size = 0.1,
    popup.vars = T,
    popup.format = list(
      digits = 2,
      decimal.mark = ",",
      big.mark = "."
    )
  ) +
  tm_layout(legend.format = list(scientific = TRUE, format = "f"))

mapa_muestra

mapas_todos <- mapa_DE + mapa_prediccion + mapa_muestra
mapas_todos

mapas_todos_lf <- tmap_leaflet(mapas_todos)
mapshot(mapas_todos_lf, "Mapas.html")

###################################################3
# este es mas concenptual el kriging smple
# porqueen necesario poner la media de la variable
# lo cual en realidad es desconocida
# Kriging Simple ####
interp_kg_s <-
  krige(REND ~ 1,
        datos,
        grilla,
        model = modelo_teorico,
        nmax = 25,
        beta = 2.08)

plot(interp_kg_s["var1.pred"],
     main = "Kriging Simple: Predicciones")

plot(interp_kg_s["var1.var"],
     main = "Kriging Simple: Varianza")
######################################################3
# este es para cuando existe algun tipo de tedencia
#  Kriging Universal ####
head(datos)
datos <- cbind(datos, st_coordinates(datos)) %>%
  dplyr::rename("x" = X, "y" = Y)# si o si el nombre en la grilla debe ser xy

px <- ggplot(datos, aes(x, REND)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

py <- ggplot(datos, aes(y, REND)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

gridExtra::grid.arrange(px, py, ncol = 2)

# Ajuste de semivariograma experimetal, con tendencia ####
semi_exp_t <- variogram(REND ~ y, datos)
plot(semi_exp_t)
plot(semi_exp)

#Ajuste de semivariograma teorico, modelo exponencial ####
modelo_teorico_t <- fit.variogram(semi_exp_t, vgm(c("Exp", "Sph")))
modelo_teorico_t
plot(semi_exp_t , modelo_teorico_t)
plot(semi_exp , modelo_teorico)

# Interpolacion Kriging Universal
interp_kg_u <-
  krige(REND ~ y,
        datos,
        grilla,
        model = modelo_teorico_t,
        nmax = 25)

plot(interp_kg_u["var1.pred"],
     main = "Kriging Universal: Predicciones", col = terrain.colors(10))

plot(interp_kg_u["var1.var"],
     main = "Kriging Universal: Varianza")

# Kriging Universal en bloque:
interp_kg_ub <-
  krige(
    REND ~ y,
    datos,
    grilla,
    model = modelo_teorico_t,
    nmax = 25,
    block = c(10, 10)# hay que poner un valor el mayor al tamaño de la grilla de preduccion
  )

plot(interp_kg_ub["var1.pred"],
     main = "Kriging Universal en Bloque: Predicciones", col = terrain.colors(10))

plot(interp_kg_ub["var1.var"],
     main = "Kriging Universal en Bloque: Varianza")

######################################################3
# Validacion cruzada ####
modelo_teorico
modelo_esferico <-
  vgm(0.201, "Sph", 131.09, 0.0228) # Modelo esferico

modelo_exponencial <-
  fit.variogram(semi_exp, vgm(c("Exp")))
modelo_exponencial

modelo_teorico_t

modelo_esferico_tendencia <-
  vgm(0.197, "Sph", 125.63, 0.0218) # Modelo exponencial con tendencia
## krige.cv => cross validation, nmax==> 25 es para krigging local
#nfold viene por defecto 10kfolg
set.seed(7)
validacion_esferico <-
  krige.cv(REND ~ 1,
           datos,
           modelo_esferico,
           nfold = 10,
           nmax = 25)

mapview(validacion_esferico["residual"])
table(validacion_esferico$fold)

set.seed(7)
validacion_exponencial <-
  krige.cv(REND ~ 1,
           datos,
           modelo_exponencial,
           nfold = 10,
           nmax = 25)
mapview(validacion_exponencial["residual"])

set.seed(7)
validacion_esferico_tendencia <-
  krige.cv(REND ~ x + y,
           datos,
           modelo_esferico_tendencia,
           nfold = 10,
           nmax = 25)
mapview(validacion_esferico_tendencia["residual"])

# Error medio de prediccion, cercano a cero mejor:
mean(abs(validacion_esferico$residual))
mean(abs(validacion_exponencial$residual))
mean(abs(validacion_esferico_tendencia$residual))

# Error cuadratico medio de prediccion, mas pequeno mejor
mean(validacion_esferico$residual ^ 2)
mean(validacion_exponencial$residual ^ 2)
mean(validacion_esferico_tendencia$residual ^ 2)

# RMSE
sqrt(mean(validacion_esferico$residual ^ 2))
sqrt(mean(validacion_exponencial$residual ^ 2))
sqrt(mean(validacion_esferico_tendencia$residual ^ 2))

# RMSE_CV
sqrt(mean(validacion_esferico$residual ^ 2)) / mean(validacion_esferico$observed) * 100
sqrt(mean(validacion_exponencial$residual ^ 2)) / mean(validacion_exponencial$observed) * 100
sqrt(mean(validacion_esferico_tendencia$residual ^ 2)) / mean(validacion_esferico_tendencia$observed) * 100

# Mean square normalized error, cercano a 1 mejor
mean(validacion_esferico$zscore ^ 2)
mean(validacion_exponencial$zscore ^ 2)
mean(validacion_esferico_tendencia$zscore ^ 2)

# Correlacion lineal entre valores observados y predichos
tabla_obs_Pred <-
  data.frame(rbind(
    data.frame(validacion_esferico, "Modelo" = "Esferrico"),
    data.frame(validacion_exponencial, "Modelo" = "Exponencial"),
    data.frame(validacion_esferico_tendencia, "Modelo" = "Esferico con Tendencia")
  ))


tabla_obs_Pred$predichos <-
  tabla_obs_Pred$observed - tabla_obs_Pred$residual

plot_obs_pred <- ggplot(tabla_obs_Pred, aes(observed, predichos)) +
  geom_point() + facet_grid(. ~ Modelo) + stat_smooth(method = "lm") +
  background_grid(major = 'y', minor = "none") +
  panel_border() +
  labs(x = "Observados", y = "Predichos")

plot_obs_pred
