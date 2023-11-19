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


# limites (bordes) del lote ####
class(suelo_finca_limites)

# Generacion de grilla de prediccion ####
grilla <- st_bbox(suelo_finca_limites) %>%
  st_as_stars(dx = 5) %>%   # tamaño de la celda, a mas datos puede ser mas chica
  st_crop(suelo_finca_limites)

plot(grilla)

mapview(suelo_finca_limites) +
  mapview(grilla)

#  Kriging universala ####
interp_MO <-
  krige(MO ~ x + y ,
        suelo_finca3,
        grilla,
        model = modelo_teorico_MO_t,
        nmax = 25)

#plot(interp_MO)
#interp_MO

# Funcio para detectar datos con coordendas repetidas ####
zd <- zerodist(as_Spatial(suelo_finca3))
zd
# no hay datos con coord repetidas


# Eliminar duplicados
# datos2 <- datos[-zd[,2], ]

# st_equals(datos)
# unicos = st_equals(datos, retain_unique = TRUE)
# datos[-unlist(datos),]

cols <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
plot(interp_MO["var1.pred"],
     main = "Kriging Ordinario: Predicciones", col = cols)

#plot(interp_MO["var1.var"], main = "Kriging Ordinario: Varianza", col = terrain.colors(10))

interp_MO$DE_pred <- sqrt(interp_MO$var1.var)
interp_MO$DE_pred

plot(interp_MO["DE_pred"],
     main = "Kriging Ordinario: DE", col = terrain.colors(15))
# 
# ####  Exportar predichos y DE de prediccion ####
# write_stars(interp_MO,
#             layer = "var1.pred",
#             "pred_rinde_MO.tiff",
#             delete_layer = T)
# write_stars(interp_MO,
#             layer = "DE_pred",
#             "DE_rinde_MO.tiff",
#             delete_layer = T)
# 
# interp_MO_poly <-
#   st_as_sf(interp_MO, as_points = FALSE, merge = TRUE)
# 
# ggplot(interp_MO_poly) +
#   geom_sf(aes(color = var1.pred))
# 
# interp_MO_ptos <-
#   st_as_sf(interp_MO,
#            as_points = TRUE)
# 
# st_write(interp_MO_ptos,
#          "pred_MO_ptos.gpMO",
#          delete_layer = TRUE)
# 
# 
# st_write(interp_MO_poly,
#          "pred_MO_poly.gpMO",
#          delete_layer = TRUE)

####  Visualizacion interactiva ####
tmap_mode('view')
#tmaptools::palette_explorer() # opcion para visulizar paletas

interp_MO_DE <- interp_MO

mapa_prediccion <-
  tm_basemap(
    c(
      Satelite = "Esri.WorldImagery",
      Politico = "Esri.WorldGrayCanvas",
      Topo = "Esri.WorldTopoMap"
    )
  ) +
  tm_shape(interp_MO) +
  tm_raster(
    col = "var1.pred",
    title = "MO Predicción (%)",
    style = "fixed",
    palette = "-Spectral",
    contrast = c(0, 1),
    breaks = seq(0,3, 0.5),
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
  tm_shape(interp_MO_DE) +
  tm_raster(
    col = "DE_pred",
    title = "DE de Predicción",
    style = "quantile",
    palette = "-Spectral",
    n = 5
  ) +
  tm_layout(legend.format = list(
    scientific = TRUE,
    format = "f",
    digits = 3
  ))

mapa_DE

mapa_muestra <- tm_shape(suelo_finca3) +
  tm_dots(
    "MO",
    title = "MO Observados (%)",
    style = "fixed",
    palette = "-Spectral",
    breaks = seq(0,3, 0.5),
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

# mapas_todos_lf <- tmap_leaflet(mapas_todos)
# mapshot(mapas_todos_lf, "Mapas.html")


set.seed(7)
validacion_tendencia <-
  krige.cv(MO ~ x + y , # la tendencia era en funcion a la latitud solamente
           suelo_finca3,
           modelo_teorico_MO_t,
           nfold = 10)
mapview(validacion_esferico_tendencia["residual"],)

validacion_tendencia %>% 
  summarise(ME = mean(residual),
            MAE =  mean(abs(residual)),
            MAPE = mean(abs(residual) / observed ) *100,
            MSE  =  mean(residual^ 2),
            MSNE= mean(zscore ^ 2),
            RMSE  =  sqrt(mean(residual ^ 2)),
            RMSE_cv  = sqrt(MSE) / mean(observed) * 100)



            




# # Kriging Simple ####
# interp_MO_s <-
#   krige(REND ~ 1,
#         datos,
#         grilla,
#         model = modelo_teorico,
#         nmax = 25,
#         beta = 2.08)
# 
# plot(interp_MO_s["var1.pred"],
#      main = "Kriging Simple: Predicciones")
# 
# plot(interp_MO_s["var1.var"],
#      main = "Kriging Simple: Varianza")

#  Kriging Universal ####
head(datos)
datos <- cbind(datos, st_coordinates(datos)) %>%
  dplyr::rename("x" = X, "y" = Y)

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
interp_MO_u <-
  krige(REND ~ y,
        datos,
        grilla,
        model = modelo_teorico_t,
        nmax = 25)

plot(interp_MO_u["var1.pred"],
     main = "Kriging Universal: Predicciones", col = terrain.colors(10))

plot(interp_MO_u["var1.var"],
     main = "Kriging Universal: Varianza")

# Kriging Universal en bloque:
interp_MO_ub <-
  krige(
    REND ~ y,
    datos,
    grilla,
    model = modelo_teorico_t,
    nmax = 25,
    block = c(10, 10)
  )

plot(interp_MO_ub["var1.pred"],
     main = "Kriging Universal en Bloque: Predicciones", col = terrain.colors(10))

plot(interp_MO_ub["var1.var"],
     main = "Kriging Universal en Bloque: Varianza")

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
  krige.cv(REND ~ y, # la tendencia era en funcion a la latitud solamente
           datos,
           modelo_esferico_tendencia,
           nfold = 10,
           nmax = 25)
mapview(validacion_esferico_tendencia["residual"])

# Error absoluto medio de prediccion, cercano a cero mejor:
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
  bacMOround_grid(major = 'y', minor = "none") +
  panel_border() +
  labs(x = "Observados", y = "Predichos")

plot_obs_pred
