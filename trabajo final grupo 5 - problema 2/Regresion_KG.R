# Carga de paquetes
pacman::p_load(gstat,spdep,mapview,raster,tmap,leaflet,
               MASS,e1071,
               stars, # permite manipular archivos tipo raster
               ggplot2,cowplot,caret,gridExtra)












### no mover

# Carga de base de datos ####
datos <- read.table("MO_Córdoba.txt", header = T)

# Graficos Exploratorios ####
px <- ggplot(datos, aes(x, MO)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

py <- ggplot(datos, aes(y, MO)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

grid.arrange(px, py, ncol = 2)

ggplot(datos, aes(x = MO)) +
  geom_histogram(aes(y = after_stat(count) / sum(count))) +
  ylab("Frecuencia Relativa") +
  theme_light()

skewness(datos$MO)

#Transformacion a sf ####
datos <-st_as_sf(datos, coords=c("x", "y"), crs=22174)
class(datos)
mapview(datos)

# Ajuste de semivariograma experimetal, sin tendencia ####
semiv_exp <- variogram(MO ~ 1, datos)
plot(semiv_exp)

# Ajuste de semivariograma experimetal, con tendencia ####
datos$x <-st_coordinates(datos)[,1]
datos$y <-st_coordinates(datos)[,2]
semiv_exp <- variogram(MO ~ x + y, datos)
plot(semiv_exp)

# Ajuste de semivariograma teórico ####
semiv_teorico <-
  fit.variogram(semiv_exp, vgm(c("Exp", "Sph", "Gau")))
plot(semiv_exp, semiv_teorico)

attr(semiv_teorico, 'SSErr')

# Generacion de grilla de prediccion ####
limites <-st_read("LimitesCBA/Cordoba_limite.shp")
limites <-st_transform(limites, crs=22174) # se transforma para que UTM zone sea igual que en datos (POSGAR 98 EN ARGENTINA FAJA 4: es el 22174)
plot(limites)
st_crs(datos) # me muestra cual es de nuestros datos
limites <-st_transform(limites, crs= st_crs(datos)) # otra opcion de transformar
#ahora limites  dice PROJECTED CRS: POSGAR 98 / ARGENTINA4
grilla <- st_bbox(limites) %>%
  st_as_stars(dx = 2500) %>%
  st_crop(limites)

plot(grilla)

# Kriging Universal ####
interp_kg <-
  krige(MO ~ x + y, datos, grilla, model = semiv_teorico, nmax = 25)

plot(interp_kg["var1.pred"],
     main = "Kriging Universal: Predicciones", col = terrain.colors(10))

interp_kg$DE_pred <-sqrt(interp_kg$var1.var)

plot(sqrt(interp_kg["var1.var"]),
     main = "Kriging Universal: DE de Prediccion",col = terrain.colors(15))

#  Exportar predichos y varianza de prediccion ####
write_stars(interp_kg,
            layer = "var1.pred",
            "pred_MO.tiff",
            delete_layer = T)

write_stars(interp_kg,
            layer = "DE_pred",
            "DE_MO.tiff",
            delete_layer = T)


# Kriging Regresión ####
names(datos)

# Seleccion de covariables   
## ajuste de modelo de regresion multiple
mlreg <- lm(MO ~ ., data = st_drop_geometry(datos))   # st_drop_geometry : elimina la columna GEOMETRY porque no tiene sentido ponerla como covariable
### MO~ NDVI + TWI + LS_Factor... etc   == es lo mismo que MO ~ .   (toma todas las columnas)
summary(mlreg)
mlreg_final <- stepAIC(mlreg, direction = "both")   #para que seleccione por AIC
mlreg_final  # muestra los coeficentes del modelo con mejor ajuste segun AIC (con las variables explicativas que corresponden)
summary(mlreg_final) 

# Incorporamos los residuos del MLR a la base de datos
datos$residuos <- mlreg_final$residuals
names(datos)

# Ajuste de semivariograma experimetal y teorico a los residuos
semivariograma_exp_rk <- variogram(residuos ~ 1 , datos)
plot(semivariograma_exp_rk)
semivariograma_exp_rk

semivariograma_teorico_rk <-
  fit.variogram(semivariograma_exp_rk , vgm(0.33, c("Sph", "Exp"), 26000, 0.15))
plot(semivariograma_exp_rk , semivariograma_teorico_rk)

semivariograma_teorico_rk <-
  fit.variogram(semivariograma_exp_rk , vgm(c("Sph", "Exp")))
plot(semivariograma_exp_rk , semivariograma_teorico_rk)

# Kriging sobre residuos
grilla_covariables <- read.table("grilla_MO.txt")
head(grilla_covariables)
grilla_covariables <-st_as_sf(grilla_covariables, coords=c("x","y"), crs=22174)
grilla_covariables$x <-st_coordinates(grilla_covariables)[,1]
grilla_covariables$y <-st_coordinates(grilla_covariables)[,2]

kgres <-
  krige(residuos ~ 1, datos, grilla_covariables, model = semivariograma_teorico_rk, nmax = 25)
### grilla_covariables es un objeto sf  tiene todas las covariables de la grilla...  se puede hacer con imagen satelital
plot(kgres["var1.pred"], main = "Kriging Residual: Predicciones")
## es mejor convertir estos graficos a raster (mas abajo explicado)

# Prediccion final del regresion krigin (RK)
grilla_covariables$RK_pred <- predict(mlreg_final, newdata = grilla_covariables) + kgres$var1.pred
st_write(grilla_covariables[,"RK_pred"], "Prediccion_RK.gpkg",delete_layer =T)

# Visualización interectiva de la prediccion ####
grilla_covariables_rast <-st_rasterize(grilla_covariables,dx=500, dy=500)
grilla_covariables_rast

tmap_mode('view')

mapa_prediccion_RK <-
  tm_basemap(
    c(
      Satelite = "Esri.WorldImagery",
      Politico = "Esri.WorldGrayCanvas",
      Topo = "Esri.WorldTopoMap"
    )
  ) +
  tm_shape(grilla_covariables_rast) +
  tm_raster(
    col="RK_pred",
    title = "MO (%)",
    style = "quantile",
    palette = "YlOrBr",
    n = 10
  ) +
  tm_layout(legend.format = list(
    scientific = TRUE,
    format = "f",
    digits = 1
  ))
mapa_prediccion_RK

mapa_muestra <- tm_shape(datos) +
  tm_dots(
    "MO",
    title = "MO (%)",
    style = "quantile",
    palette = "YlOrBr",
    n = 10,
    alpha = 0.7,
    size = 0.1,
    popup.vars = T,
    popup.format = list(
      digits = 1,
      decimal.mark = ",",
      big.mark = "."
    )
  ) +
  tm_layout(legend.format = list(scientific = TRUE, format = "f"))

mapa_muestra

mapas_RK_muestra <- mapa_prediccion_RK + mapa_muestra
mapas_RK_muestra
  

########## agregando residuos o variabilidad


mapa_prediccion_KU <-
  tm_basemap(
    c(
      Satelite = "Esri.WorldImagery",
      Politico = "Esri.WorldGrayCanvas",
      Topo = "Esri.WorldTopoMap"
    )
  ) +
  tm_shape(interp_kg) +
  tm_raster(
    col="var1.pred",
    title = "MO (%)",
    style = "quantile",
    palette = "YlOrBr",
    n = 10
  ) +
  tm_layout(legend.format = list(
    scientific = TRUE,
    format = "f",
    digits = 1
  ))

mapa_prediccion_KU

mapas_RK_muestra2 <- mapa_prediccion_RK + mapa_muestra + mapa_prediccion_KU
mapas_RK_muestra2


