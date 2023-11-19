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
library(gridExtra)

# Carga de base de datos ####
datos <- read.table("MO_Córdoba.txt", header = T)

# Graficos Exploratorios ####
px <- ggplot(datos, aes(x, MO)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

py <- ggplot(datos, aes(y, MO)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)
# un poquit mas de tendencia en y que en x
grid.arrange(px, py, ncol = 2)
# asimetria leve hacia la derecha
# pero ya esta limpia la base
ggplot(datos, aes(x = MO)) +
  geom_histogram(aes(y = after_stat(count) / sum(count))) +
  ylab("Frecuencia Relativa") +
  theme_light()
# cuando es + tendencia a la derecha
# cuando es - tendencia hacia la izquierda
# entre +- 1 / +- 2 es lo recomentadble
# esto es importante para el calculo de la semivarianza
# por eso no deberia ser muuy asimentrico de la variable respuesta
skewness(datos$MO)

#Transformacion a sf ####
datos <-st_as_sf(datos, coords=c("x", "y"), crs=22174)
class(datos)
mapview(datos)

# Ajuste de semivariograma experimetal, sin tendencia ####
#sin tendencia ~1
semiv_exp <- variogram(MO ~ 1, datos)
plot(semiv_exp)

# Ajuste de semivariograma experimetal, con tendencia ####
datos$x <-st_coordinates(datos)[,1]
datos$y <-st_coordinates(datos)[,2]
# le ponemos la tendencia en x e y
# podriamps haber puesto solo y porque se veia una mayor tendencia
semiv_exp <- variogram(MO ~ x + y, datos)
plot(semiv_exp)
# pareceria que es mejor el semiv con tendencia porque se estabiliza en un punto

# Ajuste de semivariograma teórico ####
semiv_teorico <-
  fit.variogram(semiv_exp, vgm(c("Exp", "Sph", "Gau")))
# essta funcion perite el ingreso de valores iniciales que supuestamente vi en
# el experimental
# me tira el mejor modelo que se ajusta segun suma del cuadrado del error
plot(semiv_exp, semiv_teorico)
# el mejor es el exp

attr(semiv_teorico, 'SSErr')

# Generacion de grilla de prediccion ####
limites <-st_read("LimitesCBA/Cordoba_limite.shp")
# ver las coordenadas, el archivo anterior estaba en posgar
# asi que si o si lo tengo que transformar
limites <-st_transform(limites, crs=22174)
#otra forma para poner el crs
limites2 <-st_transform(limites, crs=st_crs(datos))
plot(limites)

grilla <- st_bbox(limites) %>%
  st_as_stars(dx = 2500) %>%
  st_crop(limites)

plot(grilla)

# Kriging Universal ####
# quiero predecir MO
#nmax= 25 significa con 25 vecinos
interp_kg <-
  krige(MO ~ x + y, datos, grilla, model = semiv_teorico, nmax = 25)

plot(interp_kg["var1.pred"],
     main = "Kriging Universal: Predicciones", col = terrain.colors(10))
# La raiz cuadrada de esto seria la desviacion estandar
interp_kg$DE_pred <-sqrt(interp_kg$var1.var)

plot(sqrt(interp_kg["var1.var"]),
     main = "Kriging Universal: DE de Prediccion",col = terrain.colors(15))
# vems que donde habia puntos la sd es mas bajo
# la parte mas alta es donde hay mas incert porque no habia datos muesstreados

#  Exportar predichos y varianza de prediccion ####
write_stars(interp_kg,
            layer = "var1.pred",
            "pred_MO.tiff",
            delete_layer = T)

write_stars(interp_kg,
            layer = "DE_pred",
            "DE_MO.tiff",
            delete_layer = T)

#############################################
# Kriging Regresión ####
names(datos)
# ahora hacemos otra forma, esta vez le agrgams covariables
# Seleccion de covariables
# si ponemos . significa que van tooodas las variables
#predecimos la MO en funcion de la ndvi, twi....
# pero sin considerar la columna geometria
mlreg <- lm(MO ~ ., data = st_drop_geometry(datos))

#sino lo pnemos asi
mlreg2 <- lm(MO ~ NDVI +TWI+Tmed, data = st_drop_geometry(datos))
summary(mlreg)
# ahora vamos a usar metodo para reducir las variables porque son muchas
# aca ya calcula y me deja cuales son las variables mas relevantes
# nos quedammos con la combinacion de variables que mejor ajusta
# Criterio de información de Akaike
mlreg_final <- stepAIC(mlreg, direction = "both")
mlreg_final
summary(mlreg_final)

# Incorporamos los residuos del MLR a la base de datos
datos$residuos <- mlreg_final$residuals #parte no explicada por la co-variables
names(datos)

# Ajuste de semivariograma experimetal y teorico a los residuos
# vems la estructura espacial de los residuos
semivariograma_exp_rk <- variogram(residuos ~ 1 , datos)
plot(semivariograma_exp_rk)
semivariograma_exp_rk

semivariograma_teorico_rk <-
  fit.variogram(semivariograma_exp_rk , vgm(0.33, c("Sph", "Exp"), 26000, 0.15))
plot(semivariograma_exp_rk , semivariograma_teorico_rk)

#lo mismo sin valores inicialesx
# los resisudos tienen una relacion, una estructura espacial corta
# a diferencia de los valores en si
# porque es a menos de 5000 metrs
# porque tiene un rango bajo. La estructuera espacial no esta marcada, no hay tanta continuidad espacial
# porque?¡ porque tenemos en cuenta otras/varias variables que estan explicando la estructura espacial
# y lo remeante que serian lso residuos, el error es bajo
semivariograma_teorico_rk <-
  fit.variogram(semivariograma_exp_rk , vgm(c("Sph", "Exp")))
plot(semivariograma_exp_rk , semivariograma_teorico_rk)

##############################################

# Kriging sobre residuos
grilla_covariables <- read.table("grilla_MO.txt")
head(grilla_covariables)
grilla_covariables <-st_as_sf(grilla_covariables, coords=c("x","y"), crs=22174)
grilla_covariables$x <-st_coordinates(grilla_covariables)[,1]
grilla_covariables$y <-st_coordinates(grilla_covariables)[,2]

kgres <-
  krige(residuos ~ 1, datos, grilla_covariables, model = semivariograma_teorico_rk, nmax = 25)
# plot(kgres["var1.pred"], main = "Kriging Residual: Predicciones")

# Prediccion final!!!!
# le sumo el krigging de los residuos ==> kgres$var1.pred
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

