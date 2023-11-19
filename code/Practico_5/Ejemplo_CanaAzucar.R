## -------------------------------------------------------------------------------------------------------------------------------------------------
#| code-fold: true
#| code-summary: "Carga paquetes y funciones"
library(nlme)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)


resumir_modelo <- function(modelo) {
  
  rmse <- function(modelo) {
    sqrt(mean(modelo$residuals^2))
  }
  
  aic <- AIC(modelo)
  bic <- BIC(modelo)
  my_rmse <- rmse(modelo)
  regresora <- paste(attr(modelo$terms,"term.labels"), collapse = ", ")
  
  conCor <- ifelse(length(modelo$modelStruct), 'Si', 'No')
  data.frame('Indice' = regresora,
             'ConCorr' = conCor,
             'AIC' = aic,
             'BIC' = bic,
             'RMSE' = my_rmse, 
             check.names = FALSE)
}

diferenciaNormalizada <- function(x, y) {
  (x - y) / (x + y)
}

# tmap::tmap_options(basemap.server = c(
#   'Satelital' = leaflet::providers$Esri.WorldImagery,
#   'OSM' = leaflet::providers$OpenStreetMap))


## -------------------------------------------------------------------------------------------------------------------------------------------------
setwd("D:/Josefina/Cursos/curso_gulich/analisis_espacial/code/Practico_5")
datos <- read.table("data/No_quemadas.txt")



## -------------------------------------------------------------------------------------------------------------------------------------------------
datos <- datos |> 
  mutate(
    ndvi = diferenciaNormalizada(Banda4, Banda3),
    gndvi = diferenciaNormalizada(Banda4, Banda2),
    nci = diferenciaNormalizada(Banda5, Banda2)
  )



## -------------------------------------------------------------------------------------------------------------------------------------------------
datos_sf <-
  sf::st_as_sf(datos,
               coords = c('X_coord', 'Y_coord'),
               crs = 32720)

tmap_mode('view')



## -------------------------------------------------------------------------------------------------------------------------------------------------
tm_shape(datos_sf) +
  tm_dots(fill = 'TCH',
          fill.scale = tm_scale_continuous(values = "carto.ag_grn_yl"),
          size = 0.5)


## -------------------------------------------------------------------------------------------------------------------------------------------------
tm_shape(datos_sf) +
  tm_dots(fill = 'ndvi',
          fill.scale = tm_scale_continuous(values = "tableau.classic_green"),
          size = 0.5)


## -------------------------------------------------------------------------------------------------------------------------------------------------
#| fig-subcap: ["Histograma", "Gráfico de cajas"]
#| layout-ncol: 2

ggplot(datos, aes(TCH)) +
  geom_histogram(aes(y = after_stat(count / sum(count))),
                 bins = 15) +
  labs(y = 'Frecuencia Relativa')

ggplot(datos, aes(EDAD, TCH)) +
  geom_boxplot(width = 0.25)


## -------------------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-ndvi
#| fig-cap: "TCH en función de NDVI"
#| column: margin
#| echo: true
ggplot(datos, aes(ndvi, TCH)) +
  geom_point()



## -------------------------------------------------------------------------------------------------------------------------------------------------
# modelo lineal
#variable regresora es el nvdi
# yo quiero predecir TCH
#LM es un modelo lineal, el gls tambien ajusta lm pero con 
# REML que es maxima versolsimilitud
# en cambio el lm ajusta con minimos cuadrada
# es un Generalized least squares fit by REML/ modelo generalizado
modelo_ndvi <- gls(TCH ~ ndvi, 
                   data = datos, 
                   method = 'REML')
summary(modelo_ndvi)
# el de menor AIC y BIC es mejor poque nos dan info sobre
# la variabilidad. Significa un mejor modelo
#-116.3873 + 314.9360 X NDVI
# EL P-VALUE es <0.05 asi qye se rechaza la hipotesis nula
# la pendiente siempre es lo que interpretamos, en cambio la pendicente si lo vemos


## -------------------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-gndvi
#| fig-cap: "TCH en función de gNDVI"
#| column: margin
#| echo: true
ggplot(datos, aes(gndvi, TCH)) +
  geom_point()



## -------------------------------------------------------------------------------------------------------------------------------------------------
modelo_gndvi <- gls(TCH ~ gndvi, 
                    data = datos, 
                    method = 'REML')
summary(modelo_gndvi)
# el valor de error estandar residual me dice que ese modelo tiene un error de 14 tch en este caso?
# el 14% no se puede explicar.
# para saber si es mucho o poco, hay que referenciarlo con la media
# por ejmeplo  14.20 / 88.28 = 16% de error respecto a la media
# lo ideal es que este por debajo del 20%. Es decir que el error es moderado


## -------------------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-nci
#| fig-cap: "TCH en función de NCI"
#| column: margin
#| echo: true
ggplot(datos, aes(nci, TCH)) +
  geom_point()


## -------------------------------------------------------------------------------------------------------------------------------------------------
# el reml es la funcion de enlace REML
modelo_nci <-  gls(TCH ~ nci, 
                   data = datos, 
                   method = 'REML')
summary(modelo_nci)


## -------------------------------------------------------------------------------------------------------------------------------------------------
# comparamos los 3 modelos, el mejorcito es el GNDVI, el que
# tiene menores valores
# el no significa que no tiene correlacion espacial
rbind(
  resumir_modelo(modelo_ndvi),
  resumir_modelo(modelo_gndvi),
  resumir_modelo(modelo_nci)
  )



## -------------------------------------------------------------------------------------------------------------------------------------------------
#| label: modelo-corr-ndvi

modelo_ndvi_conCorr <- gls(
  TCH ~ ndvi,
  # Igual que antes pero le decimos que los residuos estan correlacionados
  # funcion exponencial, considerando x e y
  #ajustamos con correlacion espacial
  correlation = corExp(
    form =  ~ as.numeric(as.character(X_coord)) + 
      as.numeric(as.character(Y_coord)),
    metric = "euclidean",
    nugget = FALSE# que no calcule el  nugget, lo ideal es probar con/sin nugget
    #cual es el mejor. Cual es la variabilidad a escala menor
  ),
  data = datos,
  method = 'REML'
)
summary(modelo_ndvi_conCorr)
# Correlation Structure: Exponential spatial correlation
# 1229.264 m distancia a la cuak estan correlaionados los puntos
# los valores de los coeficientes cambiaron

## -------------------------------------------------------------------------------------------------------------------------------------------------
#| label: modelo-corr-gndvi
modelo_gndvi_conCorr <- gls(
  TCH ~ gndvi,
  correlation = corExp(
    form =  ~ as.numeric(as.character(X_coord)) + 
      as.numeric(as.character(Y_coord)),
    metric = "euclidean",
    nugget = FALSE
  ),
  data = datos,
  method = 'REML'
)
summary(modelo_gndvi_conCorr)


## -------------------------------------------------------------------------------------------------------------------------------------------------
#| label: modelo-corr-nci
modelo_nci_conCorr <- gls(
  TCH ~ nci,
  correlation = corExp(
    form =  ~ as.numeric(as.character(X_coord)) +
      as.numeric(as.character(Y_coord)),
    metric = "euclidean",
    nugget = FALSE
  ),
  data = datos,
  method = 'REML'
)
summary(modelo_nci_conCorr)



## -------------------------------------------------------------------------------------------------------------------------------------------------
# dismunye el error con correlacion espacial, el mejor ajuste es
# el gndvi que contempla una estructura espacial de los residuos es decil del error
rbind(
  resumir_modelo(modelo_ndvi),
  resumir_modelo(modelo_ndvi_conCorr),
  resumir_modelo(modelo_gndvi),
  resumir_modelo(modelo_gndvi_conCorr),
  resumir_modelo(modelo_nci),
  resumir_modelo(modelo_nci_conCorr)
  )
# criterio de seleccion para modelos espaciales AIC y BIC
# pero el RMSE NO es un buen indicador




## -------------------------------------------------------------------------------------------------------------------------------------------------
ef_fijos_ndvo_iid <- summary(modelo_ndvi)
ef_fijos_ndvo_iid$tTable


## -------------------------------------------------------------------------------------------------------------------------------------------------
ef_fijos_ndvo_corr <- summary(modelo_ndvi_conCorr)
ef_fijos_ndvo_corr$tTable


## ----obs-pred-lm----------------------------------------------------------------------------------------------------------------------------------
#| column: page
#| layout-ncol: 3
#| layout-nrow: 1

predichos_lm <- datos
predichos_lm$pred_ndvi_iid <- predict(modelo_ndvi)
predichos_lm$pred_ndvi_esp <- predict(modelo_ndvi_conCorr)


ggplot(predichos_lm, aes(pred_ndvi_iid, TCH)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(predichos_lm, aes(pred_ndvi_esp, TCH)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(predichos_lm, aes(pred_ndvi_esp, pred_ndvi_iid)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)



## -------------------------------------------------------------------------------------------------------------------------------------------------
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(c("graph", "Rgraphviz"), dep = TRUE)
# 
#  install.packages(
#    "INLA",
#    repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/testing"),
#    dep = TRUE
# )
# https://www.r-inla.org/download-install
library(INLA)
library(inlabru)

# inlabru facilita la libreria INLA

## -------------------------------------------------------------------------------------------------------------------------------------------------
inla.setOption(inla.mode = 'experimental')



## ----generacion-grilla-inla-----------------------------------------------------------------------------------------------------------------------

loc <- st_coordinates(datos_sf)
# Esta es la grilla de datos geoestadisticos
# se mueve en un continuo
# lattice seria por ejemplo calcula datos del rendimiento de cada provincia
# un poligono seria unlattice, y tiene vecinos
mesh <- INLA::inla.mesh.2d( # mesh es la grilla
  loc = loc,
  offset = c(1000, 4000),# de la linea azul
  cutoff = 800,# de la linea negra
  max.edge = c(1000, 2000),
  max.n = 10000)

#fijarse que son todos mini triangulares
# el largo corrresponde a max edge
# el dominio espacial es lo azul
# pero la mesh sugiere que sea mas grande que el area que ocupa nuestras
# observaciones. Por  eso hay dos grillas una interna y otra externa
# y que la preficcion no caiga en el ultimo valos
# eso es el offset, cuanto mas de distancia  a partir de la linea azul
# fijarse que los triangulos adentro de lo azul sonmas chicos quelos que estan  afuera en la linea negra

# como tomar esos parametros de max.edge.... son a ojo

# La prediccion se hace sobre todo y despues puedo recortar
# esto es mas que nada por el efecto de los bordes
ggplot(datos_sf) +
  gg(mesh) +
  geom_sf()

# le tenemos que especificar los parametros a priori
# antes en krigging haciamos un semivariograma. Pero aca no sabemos
# lo "inventamos" a ojo
# le asocio un rango y una varianza especifica. Hasta donde estan correlacionados los datos
spde <- INLA::inla.spde2.pcmatern(
  mesh = mesh,
  #como tengo 2 valores en el () significa que no es fijo el sigma del rango, si no que es variable
  prior.range = c(2000, 0.05),#rango
  prior.sigma = c(200, 0.01)#varianza, 0.01 distribucion/un rango de probabilidad
)

# al rango lo sacamos de las correlaciones espaciales anteriores 
# por ejemplo summary(modelo_nci_conCorr)



## ----eval=FALSE-----------------------------------------------------------------------------------------------------------------------------------
#| code-fold: true
#| code-summary: "Ajuste MJB usando INLA sin funciones de inlabru"
#|
## proj_obs <- inla.mesh.projector(mesh, loc = loc)
## proj_pred <- inla.mesh.projector(mesh, loc = mesh$loc)
## 
## A_obs <- inla.spde.make.A(mesh, loc = loc)
## A_pred <- inla.spde.make.A(mesh, loc = proj_pred$loc)
## idx <- 1:spde$n.spde
## 
## stack_obs <-
##   inla.stack(
##     data = list(y = datos_sf$TCH),
##     A = list(A_obs, 1),
##     effects = list(c(
##       list(Intercept = 1),
##       inla.spde.make.index("spatial", spde$n.spde)
##     ),
##     covar = datos_sf$ndvi),
##     tag = "obs"
##   )
## stack_pred <-
##   inla.stack(
##     data = list(y = NA),
##     A = list(A_pred),
##     effects = list(c(
##       list(Intercept = 1),
##       inla.spde.make.index("spatial", mesh$n)
##     )),
##     tag = "pred"
##   )
## stack <- inla.stack(stack_obs, stack_pred)
## 
## 
## formula <- y ~ -1 + Intercept + covar +
##     f(spatial, model = spde)
## 
## result1 <- inla(
##   formula,
##   data = inla.stack.data(stack_obs, spde = spde),
##   family = "gaussian",
##   control.predictor = list(A = inla.stack.A(stack_obs),
##                            compute = TRUE)
## )
## summary(result1)
## 
## 
## plot(datos_sf$TCH,
##      result1$summary.fitted.values[inla.stack.index(stack_obs, "obs")$data, "mean"],
##      main = "Observations vs posterior predicted values at the data locations")
## 
## #
## # field_pred <- inla.mesh.project(proj_pred,
## #                                 result1$summary.fitted.values[inla.stack.index(stack, "pred")$data, "mean"])
## # field_pred_sd <- inla.mesh.project(proj_pred,
## #                                    result1$summary.fitted.values[inla.stack.index(stack, "pred")$data, "sd"])
## #
## # image(inla.mesh.project(mesh,
## #                         field = field_pred,
## #                         dims = c(200, 200)),
## #       main = "Posterior field mean")
## # image(inla.mesh.project(mesh,
## #                         field = field_pred_sd,
## #                         dims = c(200, 200)),
## #       main = "Prediction standard deviation")


## ----ajuste-inlabru-------------------------------------------------------------------------------------------------------------------------------
#| column: page-right
#| layout-align: center
#|
ndvi_bru_spde <-
  bru( # usamos inlabru,
    # interce
    # site significa un efecto sitio que debente de las coordenadas y del modelo generado antes esto es los valores aleatorios
    # sin el site
    TCH ~ Intercept(1) + ndvi + site(main = coordinates, model = spde),
    family = "gaussian", # asumimos que el tch tiene una distribucion normal
    data = as_Spatial(datos_sf) # inla permite objetos espaciales por eso sf, pero los tenemos qe pasar a sp
  )

summary(ndvi_bru_spde)
# fiajartse que no tenemos p-values, estan los intervalos 
# de credibilidad que es el 0.025quant - 0.5quant 0.975quant
# con el 95% de credibilidad el valor de B1 esta entre 31.127 - 128.494 (es el valor eue dice ndv)
# el 5% son las dos colas
# fixed efeccts: b0 y b1

#random efect significa que usamos una correlacion espacial
# model hyperparameters es muy distinto el rango que ingersamos antes
# precision es 1/varianza
# el rango es hasta donde estan correlacionada los valores


## -------------------------------------------------------------------------------------------------------------------------------------------------
#| column: screen-inset-shaded
#| layout-nrow: 1
plot(ndvi_bru_spde, "Intercept")
plot(ndvi_bru_spde, "ndvi")


## -------------------------------------------------------------------------------------------------------------------------------------------------
#| fig-height: 12


spde.posterior(ndvi_bru_spde, "site", what = "matern.covariance") -> covplot
spde.posterior(ndvi_bru_spde, "site", what = "matern.correlation") -> corplot
spde.posterior(ndvi_bru_spde, "site", what = "range") -> rngplot
spde.posterior(ndvi_bru_spde, "site", what = "log.range") -> lgrngplot
spde.posterior(ndvi_bru_spde, "site", what = "variance") -> varplot
spde.posterior(ndvi_bru_spde, "site", what = "log.variance") -> lgvarplot

multiplot(plot(covplot), plot(corplot),
          plot(rngplot), plot(lgrngplot),
          plot(varplot), plot(lgvarplot))


## -------------------------------------------------------------------------------------------------------------------------------------------------
# hago la prediccion cpnsiderando varios parametros
pred_mesh <-
  predict(ndvi_bru_spde, as_Spatial(datos_sf), ~ Intercept + ndvi + site)
# como es u n objeto sp lo pasamos a st
predichos <- st_as_sf(pred_mesh)

ggplot(predichos, aes(mean, TCH)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)


## -------------------------------------------------------------------------------------------------------------------------------------------------
#| code-fold: true
#| code-summary: "Predicción espacial de TCH"

# No aplica para esta aplicación.
# pred_mesh <-
#   predict(ndvi_bru_spde, pixels(mesh), ~ Intercept + ndvi + site)
# 
# ggplot() +
#   gg(pred_mesh)
# ggplot(datos_sf) +
#   gg(pred_mesh) +
#   gg(mesh) +
#   geom_sf()




## -------------------------------------------------------------------------------------------------------------------------------------------------
predichos_lm$pred_ndvi_inla <- predichos$mean


ggplot(predichos_lm, aes(pred_ndvi_iid, TCH)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(predichos_lm, aes(pred_ndvi_esp, TCH)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot() +
  geom_point(data = predichos_lm, 
             aes(pred_ndvi_iid, TCH, color = 'REML iid')) +
  geom_point(data = predichos_lm, 
             aes(pred_ndvi_esp, TCH, color = 'REML Corr')) +
  geom_point(data = predichos_lm, 
             aes(pred_ndvi_inla, TCH, color = 'INLA Corr')) +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = 'TCH Predichos', y = 'TCH Observado', color = 'Estimación')


