# Carga de paquetes ####
library(sp)
library(spdep)
library(RColorBrewer)
library(mapview)
library(ggplot2)
library(spatstat)

# Carga de base de datos ####
datos_rinde <- st_read("soja_depurado.gpkg")
head(datos_rinde)
class(datos_rinde)

datos_MO <- read.table("MO_Córdoba.txt", header = T)
head(datos_MO)
class(datos_MO)

# Transformación a objeto espacial y visualización ####
datos_MO <- st_as_sf(datos_MO, coords = c("x", "y"), crs = 22174)
class(datos_MO)
mapview(datos_MO,
        zcol = "MO") + mapview(datos_rinde)

cols <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
mapview(datos_MO,
        zcol = "MO",
        alpha = 0,
        col.regions = cols)
##########################################################
# esto es para indicde de moran global
# Definición de vecindarios ####
## Datos MO ####
vecindarios_MO <- dnearneigh(datos_MO$geometry, 0, 25000)
summary(vecindarios_MO)
vecindarios_MO[20]

plot(
  vecindarios_MO,
  datos_MO$geometry,
  col = "#009999",
  pch = 20,
  cex = 1
)
# LOS VECINDARIOS
lw_MO <- nb2listw(vecindarios_MO, style = "W")
lw_MO

# Opción para contemplar observaciones sin vecindario
spdep::set.ZeroPolicyOption(TRUE)

lw_MO <- nb2listw(vecindarios_MO, style = "W")
lw_MO$weights[20]

## Datos MO ponderando por distancia ####
vecindarios_MO_p <- dnearneigh(datos_MO$geometry, 0, 25000)
summary(vecindarios_MO_p)

plot(
  vecindarios_MO_p,
  datos_MO$geometry,
  col = "#009999",
  pch = 20,
  cex = 0.5
)

vecindarios_MO_p[20]

dist <- spdep::nbdists(vecindarios_MO_p, datos_MO)
dist[20]
fdist <- lapply(dist, function(x)
  (1 / (x / 100)))

lw_MO_p <-
  try(spdep::nb2listw(vecindarios_MO_p, glist = fdist, style = "W"),
      silent = TRUE)
lw_MO_p$weights[20]

## Datos Rendimiento ####
vecindarios_rinde <- dnearneigh(datos_rinde$geom, 0, 10)
summary(vecindarios_rinde)

plot(
  vecindarios_rinde,
  datos_rinde$geom,
  col = "#009999",
  pch = 20,
  cex = 0.5
)

lw_rinde <-
  nb2listw(vecindarios_rinde, style = "W", zero.policy = T)

### Cálculo del índice de autocorrelación espacial de Moran
# hace 1000 permutaciones, hace 
# no tiene en cuenta el valor espacial
#toma aleatoriamente valores, 1000 valores
plot(imoran_MO)
imoran_MO <-
  moran.mc(datos_MO$MO, lw_MO_p, nsim = 1000, zero.policy = T)
imoran_MO
# es como que calculo 1000 indices de moran 
# vemos si es significativa
#p-value = 0.000999 ESTADISTICAMENTE SIGNIFICATIVO

imoran_rinde <-
  moran.mc(datos_rinde$REND,
           lw_rinde,
           nsim = 1000,
           zero.policy = T)
imoran_rinde



#################################################
### Cálculo del IM para multiples distancias
distancias <- function (dmax) {
  vec <- dnearneigh(datos_MO, 0, dmax)
  lw <- nb2listw(vec, style = "W")
  i.moran <- moran.mc(datos_MO$MO, lw, nsim = 999, zero.policy =
                        T)
  tabla <- data.frame("Distancia" = dmax, "MI" = i.moran$statistic)
  tabla
}

tablad <-
  do.call(rbind, lapply(c(
    seq(15000, 40000, 1000), seq(45000, 80000, 5000)
  ), distancias))
tablad

p <- ggplot(tablad, aes(x = Distancia, y = MI)) +
  geom_point(size = 3, color = "black") +
  geom_line(size = 1, color = "blue") +
  scale_x_continuous(limits = c(15000, 80000),
                     breaks = seq(15000, 80000, 5000)) +
  ylab("Moran's I") +
  xlab("Distancia (m)") +
  theme_light()
p

# Esto nos va a servir para elegir la distancia
# vemos que el indice de moran sube pero a 24 km
# tiende a bajar. el valor seleccionado de 25 esta bien
# cuanto mayor sea el indice es mejor, es decir mas cercano a 1
#significa que hay autocorrelacion espacial
#####################################################
#######################################
# Patrones de Puntos ####
data(swedishpines)
summary(swedishpines)# porque da este summary?
#  me dice cual es la intensidad, y la ventana y el area
plot(swedishpines)

data(bei)
summary(bei)
plot(bei)
# trae otro set de datos que se llama bei.extra
# que la podmeos usar como covariables
# elevacion y grad 

data(urkiola)
summary(urkiola)
plot(urkiola)
# es de clase ppp patrones de puntos
datos <- bei
pendiente <- bei.extra$grad
plot(pendiente)

## Localización de puntos en el espacio ####
plot(datos,
     pch = 20,
     cols = "grey70",
     main = "Localización de Árboles en un Bosque Tropical")
### hago una cuadrata de 5 filas y 5columnas
# ademas con el count le digo que me cuente cuantos arboles hay en cada
# pedacitos/grid
## Conteo por cuadrante ####
cuadrantes <- quadratcount(datos, nx = 5, ny = 5)
cuadrantes
# presencia de datos/arboles
plot(cuadrantes, add = TRUE)
# vemos que la distribucion en cada cuadrata tiene un cierta distribucion
# no tiene la misma intensidad en todas las grillas


## calculo Intesidad por cuadrante ####
# antes solo conte cuantos arboles habia por cuadrata
# ahora esta en cantidad por unidad de area
densidad.Q <- intensity(cuadrantes)# lo pongo pr unidad de area
densidad.Q # esta en metros

## Plot de intesidad por cuadrante ####
plot(intensity(cuadrantes, image = TRUE),
     main = NULL,
     las = 1)
plot(
  datos,
  pch = 20,
  cex = 0.6,
  col = rgb(0, 0, 0, .5),
  add = TRUE
)
# tengo que relacionar cada cuadrante en funcion del cuadrante total
# con chi cuadrado para saber si es significativo o no
# si esta por abajo o si esta por arriba

## Test de Aleatoriedad Espacial Completa ####
test_cuadrantes <- quadrat.test(datos, nx = 5, ny = 5)
test_cuadrantes
# el p-v alue es muy chicquito,
#quiere decir que el valor obs es distinto valor esperado (intensida total)
# no se distribuyen de forma homogenea en todas las cuadratas
plot(datos,
     pch = 20,
     cols = "grey70",
     main = NULL)
plot(test_cuadrantes, add = TRUE)
#hasta aca solo hice un analisis de la variables, exploratori

##############
# aca se hizo una densidad mucho mas chica, y se ve algo mas suavizado
## Plot de intensidad basada en kernel ####
den <- density(datos)

plot(den, main = "Kernel Gaussiano")
plot(datos, add = TRUE)

den2 <- density(datos, kernel = "disc")
plot(den2, main = "Kernel Circular")
plot(datos, add = TRUE)


#### K de ripley ###
k <- Kest(datos)
k
plot(k, main = NULL) 
#k de poisson, lo rojo es lo teorico 
# el negro son mis datos
# cuantas observaciones tengo hasta 20, hasta 40, por eso es creciente
# para saber si es similar o no, o si esta por arriba. 100 simulaciones para ver el rango de variacion
k_envolpes <- envelope (datos, fun = Kest, nsim = 100, verbose = F)
# EL RANGO es muy muy chiquito, hacer zoom
# estan agrupados por que la linea mia, mis datos estan por arriba
plot(k_envolpes,main=NULL)
# vemos que cuando son bajos los daots estan una sobre otra las lineas



## Modelado de la intesidad ####
# Hago un continuo
modelo1 <- ppm(datos ~ 1)
modelo1# lambda es la funcion de
# *** super significativa
# pineme las coordenadas en x e y
modelo2 <- ppm(datos ~ x + y)
modelo2

## Compración de modelos ####
anova(modelo1, modelo2, test = "Chi")
# el modelo 2 es un poquito mejor
## Intesidad Ajustada ####
fmap <- predict(modelo2)
plot(modelo2)

# nos olvidamos d ela x y y usamos la covariable de la pendiente
## Modelado de la intesidad usando covarariable ####
modelo3 <- ppm(datos ~ pendiente)
modelo3
# es poisson
plot(predict(modelo3))
anova(modelo1, modelo3, test = "Chi")
