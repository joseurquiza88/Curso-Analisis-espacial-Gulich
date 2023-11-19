library(gstat)
library(spdep)
library(plotly)
library(sf)
library(mapview)


# Carga de base de datos ####
datos <- st_read("soja_depurado.gpkg")
datos <-cbind(datos,st_coordinates(datos))

px <- ggplot(datos, aes(X, REND)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

py <- ggplot(datos, aes(Y, REND)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

gridExtra::grid.arrange(px, py, ncol = 2)

# Ajuste de semivariograma experimetal, sin tendencia ####
#con el ~1 le decimos que no hay tendecia
#que el proceso es estacionario
#si pensamos que hay tendencia con x ponemos ~x o tendencia con y ~y
semi_exp <- variogram(REND ~ 1, datos)
plot(semi_exp)
semi_exp

# Ajuste de semivariograma teorico, modelo exponencial ####
mod_exp <- fit.variogram(semi_exp, vgm(0.20, "Exp", 110, 0.025))
mod_exp
plot(semi_exp , mod_exp)

# Ajuste de semivariograma teorico, modelo esf?rico ####
#vgm (parcial sill, tipo de modelo teorico, rango y nugger)
mod_esf = fit.variogram(semi_exp, vgm(0.20, "Sph", 110, 0.025))
mod_esf
plot(semi_exp , mod_esf)

# Visualizacion conjunta ####
vgLine <-
  rbind(cbind(variogramLine(mod_exp, maxdist = max(semi_exp$dist)), id =
                "Exponencial"),
        cbind(variogramLine(mod_esf, maxdist = max(semi_exp$dist)), id = "Esferico"))

grafico <-  ggplot(semi_exp, aes(x = dist, y = gamma, color = id)) +
  geom_line(data = vgLine) +
  geom_point() +
  labs(title = "Semivariograma experimental y teorico ajustado")  +
  xlab("Distancia") +
  ylab("Semivarianza")  +
  scale_color_discrete(name = "Modelo",
                       labels = c("Esferico", "Exponencial", "Experimental"))

grafico


# Suma de cuadrado del error de modelos ajustados ####
# VALOR MAS BAJO, LA BONDAD DE AJUSTE ES MEJOR
attr(mod_exp, 'SSErr')
attr(mod_esf, 'SSErr')# El esferico es mejor

vgm()
show.vgms()

# Practica datos MO ####
datos_MO <- read.table("MO_Córdoba.txt", header = T)
head(datos_MO)
# lo transformamos a objeto espacial
#crs es posgar
datos_MO <- st_as_sf(datos_MO, coords = c("x", "y"),crs=22174)
mapview(datos_MO)
datos_MO

# Incorporación de coordenadas x e y dentro del data.frame 
datos_MO <-cbind(st_coordinates(datos_MO),datos_MO)
head(datos_MO)

### Evaluacion de tendencia con coordenadas 
# no deberia haber una tendencia espacial, deberia ser estacional
# vems que hay una leve tendencia principalmente en el y
# 
px_mo <- ggplot(datos_MO, aes(X, MO)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

py_mo <- ggplot(datos_MO, aes(Y, MO)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

gridExtra::grid.arrange(px_mo, py_mo, ncol = 2)

#Pero  vemos que el r2 es bajo (es un poquito mas que el ejemplo anterior)
# pero es baja solo se explica el 21% de la variabilidad
summary(lm(MO ~ X + Y,datos_MO))

# Ajuste de semivariograma experimetal, sin contemplar tendencia ####
semi_exp_MO<- variogram(MO ~ 1, datos_MO)# asumimos que no hay tendencias espaciales con ~1
plot(semi_exp_MO)
semi_exp_MO

# Ajuste de semivariograma teorico ####
#En comparacion al ejemplo anterior, no le ponemos
#valores iniciales como el sill, nuget y rango
# solo le decims a que modelo debe ajustar y calcula
# suma de cuadrados del error
modelo_teorico_MO<-
  fit.variogram(semi_exp_MO, vgm(c("Exp","Sph")))
#nos muestra el mejor
modelo_teorico_MO

plot(semi_exp_MO, modelo_teorico_MO)# aca muestra el mejor que es el exponencial

# Ajuste de semivariograma experimetal, contemplando tendencia ####
# vamos a considerar la tendencia en y con el ~
semi_exp_MO_t<- variogram(MO ~ Y, datos_MO)
plot(semi_exp_MO_t)
semi_exp_MO_t
#si comparamos los dos plot el que tiene tendencia en y y el que tiene ~1
# lo ideal es el que le decimos que tiene tendencia porque tiene un conrte, 
#llega a un maximo y se estabiliza
# crece mas rapido sin tendencia, y no llega a estabilizarse



# Ajuste de semivariograma teorico ####
modelo_teorico_MO_t <-
  fit.variogram(semi_exp_MO_t, vgm(c("Exp","Sph")))
modelo_teorico_MO_t

plot(semi_exp_MO_t, modelo_teorico_MO_t)
#########################################################
#####################################################
# Carga de base de datos PP marzo15, asignacion de sitstema de coordenadas ####
datos_PP <- read.table("ppMarzo.txt", header = TRUE)
head(datos_PP)
datos_PP <- st_as_sf(datos_PP, coords = c("x", "y"),crs=22174)
mapview(datos_PP)

# Incorporación de coordenadas x e y dentro del data.frame 
datos_PP <-cbind(st_coordinates(datos_PP),datos_PP)
head(datos_PP)

### Evaluacion de tendencia con coordenadas 
px_pp <- ggplot(datos_PP, aes(X, PP)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

py_pp <- ggplot(datos_PP, aes(Y, PP)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)
#Hay mas tendencia en x, que en y pero en los dos
gridExtra::grid.arrange(px_pp, py_pp, ncol = 2)

#Vemos que el r2 es del 43% es alta para estos casos
# es mas marcada
summary(lm(PP ~ X + Y,datos_PP))

# Ajuste de semivariograma experimetal, sin tendencia ####
semi_exp_PP<- variogram(PP ~ 1, datos_PP)
# vemos que se estabiliza pero despues aumenta nuevamente
# esto es porque hay pocos datos, los pares de datos son reducidas
# pero vemos que hayuna tendencia lineal, que sube y sube no se estabiliza
plot(semi_exp_PP)

# Ajuste de semivariograma teorico, modelo exponencial ####
mod_exp_PP<-
  fit.variogram(semi_exp_PP, vgm(c("Exp")))
mod_exp_PP
plot(semi_exp_PP, mod_exp_PP)

# Ajuste de semivariograma teorico, modelo lineal ####
# no es recomndable usar este tipo de modelo linal
mod_Lin_PP <- fit.variogram(semi_exp_PP, vgm( "Lin"))
mod_Lin_PP
plot(semi_exp_PP, mod_Lin_PP)

# Suma de cuadrado del error de modelos ajustados ####
attr(mod_exp_PP, 'SSErr')
attr(mod_Lin_PP, 'SSErr')
# son muy parecidos los valores del ajuste
#######---

# Ajuste de semivariograma teóricos, 3 modelos sin parametros iniciales
# Aca solo le ponemos 3 modelos de forma simultanea para quw eme diga cual es el mejor
modelos_PP <- fit.variogram(semi_exp_PP, vgm(c("Sph", "Exp", "Lin")))
modelos_PP

plot(semi_exp_PP, modelos_PP)

# Suma de cuadrado del error de modelo ajustado sin tendencia
# el lineal es el mejor
attr(modelos_PP, 'SSErr')
## ---
# Ajuste de semivariograma experimetal, con tendencia
semi_exp_PP_tend <- variogram(PP ~ X, datos_PP)
# Esto es con las dos, pero vemos que no hay diferencias con el que solo ponemos la X
semi_exp_PP_tendY <- variogram(PP ~ X + Y, datos_PP)

# vemos que haydiferencias con lo anterior, vemos que hay un tope a diferencia del anterior 
# que sube y sue
plot(semi_exp_PP_tend)

# Ajuste de semivariograma te?ricos, 2 modelos sin parametros iniciales
# Objeto con tendencias en x
modelos_PP_tend <- fit.variogram(semi_exp_PP_tend, vgm(c("Exp", "Sph")))
modelos_PP_tend# el mejor es el esferico
plot(semi_exp_PP_tend , modelos_PP_tend)

# Suma de cuadrado del error de modelo ajustados con tendencia
attr(modelos_PP_tend, 'SSErr')


