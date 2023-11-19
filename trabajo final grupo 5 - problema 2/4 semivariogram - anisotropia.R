library(gstat)
library(spdep)
library(plotly)
library(sf)
library(mapview)



# Practica datos MO ####
datos_MO <- read.table("MO_Córdoba.txt", header = T)
head(datos_MO)

datos_MO <- st_as_sf(datos_MO, coords = c("x", "y"),crs=22174)
mapview(datos_MO)
datos_MO

### x es longitud --- y es latitud !!!

# Incorporación de coordenadas x e y dentro del data.frame 
datos_MO <-cbind(st_coordinates(datos_MO),datos_MO)
head(datos_MO)

### Evaluacion de tendencia con coordenadas 
px_mo <- ggplot(datos_MO, aes(X, MO)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

py_mo <- ggplot(datos_MO, aes(Y, MO)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

gridExtra::grid.arrange(px_mo, py_mo, ncol = 2)

summary(lm(MO ~ X + Y,datos_MO))


# Ajuste de semivariograma experimetal, sin contemplar tendencia ####
semi_exp_MO<- variogram(MO ~ 1, datos_MO)
plot(semi_exp_MO)
semi_exp_MO

# Ajuste de semivariograma teorico 
#### no doy los valores iniciales de nugget sill y rango ni en exp ni en spherical 
#### como los indico, r ajusta con valores inciales razonables
#### selecciona el que tenga el menor SC error

modelo_teorico_MO<-
  fit.variogram(semi_exp_MO, vgm(c("Exp","Sph")))
modelo_teorico_MO ### la relacion nugg/(psill+nugg) = ayuda a definir el modelo (exp o sph)

p1 <- plot(semi_exp_MO, modelo_teorico_MO)

# Ajuste de semivariograma experimetal, contemplando tendencia 
#### tiene mejor forma (crece y luego estabiliza)
semi_exp_MO_t<- variogram(MO ~ Y, datos_MO, alpha = c(0,45,90,135), 
                          anis = c(45 , 150/200))   # tita=45 es el xxx con mayor rango y lambda = 150/200 
p2 <- plot(semi_exp_MO_t)
semi_exp_MO_t

gridExtra::grid.arrange(p1, p2, ncol = 2)
# puedo corregir anisotropia si habria cambios a nivel del rango

# Ajuste de semivariograma teorico ####
modelo_teorico_MO_t <-
  fit.variogram(semi_exp_MO_t, vgm(c("Exp","Sph")))
modelo_teorico_MO_t

p3 <- plot(semi_exp_MO_t, modelo_teorico_MO_t)

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)



