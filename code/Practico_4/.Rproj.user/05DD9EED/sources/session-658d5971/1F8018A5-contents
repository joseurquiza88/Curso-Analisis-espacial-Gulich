# Carga de paquetes ####
library(sf)
library(spdep)# es importante para el moran local y global
library(ggplot2)
library(mapview)
library(e1071)#para calcular coef de asimetria
library(gridExtra)

#  Carga de base de datos ####
misdatos <- st_read("soja_t.gpkg")
head(misdatos)
class(misdatos)

# Media, mediana y coeficiente de asimetría previo a la eliminación de outliers ####
(mean(misdatos$REND))
(median(misdatos$REND))
(e1071::skewness(misdatos$REND))#coeficiente de asimetria
summary(misdatos)# la distribucion no es asimetrica
# el coeficiente de asimetria deberia ser cercano a 1
# para que sea la distribucion asimetrica
# si es muy alto el coef de simetria deberiamos hacer
# alguna transformacion de los datos

#  Histograma y gráfico box-plot previo a la eliminación de outliers ####
histograma <- ggplot(misdatos, aes(x =REND)) +
  #con esto hacemos la frecuencia relativa
  geom_histogram(aes(y = after_stat(count) / sum(count))) +
  ylab("Frecuencia Relativa") +
  theme_light()

boxplot <- ggplot(misdatos, aes(y = REND)) +
  geom_boxplot() +
  ylab("Rendimiento (t/ha)")  +
  scale_x_discrete(breaks = NULL) +
  xlab(NULL) +
  theme_light()
# los valores de 600, distorsionan todo, hay que eliminarlos
# a los outliers
grid.arrange(histograma, boxplot,
             ncol = 2)

# Remoción de datos por fuera de valores fijados previamante ####
misdatos_1 <-
  subset(misdatos, misdatos$REND < 8 & misdatos$REND > 0.05)

# Cálculo de LI y LS para eliminar datos ubicados por fuera de la media +- 2.5 DE ####
Media <- mean(misdatos_1$REND)
#Desviacion estandar
DE <- sd(misdatos_1$REND)
#limite inferior
(LI <- Media - 2.5 * DE)
# Limite superior
(LS <- Media + 2.5 * DE)

misdatos_2 <-
  subset(misdatos_1, misdatos_1$REND < LS &
           misdatos_1$REND > LI)

# Media, mediana y coeficiente de asimetría luego de la eliminación de outliers ####
(mean(misdatos_2$REND))
(median(misdatos_2$REND))
# fijarse que cambio, es mas asimetrica la variable ahora
(e1071::skewness(misdatos_2$REND))

# Histograma y gráfico box-plot luego de la eliminación de outliers ####
histograma_2 <- ggplot(misdatos_2, aes(x = REND)) +
  geom_histogram(aes(y = after_stat(count) / sum(count))) +
  ylab("Frecuencia Relativa") +
  theme_light()

boxplot_2 <- ggplot(misdatos_2, aes(y = REND)) +
  geom_boxplot() +
  ylab("Rendimiento (t/ha)")  +
  scale_x_discrete(breaks = NULL) +
  xlab(NULL) +
  theme_light()

grid.arrange(histograma,
             boxplot,
             histograma_2,
             boxplot_2,
             ncol = 2,
             nrow = 2)
##########################################33
#Empieza el indice de moran, el pre-procesamiento
# Definición de vecindarios para cada punto muestreado ####

# puede pasar que algunos puntos no tengan vecindarios
# entonces permite que algunos no tenga vecinos
# sino ponemos esto, y pasa que hay no tenga vecinos puede haber errores 
# 
spdep::set.ZeroPolicyOption(TRUE)
#cuales son los vecinos, pongo una distancia minima 0 
#a distancia maxima de 20metros
# todo lo que esta en ese radio. Esto es para muestreo
#regular
vecindarios <- dnearneigh(misdatos_2$geom, 0, 20)
vecindarios <- dnearneigh(misdatos_2$geom, 0, 10) #de 0 a 10m

# hay otras librerias: kneig, que defino cuantos vecinos
# y no la distancia. Aunque el mas cercano este a 50km, busca por ejemplo
# los2 mas cercanos
# El paquete es snpdep ahi hay otras forma
summary(vecindarios)# de tipo  "nb"
#Average number of links: 50.1814 # numero de vecins unidos
# despues aparece una matriz y dice que hay: hay 1 obs con 19 vecinos
# 1 obs con 24 vecinosn


lw <- nb2listw(vecindarios, style = "W")
lw$weights[1]# estos son los pesos que les damos
#distancia entre cada dato y sus vecinos
dist <- nbdists(vecindarios, misdatos_2$geom)
dist[10]
# como me da siempre el mismo peso /n
# lo vamos a ponderar segun la distancia
# entonces cada fila me va a dar =1
fdist <- lapply(dist, function(x)
  (1 / (x / 100)))
lw2 <- nb2listw(vecindarios, glist = fdist, style = "W")
lw2$weights[10]
sum(lw2$weights[10][[1]])# esto siempre me tiene que dar 1
 
#usar el lw o el lw2 de pende de mi que veo
#####################################################
#plot(gri,misdatos_1,col = "red", pch = 20, cex = 1)

#  Generación del Moran Plot (MP) ####
MP <-
  moran.plot(
    misdatos_2$REND,
    lw2,# esto es una lista de los pesos, tambien podemos poner el lw 
    col = 3,
    quiet = T,
    labels = F,
    zero.policy = T, # esto tambien lo pusimos antes
    xlab = "Rendimiento",
    ylab = "Rendimiento Spatially Lagged"
  )
#influyentes son losnegreos, y los no influyentes son verdes
# Lo que esta en negro son los valore smayor influyentes
# la columana que dice true significa que son influyentes en sus 
# en su vecindarios
# las columnas siguientes son indices estadisticos
# para ver si son influyentes o no

head(MP)
Influ <- MP$is_inf
Influ # solo tiene T o F
###################################################
##################################################
#  Cálculo del índice de Moran local (IML) ####
ML <-
  localmoran(misdatos_2$REND, lw2, alternative = "less")
head(ML)

IMl <-
  printCoefmat(data.frame(ML, row.names = misdatos_2$Casos), check.names =
                 FALSE)
head(IMl)
#####################################################3
# el influ usamos que eran T o F
#  Eliminación de inliers utilizando IML y MP  ####
# Aca solo unimos los dos DF
misdatos_3 <- cbind(misdatos_2, Influ, IMl)
head(misdatos_3)
#st_write(misdatos_3,"Datosadepurar.gpkg")
# Aca si eliminamos datos, me quedo aquellos que tengan
#indice de moran local + y no sean estadisticamente significatco
misdatos_3 <-
  subset(misdatos_3, misdatos_3[["Ii"]] > 0 |
           misdatos_3[["Pr.z...E.Ii.."]] > 0.05)

#Me quedo con los que son influyentes
# es decir me quedo con los datos que son similares
# en los vecindarios
misdatos_3 <- misdatos_3[misdatos_3$Influ == FALSE, ]

# Media, mediana y coeficiente de asimetría luego de eliminar outliers e inliers ####
(mean(misdatos_3$REND))
(median(misdatos_3$REND))
(e1071::skewness(misdatos_3$REND))

#  Histograma y gráfico box-plot luego de eliminar outliers e inliers ####
histograma_3 <- ggplot(misdatos_3, aes(x = REND)) +
  geom_histogram(aes(y = after_stat(count) / sum(count))) +
  ylab("Frecuencia Relativa") +
  theme_light()

boxplot_3 <- ggplot(misdatos_3, aes(y = REND)) +
  geom_boxplot() +
  ylab("Rendimiento (t/ha)")  +
  scale_x_discrete(breaks = NULL) +
  xlab(NULL) +
  theme_light()

grid.arrange(histograma_3, boxplot_3,
             ncol = 2)

st_write(misdatos_3[, c("REND", "geom")], "soja_depurado.gpkg", delete_layer = T)
#################33333
#  Gráfico de dispersión de la variable REND con coordenadas x e y  para la detección de tendencias ####
# Le agrego una columna con los datos de la coordenada
# st_coordinate devuelve un df con la geometria
base_depurada <- cbind(misdatos_3, st_coordinates(misdatos_3))
head(base_depurada)
# grafico de rendimiento vs longitud
px <- ggplot(base_depurada, aes(X, REND)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)
# grafico de rendimiento vs latitud
py <- ggplot(base_depurada, aes(Y, REND)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)
px
# dev.off()
grid.arrange(px, py, ncol = 2)
# vemos una leve tendencia en y
#segun el profe no seria significativa
# poruq el r2 es 0.05
# el pvalue nos dice que si pero el r2 no
regresion <- 
  lm(formula = REND ~ 1 + X + Y ,
     data = base_depurada,
     na.action = na.omit)
summary(regresion)

#Visualización espacial de datos depurados  ####
# plot sf
plot(base_depurada)

plot(
  base_depurada[, "REND"],
  key.pos = 4,
  axes = TRUE,
  key.width = lcm(1.5),
  key.length = 1,
  pal = terrain.colors
)

plot(
  base_depurada[, "REND"],
  key.pos = 4,
  axes = TRUE,
  key.width = lcm(1.5),
  key.length = 1,
  pal = heat.colors
) # topo.colors cm.colors

plot(
  base_depurada[, "REND"],
  key.pos = 4,
  axes = TRUE,
  key.width = lcm(1.5),
  
  key.length = 1,
  breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)
)

# plot ggplot2
base_depurada %>% ggplot() +
  geom_sf(aes(color = REND)) +
  scale_color_viridis_c(direction = -1)

base_depurada %>% ggplot() +
  geom_sf(aes(color = REND)) +
  scale_color_gradient(low = "#fffb00", high = "#ff0000")

# plot mapview
mapview(base_depurada[, "REND"])
mapview(base_depurada[, "REND"],
        alpha = 0,
        col.regions = topo.colors)
