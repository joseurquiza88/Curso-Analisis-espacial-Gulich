#con alt - ==> <- 
## ----------------------------------------------
2 + 2
#Numeros aleatorios con una distribucion normal
# con sd = 1 media 0
normalAleatorio <- rnorm(10, mean = 0, sd = 1)
normalAleatorio
# si lo pongo entre () sale como un print direct
(normalAleatorio <- rnorm(10, mean = 0, sd = 1))


## ----------------------------------------------
muestreo <- read.table("datos/MuestreoSuelo.txt", header = T, sep = "\t")


## ----------------------------------------------
muestreo


## ----------------------------------------------
ggplot()


## ----------------------------------------------
ggplot(muestreo)


## ----------------------------------------------
ggplot(muestreo, aes(Limo))


## ----------------------------------------------
ggplot(muestreo, aes(Limo)) +
  geom_histogram()


## ----------------------------------------------
library(dplyr)
# pinza de datos
# |> es un pipe es lo mismo que %>%
# el pipe de dplyr es ctrl + shift + m
# agrego una nueva columna que ponga mediaLimo
# mutate viene con la libreria dply
# Igualmente esto lo estoymostrando en consola



mutate(muestreo,mediaLimo = mean(Limo, na.rm = TRUE))
# si quiero que sea parte de muestreo lo tengo que 
# asignar a una nueva variable
muestreo_2 <- muestreo |>
  mutate(mediaLimo = mean(Limo, na.rm = TRUE))

#si quiero puedo agregar otro pipe y pongo un ggplor
## ----------------------------------------------
library(dplyr)

muestreo |>
  mutate(mediaLimo = mean(Limo, na.rm = TRUE)) |> 
  # solo filtro aquellos datos que sean mayor a la media
  # slecciono filas
  filter(Limo > mediaLimo)



## ----------------------------------------------
library(dplyr)

muestreo |>
  mutate(mediaLimo = mean(Limo, na.rm = TRUE)) |> 
  filter(Limo > mediaLimo) |> 
  # con esto eliminamos la columna generada de media
  # esto nos dice: selecciona todo - menos la media
  select(-mediaLimo)



## ----------------------------------------------
library(dplyr)
#lo guardamos en un nuevo objeto
base_subset <- 
  muestreo |>
  mutate(mediaLimo = mean(Limo, na.rm = TRUE)) |> 
  filter(Limo > mediaLimo) |> 
  select(-mediaLimo)
# Lo mismo pero elimino varias columnas
base_subset2 <- 
  muestreo |>
  mutate(mediaLimo = mean(Limo, na.rm = TRUE)) |> 
  filter(Limo > mediaLimo) |> 
  #select(-mediaLimo,-K)
  # Otra forma
  select(-c(mediaLimo,CC))


## ----------------------------------------------
# paquete sf
# st_as_sf , lo ponemos como un objeto sf ==> as sf
# todas las funciones de sf comienzan con st_
#https://www.spatialreference.org/
# x=lon , y=lat
#
print(muestreo <- st_as_sf(muestreo, 
                           coords = c("Xt", "Yt"),# nombre de columnas 
                           crs = 32720), #sistema de coord UTM
      n = 5)# esto es para que me haga print de solo 5 datos



## ----------------------------------------------
summary(muestreo)


## ----fig.height = 8.5, fig.width = 13----------
plot(muestreo, pch = 18 , cex = 3)


## ----------------------------------------------
ggplot(muestreo) +
  geom_sf()


## ----------------------------------------------
ggplot(muestreo) +
  geom_sf(aes(fill = Limo), shape = 22, size = 3)



## ----------------------------------------------
print(departamentos <- read_sf("datos/deptos_cba", stringsAsFactors = TRUE), n = 3)


## ----------------------------------------------
#| class-output: highlight
#| output-line-numbers: "1,2,6,9"
#| # esta en otra proyeccion WGS 4
# Si vemos en un caso llamamos a la carpta y en otro caso leo el shape en si
print(departamentos2 <- read_sf("datos/deptos_cba/deptos_cba.shp", stringsAsFactors = TRUE), n = 3)
print(departamentos <- read_sf("datos/deptos_cba", stringsAsFactors = TRUE), n = 3)


## ----------------------------------------------
summary(departamentos)


## ----------------------------------------------
plot(departamentos)


## ----------------------------------------------
#formato geopackage
print(cuencas <- read_sf("datos/cuencas_cba/cuencas_cba.gpkg", stringsAsFactors = TRUE), n = 2)



## ----------------------------------------------
summary(cuencas, maxsum = 3)


## ----fig.height=7.5----------------------------
plot(cuencas)


## ----------------------------------------------
# cual es el sistema de referencia
# wkt wll known text
st_crs(departamentos)


## ----------------------------------------------
# este no esta tan claro que que crs tiene,
#fijate  que tiene varios codigos ESPG
st_crs(cuencas)


## ----------------------------------------------
# Tienne la misma crs
st_crs(departamentos) == st_crs(cuencas)


## ----highlight.output=c(3)---------------------
# re-proyectamos
#  objeto que queremos tranfotmar, proyeccion que queremos
cuencas <- st_transform(cuencas, st_crs(departamentos))
st_crs(cuencas)


## ----------------------------------------------
st_crs(departamentos) == st_crs(cuencas)


## ----highlight.output=c(5, 6)------------------
print(cuencas, n = 4)


## ----------------------------------------------
#ploteamos las dos cosas porque tienen la misma src
ggplot(muestreo) +
  geom_sf(aes(fill = Limo), shape = 22, size = 3) +
  geom_sf(data = departamentos)



## ----------------------------------------------
ggplot(muestreo) +
  geom_sf(data = departamentos) +
  geom_sf(aes(fill = Limo), shape = 22, size = 3) 
 


## ----------------------------------------------
ggplot() +
  geom_sf(data = cuencas)


## ----------------------------------------------
ggplot() +
  geom_sf(data = cuencas) +
  geom_sf(data = muestreo)


## ----------------------------------------------
ggplot() +
  geom_sf(data = cuencas) +
  geom_sf(data = muestreo, aes(color = Limo), size = 3)


## ----------------------------------------------
ggplot() +
  geom_sf(data = cuencas) +
  geom_sf(data = muestreo, aes(color = Limo), size = 3) +
  scale_color_continuous(type = "viridis")


## ----------------------------------------------

ggplot() +
  geom_sf(data = cuencas) +
  geom_sf(data = muestreo, aes(color = Limo), size = 1) +
  scale_color_continuous(type = "viridis", na.value = "pink")


#NaN not a number, por ejemplo 0/0 no se puede definir matematicamente
# NA no siempre es un NaN sino que significa que no hay un valor 
## ----error=TRUE--------------------------------
muestreoLatLong <- st_transform(muestreo, st_crs(departamentos))
# si cuencas esta sobre muestreo long, es decir cubre
# y me dije que ´puntos de muestreo esta sobre cuenca
st_covers(cuencas, muestreoLatLong)


## ----results='markup'--------------------------
cuencasUTM <- st_transform(cuencas, st_crs(muestreo))
# cuantos puntos caen dentro de las cuencas
lengths(st_covers(cuencasUTM, muestreo))



## ----------------------------------------------
st_area(cuencas)#Area
# densidad de muestreo
lengths(st_covers(cuencasUTM, muestreo))/st_area(cuencasUTM)



## ----------------------------------------------
#Units: [1/m^2]
puntosKm <- lengths(st_covers(cuencasUTM, muestreo))/units::set_units(st_area(cuencasUTM), km^2)
cuencasUTM$CantidadMuestrasKm <- as.numeric(puntosKm)
cuencasUTM$CantidadMuestrasKm


## ----------------------------------------------

ggplot(cuencasUTM) +
  geom_sf(aes(fill = CantidadMuestrasKm))



## ----------------------------------------------
st_covers(cuencasUTM,muestreo)



## ----highlight.output=c(1,2,3)-----------------
#sapply es como un for
#para cada objeto que st_covers(cuencasUTM,muestreo), punto de mustreo sobre cuenca
# le aplico una funcion.
#La funcion es la media de 
# Selecciono la fila muesteo y hago una media de las 6 filas, es decir la cuenca donde estan los
#puntos de muestreo
mediaLimo <- sapply(st_covers(cuencasUTM,muestreo), function(x) {
  #print(x)
  mean(muestreo[x,][["Limo"]], na.rm = TRUE)
     })
mediaLimo
#es lo mismo que tomar solo la columna
mediaLimo2 <- sapply(st_covers(cuencasUTM,muestreo), function(x) {
  mean(muestreo$Limo[x], na.rm = TRUE)
})
mediaLimo2

#[['limo']] => loa [[]] los pone como para que sea un vector, un df
## ----------------------------------------------
#| label: ggplot-cuencasMediaLimo
#| output-location: column
cuencasUTM$MediaLimo <- mediaLimo
ggplot(cuencasUTM) +
  geom_sf(aes(fill = MediaLimo))



## ----------------------------------------------
#| label: ggplot-cuencasMediaLimolab
#| output-location: column
ggplot(cuencasUTM) +
  geom_sf(aes(fill = MediaLimo)) +
  labs(fill = "Limo (%)")



## ----fig.height=5------------------------------
#| label: ggplot-cuencasMediaLimolabNorth
#| output-location: column
ggplot(cuencasUTM) +
  geom_sf(aes(fill = MediaLimo)) +
  labs(fill = "Limo (%)") + 
  ggspatial::annotation_north_arrow(
    location = "tr", 
    which_north = "grid"
    )



## ----------------------------------------------
tm_shape(cuencasUTM) +
  tm_fill()



## ----tmap-cuencasMediaLimoSB-------------------
tm_shape(cuencasUTM) +
  tm_fill("MediaLimo")



## ----------------------------------------------
#| label: tmap-cuencasMediaLimo
#| output-location: column
tm_shape(cuencasUTM) +
  tm_fill("MediaLimo") +
  tm_borders()



## ----------------------------------------------
tm_shape(cuencasUTM) +
  tm_fill("MediaLimo", style = "quantile") +
  tm_borders() 


## ----------------------------------------------
tm_shape(cuencasUTM) +
  tm_fill("MediaLimo", style = "cont") +
  tm_borders() 


## ----------------------------------------------
#| label: tmap-cuencasMediaLimoContInterac
#| output-location: column
tmap_mode("view")
tm_shape(cuencasUTM) +
  tm_fill("MediaLimo", style = "cont") +
  tm_borders() +
  tm_basemap("Esri.WorldTopoMap")




## ----------------------------------------------
#| label: tmap-cuencasMediaLimoquantInteracFondo
#| output-location: column
#|
tmap_mode("view")
tm_shape(cuencasUTM) +
  tm_fill("MediaLimo",
          fill.scale = tm_scale_intervals(style = 'quantile'),
          fill_alpha = 0.8) +
  tm_borders() +
  tm_basemap(c("Stadia.Stamen.Watercolor",
               "Esri",
               "OpenTopoMap",
               "Stamen.Terrain"))
# names(leaflet::providers)


## ----------------------------------------------
#| label: tmap-cuencasUTMPllette
#| output-location: column
#|
tmap_mode("plot")
cuencas_tmap <- tm_shape(cuencasUTM) +
  tm_fill(
    fill = "MediaLimo",
    fill.scale = tm_scale_continuous(),
    fill.legend = tm_legend(
      title = 'Media Limo',
      text.size = 20,
      title.size = 23,
      legend.outside = TRUE,
      frame = "gray50"
    )
  ) +
  tm_borders()
cuencas_tmap



## ----------------------------------------------
#| label: tmap-muestreoNA
#| output-location: column
#|
muestreo_tmap <- tm_shape(muestreo) +
  tm_dots("Limo", size = 0.5,
          palette = "BuGn", colorNA= NULL,
          legend.hist=T) +
  tm_layout(legend.format = list(text.separator= " a "),
            legend.outside = TRUE,
            legend.hist.width = 2.5)
muestreo_tmap







## ----------------------------------------------
#| label: tmap-doscapas
#| output-location: column
#|
tm_shape(cuencasUTM) +
  tm_fill("MediaLimo", 
          style = "cont", 
          # palette = c("red", "blue"),
          textNA = "Sin Datos",
          title.size = "Media Limo") +
  tm_borders() +
  tm_legend(
    text.size=1,
    title.size=1.2,
    legend.outside=TRUE,
    frame="gray50",
    height=.6) +
  tm_shape(muestreo) +
  tm_dots("Limo", size = 0.5,
          palette = "BuGn", colorNA= NULL,
          legend.hist=T) +
  tm_layout(legend.format = list(text.separator= " a "),
            legend.outside = TRUE,
            legend.hist.width = 2.5)


## ----tmap-dosObjetos, fig.show = 'hide', results = 'hide'----
#| label: tmap-dosObjetos
#| output-location: column
#|
cuencas_tmap +
muestreo_tmap


## ----------------------------------------------
#| label: tmap-escala
#| output-location: column
#|
cuencas_tmap +
muestreo_tmap +
  tm_scale_bar() +
  tm_compass(position = c( "right", "top"))



## ----fig.height=4.5----------------------------
tmap_cuencas <- tm_shape(cuencasUTM) +
  tm_fill("MediaLimo", style = "quantile") +
  tm_borders() +
  tm_legend(legend.outside = TRUE)
tmap_cuencas




## ----fig.height=4.5----------------------------
tmap_muestreo <-   tm_shape(muestreo) +
  tm_bubbles(col = "K", style = "cont", textNA = "Sin dato") +
  tm_legend(legend.outside = TRUE)
tmap_muestreo



## ----fig.width=7-------------------------------
tmap_arrange(tmap_cuencas, tmap_muestreo)



## ----------------------------------------------
# tmap_mode("view")
p <- tm_shape(cuencasUTM) +
  tm_fill("MediaLimo",
          fill.scale = tm_scale_continuous(values = "RdYlGn"),
          fill.legend = tm_legend(title = "Media Limo")) +
  tm_borders() +
  tm_facets("SISTEMA", nrow = 1, sync = TRUE) +
  tm_basemap("OpenStreetMap")

htmlwidgets::saveWidget(example_plot , "./func_expossure_PLOT_2.html")

