
# Cálculo de LI y LS para eliminar datos ubicados por fuera de la media +- 2.5 DE ####
resumen <- suelo_finca %>% 
  summarise(Media = mean(MO),
         DE = sd(MO),
         LI = Media - 2.5 * DE,
         LS = Media + 2.5 * DE,
         min = min(MO), max = max(MO),
         asimetria = e1071::skewness(MO))
resumen 

suelo_finca1 <- suelo_finca %>% 
  filter(MO >= resumen$LI)


# solo se elimina outlier con el criterio de regla empirica

histograma <- ggplot(suelo_finca1, aes(x = MO)) +
  geom_histogram(aes(y = after_stat(count) / sum(count))) +
  ylab("Frecuencia Relativa") +
  theme_grey() + theme_bw()

boxplot <- ggplot(suelo_finca1, aes(y = MO)) +
  geom_boxplot(coef = 1.5) +
  ylab("Materia Orgánica (%)")  +
  scale_x_discrete(breaks = NULL) +
  xlab(NULL) +
  theme_light()

grid.arrange(histograma, boxplot, ncol = 2)

# Definición de vecindarios para cada punto muestreado ####
spdep::set.ZeroPolicyOption(TRUE)
vecindarios <- dnearneigh(suelo_finca1$geom, 0, 20)
summary(vecindarios)
lw <- nb2listw(vecindarios, style = "W")
lw$weights[10]

dist <- nbdists(vecindarios, suelo_finca1$geom)
dist[10]
fdist <- lapply(dist, function(x)
  (1 / (x / 100)))
lw2 <- nb2listw(vecindarios, glist = fdist, style = "W")
lw2$weights[10]
sum(lw2$weights[10][[1]])



#plot(gri,misdatos_1,col = "red", pch = 20, cex = 1)
# 
# #  Generación del Moran Plot (MP) ####
# MP <-
#   moran.plot(
#     suelo_finca1$MO,
#     lw2,
#     col = 3,
#     quiet = T,
#     labels = F,
#     zero.policy = T,
#     xlab = "Rendimiento",
#     ylab = "Rendimiento Spatially Lagged"
#   )
# head(MP)
# Influ <- MP$is_inf
# Influ

# #  Cálculo del índice de Moran local (IML) ####
# ML <-
#   localmoran(misdatos_2$REND, lw2, alternative = "less")
# head(ML)
# 
# IMl <-
#   printCoefmat(data.frame(ML, row.names = suelo_finca1$Casos), check.names =
#                  FALSE)
# head(IMl)
# 
# #  Eliminación de inliers utilizando IML y MP  ####
# misdatos_3 <- cbind(suelo_finca1, Influ, IMl)
# head(misdatos_3)
# #st_write(misdatos_3,"Datosadepurar.gpkg")
# # elimina los que tienen indice de moran negativo y son significativos....
# misdatos_3 <-
#   subset(misdatos_3, misdatos_3[["Ii"]] > 0 |
#            misdatos_3[["Pr.z...E.Ii.."]] > 0.05)
# 
# misdatos_3 <- misdatos_3[misdatos_3$Influ == FALSE, ]
# 
# # Media, mediana y coeficiente de asimetría luego de eliminar outliers e inliers ####
# (mean(misdatos_3$REND))
# (median(misdatos_3$REND))
# (e1071::skewness(misdatos_3$REND))
# 
# #  Histograma y gráfico box-plot luego de eliminar outliers e inliers ####
# histograma_3 <- ggplot(misdatos_3, aes(x = REND)) +
#   geom_histogram(aes(y = after_stat(count) / sum(count))) +
#   ylab("Frecuencia Relativa") +
#   theme_light()
# 
# boxplot_3 <- ggplot(misdatos_3, aes(y = REND)) +
#   geom_boxplot() +
#   ylab("Rendimiento (t/ha)")  +
#   scale_x_discrete(breaks = NULL) +
#   xlab(NULL) +
#   theme_light()
# 
# grid.arrange(histograma_3, boxplot_3,
#              ncol = 2)
# 
# st_write(misdatos_3[, c("REND", "geom")], "soja_depurado.gpkg", delete_layer = T)
# 
# #  Gráfico de dispersión de la variable REND con coordenadas x e y  para la detección de tendencias ####
# base_depurada <- cbind(misdatos_3, st_coordinates(misdatos_3))
# head(base_depurada)
# 
# px <- ggplot(base_depurada, aes(X, REND)) +
#   geom_point() +
#   geom_smooth(method = lm, se = FALSE)
# 
# py <- ggplot(base_depurada, aes(Y, REND)) +
#   geom_point() +
#   geom_smooth(method = lm, se = FALSE)
# 
# grid.arrange(px, py, ncol = 2)
# 
# regresion <-
#   lm(formula = REND ~ 1 + X + Y ,
#      data = base_depurada,
#      na.action = na.omit)
# summary(regresion)

#Visualización espacial de datos depurados  ####
# plot sf
plot(suelo_finca1$MO)

plot(
  suelo_finca1[, "MO"],
  key.pos = 4,
  axes = TRUE,
  key.width = lcm(1.5),
  key.length = 1,
  pal = terrain.colors
)

# plot ggplot2
suelo_finca1 %>% ggplot() +
  geom_sf(aes(color = MO)) +
  scale_color_viridis_c(direction = -1)

suelo_finca1 %>% ggplot() +
  geom_sf(aes(color = MO)) +
  scale_color_gradient(low = "#fffb00", high = "#ff0000")

# plot mapview
mapview(suelo_finca1[, "MO"])
mapview(suelo_finca1[, "MO"],
        alpha = 0,
        col.regions = topo.colors)
