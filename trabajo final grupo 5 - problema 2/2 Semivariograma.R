
# Carga de base de datos ####

suelo_finca3 <- cbind(suelo_finca3, st_coordinates(suelo_finca3)) %>%
  rename("x" = X, "y" = Y)

px <- ggplot(suelo_finca3, aes(x, MO)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

py <- ggplot(suelo_finca3, aes(y, MO)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

gridExtra::grid.arrange(px, py, ncol = 2)

# Evaluamos la tendencia y se observa  tendencia en x (longitud) e y (latitud)

# No hay tendencia espacial y se concluye que es estacional (no hay tendencia en x ni en y)
summary(lm(MO ~ x + y, suelo_finca3))
# ambos estimadores de los coeficientes de la regresion no son significativos
# R2 no muestra poca variabilidad explicada (2.46%)



# Ajuste de semivariograma experimetal, sin tendencia ####

semi_exp <- variogram(MO ~ 1, suelo_finca3, cutoff=180)
plot(semi_exp)

# se observa 2 np a 7 mts se observa que la semi varianza es alta (0.208)


# Ajuste de semivariograma teorico 
#### no doy los valores iniciales de nugget sill y rango ni en exp ni en spherical 
#### como los indico, r ajusta con valores inciales razonables
#### selecciona el que tenga el menor SC error

modelo_teorico_MO<-
  fit.variogram(semi_exp, vgm(c("Exp","Sph", "Gau")))
modelo_teorico_MO ### la relacion nugg/(psill+nugg) = ayuda a definir el modelo (exp o sph)

# se selecciona el modelo EXPONENCIAL, con psill = 0.1 y un rango de 51.76

p1 <- plot(semi_exp, modelo_teorico_MO)
p1

# Ajuste de semivariograma experimetal, contemplando tendencia 
semi_exp_MO_t<- variogram(MO ~ x + y, suelo_finca3)
# 
# # Ajuste de semivariograma teorico con tendencia ####
 modelo_teorico_MO_t <-
  fit.variogram(semi_exp_MO_t, vgm(c("Exp","Sph", "Gau")))
modelo_teorico_MO_t
# 
p2 <- plot(semi_exp_MO_t, modelo_teorico_MO_t)
# 
 gridExtra::grid.arrange(p1, p2, ncol = 2)

attr(modelo_teorico_MO, 'SSErr')  # da 0.00012 - es el exponencial da menor SSErr (SCE suma de cuadrados del error)
attr(modelo_teorico_MO_t, 'SSErr')  # aca da 0.0012













