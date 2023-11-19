# Validacion... genera una funcion con un unico argumento: fold
library(parallel)

validacion <- function (fold) {
  require(sf)
  require(caret)
  require(gstat)
  require(sp)
  
  datos <- read.table("suelo_finca.txt", header = T)
  datos <- st_as_sf(datos, coords = c("x", "y"), crs = 32618)
  datos <- cbind(datos, st_coordinates(datos))

  
  set.seed(7)
  datos$id <-
    sample(rep(1:10, nrow(datos), length.out = nrow(datos)))
  
  list <- 1:10
  prediccion <- data.frame()
  testset <- data.frame()
  
  training <- subset(datos, id %in% list[-fold])
  testing <- subset(datos, id %in% c(fold))
  
  # Kriging Universal
  train_kg <- training
  test_kg <- testing
  vario <- variogram(MO ~ X + Y, training)
  vario_teorico <-
    fit.variogram(vario, vgm(c("Sph", "Exp", "Gau")))
  KU <- krige(MO ~ X + Y, training, testing, vario_teorico)
  
  # Regression Kriging
   mlr <- lm(MO ~  X + Y, training)

  training$residuos_rk <- mlr$residuals
  vario_rk <- variogram(residuos_rk ~ 1, training)
  model_rk_kg <-
    fit.variogram(vario_rk, vgm(c("Sph", "Exp")))

  test_k <- krige(residuos_rk ~ 1 , training, testing, model_rk_kg)
  test_rk <- predict(mlr, newdata = testing) + test_k$var1.pred

  # Random Forest
  fitControl <- trainControl(method = "cv", number = 10)
  fitControl <- trainControl(method = "none")

  set.seed(7)
  rf <- train(
    MO ~  X + Y,
    data = training,
    method = "rf",
    trControl = fitControl,
    verbose = FALSE
  )

  test_rf <- predict(rf, newdata = testing)

  # # Random Forest + Kriging Ordinario
  training$residuos_rf <-
    training$MO - predict(rf, newdata = training)
  vario_rf <- variogram(residuos_rf ~ 1, training)
  model_rf_ko <-
    fit.variogram(vario_rf, vgm(c("Sph", "Exp", "Gau")))
  test_ko <- krige(residuos_rf ~ 1 , training, testing, model_rf_ko)
  test_rf_ko <- test_rf + test_ko$var1.pred


  # Tabla observados y predichos
  testset <- rbind(testset, as.data.frame(testing[, "MO"]))
  result <- data.frame(
    data.frame(
      "x" = testing$X,
      "y" = testing$Y,
      "k-fold" = fold,
      "Observado" = testset[, 1],
       "KU" = KU$var1.pred,
       "RK" = test_rk,
       "RF" = test_rf,
       "RF_KO" = test_rf_ko
    )
  )
  
  return(result)
  
}


# Correr validacion cruzada
resultados <- do.call(rbind,lapply(1:10, validacion))

# Correr validacion cruzada paralelizado
# num_cores <- detectCores() - 1
# cl <- makeCluster(num_cores)
# 
# resultados <- do.call(rbind, parLapply(cl, 1:10, validacion))
# stopCluster(cl)

# Comparacion de metodos
head(resultados)
tabla <- resultados[, 4:8]
resumen <- function (j) {
  ME <- mean(tabla [, j] - tabla[, "Observado"])
  MAE <- mean(abs(tabla [, j] - tabla[, "Observado"]))
  MAPE <-
    mean(abs(tabla [, j] - tabla[, "Observado"]) / tabla[, "Observado"]) *
    100
  MSE <- mean((tabla [, j] - tabla[, "Observado"]) ^ 2)
  RMSE <- sqrt(mean((tabla [, j] - tabla[, "Observado"]) ^ 2))
  RMSE_cv <- sqrt(MSE) / mean(tabla[, "Observado"]) * 100
  rLM <- lm(tabla [, j] ~ tabla[, "Observado"])
  R2 <- as.matrix(summary(rLM)$adj.r.squared)
  resumen <-
    data.frame("Modelo" = names(tabla [j]), ME, MAE, MAPE, MSE, RMSE, RMSE_cv, R2)
  return(resumen)
}

tablafinal <- do.call("rbind", lapply(2:5, resumen))
tablafinal
