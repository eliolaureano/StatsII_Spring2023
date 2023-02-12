library(stats)

# creamos la semilla
set.seed(123)
# generamos las 1000 variables aleatorias de Cauchy
x <- rcauchy(1000, location = 0, scale = 1)
#x <- rnorm(1000, mean = 0, sd = 1)
# ordenamos los datos
x = sort(x)
# create empirical distribution of observed data
ECDF <− ecdf(x)
empiricalCDF <− ECDF(x)
# generamos el estadistico de prueba
D <- max(abs(empiricalCDF - pnorm(x)))
# calculamos el valor del pvalue
p_value = 1 - pnorm((sqrt(1000) + 0.12 + 0.11/sqrt(1000))*D)
# damos el valor de alfa 
alfa = 0.01
if (p_value < alfa) {
  cat("Se rechaza la hipótesis nula, los datos no provienen de una distribucion normal")
} else {
  cat("No se rechaza la hipótesis nula, los datos provienen de una distribucion normal")
}

install.packages("optimx") 


# Setear semilla para reproducibilidad
set.seed(123)

# Crear el conjunto de datos
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75 * data$x + rnorm(200, 0, 1.5)

# Estimar la regresión OLS utilizando el algoritmo de Newton-Raphson
library(optimx)
ols <- optimx(par = c(2, 2), fn = function(b) {
  yhat <- b[1] + b[2] * data$x
  sum((data$y - yhat)^2)
}, method = "BFGS", hessian = TRUE)



# Estimar la regresión OLS utilizando la función lm
model_lm <- lm(y ~ x, data = data)

print(paste("Coeficientes OLS (BFGS):",ols["p1"],ols["p2"]))
print(paste("Coeficientes OLS (lm):",beta_lm))




