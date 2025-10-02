# Lista de ejercicios 3
# Introduccion a R
#
# Programacion II
# Programa de Estudios Superiores
# Banco de Guatemala
# 2025
# TAREA III

# Integrantes: 
# Nuria Saraí Torres Aldana
# David Alejandro Del Cid De León
# Diego Andrés Menendez Barillas

setwd("C:/Users/diego/OneDrive/PES/10 Programacion II/PROGRA2/PS3/")
rm(list = ls()) # Limpiar el entorno de R
graphics.off()  # Cerrar gráficos abiertos
set.seed(123)   # Fijar semilla para reproducibilidad
################################################################################

#PARTE 1: ESTIMACIÓN DE UN AR(1) POR MÁXIMA VEROSIMILITUD

################################################################################
# 1) INCISO 1: FUNCIÓN DE LOG-VEROSIMILITUD
################################################################################
# Cargamos la librería necesaria
library(ggplot2)

#Creación de la función de log-verosimilitud de un AR(1)
log_verosimilitud_AR1 <- function(theta, y) {
  #Definimos los parámetros de theta
  mu <- theta[1]
  rho <- theta[2]
  sigma2 <- theta[3]

  if(sigma2 <= 0 || abs(rho) >= 1) return(-Inf) # Verificamos que sigma2 > 0 y |rho| < 1
  
  # Obtenemos el tamaño de la muestra
  T <- length(y)
  
  # Extraemos la primera observación y el resto de la serie de tiempo
  y1 <- y[1]
  y_t <- y[2:T]
  y_t_1 <- y[1:(T-1)]
  
  # Calculamos cada parte de la fórmula de log-verosimilitud 
  # Parte 1: Contribución de la primera observación (y1)
  # Corresponde a los términos: -0.5*log(2*pi) - 0.5*log(sigma2/(1-rho^2)) - ...
  logL_y1 <- -0.5 * log(2 * pi) - 0.5 * log(sigma2 / (1 - rho^2)) - 
             ((y1 - (mu / (1 - rho)))^2) / (2 * sigma2 / (1 - rho^2))
  
  # Parte 2: Contribución del resto de las observaciones (de y2 a yT)
  # Corresponde a los términos: -((T-1)/2)*log(2*pi) - ((T-1)/2)*log(sigma2) - sum(...)/2*sigma2
  # Calculamos los residuos (errores) de las predicciones del modelo
  residuos <- y_t - mu - rho * y_t_1
  logL_resto <- -((T - 1) / 2) * log(2 * pi) - ((T - 1) / 2) * log(sigma2) - 
                sum(residuos^2) / (2 * sigma2)
                
  # Sumamos ambas partes para obtener la log-verosimilitud total
  logL_total <- logL_y1 + logL_resto
  
  return(logL_total)
}

################################################################################
# 2) INCISO 2: SIMULACIÓN DE DATOS
################################################################################

# Parámetros verdaderos (poblacionales)
mu_inicial <- 1.0
rho_inicial <- 0.4
sigma2_inicial <- 0.5
T <- 100

# Creación de un vector vacío para guardar nuestros datos simulados
y_simulado <- numeric(T)
epsilon_t <- is.numeric(T)

# Generamos el valor inicial y1 a partir de su distribución teórica
# y1 ~ N( mu/(1-rho), sigma2/(1-rho^2) )
y_simulado[1] <- rnorm(1, mean = mu_inicial / (1 - rho_inicial), 
                       sd = sqrt(sigma2_inicial / (1 - rho_inicial^2)))
epsilon_t[1] <- 0  # No hay error para t=1 ya que es dado

# Generamos el resto de los valores (y2 hasta yT) usando un ciclo
# yt = mu + rho*y_{t-1} + epsilon_t 
for (t in 2:T) {  
  # epsilon_t es una variable aleatoria de distribución normal
  epsilon_t[t] <- rnorm(1, mean = 0, sd = sqrt(sigma2_inicial))
  
  # Calculamos el valor de yt=mu + rho*yt-1 + epsilon_t
  y_simulado[t] <- mu_inicial + rho_inicial * y_simulado[t - 1] + epsilon_t[t]
}

# Mostramos los valores simulados
#Generación del vector Et
epsilon_t
#Generación del vector Yt (incluye y1)
y_simulado


################################################################################
# 3) INCISO 3: ESTIMACIÓN DE PARÁMETROS POR MÁXIMA VEROSIMILITUD 
################################################################################

# Valores iniciales para el optimizador, como se indica en el PDF
valores_theta <- c(0.5, 0.5, 0.5)

# Usamos optim() para encontrar los parámetros que maximizan la log-verosimilitud
# 'par' son los valores iniciales
# 'fn' es la función a optimizar
# 'y' son los datos 
mle_resultado <- optim(par = valores_theta,          # Valores iniciales
                       fn = log_verosimilitud_AR1,  # Función de log-verosimilitud 
                       y = y_simulado,               # Datos simulados
                       method = "L-BFGS-B",          # Método de optimización con restricciones
                       lower = c(-Inf, -0.99, 1e-6), # Restricciones para rho y sigma2
                       upper = c(Inf, 0.99, Inf),    # Restricciones para rho
                       control = list(fnscale = -1)   # Para maximizar la función
)

# Accedemos a los parámetros estimados están en $par (mu_hat, rho_hat, sigma2_hat)
theta_estimado <- mle_resultado$par

# Imprimimos los resultados para comparar con los parámetros verdaderos
cat("Parámetros Verdaderos:\n")
cat("mu =", mu_inicial, ", rho =", rho_inicial, ", sigma2 =", sigma2_inicial, "\n\n")

cat("Parámetros Estimados (MLE):\n")
cat("mu_estimado =", theta_estimado[1], "\n")
cat("rho_estimado =", theta_estimado[2], "\n")
cat("sigma2_estimado =", theta_estimado[3], "\n")

################################################################################
# 4) INCISO 4:REPETICIÓN DE LA SIMULACIÓN Y ANÁLISIS DE RESULTADOS 
################################################################################

#Generamos la función para simular AR(1)
simular_AR1 <- function(T, mu, rho, sigma2) {
  y <- numeric(T)
  epsilon_t <- numeric(T)
  y[1] <- rnorm(1, mean = mu / (1 - rho), sd = sqrt(sigma2 / (1 - rho^2)))
  for (t in 2:T) {
    epsilon_t <- rnorm(T, mean = 0, sd = sqrt(sigma2))
    y[t] <- mu + rho * y[t - 1] + epsilon_t[t]
  }
  return(y)
}

# Número de repeticiones para el experimento de Monte Carlo.
R <- 1000

# Creación de vectores numéricos vacíos para almacenar las estimaciones de cada parámetro por simulación.
mu_hat_vec     <- numeric(R)
rho_hat_vec    <- numeric(R)
sigma2_hat_vec <- numeric(R)

# Define los valores iniciales (el "punto de partida") que usará el optimizador.
valores_theta <- c(0.5, 0.5, 0.5)

# Inicia el bucle que se ejecutará desde r=1 hasta R.
for (r in 1:R) {
  
  # Simulación de Datos:
  # En cada iteración, se genera una NUEVA serie de tiempo AR(1) de tamaño T.
  y_r <- simular_AR1(T, mu_inicial, rho_inicial, sigma2_inicial)

  # Estimación por Máxima Verosimilitud (MLE):
  # Se utiliza la función optim() para encontrar los parámetros que maximizan la log-verosimilitud.
  theta_r <- optim(
    par     = valores_theta,           # Punto de partida para la búsqueda
    fn      = log_verosimilitud_AR1,   # La función a maximizar
    y       = y_r,                     # Los datos simulados en esta iteración
    method  = "L-BFGS-B",
    lower   = c(-Inf, -0.99, 1e-6),
    upper   = c( Inf,  0.99, Inf),
    control = list(fnscale = -1)
  )

  # Almacenamiento de Resultados:
  # Extrae el vector de parámetros estimados ('$par') y los guarda en la posición 'r' de los vectores de resultados.
  mu_hat_vec[r]     <- theta_r$par[1]
  rho_hat_vec[r]    <- theta_r$par[2]
  sigma2_hat_vec[r] <- theta_r$par[3]
}

# Cálculo del promedio de las R estimaciones para cada parámetro.
prom_mu  <- mean(mu_hat_vec)
prom_rho <- mean(rho_hat_vec)
prom_s2  <- mean(sigma2_hat_vec)

# Cálculo de la desviación estándar de las R estimaciones.
sd_mu  <- sd(mu_hat_vec)
sd_rho <- sd(rho_hat_vec)
sd_s2  <- sd(sigma2_hat_vec)

# Impresión de resultados
cat("\n--- Resumen de la Simulación de Monte Carlo ---\n")
cat("Promedios de los estimadores (deberían aproximarse a los valores verdaderos):\n")
cat("  - E[mu_hat]     ≈", round(prom_mu, 4), " (Valor Verdadero =", mu_inicial, ")\n")
cat("  - E[rho_hat]    ≈", round(prom_rho, 4), " (Valor Verdadero =", rho_inicial, ")\n")
cat("  - E[sigma2_hat] ≈", round(prom_s2, 4), " (Valor Verdadero =", sigma2_inicial, ")\n\n")

cat("Desviaciones estándar de los estimadores (Errores Estándar de Monte Carlo):\n")
cat("  - sd(mu_hat)     =", round(sd_mu, 4), "\n")
cat("  - sd(rho_hat)    =", round(sd_rho, 4), "\n")
cat("  - sd(sigma2_hat) =", round(sd_s2, 4), "\n")

histogramas <- par(mfrow = c(1, 3))

# Histograma para mu_hat
hist(mu_hat_vec,
     breaks = 30,                             # Número de barras.
     main = expression(hat(mu)),              # Título con formato matemático.
     xlab = expression(hat(mu)),              # Etiqueta del eje X.
     col = "lightblue")
# Agrega una línea vertical en el valor verdadero para comparación visual.
abline(v = mu_inicial, col = "red", lwd = 2)

# Histograma para rho_hat
hist(rho_hat_vec,
     breaks = 30,
     main = expression(hat(rho)),
     xlab = expression(hat(rho)),
     col = "lightcoral")
abline(v = rho_inicial, col = "red", lwd = 2)

# Histograma para sigma2_hat
hist(sigma2_hat_vec,
     breaks = 30,
     main = expression(hat(sigma^2)),
     xlab = expression(hat(sigma^2)),
     col = "lightgreen")
abline(v = sigma2_inicial, col = "red", lwd = 2)

par(histogramas)

################################################################################
# --- FIN DEL CÓDIGO DE LA PARTE 1---
################################################################################

################################################################################
# ESTIMACIÓN DE UN AR(1), PARTE II
################################################################################

################################################################################
# 1) INCISO 1: FUNCIÓN DE CALCULO DE ERRORES ESTÁNDAR DE MÁXIMA VEROSIMILITUD
################################################################################

set.seed(123)  # Fijar semilla para reproducibilidad
datos_inflacion <- read.csv("inflacion.csv")

# Extraer la columna de inflación como un vector numérico.
serie_inflacion <- as.numeric(datos_inflacion$inflacion)
Tn <- length(serie_inflacion)
# Establecer valores iniciales para los parámetros para el optimizador (mu, rho, sigma^2)
valores_iniciales <- c(mean(serie_inflacion), 0.5, var(serie_inflacion)) 

# Encontrar el estimador de máxima verosimilitud, theta_estimado.
mle_fit_inflacion <- optim(
par = valores_iniciales,
fn = log_verosimilitud_AR1,
y = serie_inflacion,
method = "L-BFGS-B",           # Método que permite restricciones
lower = c(-Inf, -0.99, 1e-8),    # Cotas inferiores para rho y sigma^2
upper = c(Inf, 0.99, Inf),       # Cotas superiores
control = list(fnscale = -1),  # Maximizar en lugar de minimizar
hessian = TRUE                
)

theta_estimado <- mle_fit_inflacion$par

# DEFINICIÓN DE LA FUNCION QUE CALCULA LOS ERRORES ESTÁNDAR DE MÁXIMA VEROSIMILITUD
simular_se <- function(theta, y) {
    J1 <- -mle_fit_inflacion$hessian
    var_theta_hat <- solve(J1)
    se_hat <- sqrt(diag(var_theta_hat))
return(se_hat)
} 

################################################################################
# 2) INCISO 2: UTILIZACIÓN DE LA FUNCIÓN PARA ENCONTRAR EL ESTIMADOR Y ERRORES ESTÁNDAR
################################################################################

simular_se(valores_iniciales, serie_inflacion)
se_hat <- simular_se(valores_iniciales, serie_inflacion)

# Data frame para mostrar los resultados de forma ordenada.
tabla_resultados <- data.frame(
  Parametro = c("mu (μ)", "rho (ρ)", "sigma^2 (σ²)"),
  Estimacion = theta_estimado,
  Error_Estandar = se_hat
)
cat("\n--- Resultados de la Estimación AR(1) para la Inflación ---\n")
tabla_resultados
