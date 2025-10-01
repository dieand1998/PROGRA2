# Lista de ejercicios 1
# Introduccion a R
#
# Programacion II
# Programa de Estudios Superiores
# Banco de Guatemala
# 2025
# TAREA II

# Integrantes: 
# Nuria Saraí Torres Aldana
# David Alejandro Del Cid De León
# Diego Andrés Menendez Barillas

rm(list=ls())
graphics.off()

#################################################################
# Ejercicio: Solución Numérica del Problema de "Super Frank"
# Método: Resolver la Condición de Primer Orden (CPO)
#################################################################

# --- PASO 1: Definir los Parámetros del Modelo ---

# Asignamos los valores específicos proporcionados en la nota del enunciado.
# Estos valores definen el "mundo" económico en el que Frank toma su decisión.
eps   <- 3     # Parámetro ε de la función de utilidad 
alpha <- 2     # Parámetro α de la función de utilidad
k     <- 0.8   # Parámetro κ de la función de utilidad
w     <- 1     # Salario (w) de Frank


# --- PASO 2: Definir la Ecuación a Resolver (La Condición de Primer Orden) ---

# Para maximizar la utilidad, el cálculo nos dice que debemos encontrar el punto
# donde la derivada de la función de utilidad con respecto al consumo (C) es igual a cero.
# Esta función `focC` representa esa derivada, cumpliendo con el requisito de
# encontrar la condición de primer orden.
focC <- function(C, eps, alpha, k, w) {
  # Este es el resultado matemático de derivar la función de utilidad e igualarla a cero.
  (1/eps) * C^(1/eps - 1) - alpha * (k/(eps*w)) * (1 - C/w)^(k/eps - 1)
}


# --- PASO 3: Encontrar la Solución Numérica ---

# La ecuación definida en `focC` es difícil de resolver algebraicamente.
# Por eso, usamos una función de R llamada `uniroot`, que es un "buscador de raíces".
# Le pedimos que encuentre numéricamente el nivel de consumo que resuelve la ecuación.
solucion <- uniroot(
  f = focC,                         # La ecuación que queremos resolver (la CPO).
  interval = c(1e-8, w - 1e-8),     # El rango de búsqueda para C (entre casi 0 y casi w).
  
  # Pasamos los parámetros necesarios a la función focC.
  eps = eps,
  alpha = alpha,
  k = k,
  w = w
)


# --- PASO 4: Calcular Todos los Valores Óptimos ---

# 4.1. Extraer el consumo óptimo (C*) de la solución.
# La función `uniroot` devuelve un objeto; la solución está en el elemento `$root`.
C_star <- solucion$root

# 4.2. Calcular el trabajo óptimo (L*) usando la restricción presupuestaria.
# La restricción es C = wL, por lo que L = C/w.
L_star <- C_star / w

# 4.3. Calcular la utilidad máxima (U*) usando el L* encontrado.
# Para esto, definimos primero la función de utilidad original en términos de L.
# Esta es la función objetivo después de reemplazar la restricción.
U <- function(L, eps, alpha, k, w){
  ((w*L)^(1/eps) + alpha*(1 - L)^(k/eps))^eps
}

# Calculamos el valor de la utilidad máxima.
U_star <- U(L_star, eps, alpha, k, w)


# --- PASO 5: Presentar los Resultados Finales ---

# Usamos la función cat(sprintf(...)) para imprimir los resultados
# con un formato claro y 10 decimales de precisión.
cat("--- Resultados Óptimos para Frank ---\n")
cat(sprintf("Consumo Óptimo (C*) = %.10f\n", C_star))
cat(sprintf("Trabajo Óptimo (L*) = %.10f\n", L_star))
cat(sprintf("Utilidad Máxima (U*) = %.10f\n", U_star))

#################################################################
# MÉTODO 2: Optimización Directa (El enfoque más sencillo)
#################################################################
cat("--- MÉTODO 2: Solución con Optimización Directa ---\n")

# Este método es más directo. Simplemente le pedimos a R que encuentre
# el valor de L entre 0 y 1 que maximiza la función de utilidad.

# La función `optimize` minimiza por defecto, así que le pedimos que
# maximice indicando `maximum = TRUE`.
solucion_directa <- optimize(
    f = U,                  # CORRECCIÓN: Se pasa la FUNCIÓN U, no el valor U_star.
    interval = c(0, 1),     # El rango de búsqueda para L.
    maximum = TRUE,         # ¡Queremos maximizar!
    
    # Pasamos los parámetros adicionales que la función U necesita.
    eps = eps,
    alpha = alpha,
    k = k,
    w = w
)

# Extraemos los resultados
L_optimo_metodo2 <- solucion_directa$maximum
C_optimo_metodo2 <- w * L_optimo_metodo2
U_maxima_metodo2 <- solucion_directa$objective

cat(sprintf("Trabajo Óptimo (L*) = %.10f\n", L_optimo_metodo2))
cat(sprintf("Consumo Óptimo (C*) = %.10f\n", C_optimo_metodo2))
cat(sprintf("Utilidad Máxima (U*) = %.10f\n\n", U_maxima_metodo2))

#################################################################
#################################################################
# Ejercicio de Simulación: Frank vs. Mourinho
#
# Objetivo: Estimar la probabilidad de victoria de Frank y encontrar
#           su estrategia ofensiva óptima.
# Método:   Simulación de Montecarlo.
#
#################################################################

# --- PASO 1: Simular un Partido Completo (la "regla" del juego) ---
 
# Esta función simula un partido completo de 90 minutos. Es el núcleo
# de toda la simulación, ya que contiene la lógica de cada minuto.
#
# Argumentos:
#   p_frank_ofensiva: La probabilidad de que Frank decida jugar
#                     a la ofensiva en un minuto cualquiera.
#
# Retorno:
#   1 si Frank gana, -1 si pierde, 0 si empatan.
#
simular_partido_corregido <- function(p_frank_ofensiva) {
  
  # Inicializa los marcadores en cero al inicio del partido.
  goles_frank <- 0
  goles_mou <- 0
  
  # Bucle que simula cada minuto del partido, desde el 1 hasta el 90.
  for (minuto in 1:90) {
    
    # Decisiones aleatorias de los entrenadores para este minuto.
    # `runif(1)` genera un número aleatorio entre 0 y 1.
    frank_ataca <- runif(1) <= p_frank_ofensiva
    mou_defiende <- runif(1) <= 0.95 # Mou es defensivo el 95% de las veces.
    
    # Se evalúan los 4 posibles escenarios de juego y se aplican
    # las probabilidades de gol correspondientes para cada uno.
    
    # Escenario 1: Frank ataca, Mou defiende (Ofensivo vs. Defensivo)
    if (frank_ataca && mou_defiende) {
      if (runif(1) <= 0.03) goles_frank <- goles_frank + 1 # P(Gol Frank) = 0.03
      if (runif(1) <= 0.01) goles_mou <- goles_mou + 1   # P(Gol Mou) = 0.01
    }
    # Escenario 2: Ambos atacan (Ofensivo vs. Ofensivo)
    else if (frank_ataca && !mou_defiende) {
      if (runif(1) <= 0.05) goles_frank <- goles_frank + 1 # P(Gol Frank) = 0.05
      if (runif(1) <= 0.05) goles_mou <- goles_mou + 1   # P(Gol Mou) = 0.05
    }
    # Escenario 3: Frank defiende, Mou ataca (Defensivo vs. Ofensivo)
    else if (!frank_ataca && !mou_defiende) {
      if (runif(1) <= 0.01) goles_frank <- goles_frank + 1 # P(Gol Frank) = 0.01
      if (runif(1) <= 0.03) goles_mou <- goles_mou + 1   # P(Gol Mou) = 0.03
    }
    # Escenario 4: Ambos defienden (Defensivo vs. Defensivo) -> No hay goles.
    
  }
  
  # `sign()` devuelve 1 si el número es positivo (Frank gana),
  # -1 si es negativo (Frank pierde) y 0 si es cero (empate).
  return(sign(goles_frank - goles_mou))
}


# --- PASO 2: Responder la Pregunta 1 (Probabilidad de ganar con p=0.5) ---
set.seed(123) # Fijamos la semilla para reproducibilidad.

# Simulamos 50,000 partidos para obtener una estimación precisa.
# `replicate` ejecuta la función `simular_partido_corregido` 50,000 veces.
cat("--- Pregunta 1: Probabilidad de Victoria con p=0.5 ---\n")
resultados_p05 <- replicate(50000, simular_partido_corregido(p_frank_ofensiva = 0.5))

# `mean(resultados_p05 == 1)` calcula la proporción de simulaciones
# en las que el resultado fue 1 (victoria de Frank).
prob_victoria_p05 <- mean(resultados_p05 == 1)
cat(paste("Con p=0.5, la probabilidad estimada de que Frank gane es:", prob_victoria_p05, "\n\n"))


# --- PASO 3: Responder la Pregunta 2 (Encontrar la estrategia óptima p*) ---

# 3.1. Definir la Función Objetivo para la Optimización
# Esta función toma una probabilidad `p` y devuelve una "calificación" de qué tan buena es.
# Una calificación más baja significa una mayor probabilidad de victoria.
funcion_objetivo_calificacion <- function(p) {
  # Ejecutamos una simulación más pequeña para evaluar `p`.
  resultados_opt <- replicate(10000, simular_partido_corregido(p))
  prob_victoria <- mean(resultados_opt == 1)
  
  # Devolvemos el negativo porque `optim` minimiza por defecto.
  # Minimizar la probabilidad negativa es lo mismo que maximizar la probabilidad.
  return(-prob_victoria) 
}

# 3.2. Ejecutar la Optimización Numérica
set.seed(123) # Fijamos la semilla para reproducibilidad.

# `optim` es un "buscador inteligente" que prueba diferentes valores de `p`
# para encontrar el que minimiza la función objetivo.
cat("--- Pregunta 2: Búsqueda de la Estrategia Óptima (p*) ---\n")
resultado_opt <- optim(
  par = 0.5,                               # Punto de partida para la búsqueda.
  fn = funcion_objetivo_calificacion,         # La función a minimizar.
  method = "L-BFGS-B",                     # Método que permite definir un rango de búsqueda.
  lower = 0.01,                            # El valor más bajo de `p` a probar.
  upper = 1.0                              # El valor más alto de `p` a probar.
)

# Imprimimos los resultados de la optimización.
cat(paste("El 'p' óptimo que maximiza la victoria es aproximadamente:", resultado_opt$par, "\n"))
cat(paste("La máxima probabilidad de victoria estimada con este 'p' es:", -resultado_opt$value, "\n"))