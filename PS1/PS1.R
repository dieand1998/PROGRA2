# Lista de ejercicios 1
# Introduccion a R
#
# Programacion II
# Programa de Estudios Superiores
# Banco de Guatemala
# 2025

# Integrantes: 
# Nuria Saraí Torres Aldana
# David Alejandro Del Cid De León
# Diego Andrés Menendez Barillas

# ENTORNO DE TRABAJO
setwd("/Users/diego/OneDrive/PES/10 Programacion II/00 Clases/Day_1/PS")
rm(list=ls())
graphics.off()
################################################################################
# 1)  Control de Lectura
################################################################################

# Las siguientes preguntas estan relacionadas con la forma de realizar distintas
# operaciones en R

# Basado en lo mostrado en la clase y los resultados de los "scripts" usados,
# responda las siguientes preguntas:

# 1) ¿Que comando de R podemos usar para limpiar el escritorio?
# R. rm(list=ls())

# 2) ¿Que comando de R podemos uar para cerrar las ventanas de graficas activas?
# R. graphics.off()

# 3) ¿Como podemos realizar producto matricial?
# R. MATRIZ_A %*% MATRIZ_B

# 4) ¿Como podemos realizar producto punto?
# R. sum(VECTOR1 * VECTOR2) o tambien t(VECTOR1) %*% VECTOR2

# 5) ¿Como podemos definir una matriz 10x10 llena de ceros? 
# R. matrix(0, nrow=10, ncol=10)

# 6) ¿Como podemos  crear un vector con cien numeros equidistantes en el 
#     intervalo [0,1]? 
# R. seq(0, 1, length.out=100)

# 7) Mencione tres tipos de datos comunmente encontrados en R
# R. numerico, caracter, logico

# 8) ¿Que libreria nos permite cargar archivos de Excel en R?
# R. readxl

# 9) ¿Cual es el coeficiente de correlacion entre el numero de peliculas  en que
#     aparecio Nicolas Cage y el numero de muejeres editoras de la revista 
#     "Harvard Law Review" entre 2005 y 2009?
# R. 

# No. de peliculas en que aparecio Nicolas Cage entre 2005 y 2009
NC <- c(2,3,4,1,4)     

# No. de muejeres editoras en "Harvard Law Review" entre 2005 y 2009
ME <- c(9,14,19,12,19) 

Correlacion_Espurea <- data.frame(nicolas.cage=NC,mujeres.editoras=ME)
cor(Correlacion_Espurea$mujeres.editoras,Correlacion_Espurea$nicolas.cage)
# Resultado: 0.8554467

# 10) Haga un grafico de barras ilustrando los balones de oro ganados por 
#     Cristiano, Messi, Cruyff, Iniesta y Ronaldinho.
# R. 

# BALONES DE ORO ACTUALIZADO MESSI E INIESTA
x11()
balones_de_oro <- c(5,8,3,1,0)
names(balones_de_oro) <- c("Cristiano","Messi","Cruyff","Ronaldinho","Iniesta")
names.sort <- balones_de_oro[order(-balones_de_oro)]
barplot(names.sort, col = "#a18a08", main = "Balones de Oro Ganados")


# 11) ¿Si la probabilidad de que Falcao se lesione es 0.2, cuantos partidos 
#      podemos esperar que juegue antes de lesionarse?
# R. 
Partidos_esperados <- function(prob_lesion) {
#  Simulacion de partidos jugados antes de lesion
    num_simulaciones <- 10000
    num_partidos <- rep(0,num_simulaciones)

# LOOP para simular el numero de partidos jugados antes de lesionarse
    for (i in 1:num_simulaciones) {
    partidos <- 0
    lesion <- 0

    while (lesion == 0) {
        partidos <- partidos + 1 
        azar_lesion <- runif(1)

        if (azar_lesion < prob_lesion) {
        lesion <- 1
            }
        }
        num_partidos[i] <- partidos
    }
    return(mean(num_partidos))
}
Partidos_esperados(0.2) #APROXIMADAMENTE 5 PARTIDOS. 

################################################################################
# 2) Mi primer funcion 
################################################################################

# Adjunto encontrara el archivo "CPI.csv" con datos mensuales del indice de 
# precios al consumidor de Guatemala (El primer dato es inventado)

# A continuacion, escriba un codigo que haga lo siguiente:
# 1) Cargue los datos del archivo CPI.csv (incluya los codigos necesarios para 
#    descargar y cargar un paquete, si lo necesita)

rm(list=ls()) # Limpiar escritorio
graphics.off() # Cerrar ventanas de graficas abiertas

datos_desde_csv <- read.csv("CPI.csv", header = TRUE)
datos_desde_csv

# 2) Cree una variable inflacion.mensual que contenga la inflacion mensual de 
#    Guatemala calculada usando los datos del punto anterior.
n <- nrow(datos_desde_csv)
n

CPI_actual <- datos_desde_csv$CPI[2:n]
CPI_anterior <- datos_desde_csv$CPI[1:(n-1)]
inflacion_mensual <- (CPI_actual - CPI_anterior) / CPI_anterior * 100
inflacion_mensual


# 3) Defina una funcion que calcule la inflacion trimestral usando el PROMEDIO 
#    de cada trimestre
inflacion_trimestral_promedio <- function(inflacion_mensual){
    inflacion_mensual <- inflacion_mensual[!is.na(inflacion_mensual)]
    inflacion_trimestral <- c()
    for (i in seq(0, length(inflacion_mensual), by = 3)) {
        if (i + 2 <= length(inflacion_mensual)) {
            trimestre <- inflacion_mensual[i:(i + 2)]
            inflacion_trimestral <- c(inflacion_trimestral, mean(trimestre))
        } else {
            # Manejar el caso de un trimestre incompleto al final
            trimestre <- inflacion_mensual[i:length(inflacion_mensual)]
            inflacion_trimestral <- c(inflacion_trimestral, mean(trimestre))
        }
    }
    return(inflacion_trimestral)
}
inflacion_trimestral_promedio(inflacion_mensual)
    

# 4) Defina otra funcion que calcula la inflacion trimestral usando el 
#    ULTIMO MES de cada trimestre

inflacion_trimestral_final <- function(inflacion_mensual) {
    inflacion_mensual1 <- inflacion_mensual[!is.na(inflacion_mensual)]
    inflacion_trimestral <- c()

        for (i in seq(2, length(inflacion_mensual1), by = 3)) {
            inflacion_trimestral = c(inflacion_trimestral, inflacion_mensual1[i])
    } 
    return(inflacion_trimestral)
}
inflacion_trimestral_final(inflacion_mensual)

# 5) Use las anteriores funciones para calcular las variables
#    "inflacion.trimestral.promedio" y "inflacion.trimestral.findemes"
#    correspondientes a cada metodo
inflacion_trimestral_promedio <-  inflacion_trimestral_promedio(inflacion_mensual)
inflacion_trimestral_findemes <- inflacion_trimestral_final(inflacion_mensual)

inflacion_trimestral_promedio
inflacion_trimestral_findemes
# 6) En una misma figura, muestre la grafica de cada una de las dos variables
#    calculadas en el paso anterior para comparar los resultados de cada metodo
#
## Pista: Los resultados deven ser iguales a los de la hoja ".csv" en el 
# mismo archivo CPI.csv"

x11() #PARA CREAR UNA VENTANA NUEVA
plot(
    inflacion_trimestral_promedio,
    col="blue", 
    type = "l",
    xlab = "Trimestres", 
    ylab = "Inflación Trimestral", 
    main = "Inflación Trimestral: Promedio vs Último Mes"
)
legend("topright", 
    legend = c("Promedio Trimestral", "Último Mes Trimestral"), 
    col = c("blue", "red"), 
    lty = 1
)
lines(inflacion_trimestral_findemes,
     col = "red",
     type = "l"
)

################################################################################
# 3) Github e intereses de ustedes
################################################################################

# 1) Lea esta breve introduccion a Github
# https://conociendogithub.readthedocs.io/en/latest/
# Check: https://guides.github.com/activities/hello-world/ 

# 2) Cree una cuenta de github y escriba aqui el usuario de cada integrante
# del grupo:
# - Usuario: nuriast (Nuria Saraí Torres Aldana)
# - Usuario: adelcid98-d98 (David Alejandro Del Cid De León)
# - Usuario: dieand1998 (Diego Andrés Menendez Barillas)
#
# 3) Escriba aca las areas de interes en economia de cada integrante
# (microeconomia teorica, macroeconomia aplicada, macroeconometria,
#  microeconometria, econometria, finanzas, etc... )
# - Nuria Saraí Torres Aldana: Microeconomía.
# - David Alejandro Del Cid De León: Macroeconomía aplicada, estadística y finanzas.
# - Diego Andrés Menendez Barillas: Macroeconomía aplicada y econometría.
#
# 4) Escriba aca algun topico de interes que le llamaria la atencion aprender
#    durante el curso
#  Modelos de regresión y su aplicación en finanzas.
