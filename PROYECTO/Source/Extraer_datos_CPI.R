# ==============================================================================
# Script para Filtrar IPC República (Código 0) y Extraer Columnas Regionales
#
# Descripción:
# Este script carga los datos del IPC de múltiples años, filtra para
# conservar únicamente las filas correspondientes al nivel nacional ("República",
# Código 0), y extrae las columnas de Año, Mes, el índice nacional (Rep) y los
# índices de las ocho regiones.
#
# ==============================================================================

# ------------------------------------------------------------------------------
# 0) Inicialización y Carga de Librerías
# ------------------------------------------------------------------------------

# Cargar las librerías necesarias
library(dplyr)
library(readr)
library(readxl)
library(magrittr)

# Limpiar el entorno de trabajo, gráficos y consola
graphics.off()
rm(list = ls())
cat("\014")


# ------------------------------------------------------------------------------
# 1) Carga de Todos los Datos Anuales
# ------------------------------------------------------------------------------

# NOTA: Asegúrate de tener la carpeta "./input" con los archivos Excel del IPC.

# Nombres de las columnas originales en los archivos Excel
def_names <- c("t_year", "t_month", "id_item", "descr", "ipc", "ipc_r_1",
               "ipc_r_2", "ipc_r_3", "ipc_r_4", "ipc_r_5", "ipc_r_6",
               "ipc_r_7", "ipc_r_8")

# Tipos de datos para cada columna para una carga correcta
def_type <- c("numeric", "text", "text", "text", "numeric", "numeric",
              "numeric", "numeric", "numeric", "numeric", "numeric",
              "numeric", "numeric")

# Definir el rango de años a procesar
start_year <- 2022
end_year   <- 2024

# Crear una lista vacía para almacenar los data frames completos de cada año
lista_datos_completos <- list()

# Bucle para leer cada archivo y guardarlo en la lista
for (iYear in start_year:end_year) {
    file_path <- paste0("input/IPC_", iYear, ".xls")
    cat(paste("Cargando archivo:", file_path, "\n"))
    
    # Leer el archivo y agregarlo a la lista
    lista_datos_completos[[as.character(iYear)]] <- read_excel(
        file_path, col_types = def_type, col_names = def_names
    )
}


# ------------------------------------------------------------------------------
# 2) Consolidar, Filtrar y Seleccionar
# ------------------------------------------------------------------------------

# Unir todos los data frames de la lista en una sola tabla grande
datos_consolidados <- bind_rows(lista_datos_completos)

# Eliminar las filas completamente vacías
datos_consolidados %<>% filter(!is.na(t_year))

# -- INICIO DE LA LÓGICA CLAVE --
# Proceso final: Filtrar por Código "0" y luego seleccionar las columnas deseadas
datos_finales <- datos_consolidados %>%
    
    # Paso 1: Filtrar para obtener solo el IPC General (Código "0").
    # Se incluye is.na(id_item) porque en algunos archivos (ej. 2025)
    # el código para la República viene como NA en lugar de "0".
    filter(id_item == "0" | is.na(id_item)) %>%
    
    # Paso 2: Seleccionar y renombrar las columnas de interés.
    # No incluimos la columna de Código porque ya sabemos que todas son "0".
    select(
        Anio = t_year,
        Mes = t_month,
        Rep = ipc,
        `Reg I` = ipc_r_1,
        `Reg II` = ipc_r_2,
        `Reg III` = ipc_r_3,
        `Reg IV` = ipc_r_4,
        `Reg V` = ipc_r_5,
        `Reg VI` = ipc_r_6,
        `Reg VII` = ipc_r_7,
        `Reg VIII` = ipc_r_8
    ) %>% 
    
    # Paso 3: Ordenar por año para asegurar un orden cronológico.
    arrange(Anio)

# -- FIN DE LA LÓGICA CLAVE --


# ------------------------------------------------------------------------------
# 3) Inspeccionar y Exportar el Resultado Final
# ------------------------------------------------------------------------------

cat("\nEstructura de la base de datos final (filtrada y seleccionada):\n")
glimpse(datos_finales)

cat("\nPrimeras 6 filas de los datos:\n")
print(head(datos_finales))

cat("\nÚltimas 6 filas de los datos:\n")
print(tail(datos_finales))

# Crear el directorio de salida si no existe
if (!dir.exists("./output")) {
    dir.create("./output")
}

# Exportar la base de datos final a un archivo CSV
write_csv(datos_finales, "./output/ipc_republica_y_regiones.csv")

cat("\n¡Proceso completado! ✅\n")
cat("La base de datos filtrada se ha guardado en: './output/ipc_republica_y_regiones.csv'\n")

