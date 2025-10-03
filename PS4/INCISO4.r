# --- 1. PREPARACIÓN DEL ENTORNO ---
#
# Cargamos la librería ggplot2, que es el estándar de oro para la visualización
# de datos en R. Si no la tienes instalada, ejecuta: install.packages("ggplot2")
library(ggplot2)

# --- 2. CARGA Y PREPARACIÓN DE DATOS ---
#
# Para este ejemplo, usamos el dataset 'faithful' que ya viene incluido en R.
# Extraemos la columna 'waiting' que contiene los datos que vamos a graficar.
#
# SI QUIERES USAR TUS PROPIOS DATOS:
# Reemplaza esta sección con tu código para cargar datos, por ejemplo:
# mi_df <- read.csv("ruta/a/tus/datos.csv")
# datos_para_graficar <- mi_df$tu_columna
datos_para_graficar <- faithful$waiting

# (Opcional) Creamos un data.frame. ggplot2 funciona mejor cuando los
# datos están dentro de un data.frame.
df <- data.frame(valores = datos_para_graficar)


# --- 3. CREACIÓN DEL GRÁFICO CON ggplot2 ---
#
# ggplot2 funciona construyendo el gráfico capa por capa.
#
# ggplot(data = df, aes(x = valores))
#   - Inicia el objeto del gráfico.
#   - `data = df`: Especifica el data.frame que contiene los datos.
#   - `aes(x = valores)`: Define la "estética" (aesthetics). Aquí le decimos
#     que la variable 'valores' se mapeará al eje X.

histograma <- ggplot(data = df, aes(x = valores)) +
  
  # --- Capa 1: Histograma ---
  # `geom_histogram()` es la capa que dibuja el histograma.
  geom_histogram(
    aes(y = ..density..), # Mapea el eje Y a la densidad en lugar de la frecuencia (para poder superponer la curva)
    bins = 30,            # Número de barras (puedes ajustarlo)
    fill = "#4e79a7",     # Color de relleno de las barras (un azul suave)
    color = "white",      # Color del borde de las barras
    alpha = 0.8           # Transparencia de las barras (para que se vea la curva de densidad)
  ) +
  
  # --- Capa 2: Curva de Densidad ---
  # `geom_density()` añade una curva suavizada que muestra la distribución.
  geom_density(
    color = "#f28e2b",    # Color de la línea (un naranja contrastante)
    linewidth = 1.2       # Grosor de la línea para que destaque
  ) +
  
  # --- Capa 3: Líneas Verticales de Referencia ---
  # `geom_vline()` dibuja líneas verticales. Son útiles para marcar estadísticas.
  geom_vline(
    aes(xintercept = mean(valores)), # Ubica la línea en la media de los datos
    color = "#e15759",              # Color de la línea (un rojo)
    linetype = "dashed",            # Estilo de la línea (punteada)
    linewidth = 1                     # Grosor de la línea
  ) +
  
  # --- Capa 4: Anotaciones de Texto ---
  # `annotate()` nos permite añadir texto directamente en el gráfico.
  annotate(
    "text",
    x = mean(df$valores) * 1.05, # Posición X del texto (un poco a la derecha de la línea)
    y = 0.03,                    # Posición Y del texto (ajústala según tus datos)
    label = paste("Media =", round(mean(df$valores), 1)), # El texto a mostrar
    color = "#e15759",           # Color del texto (coincide con la línea)
    fontface = "bold"            # Estilo de la fuente
  ) +
  
  # --- 4. DISEÑO Y ETIQUETAS ---
  #
  # `labs()` se usa para definir todos los títulos y etiquetas.
  labs(
    title = "Distribución de los Tiempos de Espera del Géiser 'Old Faithful'",
    subtitle = "Histograma con curva de densidad y media de la muestra",
    x = "Tiempo de Espera (minutos)",
    y = "Densidad",
    caption = "Fuente: Dataset 'faithful' de R"
  ) +
  
  # `theme_minimal()` aplica un tema limpio y moderno.
  # `theme()` nos permite personalizar detalles finos del tema.
  theme_minimal(base_size = 14) + # Aumenta el tamaño base de la fuente
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#333333"),
    plot.subtitle = element_text(size = 12, color = "#555555"),
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(italic = TRUE, color = "grey40")
  )

# --- 5. MOSTRAR EL GRÁFICO ---
#
# Al final, simplemente "imprimimos" el objeto del gráfico que hemos construido.
print(histograma)
