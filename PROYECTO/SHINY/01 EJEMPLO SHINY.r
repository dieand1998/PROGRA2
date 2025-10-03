# Cargar la librería shiny
library(shiny)

# 1. Definir la Interfaz de Usuario (UI)
ui <- fluidPage(

  # Título de la aplicación
  titlePanel("Histograma del dataset Faithful"),

  # Diseño con una barra lateral y un panel principal
  sidebarLayout(
    
    # Panel de la barra lateral con los controles de entrada
    sidebarPanel(
      # Deslizador para controlar el número de bins
      sliderInput(inputId = "bins",              # El ID único
                  label = "Número de barras:",   # El texto que ve el usuario
                  min = 1,
                  max = 50,
                  value = 30)                    # Valor inicial
    ),

    # Panel principal donde se mostrarán las salidas
    mainPanel(
      # Espacio reservado para nuestro gráfico
      plotOutput(outputId = "distPlot")
    )
  )
)

# 2. Definir la lógica del Servidor
server <- function(input, output, session) {

  # Conectar nuestro gráfico (salida) con el contenedor de la UI
  output$distPlot <- renderPlot({
    
    # Accedemos a la columna 'waiting' del dataset faithful
    x <- faithful$waiting
    
    # El código aquí dentro es REACTIVO.
    # Depende de input$bins. Cada vez que input$bins cambie,
    # este bloque de código se ejecutará de nuevo.
    
    # Generamos el histograma
    hist(x, 
         breaks = input$bins, # El número de barras viene del deslizador
         col = "#75AADB", 
         border = "white",
         xlab = "Tiempo de espera entre erupciones (min)",
         main = "Histograma de tiempos de espera")
  })
}

# 3. Correr la aplicación
shinyApp(ui = ui, server = server)