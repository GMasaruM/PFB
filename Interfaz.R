# Carga de librería
#install.packages("shiny")
library(shiny)

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Modelo cinético de fermentación en chicha de jora: Impacto de la amilasa"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("temp", "Temperatura de fermentación (°C):", value = 25, min = 15, max = 40)
    ),
    
    mainPanel(
      h3("Predicción de parámetros fermentativos"),
      verbatimTextOutput("resultados"),
      br(),
      h3("Constantes cinéticas de la amilasa enzimáticamente activa"),
      tableOutput("constantes_amilasa")
    )
  )
)

# Lógica del servidor
server <- function(input, output) {
  
  output$resultados <- renderText({
    temp <- input$temp
    if (temp >= 20 && temp <= 24) {
      glucosa <- "30–40 g/L"
      etanol <- "10–20 g/L"
    } else if (temp > 24 && temp <= 28) {
      glucosa <- "15–25 g/L"
      etanol <- "25–35 g/L"
    } else if (temp > 28 && temp <= 32) {
      glucosa <- "20–35 g/L"
      etanol <- "15–25 g/L"
    } else {
      glucosa <- "Fuera de rango experimental"
      etanol <- "Fuera de rango experimental"
    }
    
    paste("Glucosa restante estimada: ", glucosa, "\nEtanol producido estimado: ", etanol)
  })
  
  output$constantes_amilasa <- renderTable({
    data.frame(
      Parámetro = c("Km (afinidad por el sustrato)", "Vmax (velocidad máxima)"),
      Valor = c("2.5 mM", "120 µmol/min·mg")
    )
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)


