# app.R
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
# Para la curva de calibración interactiva
library(plotly) # Necesario para los gráficos de los módulos
library(stringr) # Necesario para str_extract
library(patchwork) # Para combinar gráficos de consumo si hay múltiples grupos
# modulo_integrado.R

library(scales) # Para labeller con scales



# 1. Cargar todos los módulos
source("modulo_etanol.R")
source("modulo_glucosa.R")
source("modulo_amilasa.R")
source("modulo_integrado.R")

# 2. UI principal con navbarPage
ui <- navbarPage(
  title = "Panel de Análisis Bioquímico",
  tabPanel("Glucosa", glucosaUI("glucosa_app")),
  tabPanel("Amilasa", amilasaUI("amilasa_app")),
  tabPanel("Etanol", etanolUI("etanol_app")),
  tabPanel("Análisis Integrado", integradoUI("integrado_app"))
)

# 3. Server principal que conecta los módulos
server <- function(input, output, session) {
  datos_glucosa_reactivos <- glucosaServer("glucosa_app")
  datos_amilasa_reactivos <- amilasaServer("amilasa_app")
  datos_etanol_reactivos <- etanolServer("etanol_app")
  
  integradoServer("integrado_app", 
                  datos_etanol_r = datos_etanol_reactivos,
                  datos_glucosa_r = datos_glucosa_reactivos,
                  datos_amilasa_r = datos_amilasa_reactivos)
}

shinyApp(ui, server)