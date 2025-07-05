# app.R

# --- Cargar librerías principales ---
library(shiny)
library(dplyr)
library(tidyr)

# Para la curva de calibración interactiva
library(plotly) # Necesario para los gráficos de los módulos
library(stringr) # Necesario para str_extract
library(patchwork) # Para combinar gráficos de consumo si hay múltiples grupos
# modulo_integrado.R

library(scales) # Para labeller con scales


# --- Cargar los archivos de los módulos ---
source("modulo_glucosa.R")
source("modulo_etanol.R")
source("modulo_amilasa.R")
source("modulo_integrado.R")

# --- Interfaz de Usuario (UI) ---
ui <- navbarPage(
  "App de Análisis de Fermentación",
  id = "main_nav",
  theme = bs_theme(version = 5, bootswatch = "litera"),
  
  tabPanel("Glucosa", glucosaUI("glucosa_mod")),
  tabPanel("Etanol", etanolUI("etanol_mod")),
  tabPanel("Amilasa", amilasaUI("amilasa_mod")),
  tabPanel("Análisis Integrado", integradoUI("integrado_mod"))
)

# --- Servidor (Server) ---
server <- function(input, output, session) {
  
  # Llamar a los servidores de cada módulo y capturar los datos que devuelven
  glucosa_data_r <- glucosaServer("glucosa_mod")
  etanol_data_r  <- etanolServer("etanol_mod")
  amilasa_data_r <- amilasaServer("amilasa_mod")
  
  # Pasar esos datos reactivos al servidor del módulo integrado
  integradoServer(
    "integrado_mod",
    datos_glucosa_r = glucosa_data_r,
    datos_etanol_r  = etanol_data_r,
    datos_amilasa_r = amilasa_data_r
  )
}

# --- Ejecutar la aplicación ---
shinyApp(ui, server)


