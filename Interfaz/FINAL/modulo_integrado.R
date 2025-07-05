# modulo_integrado.R
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(scales) # Para labeller con scales

integradoUI <- function(id) {
  ns <- NS(id) # Namespace para IDs
  
  fluidPage(
    titlePanel("Análisis Integrado de Parámetros de Fermentación"),
    sidebarLayout(
      sidebarPanel(
        h4("Opciones de Visualización"),
        selectInput(ns("grupo_filtro"),
                    "Seleccionar Grupo:",
                    choices = c("Todos", "A", "B"),
                    selected = "Todos"),
        hr(),
        checkboxGroupInput(ns("parametro_filtro"),
                           "Mostrar Parámetro(s):",
                           choices = c("Glucosa", "Amilasa", "Etanol"),
                           selected = c("Glucosa", "Amilasa", "Etanol")),
        hr(),
        checkboxInput(ns("mostrar_sd"),
                      "Mostrar Desviación Estándar",
                      value = TRUE),
        helpText("Nota: La desviación estándar se muestra como una banda sombreada alrededor de la media.")
      ),
      mainPanel(
        width = 9, # Ajusta el ancho del panel principal si es necesario
        helpText("Este gráfico combina los resultados de Glucosa, Amilasa y Etanol. Asegúrate de presionar 'Calcular' en cada pestaña individual para que los datos estén disponibles aquí."),
        hr(),
        plotlyOutput(ns("grafico_combinado"), height = "800px") # Aumentado el alto para mejor visualización
      )
    )
  )
}

integradoServer <- function(id, datos_etanol_r, datos_glucosa_r, datos_amilasa_r) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Combinar todos los datos reactivos en un solo dataframe
    datos_combinados_crudos <- reactive({
      # Asegurarse de que todos los módulos hayan cargado datos al menos una vez
      req(datos_etanol_r(), datos_glucosa_r(), datos_amilasa_r())
      
      df_etanol <- datos_etanol_r()
      df_glucosa <- datos_glucosa_r()
      df_amilasa <- datos_amilasa_r()
      
      # Asegurarse de que todas las columnas necesarias existan, incluso si es NA
      # Esto es crucial si algún módulo aún no devuelve Valor_sd
      cols_to_check <- c("Grupo", "Tiempo_fermentacion", "Valor", "Valor_sd", "TipoMedicion")
      
      # Función para estandarizar dataframes
      standardize_df <- function(df, type_name) {
        if ("Tiempo_fermentacion" %in% names(df) && "Valor" %in% names(df) && "Grupo" %in% names(df)) {
          # Si Valor_sd no existe, añadirla como NA
          if (!"Valor_sd" %in% names(df)) {
            df$Valor_sd <- NA_real_
          }
          df <- df %>%
            select(all_of(cols_to_check)) %>%
            mutate(TipoMedicion = type_name)
        } else {
          # Retornar un dataframe vacío o con NA si no cumple los requisitos mínimos
          data.frame(Grupo = character(), Tiempo_fermentacion = numeric(), Valor = numeric(), Valor_sd = numeric(), TipoMedicion = character())
        }
        return(df)
      }
      
      df_etanol_s <- standardize_df(df_etanol, "Etanol")
      df_glucosa_s <- standardize_df(df_glucosa, "Glucosa")
      df_amilasa_s <- standardize_df(df_amilasa, "Amilasa")
      
      # Combinar los dataframes estandarizados
      combined_df <- bind_rows(df_etanol_s, df_glucosa_s, df_amilasa_s)
      
      # Filtrar filas donde Valor sea NA si eso ocurre por módulos vacíos
      combined_df %>% filter(!is.na(Valor))
    })
    
    # 2. Filtrar datos basados en la selección del usuario
    datos_filtrados <- reactive({
      df <- datos_combinados_crudos()
      
      # Filtrar por grupo
      if (input$grupo_filtro != "Todos") {
        df <- df %>% filter(Grupo == input$grupo_filtro)
      }
      
      # Filtrar por parámetro (TipoMedicion)
      df <- df %>% filter(TipoMedicion %in% input$parametro_filtro)
      
      return(df)
    })
    
    # 3. Mapeo de etiquetas para el eje Y dinámico
    y_axis_labels <- c(
      "Glucosa" = "Concentración (µM)",
      "Amilasa" = "Actividad (U/L)",
      "Etanol" = "Concentración (%)"
    )
    
    # 4. Generar el gráfico combinado
    output$grafico_combinado <- renderPlotly({
      df_plot <- datos_filtrados()
      req(nrow(df_plot) > 0) # Asegurarse de que haya datos para graficar
      
      # Crear una función labeller personalizada para el eje Y
      custom_labeller <- function(variable, value) {
        y_axis_labels[value]
      }
      
      p <- ggplot(df_plot, aes(x = Tiempo_fermentacion, y = Valor, color = Grupo, group = Grupo)) +
        # Añadir geom_ribbon para la desviación estándar si se selecciona
        {
          if (input$mostrar_sd) {
            # Filtrar NA de Valor_sd antes de pasar a geom_ribbon para evitar errores
            geom_ribbon(aes(ymin = Valor - Valor_sd, ymax = Valor + Valor_sd, fill = Grupo), alpha = 0.2, na.rm = TRUE)
          } else {
            NULL # No añadir geom_ribbon
          }
        } +
        geom_line(size = 1) +
        geom_point(size = 2.5) +
        facet_wrap(~ TipoMedicion, scales = "free_y", ncol = 1, labeller = as_labeller(y_axis_labels)) + # labeller para etiquetas dinámicas
        labs(title = "Evolución de Parámetros Clave Durante la Fermentación",
             x = "Tiempo (min)", # Asumiendo que Tiempo_fermentacion está en minutos
             y = "Valor") + # Etiqueta genérica, ya que la específica la da facet_wrap
        theme_bw(base_size = 14) +
        theme(
          strip.text = element_text(face = "bold"),
          legend.position = "top",
          plot.title = element_text(hjust = 0.5)
        )
      
      ggplotly(p) %>% config(displaylogo = FALSE)
    })
  })
}