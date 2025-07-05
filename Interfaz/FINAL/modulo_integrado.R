

integradoUI <- function(id) {
  ns <- NS(id) # Namespace para IDs
  
  fluidPage(
    titlePanel("Análisis Integrado de Parámetros de Fermentación"),
    
    fluidRow(
      column(3, # Columna para las opciones (3 de 12 unidades de ancho)
             h4("Opciones de Visualización"),
             selectInput(ns("grupo_filtro"),
                         "Filtrar por Grupo:",
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
             # Nuevo radioButtons para el tipo de visualización de SD
             conditionalPanel(
               condition = paste0("input['", ns("mostrar_sd"), "'] == true"), # Solo mostrar si "mostrar_sd" está marcado
               radioButtons(ns("tipo_sd_visualizacion"),
                            "Tipo de Visualización de SD:",
                            choices = c("Área (banda sombreada)", "Barras de Error"),
                            selected = "Área (banda sombreada)")
             ),
             helpText("Nota: La desviación estándar representa ±1 SD de la media de los replicados experimentales.")
      ),
      
      column(9, # Columna para el gráfico principal (9 de 12 unidades de ancho)
             helpText("Este gráfico combina los resultados de Glucosa, Amilasa y Etanol. Asegúrate de presionar 'Calcular' en cada pestaña individual para que los datos estén disponibles aquí."),
             hr(),
             plotlyOutput(ns("grafico_combinado"), height = "800px") # Alto aumentado para mejor visibilidad
      )
    )
  )
}

integradoServer <- function(id, datos_etanol_r, datos_glucosa_r, datos_amilasa_r) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Combinar todos los datos reactivos en un solo dataframe
    datos_combinados_crudos <- reactive({
      # Definir las columnas esperadas en los dataframes de los módulos
      cols_expected <- c("Grupo", "Tiempo_fermentacion", "Valor", "Valor_sd", "TipoMedicion")
      
      # Función para estandarizar dataframes y asegurar las columnas necesarias
      standardize_df <- function(df, type_name) {
        if (!is.null(df) && nrow(df) > 0) {
          # Si 'Valor_sd' no existe en el dataframe del módulo, añadirla como NA
          if (!"Valor_sd" %in% names(df)) {
            df$Valor_sd <- NA_real_
          }
          # Seleccionar y reordenar las columnas esperadas, asegurando que estén presentes
          df_standardized <- df %>%
            select(any_of(cols_expected)) # Selecciona solo las columnas que existen y son esperadas
          
          # Añadir TipoMedicion (se añade aquí para sobrescribir cualquier TipoMedicion preexistente)
          df_standardized <- df_standardized %>% mutate(TipoMedicion = type_name)
          
          # Asegurarse de que todas las columnas esperadas estén presentes, añadiendo NA si faltan
          missing_cols <- setdiff(cols_expected, names(df_standardized))
          for(col_name in missing_cols) {
            if (col_name %in% c("Grupo", "TipoMedicion")) {
              df_standardized[[col_name]] <- NA_character_ # Columnas de texto
            } else {
              df_standardized[[col_name]] <- NA_real_ # Columnas numéricas
            }
          }
          df_standardized <- df_standardized %>% select(all_of(cols_expected)) # Reordenar a la secuencia estándar
          
        } else {
          # Si el dataframe es NULL o vacío, retornar un dataframe vacío con la estructura correcta
          empty_df <- data.frame(matrix(ncol = length(cols_expected), nrow = 0))
          colnames(empty_df) <- cols_expected
          empty_df$Grupo <- character(0) # Asegurar tipos de columnas
          empty_df$Tiempo_fermentacion <- numeric(0)
          empty_df$Valor <- numeric(0)
          empty_df$Valor_sd <- numeric(0)
          empty_df$TipoMedicion <- character(0)
          empty_df$TipoMedicion <- type_name # Asegurar el tipo para que el filtro funcione incluso en DFs vacíos
          df_standardized <- empty_df
        }
        return(df_standardized)
      }
      
      # Obtener los datos de cada fuente reactiva (pueden ser NULL o vacíos)
      df_etanol_data <- datos_etanol_r()
      df_glucosa_data <- datos_glucosa_r()
      df_amilasa_data <- datos_amilasa_r()
      
      # Estandarizar y combinar dataframes
      df_etanol_s <- standardize_df(df_etanol_data, "Etanol")
      df_glucosa_s <- standardize_df(df_glucosa_data, "Glucosa")
      df_amilasa_s <- standardize_df(df_amilasa_data, "Amilasa")
      
      # Combinar todos los dataframes estandarizados
      combined_df <- bind_rows(df_etanol_s, df_glucosa_s, df_amilasa_s)
      
      # Filtrar filas donde 'Valor' sea NA (datos no producidos por un módulo)
      combined_df <- combined_df %>% filter(!is.na(Valor))
      
      return(combined_df) # Esto puede devolver un dataframe vacío si ningún módulo ha producido datos válidos
    })
    
    # 2. Filtrar y preparar datos basados en la selección del usuario
    datos_filtrados <- reactive({
      df <- datos_combinados_crudos()
      
      # Filtrar por grupo
      if (input$grupo_filtro != "Todos") {
        df <- df %>% filter(Grupo == input$grupo_filtro)
      }
      
      # Filtrar por parámetro (TipoMedicion)
      df <- df %>% filter(TipoMedicion %in% input$parametro_filtro)
      
      # Convertir a factores para un orden y leyendas consistentes en ggplot
      df$TipoMedicion <- factor(df$TipoMedicion, levels = c("Glucosa", "Amilasa", "Etanol"))
      df$Grupo <- factor(df$Grupo, levels = c("A", "B")) # Asumiendo que siempre son A y B
      
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
      
      # Mensaje si no hay datos para mostrar
      if (nrow(df_plot) == 0) {
        return(plotly_empty() %>%
                 layout(title = "No hay datos para mostrar con los filtros seleccionados",
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
      }
      
      # Crear el gráfico base
      # Group = interaction(Grupo, TipoMedicion) es crucial para que las líneas se dibujen correctamente
      # 'color = Grupo' mapea los colores a los grupos A y B, lo cual controlará la leyenda principal
      p <- ggplot(df_plot, aes(x = Tiempo_fermentacion, y = Valor, color = Grupo, group = interaction(Grupo, TipoMedicion))) +
        # Añadir las capas de desviación estándar condicionalmente
        {
          if (input$mostrar_sd) {
            if (input$tipo_sd_visualizacion == "Área (banda sombreada)") {
              # show.legend = FALSE para que la capa de área no cree su propia entrada en la leyenda
              geom_ribbon(aes(ymin = Valor - Valor_sd, ymax = Valor + Valor_sd, fill = Grupo),
                          alpha = 0.2, na.rm = TRUE, show.legend = FALSE)
            } else if (input$tipo_sd_visualizacion == "Barras de Error") {
              # show.legend = FALSE para que las barras de error no creen su propia entrada en la leyenda
              geom_errorbar(aes(ymin = Valor - Valor_sd, ymax = Valor + Valor_sd),
                            width = 0.5, size = 0.5, na.rm = TRUE, show.legend = FALSE)
            } else {
              NULL # En caso de selección inválida (no debería pasar con radioButtons)
            }
          } else {
            NULL # No SD visualization
          }
        } +
        # Líneas y puntos. Estos usarán 'color = Grupo' del mapeo global para la leyenda principal.
        # Al no tener un 'group' o 'color' específico aquí, se hereda del 'ggplot()' global.
        geom_line(size = 1) +
        geom_point(size = 2.5) +
        # Facetas con etiquetas de eje Y dinámicas
        facet_wrap(~ TipoMedicion, scales = "free_y", ncol = 1, labeller = as_labeller(y_axis_labels)) +
        labs(title = "Evolución de Parámetros Clave Durante la Fermentación",
             x = "Tiempo (min)",
             y = "Valor") + # Etiqueta genérica, la específica la da facet_wrap
        theme_bw(base_size = 14) +
        theme(
          strip.text = element_text(face = "bold"),
          legend.position = "top",
          plot.title = element_text(hjust = 0.5) # Centrar título del gráfico
        ) +
        # EXPLICITAMENTE suprimir la leyenda de `fill` si existe (para geom_ribbon)
        # Esto es crucial para que Plotly no duplique la leyenda de relleno si la detecta.
        guides(fill = "none")
      
      
      # Convertir a Plotly. Este paso es el que a veces introduce leyendas inesperadas.
      # Con las previas configuraciones (show.legend = FALSE y guides(fill = "none")),
      # debería mostrar solo la leyenda de 'color' (Grupos A y B).
      ggplotly(p) %>% config(displaylogo = FALSE)
    })
  })
}