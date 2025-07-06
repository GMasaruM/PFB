# Cargar librerías necesarias
library(shiny)
library(dplyr)
library(tidyr)
library(plotly)

# -----------------------------------------------------------------------------
# Interfaz de Usuario (UI) - SIN CAMBIOS
# -----------------------------------------------------------------------------
etanolUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Carga de Datos de Etanol"),
      fileInput(ns("file_datos"), "Selecciona archivo CSV único:", accept = c(".csv")),
      helpText(HTML("El CSV debe contener: <b>Tipo, Muestra_ID, Tiempo_fermentacion, concentracion_etanol, OD1, OD2</b>.")),
      hr(),
      h4("2. Parámetros de Cálculo"),
      numericInput(ns("DF"), "Factor de dilución (DF):", value = 50, min = 1, step = 1),
      numericInput(ns("cantidad_sustrato"), "Cantidad de Sustrato Utilizado (g):", value = 100, min = 1, step = 1),
      hr(),
      actionButton(ns("calcular"), "Calcular y Graficar", class = "btn-primary", icon = icon("cogs"))
    ),
    mainPanel(
      tabsetPanel(
        id = ns("tabs"),
        tabPanel("Resultados Tabulados", uiOutput(ns("tablas_resultados_ui"))),
        tabPanel("Parámetros Cinéticos", uiOutput(ns("parametros_ui"))),
        tabPanel("Gráficas",
                 h4("Curva de Calibración"), 
                 plotlyOutput(ns("plotCurvaCalibracion")),
                 hr(),
                 h4("Absorbancia vs Concentración"),
                 fluidRow(
                   column(3, checkboxInput(ns("show_prom_abs"), "Promedio", value = TRUE)),
                   column(3, checkboxInput(ns("show_desv_abs"), "Desv. Est.", value = TRUE)),
                   column(3, checkboxInput(ns("show_od1_abs"), "Réplicas OD1", value = FALSE)),
                   column(3, checkboxInput(ns("show_od2_abs"), "Réplicas OD2", value = FALSE))
                 ),
                 plotlyOutput(ns("plotAbsorbanciaMuestras")),
                 hr(),
                 h4("Concentración de Etanol vs Tiempo"),
                 fluidRow(
                   column(3, checkboxInput(ns("show_prom_conc"), "Promedio", value = TRUE)),
                   column(3, checkboxInput(ns("show_desv_conc"), "Desv. Est.", value = TRUE)),
                   column(3, checkboxInput(ns("show_od1_conc"), "Réplicas OD1", value = FALSE)),
                   column(3, checkboxInput(ns("show_od2_conc"), "Réplicas OD2", value = FALSE))
                 ),
                 plotlyOutput(ns("plotConcentracionEtanolTiempo"))
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
# Lógica del Servidor con la corrección
# -----------------------------------------------------------------------------
etanolServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # --- Lógica de cálculo (sin cambios) ---
    datos_procesados <- eventReactive(input$calcular, {
      req(input$file_datos)
      df_raw <- read.csv(input$file_datos$datapath, header = TRUE, stringsAsFactors = FALSE)
      cols_requeridas <- c("Tipo", "Muestra_ID", "Tiempo_fermentacion", "concentracion_etanol", "OD1", "OD2")
      validate(need(all(cols_requeridas %in% names(df_raw)), paste("Error: Faltan columnas. Se requieren:", paste(cols_requeridas, collapse = ", "))))
      
      blancos <- df_raw %>% filter(Tipo == "Blanco")
      validate(need(nrow(blancos) > 0, "Error: No se encontraron 'Blancos'."))
      OD_blanco_promedio <- mean(c(blancos$OD1, blancos$OD2), na.rm = TRUE)
      
      estandares <- df_raw %>% filter(Tipo == "Estandar")
      validate(need(nrow(estandares) > 0, "Error: No se encontraron 'Estandares'."))
      
      curva_calib <- estandares %>% 
        pivot_longer(cols = c(OD1, OD2), names_to = "Replica", values_to = "OD") %>%
        group_by(concentracion_etanol) %>%
        summarise(OD_promedio = mean(OD, na.rm = TRUE), .groups = 'drop') %>%
        bind_rows(data.frame(concentracion_etanol = 0, OD_promedio = OD_blanco_promedio))
      
      modelo_calib <- lm(OD_promedio ~ concentracion_etanol, data = curva_calib)
      Slope <- coef(modelo_calib)[2]
      Intercept <- coef(modelo_calib)[1]
      R_squared <- summary(modelo_calib)$r.squared
      
      muestras <- df_raw %>% filter(Tipo == "Muestra")
      validate(need(nrow(muestras) > 0, "Error: No se encontraron 'Muestras'."))
      
      df_resultados <- muestras %>%
        mutate(
          Grupo = gsub("[0-9].*$", "", Muestra_ID), 
          Concentracion_OD1 = ((OD1 - Intercept) / Slope) * input$DF,
          Concentracion_OD2 = ((OD2 - Intercept) / Slope) * input$DF
        ) %>%
        rowwise() %>%
        mutate(
          Concentracion_promedio = mean(c(Concentracion_OD1, Concentracion_OD2)),
          Concentracion_desv_std = sd(c(Concentracion_OD1, Concentracion_OD2)),
          OD_promedio = mean(c(OD1, OD2)),
          OD_desv_std = sd(c(OD1, OD2))
        ) %>%
        ungroup() %>%
        arrange(Grupo, Tiempo_fermentacion, Muestra_ID)
      
      validate(need(length(unique(df_resultados$Grupo)) > 0, "No se pudo identificar ningún grupo a partir de los prefijos de Muestra_ID."))
      
      return(list(df_final = df_resultados, curva_calib = curva_calib, Slope = Slope, Intercept = Intercept, R_squared = R_squared))
    })
    
    # Renderizado de tablas de resultados (sin cambios)
    output$tablas_resultados_ui <- renderUI({ req(datos_procesados()); lapply(unique(datos_procesados()$df_final$Grupo), function(g) { ns <- session$ns; tagList(h4(paste("Resultados para el Grupo:", g)), tableOutput(ns(paste0("tabla_", g))), hr()) }) })
    observe({ req(datos_procesados()); df_final <- datos_procesados()$df_final; lapply(unique(df_final$Grupo), function(g) { output[[paste0("tabla_", g)]] <- renderTable({ df_final %>% filter(Grupo == g) %>% select(Muestra_ID, Tiempo_fermentacion, OD1, OD2, OD_promedio, OD_desv_std, Concentracion_OD1, Concentracion_OD2, Concentracion_promedio, Concentracion_desv_std) %>% rename("Muestra" = Muestra_ID, "Tiempo (min)" = Tiempo_fermentacion, "Abs. OD1" = OD1, "Abs. OD2" = OD2, "Abs. Promedio" = OD_promedio, "Desv. Estandar Abs." = OD_desv_std, "Concentracion OD1 (%)" = Concentracion_OD1, "Concentracion OD2 (%)" = Concentracion_OD2, "Concentracion Promedio (%)" = Concentracion_promedio, "Desv. estandar Conc. (%)" = Concentracion_desv_std) %>% mutate(across(where(is.numeric), ~round(., 3))) }, striped = TRUE, hover = TRUE, bordered = TRUE) }) })
    
    
    # --- CAMBIO EN EL SERVIDOR: RENDERIZADO DE TABLAS DE PARÁMETROS ---
    
    parametros_calculados <- eventReactive(input$calcular, {
      req(datos_procesados())
      datos_procesados()$df_final %>%
        group_by(Grupo) %>%
        summarise(
          `Concentración Máxima (A_max, % v/v)` = max(Concentracion_promedio, na.rm = TRUE),
          `Tiempo Óptimo (t_opt, min)` = Tiempo_fermentacion[which.max(Concentracion_promedio)],
          `Rendimiento (g etanol / g sustrato)` = (max(Concentracion_promedio, na.rm=TRUE) * input$DF / 100 * 1000 * 0.789) / input$cantidad_sustrato,
          .groups = "drop"
        )
    })
    
    # UI dinámica para las tablas de parámetros
    output$parametros_ui <- renderUI({
      req(parametros_calculados())
      nombres_grupos <- unique(parametros_calculados()$Grupo)
      
      lapply(nombres_grupos, function(nombre_grupo) {
        ns <- session$ns
        tagList(
          h4(paste("Parámetros Cinéticos para el Grupo:", nombre_grupo)),
          tableOutput(ns(paste0("tabla_param_", nombre_grupo))),
          hr()
        )
      })
    })
    
    # Renderizado de cada tabla de parámetros individualmente
    observe({
      req(parametros_calculados())
      df_parametros <- parametros_calculados()
      nombres_grupos <- unique(df_parametros$Grupo)
      
      for (nombre_grupo in nombres_grupos) {
        local({
          grupo_actual <- nombre_grupo
          
          output[[paste0("tabla_param_", grupo_actual)]] <- renderTable({
            df_parametros %>%
              filter(Grupo == grupo_actual) %>%
              select(-Grupo) %>%
              pivot_longer(everything(), names_to = "Parámetro Cinético", values_to = "Valor") %>%
              mutate(Valor = sprintf("%.3f", as.numeric(Valor)))
          }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = 'l')
        })
      }
    })
    
    # --- Lógica de Gráficos (sin cambios) ---
    output$plotCurvaCalibracion <- renderPlotly({
      req(datos_procesados())
      datos <- datos_procesados()
      ecuacion <- paste0("OD = ", round(datos$Slope, 4), " * Conc. + ", round(datos$Intercept, 4), "<br>R<sup>2</sup> = ", round(datos$R_squared, 4))
      plot_ly(data = datos$curva_calib, x = ~concentracion_etanol, y = ~OD_promedio, type = 'scatter', mode = 'markers', name = "Estándares", marker = list(color = "blue", size = 8), hoverinfo = 'text', text = ~paste0("Conc. Etanol: ", concentracion_etanol, "%<br>Absorbancia: ", round(OD_promedio, 3))) %>%
        add_lines(y = ~fitted(lm(OD_promedio ~ concentracion_etanol, data = datos$curva_calib)), name = 'Regresión', line = list(color = 'red')) %>%
        layout(title = "Curva de Calibración", xaxis = list(title = "Concentración de Etanol (%)"), yaxis = list(title = "Absorbancia (OD)"), annotations = list(x = 0.05, y = 0.95, text = ecuacion, showarrow = FALSE, xref = "paper", yref = "paper", xanchor = 'left', yanchor = 'top')) %>%
        config(displaylogo = FALSE)
    })
    output$plotAbsorbanciaMuestras <- renderPlotly({
      req(datos_procesados())
      df_plot <- datos_procesados()$df_final %>% arrange(Grupo, Concentracion_promedio)
      fig <- plot_ly()
      if (isTRUE(input$show_desv_abs) && isTRUE(input$show_prom_abs)) { fig <- fig %>% add_ribbons(data = df_plot, x = ~Concentracion_promedio, ymin = ~OD_promedio - OD_desv_std, ymax = ~OD_promedio + OD_desv_std, color = ~Grupo, legendgroup = ~Grupo, opacity = 0.2, line = list(width = 0), name = ~paste(Grupo, "SD"), showlegend = FALSE, hoverinfo = 'none') }
      if (isTRUE(input$show_prom_abs)) { fig <- fig %>% add_trace(data = df_plot, x = ~Concentracion_promedio, y = ~OD_promedio, type = 'scatter', mode = 'lines+markers', color = ~Grupo, legendgroup = ~Grupo, name = ~Grupo, line = list(dash = 'dash'), hoverinfo = 'text', text = ~paste0("<b>Grupo: ", Grupo, "</b><br>", "Conc. Media: ", round(Concentracion_promedio, 2), "%<br>", "Abs. Media: ", round(OD_promedio, 3))) }
      if (isTRUE(input$show_od1_abs)) { fig <- fig %>% add_trace(data = df_plot, x = ~Concentracion_promedio, y = ~OD1, type = 'scatter', mode = 'markers', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "OD1"), marker = list(symbol = 'circle-open'), showlegend = FALSE, hoverinfo = 'text', text = ~paste0("<b>Muestra: ", Muestra_ID, " (OD1)</b><br>", "Conc. Media: ", round(Concentracion_promedio, 2), "%<br>", "Absorbancia: ", round(OD1, 3))) }
      if (isTRUE(input$show_od2_abs)) { fig <- fig %>% add_trace(data = df_plot, x = ~Concentracion_promedio, y = ~OD2, type = 'scatter', mode = 'markers', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "OD2"), marker = list(symbol = 'square-open'), showlegend = FALSE, hoverinfo = 'text', text = ~paste0("<b>Muestra: ", Muestra_ID, " (OD2)</b><br>", "Conc. Media: ", round(Concentracion_promedio, 2), "%<br>", "Absorbancia: ", round(OD2, 3))) }
      fig %>% layout(title = "Absorbancia vs Concentración de Etanol", xaxis = list(title = "Concentración de Etanol Promedio (%)"), yaxis = list(title = "Absorbancia (OD)"), legend = list(title = list(text = 'Grupo'))) %>% config(displaylogo = FALSE)
    })
    output$plotConcentracionEtanolTiempo <- renderPlotly({
      req(datos_procesados())
      df_plot <- datos_procesados()$df_final %>% arrange(Grupo, Tiempo_fermentacion)
      fig <- plot_ly()
      if (isTRUE(input$show_desv_conc) && isTRUE(input$show_prom_conc)) { fig <- fig %>% add_ribbons(data = df_plot, x = ~Tiempo_fermentacion, ymin = ~Concentracion_promedio - Concentracion_desv_std, ymax = ~Concentracion_promedio + Concentracion_desv_std, color = ~Grupo, legendgroup = ~Grupo, opacity = 0.2, line = list(width = 0), name = ~paste(Grupo, "SD"), showlegend = FALSE, hoverinfo = 'none') }
      if (isTRUE(input$show_prom_conc)) { fig <- fig %>% add_trace(data = df_plot, x = ~Tiempo_fermentacion, y = ~Concentracion_promedio, type = 'scatter', mode = 'lines+markers', color = ~Grupo, legendgroup = ~Grupo, name = ~Grupo, hoverinfo = 'text', text = ~paste0("<b>Grupo: ", Grupo, "</b><br>", "Tiempo: ", Tiempo_fermentacion, " min<br>", "Conc. Media: ", round(Concentracion_promedio, 2), "%")) }
      if (isTRUE(input$show_od1_conc)) { fig <- fig %>% add_trace(data = df_plot, x = ~Tiempo_fermentacion, y = ~Concentracion_OD1, type = 'scatter', mode = 'markers', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "OD1"), marker = list(symbol = 'circle-open'), showlegend = FALSE, hoverinfo = 'text', text = ~paste0("<b>Muestra: ", Muestra_ID, " (OD1)</b><br>", "Tiempo: ", Tiempo_fermentacion, " min<br>", "Concentración: ", round(Concentracion_OD1, 2), "%")) }
      if (isTRUE(input$show_od2_conc)) { fig <- fig %>% add_trace(data = df_plot, x = ~Tiempo_fermentacion, y = ~Concentracion_OD2, type = 'scatter', mode = 'markers', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "OD2"), marker = list(symbol = 'square-open'), showlegend = FALSE, hoverinfo = 'text', text = ~paste0("<b>Muestra: ", Muestra_ID, " (OD2)</b><br>", "Tiempo: ", Tiempo_fermentacion, " min<br>", "Concentración: ", round(Concentracion_OD2, 2), "%")) }
      fig %>% layout(title = "Concentración de Etanol vs Tiempo", xaxis = list(title = "Tiempo (min)"), yaxis = list(title = "Concentración de Etanol (%)"), legend = list(title = list(text = 'Grupo'))) %>% config(displaylogo = FALSE)
    })
    
    # --- Lógica de retorno para el módulo integrado (sin cambios) ---
    datos_para_integracion <- reactive({
      req(datos_procesados())
      datos_procesados()$df_final %>%
        group_by(Grupo, Tiempo_fermentacion) %>%
        summarise(
          Valor = mean(Concentracion_promedio, na.rm = TRUE),
          Valor_sd = sd(Concentracion_promedio, na.rm = TRUE)
        ) %>%
        ungroup()
    })
    
    return(datos_para_integracion)
  })
}