# -----------------------------------------------------------------------------
# Interfaz de Usuario (UI)
# -----------------------------------------------------------------------------
etanolUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Carga de Datos de Etanol"),
      fileInput(ns("file_datos"), "Selecciona archivo CSV único:", accept = c(".csv")),
      helpText(HTML("El CSV debe contener: <b>Tipo, Muestra_ID, Tiempo_fermentacion, concentracion_etanol, OD1, OD2</b>.<br>
                     - <b>Tipo</b>: 'Muestra', 'Blanco', o 'Estandar'.<br>
                     - <b>Muestra_ID</b> debe identificar los grupos con una letra inicial (ej: A1, B2).<br>
                     - <b>OD1 y OD2</b> son los duplicados de la absorbancia.")),
      hr(),
      h4("2. Parámetros de Cálculo"),
      numericInput(ns("DF"), "Factor de dilución (DF):", value = 50, min = 1, step = 1),
      numericInput(ns("cantidad_sustrato"), "Cantidad de Sustrato Utilizado (g):", value = 1000, min = 1, step = 1),
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
                 h4("Absorbancia vs. Concentración de Etanol"),
                 fluidRow(
                   column(3, checkboxInput(ns("show_prom_abs"), "Promedio", value = TRUE)),
                   column(3, checkboxInput(ns("show_desv_abs"), "Desv. Est.", value = TRUE)),
                   column(3, checkboxInput(ns("show_od1_abs"), "OD1", value = FALSE)),
                   column(3, checkboxInput(ns("show_od2_abs"), "OD2", value = FALSE))
                 ),
                 plotlyOutput(ns("plotAbsorbanciaMuestras")),
                 hr(),
                 h4("Concentración de Etanol vs. Tiempo"),
                 fluidRow(
                   column(3, checkboxInput(ns("show_prom_conc"), "Promedio", value = TRUE)),
                   column(3, checkboxInput(ns("show_desv_conc"), "Desv. Est.", value = TRUE)),
                   column(3, checkboxInput(ns("show_od1_conc"), "Réplica 1", value = FALSE)),
                   column(3, checkboxInput(ns("show_od2_conc"), "Réplica 2", value = FALSE))
                 ),
                 plotlyOutput(ns("plotConcentracionEtanolTiempo"))
        )
      )
    )
  )
}
# -----------------------------------------------------------------------------
# Lógica del Servidor (ACTUALIZADA PARA USAR BLANCO COMO ESTÁNDAR CERO)
# -----------------------------------------------------------------------------
etanolServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # --- Lógica de cálculo centralizada y robusta ---
    datos_procesados <- eventReactive(input$calcular, {
      req(input$file_datos)
      df_raw <- tryCatch(read.csv(input$file_datos$datapath, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) validate("No se pudo leer el archivo CSV."))
      cols_requeridas <- c("Tipo", "Muestra_ID", "Tiempo_fermentacion", "concentracion_etanol", "OD1", "OD2")
      validate(need(all(cols_requeridas %in% names(df_raw)), paste("Error: Faltan columnas. Se requieren:", paste(cols_requeridas, collapse = ", "))))
      
      # 1. Unificar Estándares y Blancos en un solo dataframe de calibración
      df_calibracion_para_modelo <- df_raw %>%
        filter(Tipo %in% c("Estandar", "Blanco")) %>%
        mutate(
          # Asignar explícitamente concentración 0 al blanco
          concentracion_etanol = ifelse(Tipo == "Blanco", 0, concentracion_etanol)
        )
      
      validate(need(nrow(df_calibracion_para_modelo) > 0, "Error: No se encontraron 'Blancos' o 'Estandares' en el archivo."))
      
      # 2. Pivotar para tener todos los puntos de calibración individuales
      calibracion_long <- df_calibracion_para_modelo %>%
        pivot_longer(cols = c(OD1, OD2), names_to = "Replica", values_to = "OD")
      
      # 3. Ajustar el modelo lineal a todos los puntos (incluyendo el blanco como punto (0, OD_blanco))
      validate(need(nrow(calibracion_long) >= 4, "Se necesitan al menos 2 estándares (4 puntos de datos) para la regresión."))
      modelo_calib <- lm(OD ~ concentracion_etanol, data = calibracion_long)
      Slope <- coef(modelo_calib)["concentracion_etanol"]
      Intercept <- coef(modelo_calib)["(Intercept)"]
      R_squared <- summary(modelo_calib)$r.squared
      
      # 4. Calcular concentraciones en las muestras
      muestras <- df_raw %>% filter(Tipo == "Muestra")
      validate(need(nrow(muestras) > 0, "Error: No se encontraron 'Muestras' en el archivo."))
      
      df_resultados <- muestras %>%
        mutate(
          Grupo = gsub("[0-9].*$", "", Muestra_ID),
          # Usar la fórmula directa: Conc = (OD_bruta - Intercepto) / Pendiente
          # El intercepto del modelo ya representa el blanco.
          Concentracion_OD1 = pmax(0, ((OD1 - Intercept) / Slope) * input$DF),
          Concentracion_OD2 = pmax(0, ((OD2 - Intercept) / Slope) * input$DF)
        ) %>%
        rowwise() %>%
        mutate(
          Concentracion_promedio = mean(c(Concentracion_OD1, Concentracion_OD2), na.rm = TRUE),
          Concentracion_desv_std = sd(c(Concentracion_OD1, Concentracion_OD2), na.rm = TRUE),
          OD_promedio = mean(c(OD1, OD2), na.rm = TRUE),
          OD_desv_std = sd(c(OD1, OD2), na.rm = TRUE)
        ) %>%
        ungroup() %>%
        arrange(Grupo, Tiempo_fermentacion, Muestra_ID)
      
      validate(need(length(unique(df_resultados$Grupo)) > 0, "No se pudo identificar ningún grupo a partir de los prefijos de Muestra_ID."))
      
      # Preparar datos promediados para el gráfico de calibración
      curva_calib_promedios <- calibracion_long %>%
        group_by(concentracion_etanol) %>%
        summarise(OD_promedio = mean(OD, na.rm = TRUE), .groups = 'drop')
      
      return(list(
        df_final = df_resultados, 
        curva_calib_promedios = curva_calib_promedios,
        modelo_calib = modelo_calib,
        Slope = Slope, 
        Intercept = Intercept, 
        R_squared = R_squared
      ))
    })
    
    # Renderizado de tablas de resultados (sin cambios)
    output$tablas_resultados_ui <- renderUI({ req(datos_procesados()); lapply(unique(datos_procesados()$df_final$Grupo), function(g) { ns <- session$ns; tagList(h4(paste("Resultados para el Grupo", g)), tableOutput(ns(paste0("tabla_", g))), hr()) }) })
    observe({ req(datos_procesados()); df_final <- datos_procesados()$df_final; lapply(unique(df_final$Grupo), function(g) { output[[paste0("tabla_", g)]] <- renderTable({ df_final %>% filter(Grupo == g) %>% select(Muestra_ID, Tiempo_fermentacion, OD1, OD2, OD_promedio, OD_desv_std, Concentracion_OD1, Concentracion_OD2, Concentracion_promedio, Concentracion_desv_std) %>% rename("Muestra ID" = Muestra_ID, "Tiempo (min)" = Tiempo_fermentacion, "Abs. OD1" = OD1, "Abs. OD2" = OD2, "Abs. Promedio" = OD_promedio, "Desv. Est." = OD_desv_std, "Conc. Etanol 1 (%)" = Concentracion_OD1, "Conc. Etanol 2 (%)" = Concentracion_OD2, "Conc. Etanol Promedio (%)" = Concentracion_promedio, "Desv. Est. (%)" = Concentracion_desv_std) %>% mutate(across(where(is.numeric), ~round(., 3))) }, striped = TRUE, hover = TRUE, bordered = TRUE) }) })
    
    # --- TABLAS DE PARÁMETROS (sin cambios) ---
    parametros_calculados <- eventReactive(input$calcular, {
      req(datos_procesados())
      datos <- datos_procesados()$df_final
      datos %>%
        group_by(Grupo) %>%
        summarise(
          max_conc = max(Concentracion_promedio, na.rm = TRUE),
          tiempo_optimo = if(all(is.na(Concentracion_promedio))) NA else Tiempo_fermentacion[which.max(Concentracion_promedio)],
          .groups = "drop"
        ) %>%
        mutate(
          `Concentración Máxima (% v/v)` = max_conc,
          `Tiempo Óptimo (min)` = tiempo_optimo,
          `Rendimiento (g etanol / g sustrato)` = (max_conc / 100 * 1000 * 0.789) / input$cantidad_sustrato
        ) %>%
        select(Grupo, `Concentración Máxima (% v/v)`, `Tiempo Óptimo (min)`, `Rendimiento (g etanol / g sustrato)`)
    })
    
    output$parametros_ui <- renderUI({ req(parametros_calculados()); lapply(unique(parametros_calculados()$Grupo), function(nombre_grupo) { ns <- session$ns; tagList(h4(paste("Parámetros para el Grupo", nombre_grupo)), tableOutput(ns(paste0("tabla_param_", nombre_grupo))), hr()) }) })
    
    observe({
      req(parametros_calculados())
      df_parametros <- parametros_calculados()
      for (nombre_grupo in unique(df_parametros$Grupo)) {
        local({
          grupo_actual <- nombre_grupo
          output[[paste0("tabla_param_", grupo_actual)]] <- renderTable({ df_parametros %>% filter(Grupo == grupo_actual) %>% select(-Grupo) %>% pivot_longer(everything(), names_to = "Parámetro Cinético", values_to = "Valor") %>% mutate(Valor = sprintf("%.4f", as.numeric(Valor))) }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = 'l')
        })
      }
    })
    
    # --- GRÁFICO DE CURVA DE CALIBRACIÓN (ACTUALIZADO) ---
    output$plotCurvaCalibracion <- renderPlotly({
      req(datos_procesados())
      datos <- datos_procesados()
      ecuacion <- paste0(
        "OD = ", round(datos$Slope, 4), " * Conc. + ", round(datos$Intercept, 4),
        "<br>R<sup>2</sup> = ", round(datos$R_squared, 4)
      )
      
      p <- ggplot(datos$curva_calib_promedios, aes(x = concentracion_etanol, y = OD_promedio)) +
        geom_point(aes(text = paste0("Conc. Etanol: ", concentracion_etanol, "%<br>Absorbancia Media: ", round(OD_promedio, 3))), color = "blue", size = 3) +
        geom_abline(intercept = datos$Intercept, slope = datos$Slope, color = "red") +
        labs(
          title = "Curva de Calibración de Etanol",
          x = "Concentración de Etanol (%)",
          y = "Absorbancia (OD)" # Eje Y es Absorbancia bruta
        ) +
        theme_minimal()
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          annotations = list(
            x = 0.95, y = 0.05, text = ecuacion, showarrow = FALSE, 
            xref = "paper", yref = "paper", xanchor = 'right', yanchor = 'bottom',
            align = 'right'
          )
        ) %>%
        config(displaylogo = FALSE)
    })
    
    # --- OTROS GRÁFICOS (sin cambios) ---
    output$plotAbsorbanciaMuestras <- renderPlotly({
      req(datos_procesados())
      df_plot <- datos_procesados()$df_final %>% arrange(Grupo, Concentracion_promedio)
      fig <- plot_ly()
      if (isTRUE(input$show_desv_abs) && isTRUE(input$show_prom_abs)) { fig <- fig %>% add_ribbons(data = df_plot, x = ~Concentracion_promedio, ymin = ~OD_promedio - OD_desv_std, ymax = ~OD_promedio + OD_desv_std, color = ~Grupo, legendgroup = ~Grupo, opacity = 0.2, line = list(width = 0), name = ~paste(Grupo, "SD"), showlegend = FALSE, hoverinfo = 'none') }
      if (isTRUE(input$show_prom_abs)) { fig <- fig %>% add_trace(data = df_plot, x = ~Concentracion_promedio, y = ~OD_promedio, type = 'scatter', mode = 'lines+markers', color = ~Grupo, legendgroup = ~Grupo, name = ~Grupo, line = list(dash = 'dash'), hoverinfo = 'text', text = ~paste0("<b>Grupo: ", Grupo, "</b><br>", "Conc. Media: ", round(Concentracion_promedio, 2), "%<br>", "Abs. Media: ", round(OD_promedio, 3))) }
      if (isTRUE(input$show_od1_abs)) { fig <- fig %>% add_trace(data = df_plot, x = ~Concentracion_promedio, y = ~OD1, type = 'scatter', mode = 'markers', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "OD1"), marker = list(symbol = 'circle-open'), showlegend = FALSE, hoverinfo = 'text', text = ~paste0("<b>Muestra: ", Muestra_ID, " (OD1)</b><br>", "Conc. Media: ", round(Concentracion_promedio, 2), "%<br>", "Absorbancia: ", round(OD1, 3))) }
      if (isTRUE(input$show_od2_abs)) { fig <- fig %>% add_trace(data = df_plot, x = ~Concentracion_promedio, y = ~OD2, type = 'scatter', mode = 'markers', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "OD2"), marker = list(symbol = 'square-open'), showlegend = FALSE, hoverinfo = 'text', text = ~paste0("<b>Muestra: ", Muestra_ID, " (OD2)</b><br>", "Conc. Media: ", round(Concentracion_promedio, 2), "%<br>", "Absorbancia: ", round(OD2, 3))) }
      fig %>% layout(title = "Absorbancia vs. Concentración", xaxis = list(title = "Concentración de Etanol Promedio (%)"), yaxis = list(title = "Absorbancia (OD)"), legend = list(title = list(text = 'Grupo'))) %>% config(displaylogo = FALSE)
    })
    
    output$plotConcentracionEtanolTiempo <- renderPlotly({
      req(datos_procesados())
      df_plot <- datos_procesados()$df_final %>% arrange(Grupo, Tiempo_fermentacion)
      fig <- plot_ly()
      if (isTRUE(input$show_desv_conc) && isTRUE(input$show_prom_conc)) { fig <- fig %>% add_ribbons(data = df_plot, x = ~Tiempo_fermentacion, ymin = ~Concentracion_promedio - Concentracion_desv_std, ymax = ~Concentracion_promedio + Concentracion_desv_std, color = ~Grupo, legendgroup = ~Grupo, opacity = 0.2, line = list(width = 0), name = ~paste(Grupo, "SD"), showlegend = FALSE, hoverinfo = 'none') }
      if (isTRUE(input$show_prom_conc)) { fig <- fig %>% add_trace(data = df_plot, x = ~Tiempo_fermentacion, y = ~Concentracion_promedio, type = 'scatter', mode = 'lines+markers', color = ~Grupo, legendgroup = ~Grupo, name = ~Grupo, hoverinfo = 'text', text = ~paste0("<b>Grupo: ", Grupo, "</b><br>", "Tiempo: ", Tiempo_fermentacion, " min<br>", "Conc. Media: ", round(Concentracion_promedio, 2), "%")) }
      if (isTRUE(input$show_od1_conc)) { fig <- fig %>% add_trace(data = df_plot, x = ~Tiempo_fermentacion, y = ~Concentracion_OD1, type = 'scatter', mode = 'markers', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "OD1"), marker = list(symbol = 'circle-open'), showlegend = FALSE, hoverinfo = 'text', text = ~paste0("<b>Muestra: ", Muestra_ID, " (OD1)</b><br>", "Tiempo: ", Tiempo_fermentacion, " min<br>", "Concentración: ", round(Concentracion_OD1, 2), "%")) }
      if (isTRUE(input$show_od2_conc)) { fig <- fig %>% add_trace(data = df_plot, x = ~Tiempo_fermentacion, y = ~Concentracion_OD2, type = 'scatter', mode = 'markers', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "OD2"), marker = list(symbol = 'square-open'), showlegend = FALSE, hoverinfo = 'text', text = ~paste0("<b>Muestra: ", Muestra_ID, " (OD2)</b><br>", "Tiempo: ", Tiempo_fermentacion, " min<br>", "Concentración: ", round(Concentracion_OD2, 2), "%")) }
      fig %>% layout(title = "Concentración de Etanol Durante la Fermentación", xaxis = list(title = "Tiempo (min)"), yaxis = list(title = "Concentración de Etanol (%)", rangemode="tozero"), legend = list(title = list(text = 'Grupo'))) %>% config(displaylogo = FALSE)
    })
    
    # --- Lógica de retorno para el módulo integrado ---
    return(reactive({
      req(datos_procesados())
      datos_procesados()$df_final %>%
        group_by(Grupo, Tiempo_fermentacion) %>%
        summarise(
          Valor = mean(Concentracion_promedio, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(TipoMedicion = "Etanol")
    }))
  })
}
