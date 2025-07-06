glucosaUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("1. Carga de Datos"),
      fileInput(ns("file_datos"), "Selecciona archivo CSV único:", accept = c(".csv", "text/csv")),
      helpText(HTML("El CSV debe contener: <b>Tipo, Muestra_ID, Tiempo_fermentacion, Concentracion, OD1, OD2</b>.<br>
                     - <b>Tipo</b>: 'Muestra', 'Blanco', o 'Estandar'.<br>
                     - <b>Muestra_ID</b> debe identificar los grupos con una letra inicial (ej: A1, B2).<br>
                     - <b>OD1 y OD2</b> son los duplicados de la absorbancia.")),
      hr(),
      h4("2. Parámetros de Cálculo"),
      numericInput(ns("DF"), "Factor de dilución (DF):", value = 50, min = 1, step = 1),
      hr(),
      actionButton(ns("calcular"), "Calcular y Graficar", class = "btn-primary btn-lg", icon = icon("cogs"))
    ),
    mainPanel(
      tabsetPanel(
        id = ns("tabs"),
        tabPanel("Resultados Tabulados", uiOutput(ns("tablas_resultados"))),
        tabPanel("Parámetros Cinéticos", uiOutput(ns("analisis_por_grupo"))),
        tabPanel("Gráficas",
                 h4("Curva de Calibración"),
                 plotlyOutput(ns("plot_calibracion")),
                 hr(),
                 h4("Absorbancia vs. Concentración de Glucosa"),
                 uiOutput(ns("diagnostic_controls_ui")), 
                 plotlyOutput(ns("plot_diagnostico_muestras")),
                 hr(),
                 h4("Concentración de Glucosa vs. Tiempo"),
                 uiOutput(ns("consumption_controls_ui")), 
                 plotlyOutput(ns("plot_consumo_con_sd"))
        )
      )
    )
  )
}







glucosaServer <- function(id, datos_crudos_r = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    
    # --- 1. CÁLCULO CENTRALIZADO  ---
    datos_analizados <- eventReactive(input$calcular, {
      req(input$file_datos); df_raw <- tryCatch(read.csv(input$file_datos$datapath, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) shiny::validate("No se pudo leer el archivo CSV.")); cols_requeridas <- c("Tipo", "Muestra_ID", "Tiempo_fermentacion", "Concentracion", "OD1", "OD2"); validate(need(all(cols_requeridas %in% names(df_raw)), "Faltan columnas requeridas.")); od_blanco_df <- df_raw %>% filter(Tipo == "Blanco") %>% pivot_longer(c(OD1, OD2), values_to = "OD"); validate(need(nrow(od_blanco_df) > 0, "No hay datos de 'Blanco'.")); od_blanco <- mean(od_blanco_df$OD, na.rm = TRUE); validate(need(!is.na(od_blanco), "OD del blanco no es válido.")); df_estandares <- df_raw %>% filter(Tipo == "Estandar") %>% mutate(OD_prom = rowMeans(select(., OD1, OD2), na.rm = TRUE), OD_corregido = OD_prom - od_blanco); validate(need(nrow(df_estandares) >= 2, "Se necesitan al menos 2 estándares.")); modelo_lm <- lm(Concentracion ~ OD_corregido - 1, data = df_estandares); slope <- coef(modelo_lm)[["OD_corregido"]]; validate(need(slope > 0, "El slope de la curva de calibración debe ser positivo."))
      df_muestras_long <- df_raw %>% filter(Tipo == "Muestra") %>% pivot_longer(cols = c(OD1, OD2), names_to = "replica", values_to = "OD_raw") %>% mutate(Grupo = substr(Muestra_ID, 1, 1), OD_corregida = OD_raw - od_blanco, Glucosa_uM = pmax(0, (OD_corregida / slope) * input$DF)); validate(need(nrow(df_muestras_long) > 0, "No se encontraron datos de 'Muestra'."))
      df_muestras_sumarizado <- df_muestras_long %>% group_by(Grupo, Tiempo_fermentacion) %>% summarise(Glucosa_uM_mean = mean(Glucosa_uM, na.rm = TRUE), Glucosa_uM_sd = sd(Glucosa_uM, na.rm = TRUE), OD_corregida_mean = mean(OD_corregida, na.rm = TRUE), OD_corregida_sd = sd(OD_corregida, na.rm = TRUE), .groups = 'drop') %>% arrange(Grupo, Tiempo_fermentacion)
      
      # Este es el dataframe para la tabla de resultados.
      df_tabla_resultados <- df_raw %>%
        filter(Tipo == "Muestra") %>%
        mutate(
          Grupo = substr(Muestra_ID, 1, 1),
          ID = Muestra_ID,
          OD1_corregida = OD1 - od_blanco,
          OD2_corregida = OD2 - od_blanco,
          Glucosa_uM_OD1 = pmax(0, (OD1_corregida / slope) * input$DF),
          Glucosa_uM_OD2 = pmax(0, (OD2_corregida / slope) * input$DF),
          Glucosa_uM_mean = rowMeans(cbind(Glucosa_uM_OD1, Glucosa_uM_OD2), na.rm = TRUE),
          Glucosa_uM_sd = apply(cbind(Glucosa_uM_OD1, Glucosa_uM_OD2), 1, sd, na.rm = TRUE)
        ) %>%
        arrange(Grupo, Tiempo_fermentacion, ID)
      
      list(estandares = df_estandares, modelo_calibracion = modelo_lm, muestras_long = df_muestras_long, muestras_sumarizado = df_muestras_sumarizado, tabla_resultados = df_tabla_resultados)
    })
    
    # --- 2. Pestañas de Tablas y Análisis ---
    output$tablas_resultados <- renderUI({ req(datos_analizados()); lapply(unique(datos_analizados()$tabla_resultados$Grupo), function(g) { tagList(h5(paste("Resultados para el Grupo", g)), tableOutput(session$ns(paste0("tabla_res_", g))), hr()) }) })
    
    observe({ 
      req(datos_analizados())
      df_tabla <- datos_analizados()$tabla_resultados
      for (g in unique(df_tabla$Grupo)) { 
        local({ 
          grupo_local <- g
          output[[paste0("tabla_res_", grupo_local)]] <- renderTable({
            df_tabla %>%
              filter(Grupo == grupo_local) %>%
              mutate(Tiempo = Tiempo_fermentacion) %>%
              select(
                `Muestra ID` = ID,
                `Tiempo (min)` = Tiempo,
                `Abs. OD1 Corregida` = OD1_corregida,
                `Abs. OD2 Corregida` = OD2_corregida,
                `Conc. Glucosa 1 (µM)` = Glucosa_uM_OD1,
                `Conc. Glucosa 2 (µM)` = Glucosa_uM_OD2,
                `Conc. Glucosa Promedio (µM)` = Glucosa_uM_mean,
                `Desv. Est. (µM)` = Glucosa_uM_sd
              ) %>%
              mutate(across(where(is.numeric), ~ sprintf("%.3f", .)))
          }, striped = TRUE, hover = TRUE, bordered = TRUE)
        }) 
      } 
    })
    
    output$analisis_por_grupo <- renderUI({ req(datos_analizados()); tagList(lapply(unique(datos_analizados()$muestras_sumarizado$Grupo), function(g) { tagList(h5(paste("Parámetros para el Grupo", g)), tableOutput(session$ns(paste0("tabla_analisis_", g))), hr()) })) })
    observe({ req(datos_analizados()); df_sumarizado <- datos_analizados()$muestras_sumarizado; for (g in unique(df_sumarizado$Grupo)) { local({ grupo_local <- g; datos_grupo <- df_sumarizado %>% filter(Grupo == grupo_local) %>% arrange(Tiempo_fermentacion); output[[paste0("tabla_analisis_", grupo_local)]] <- renderTable({ if (nrow(datos_grupo) < 2) { return(data.frame(Parámetro = "Error", Valor = "Se necesitan al menos 2 puntos de tiempo.")) }; glucosa_inicial_medida <- datos_grupo$Glucosa_uM_mean[1]; glucosa_final_medida <- tail(datos_grupo$Glucosa_uM_mean, 1); cambio_neto_total_glucosa <- glucosa_final_medida - glucosa_inicial_medida; duracion_total <- tail(datos_grupo$Tiempo_fermentacion, 1) - datos_grupo$Tiempo_fermentacion[1]; min_glucosa_val <- min(datos_grupo$Glucosa_uM_mean, na.rm = TRUE); max_glucosa_val <- max(datos_grupo$Glucosa_uM_mean, na.rm = TRUE); time_at_max_glucosa <- datos_grupo$Tiempo_fermentacion[which.max(datos_grupo$Glucosa_uM_mean)]; glucosa_producida_inicial_a_max <- max_glucosa_val - glucosa_inicial_medida; glucosa_consumida_max_a_final <- max_glucosa_val - glucosa_final_medida; data.frame( Parámetro = c("Glucosa Inicial Media (µM)", "Glucosa Final Media (µM)", "Glucosa Mínima Observada (µM)", "Glucosa Máxima Observada (µM)", "Tiempo a la Glucosa Máxima (min)", "Glucosa Producida (Inicial a Máx) (µM)", "Glucosa Consumida (Máx a Final) (µM)", "Cambio Neto Total Glucosa (µM)", "Duración del Experimento (min)"), Valor = sprintf("%.3f", c(glucosa_inicial_medida, glucosa_final_medida, min_glucosa_val, max_glucosa_val, time_at_max_glucosa, glucosa_producida_inicial_a_max, glucosa_consumida_max_a_final, cambio_neto_total_glucosa, duracion_total)) ) }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'l') }) } })
    
    # --- 3. PESTAÑA GRÁFICOS  ---
    grupos_disponibles_reactivos <- reactive({ req(datos_analizados()); unique(datos_analizados()$muestras_sumarizado$Grupo) })
    
    output$diagnostic_controls_ui <- renderUI({
      grupos <- grupos_disponibles_reactivos()
      tagList(
        checkboxGroupInput(session$ns("selected_groups_diagnostic"), label=NULL, choices = grupos, selected = grupos, inline = TRUE),
        fluidRow(
          column(3, checkboxInput(session$ns("show_mean_diag"), "Promedio", value = TRUE)),
          column(3, checkboxInput(session$ns("show_sd_diag"), "Desv. Est.", value = TRUE)),
          column(3, checkboxInput(session$ns("show_OD1_diag"), "OD1", value = FALSE)),
          column(3, checkboxInput(session$ns("show_OD2_diag"), "OD2", value = FALSE))
        )
      )
    })
    
    output$consumption_controls_ui <- renderUI({
      grupos <- grupos_disponibles_reactivos()
      tagList(
        checkboxGroupInput(session$ns("selected_groups_consumption"), label=NULL, choices = grupos, selected = grupos, inline = TRUE),
        fluidRow(
          column(3, checkboxInput(session$ns("show_mean_cons"), "Promedio", value = TRUE)),
          column(3, checkboxInput(session$ns("show_sd_cons"), "Desv. Est.", value = TRUE)),
          column(3, checkboxInput(session$ns("show_OD1_cons"), "Réplica 1", value = FALSE)),
          column(3, checkboxInput(session$ns("show_OD2_cons"), "Réplica 2", value = FALSE))
        )
      )
    })
    
    output$plot_calibracion <- renderPlotly({ req(datos_analizados()); datos <- datos_analizados(); df_estandares <- datos$estandares; modelo <- datos$modelo_calibracion; r2_valor <- summary(modelo)$r.squared; p <- ggplot(df_estandares, aes(x = OD_corregido, y = Concentracion, text = paste("Abs Corregida:", round(OD_corregido, 3), "<br>Concentración:", round(Concentracion, 2), "µM"))) + geom_point(color = "blue", size = 3, alpha = 0.8) + labs(title = "Curva de Calibración", x = "Absorbancia Corregida", y = "Concentración de Glucosa (µM)") + theme_minimal(base_size = 10) + theme(plot.title = element_text(size = 12, hjust = 0.5)); fig <- ggplotly(p, tooltip = "text"); fig <- fig %>% add_lines(x = ~OD_corregido, y = ~predict(modelo), data = df_estandares, name = 'Regresión', line = list(color = 'red', width = 2), inherit = FALSE); formula_texto <- sprintf("[Glucosa] = %.3f * OD", coef(modelo)[["OD_corregido"]]); r2_texto <- sprintf("R<sup>2</sup> = %.4f", r2_valor); fig <- fig %>% layout(annotations = list(x = 0.95, y = 0.05, xref = "paper", yref = "paper", text = paste(formula_texto, r2_texto, sep = "<br>"), showarrow = FALSE, xanchor = 'right', yanchor = 'bottom', font = list(size=12))) %>% config(displaylogo = FALSE); fig })
    
    output$plot_diagnostico_muestras <- renderPlotly({
      req(datos_analizados(), input$selected_groups_diagnostic)
      df_sumarizado <- datos_analizados()$muestras_sumarizado %>% filter(Grupo %in% input$selected_groups_diagnostic) %>% arrange(Tiempo_fermentacion)
      df_long <- datos_analizados()$muestras_long %>% filter(Grupo %in% input$selected_groups_diagnostic)
      fig <- plot_ly()
      if (isTRUE(input$show_mean_diag) && isTRUE(input$show_sd_diag)) {
        fig <- fig %>% add_ribbons(data = df_sumarizado, x = ~OD_corregida_mean, ymin = ~Glucosa_uM_mean - Glucosa_uM_sd, ymax = ~Glucosa_uM_mean + Glucosa_uM_sd, color = ~Grupo, legendgroup = ~Grupo, opacity = 0.2, line = list(width = 0), name = ~paste(Grupo, "SD"), showlegend = FALSE, hoverinfo = 'none')
      }
      if (isTRUE(input$show_mean_diag)) {
        fig <- fig %>% add_trace(data = df_sumarizado, x = ~OD_corregida_mean, y = ~Glucosa_uM_mean, type = 'scatter', mode = 'lines', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "Promedio"), line = list(dash = 'dash'), hoverinfo = 'text', text = ~paste("Grupo:", Grupo, "<br>Tiempo:", Tiempo_fermentacion))
      }
      if (isTRUE(input$show_OD1_diag)) {
        df_od1 <- filter(df_long, replica == "OD1")
        fig <- fig %>% add_trace(data = df_od1, x = ~OD_corregida, y = ~Glucosa_uM, type = 'scatter', mode = 'markers', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "OD1"), marker = list(symbol = 'circle'), showlegend = FALSE, hoverinfo = 'text', text = ~paste("Muestra:", Muestra_ID))
      }
      if (isTRUE(input$show_OD2_diag)) {
        df_od2 <- filter(df_long, replica == "OD2")
        fig <- fig %>% add_trace(data = df_od2, x = ~OD_corregida, y = ~Glucosa_uM, type = 'scatter', mode = 'markers', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "OD2"), marker = list(symbol = 'square'), showlegend = FALSE, hoverinfo = 'text', text = ~paste("Muestra:", Muestra_ID))
      }
      fig %>% layout(title = "Relación Absorbancia vs. Concentración", xaxis = list(title = "Absorbancia Corregida"), yaxis = list(title = "Glucosa Calculada (µM)"), legend = list(title = list(text = 'Grupo')))
    })
    
    output$plot_consumo_con_sd <- renderPlotly({
      req(datos_analizados(), input$selected_groups_consumption)
      df_sumarizado <- datos_analizados()$muestras_sumarizado %>% filter(Grupo %in% input$selected_groups_consumption)
      df_long <- datos_analizados()$muestras_long %>% filter(Grupo %in% input$selected_groups_consumption)
      fig <- plot_ly()
      if (isTRUE(input$show_mean_cons) && isTRUE(input$show_sd_cons)) {
        fig <- fig %>% add_ribbons(data = df_sumarizado, x = ~Tiempo_fermentacion, ymin = ~Glucosa_uM_mean - Glucosa_uM_sd, ymax = ~Glucosa_uM_mean + Glucosa_uM_sd, color = ~Grupo, legendgroup = ~Grupo, opacity = 0.2, line = list(width = 0), name = ~paste(Grupo, "SD"), showlegend = FALSE, hoverinfo = 'none')
      }
      if (isTRUE(input$show_mean_cons)) {
        fig <- fig %>% add_trace(data = df_sumarizado, x = ~Tiempo_fermentacion, y = ~Glucosa_uM_mean, type = 'scatter', mode = 'lines+markers', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "Promedio"), hoverinfo = 'text', text = ~paste("Glucosa Media:", round(Glucosa_uM_mean, 2)))
      }
      if (isTRUE(input$show_OD1_cons)) {
        df_od1 <- filter(df_long, replica == "OD1")
        fig <- fig %>% add_trace(data = df_od1, x = ~Tiempo_fermentacion, y = ~Glucosa_uM, type = 'scatter', mode = 'markers', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "OD1"), marker = list(symbol = 'circle'), showlegend = FALSE, hoverinfo = 'text', text = ~paste("Muestra:", Muestra_ID))
      }
      if (isTRUE(input$show_OD2_cons)) {
        df_od2 <- filter(df_long, replica == "OD2")
        fig <- fig %>% add_trace(data = df_od2, x = ~Tiempo_fermentacion, y = ~Glucosa_uM, type = 'scatter', mode = 'markers', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "OD2"), marker = list(symbol = 'square'), showlegend = FALSE, hoverinfo = 'text', text = ~paste("Muestra:", Muestra_ID))
      }
      fig %>% layout(title = "Concentración de Glucosa Durante la Fermentación", xaxis = list(title = "Tiempo (min)"), yaxis = list(title = "Concentración de Glucosa (µM)", rangemode = "tozero"), legend = list(title = list(text = 'Grupo')))
    })
    
    return(reactive({ req(datos_analizados()); datos_analizados()$muestras_sumarizado %>% select(Grupo, Tiempo_fermentacion, Valor = Glucosa_uM_mean) %>% mutate(TipoMedicion = "Glucosa") }))
  })
}