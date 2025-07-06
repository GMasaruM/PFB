amilasaUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
     
      h4("1. Carga de Datos"),
      fileInput(ns("file_datos"), "Selecciona archivo CSV único:", accept = c(".csv")),
      helpText(HTML("El CSV debe contener: <b>Tipo, Muestra_ID, Tiempo_fermentacion, OD1, OD2</b>.<br>
                     - <b>Tipo</b>: 'Muestra', 'Blanco', o 'Estandar'.<br>
                     - <b>Muestra_ID</b> debe identificar los grupos con una letra inicial (ej: A1, B2).<br>
                     - <b>OD1 y OD2</b> son los duplicados de la absorbancia.")),
      hr(),
      h4("2. Parámetros de Cálculo"),
      numericInput(ns("DF"), "Factor de dilución (DF):", value = 50, min = 1, step = 1),
      numericInput(ns("T"), "Tiempo de reacción (min):", value = 20, min = 1, step = 1),
      actionButton(ns("calcular"), "Calcular y Graficar", class = "btn-primary btn-lg", icon = icon("cogs"))
    ),
    mainPanel(
      tabsetPanel(
        id = ns("tabs"),
        
        tabPanel("Resultados Tabulados", uiOutput(ns("resultados_tabulados_ui"))),
        tabPanel("Parámetros Cinéticos", uiOutput(ns("parametros_cineticos_ui"))),
        tabPanel("Gráficas",
                 # <<--- GRAFICAS --->>
                 fluidRow(
                   column(12, h4("Actividad de amilasa vs. Absorbancia")),
                   column(3, checkboxInput(ns("show_od1_abs"), "OD1", value = FALSE)),
                   column(3, checkboxInput(ns("show_od2_abs"), "OD2", value = FALSE)),
                   column(3, checkboxInput(ns("show_avg_abs"), "Promedios", value = TRUE)),
                   column(3, checkboxInput(ns("show_sd_abs"), "Desv. Est.", value = TRUE))
                 ),
                 fluidRow(
                   column(6, checkboxGroupInput(ns("plotAbs_groups"), label = NULL, choices = NULL, inline = TRUE))
                 ),
                 plotlyOutput(ns("plotAbsorbance"), height = "400px"), 
                 hr(),
                 
                 fluidRow(
                   column(12, h4("Actividad de amilasa vs. Tiempo")),
                   column(3, checkboxInput(ns("show_od1_time"), "Réplica 1", value = FALSE)),
                   column(3, checkboxInput(ns("show_od2_time"), "Réplica 2", value = FALSE)),
                   column(3, checkboxInput(ns("show_avg_time"), "Promedios", value = TRUE)),
                   column(3, checkboxInput(ns("show_sd_time"), "Desv. Est.", value = TRUE))
                 ),
                 fluidRow(
                   column(6, checkboxGroupInput(ns("plotAct_groups"), label = NULL, choices = NULL, inline = TRUE))
                 ),
                 plotlyOutput(ns("plotActivity"), height = "400px") 
                 # <<--- FIN DE LAS GRAFICAS --->>
        )
      )
    )
  )
}

amilasaServer <- function(id, datos_crudos_r = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    
    # --- CÁLCULO Y TABLAS  ---
    datos_procesados <- eventReactive(input$calcular, {
      req(input$file_datos)
      df_raw <- read.csv(input$file_datos$datapath, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
      cols_requeridas <- c("Tipo", "Muestra_ID", "Tiempo_fermentacion", "OD1", "OD2")
      validate(need(all(cols_requeridas %in% names(df_raw)), paste("El archivo CSV debe contener las columnas:", paste(cols_requeridas, collapse = ", "))))
      od_blancos <- df_raw %>% filter(Tipo == "Blanco") %>% pivot_longer(c(OD1, OD2), values_to = "od")
      od_estandares <- df_raw %>% filter(Tipo == "Estandar") %>% pivot_longer(c(OD1, OD2), values_to = "od")
      validate(need(nrow(od_blancos) > 0, "No se encontraron datos de 'Blanco'."))
      validate(need(nrow(od_estandares) > 0, "No se encontraron datos de 'Estandar'."))
      OD_blanco_promedio  <- mean(od_blancos$od, na.rm = TRUE)
      OD_estandar_promedio <- mean(od_estandares$od, na.rm = TRUE)
      validate(need(is.finite(OD_blanco_promedio), "Datos inválidos para 'Blanco'."), need(is.finite(OD_estandar_promedio), "Datos inválidos para 'Estandar'."), need(abs(OD_estandar_promedio - OD_blanco_promedio) > 1e-9, "Diferencia entre estándar y blanco es cero."))
      df_muestras <- df_raw %>% filter(Tipo == "Muestra")
      validate(need(nrow(df_muestras) > 0, "No se encontraron filas con Tipo 'Muestra'."))
      calc_act <- function(OD_muestra) { pmax(0, ((OD_muestra - OD_blanco_promedio) / (OD_estandar_promedio - OD_blanco_promedio)) * (400 / input$T) * input$DF) }
      df_final <- df_muestras %>% mutate(Grupo = str_extract(Muestra_ID, "^[A-Za-z]+")) %>% mutate(U_L_1 = calc_act(OD1), U_L_2 = calc_act(OD2)) %>% rowwise() %>% mutate(Promedio_U_L = mean(c(U_L_1, U_L_2), na.rm = TRUE), SD_U_L = sd(c(U_L_1, U_L_2), na.rm = TRUE), Absorbancia_Neta = mean(c(OD1, OD2), na.rm = TRUE) - OD_blanco_promedio) %>% ungroup() %>% mutate(SD_U_L = ifelse(is.na(SD_U_L), 0, SD_U_L), Tiempo_fermenta = Tiempo_fermentacion) %>% arrange(Grupo, Tiempo_fermentacion)
      return(df_final)
    })
    output$resultados_tabulados_ui <- renderUI({ req(datos_procesados()); df <- datos_procesados(); grupos <- unique(df$Grupo); lapply(grupos, function(g) { df_grupo <- df %>% filter(Grupo == g) %>% select(Muestra_ID, Tiempo_fermentacion, OD1, OD2, U_L_1, U_L_2, Promedio_U_L, SD_U_L) %>% rename(`Muestra ID` = Muestra_ID, `Tiempo (min)` = Tiempo_fermentacion, `Abs. OD1` = OD1, `Abs. OD2` = OD2, `Act. α-amilasa 1 (U/L)` = U_L_1, `Act. α-amilasa 2 (U/L)` = U_L_2, `Act. α-amilasa Promedio (U/L)` = Promedio_U_L, `Desv. Est. (U/L)` = SD_U_L) %>% mutate(across(c(`Act. α-amilasa Promedio (U/L)`, `Desv. Est. (U/L)`), ~ round(., 2))); tagList(h5(paste("Resultados para el Grupo", g), style = "font-weight: bold;"), renderTable(df_grupo, striped = TRUE, hover = TRUE, bordered = TRUE)) }) })
    
    # --- PARÁMETROS CINÉTICOS  ---
    parametros_cineticos_por_grupo <- reactive({
      req(datos_procesados())
      df <- datos_procesados()
      
      all_params_df <- df %>%
        group_by(Grupo) %>%
        do({
          df_grupo <- .
          grupo_actual <- unique(df_grupo$Grupo)
          params_df_temp <- data.frame(Parámetro = character(), Valor = character(), stringsAsFactors = FALSE)
          
          add_param_row <- function(df, param_name, param_value, format_string = "%.3f") {
            new_row <- data.frame(Parámetro = param_name, Valor = sprintf(format_string, param_value), stringsAsFactors = FALSE)
            bind_rows(df, new_row)
          }
          
          if(nrow(df_grupo) < 3) {
            params_df_temp <- add_param_row(params_df_temp, "Nota", "Se necesitan al menos 3 puntos de datos para el ajuste cinético.", "%s")
          } else {
            modelo_q <- tryCatch(lm(Promedio_U_L ~ poly(Tiempo_fermentacion, 2, raw = TRUE), data = df_grupo), error = function(e) NULL)
            
            if (is.null(modelo_q) || length(coef(modelo_q)) < 3 || any(is.na(coef(modelo_q)))) {
              params_df_temp <- add_param_row(params_df_temp, "Nota", "No se pudo ajustar un modelo cuadrático.", "%s")
            } else {
              coefs <- coef(modelo_q); c0 <- coefs[1]; b <- coefs[2]; a <- coefs[3]
              
              if(abs(a) < 1e-9) {
                params_df_temp <- add_param_row(params_df_temp, "Nota", "Datos con tendencia lineal, no cuadrática.", "%s")
              } else {
                t_opt <- -b / (2 * a)
                A_max <- c0 + b * t_opt + a * t_opt^2
                
                params_df_temp <- add_param_row(params_df_temp, "Tiempo optimo (min)", t_opt)
                params_df_temp <- add_param_row(params_df_temp, "Tiempo optimo (h)", t_opt / 60)
                params_df_temp <- add_param_row(params_df_temp, "Actividad maxima (U/L)", A_max)
                
                if (!is.na(t_opt) && t_opt >= min(df_grupo$Tiempo_fermentacion, na.rm = TRUE) && A_max > 0) {
                  decay_data <- subset(df_grupo, Tiempo_fermentacion >= t_opt & Promedio_U_L > 0)
                  
                  if(nrow(decay_data) > 1) {
                    decay_data$invA <- 1 / decay_data$Promedio_U_L
                    mod2o <- tryCatch(lm(invA ~ Tiempo_fermentacion, data = decay_data), error = function(e) NULL)
                    
                    if (!is.null(mod2o) && !is.na(coef(mod2o)[2])) {
                      k <- coef(mod2o)[2]
                      
                      if (k > 0 && A_max > 0) {
                        t_half <- 1 / (k * A_max)
                        
                        params_df_temp <- add_param_row(params_df_temp, "k decaimiento 2do orden (L/(U*min))", k, "%.5f")
                        params_df_temp <- add_param_row(params_df_temp, "Vida media (t1/2) (min)", t_half)
                        params_df_temp <- add_param_row(params_df_temp, "Vida media (t1/2) (h)", t_half / 60)
                      }
                    }
                  }
                }
              }
            }
          }
          data.frame(Grupo = grupo_actual, params_df_temp, stringsAsFactors = FALSE)
        }) %>%
        ungroup()
      
      return(all_params_df)
    })
    
    output$parametros_cineticos_ui <- renderUI({ req(parametros_cineticos_por_grupo()); df_params <- parametros_cineticos_por_grupo(); if (nrow(df_params) == 0) { return(tags$p("No hay parámetros.")) }; grupos <- unique(df_params$Grupo); lapply(grupos, function(g) { df_grupo_params <- df_params %>% filter(Grupo == g) %>% select(-Grupo); tagList(h5(paste("Parámetros para el Grupo", g), style = "font-weight: bold;"), tableOutput(session$ns(paste0("tabla_parametros_", g))), hr()) }) })
    observe({ req(parametros_cineticos_por_grupo()); df_params <- parametros_cineticos_por_grupo(); for (g in unique(df_params$Grupo)) { local({ grupo_local <- g; output[[paste0("tabla_parametros_", grupo_local)]] <- renderTable({ df_params %>% filter(Grupo == grupo_local) %>% select(-Grupo) }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'l') }) } })
    
    # --- GRÁFICAS  ---
    observeEvent(datos_procesados(), { df <- datos_procesados(); req(df); grupos_disponibles <- unique(df$Grupo); updateCheckboxGroupInput(session, "plotAbs_groups", choices = grupos_disponibles, selected = grupos_disponibles, inline = TRUE); updateCheckboxGroupInput(session, "plotAct_groups", choices = grupos_disponibles, selected = grupos_disponibles, inline = TRUE) })
    
    output$plotAbsorbance <- renderPlotly({
      req(datos_procesados(), input$plotAbs_groups)
      df_plot <- datos_procesados() %>% filter(Grupo %in% input$plotAbs_groups) %>% arrange(Grupo, Absorbancia_Neta)
      validate(need(nrow(df_plot) > 0, "Selecciona al menos un grupo para graficar."))
      fig <- plot_ly()
      if (isTRUE(input$show_sd_abs)) { fig <- fig %>% add_ribbons(data = df_plot, x = ~Absorbancia_Neta, ymin = ~Promedio_U_L - SD_U_L, ymax = ~Promedio_U_L + SD_U_L, color = ~Grupo, legendgroup = ~Grupo, opacity = 0.2, line = list(width = 0), name = ~paste(Grupo, "SD"), showlegend = FALSE, hoverinfo = 'none') }
      if (isTRUE(input$show_avg_abs)) { fig <- fig %>% add_trace(data = df_plot, x = ~Absorbancia_Neta, y = ~Promedio_U_L, type = 'scatter', mode = 'lines+markers', color = ~Grupo, legendgroup = ~Grupo, name = ~Grupo, line = list(dash = 'dash'), hoverinfo = 'text', text = ~paste0("<b>Grupo: ", Grupo, "</b><br>", "Abs Neta: ", round(Absorbancia_Neta, 3), "<br>", "Actividad Media: ", round(Promedio_U_L, 2), " U/L")) }
      df_long <- df_plot %>% pivot_longer(c(U_L_1, U_L_2), names_to = "Replica", values_to = "Actividad")
      if (isTRUE(input$show_od1_abs)) { fig <- fig %>% add_trace(data = filter(df_long, Replica == "U_L_1"), x = ~Absorbancia_Neta, y = ~Actividad, type = 'scatter', mode = 'markers', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "OD1"), marker = list(symbol = 'circle-open'), showlegend = FALSE, hoverinfo = 'text', text = ~paste0("<b>Muestra: ", Muestra_ID, " (OD1)</b><br>", "Abs Neta: ", round(Absorbancia_Neta, 3), "<br>", "Actividad: ", round(Actividad, 2), " U/L")) }
      if (isTRUE(input$show_od2_abs)) { fig <- fig %>% add_trace(data = filter(df_long, Replica == "U_L_2"), x = ~Absorbancia_Neta, y = ~Actividad, type = 'scatter', mode = 'markers', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "OD2"), marker = list(symbol = 'square-open'), showlegend = FALSE, hoverinfo = 'text', text = ~paste0("<b>Muestra: ", Muestra_ID, " (OD2)</b><br>", "Abs Neta: ", round(Absorbancia_Neta, 3), "<br>", "Actividad: ", round(Actividad, 2), " U/L")) }
      fig %>% layout(title = "Actividad Enzimática vs. Absorbancia", xaxis = list(title = "Absorbancia Correguida"), yaxis = list(title = "Actividad Calculada (U/L)"), legend = list(title = list(text = 'Grupo'))) %>% config(displaylogo = FALSE)
    })
    
    output$plotActivity <- renderPlotly({
      req(datos_procesados(), input$plotAct_groups)
      df_plot <- datos_procesados() %>% filter(Grupo %in% input$plotAct_groups) %>% arrange(Grupo, Tiempo_fermentacion)
      validate(need(nrow(df_plot) > 0, "Selecciona al menos un grupo para graficar."))
      fig <- plot_ly()
      if (isTRUE(input$show_sd_time)) { fig <- fig %>% add_ribbons(data = df_plot, x = ~Tiempo_fermentacion, ymin = ~Promedio_U_L - SD_U_L, ymax = ~Promedio_U_L + SD_U_L, color = ~Grupo, legendgroup = ~Grupo, opacity = 0.2, line = list(width = 0), name = ~paste(Grupo, "SD"), showlegend = FALSE, hoverinfo = 'none') }
      if (isTRUE(input$show_avg_time)) { fig <- fig %>% add_trace(data = df_plot, x = ~Tiempo_fermentacion, y = ~Promedio_U_L, type = 'scatter', mode = 'lines+markers', color = ~Grupo, legendgroup = ~Grupo, name = ~Grupo, hoverinfo = 'text', text = ~paste0("<b>Grupo: ", Grupo, "</b><br>", "Tiempo: ", Tiempo_fermentacion, " min<br>", "Actividad Media: ", round(Promedio_U_L, 2), " U/L")) }
      df_long <- df_plot %>% pivot_longer(c(U_L_1, U_L_2), names_to = "Replica", values_to = "Actividad")
      if (isTRUE(input$show_od1_time)) { fig <- fig %>% add_trace(data = filter(df_long, Replica == "U_L_1"), x = ~Tiempo_fermentacion, y = ~Actividad, type = 'scatter', mode = 'markers', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "OD1"), marker = list(symbol = 'circle-open'), showlegend = FALSE, hoverinfo = 'text', text = ~paste0("<b>Muestra: ", Muestra_ID, " (OD1)</b><br>", "Tiempo: ", Tiempo_fermentacion, " min<br>", "Actividad: ", round(Actividad, 2), " U/L")) }
      if (isTRUE(input$show_od2_time)) { fig <- fig %>% add_trace(data = filter(df_long, Replica == "U_L_2"), x = ~Tiempo_fermentacion, y = ~Actividad, type = 'scatter', mode = 'markers', color = ~Grupo, legendgroup = ~Grupo, name = ~paste(Grupo, "OD2"), marker = list(symbol = 'square-open'), showlegend = FALSE, hoverinfo = 'text', text = ~paste0("<b>Muestra: ", Muestra_ID, " (OD2)</b><br>", "Tiempo: ", Tiempo_fermentacion, " min<br>", "Actividad: ", round(Actividad, 2), " U/L")) }
      fig %>% layout(title = "Evolución Temporal de la Actividad Enzimática", xaxis = list(title = "Tiempo (min)"), yaxis = list(title = "Actividad de amilasa (U/L)"), legend = list(title = list(text = 'Grupo'))) %>% config(displaylogo = FALSE)
    })
    
    # --- RETURN VALUE ---
    return(reactive({
      req(datos_procesados())
      datos_procesados() %>%
        select(Grupo, Tiempo_fermentacion, Valor = Promedio_U_L) %>%
        mutate(TipoMedicion = "Amilasa")
    }))
  })
}