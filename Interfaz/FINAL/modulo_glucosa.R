# modulo_glucosa.R


glucosaUI <- function(id) {
  ns <- NS(id) # Crear el namespace para los IDs
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Carga de Datos"),
      fileInput(ns("file_datos"), "Selecciona archivo CSV único:", accept = c(".csv")),
      helpText(HTML("El CSV debe contener: <b>Tipo, Muestra_ID, Tiempo_fermentacion, Concentracion, OD1, OD2</b>.<br>
                     - <b>Tipo</b>: 'Muestra', 'Blanco', o 'Estandar'.<br>
                     - Para 'Blanco'/'Estandar', deje Muestra_ID y Tiempo_fermentacion en blanco.<br>
                     - <b>Concentracion</b>: Solo para 'Estandar' (concentración conocida de glucosa).<br>
                     - Cada fila representa un punto de datos con sus dos réplicas.<br>
                     - <b>Muestra_ID</b> debe tener un prefijo de grupo (ej. A, B) y un sufijo numérico (ej. A1, B2).<br>
                     <b>Ejemplo de formato:</b>
                     <pre style='font-size: 10px;'>Tipo,Muestra_ID,Tiempo_fermentacion,Concentracion,OD1,OD2\nBlanco,,,,,0.127,0.111\nEstandar,,,,100,0.600,0.614\nEstandar,,,,50,0.350,0.345\nMuestra,A1,0,,0.150,0.155\nMuestra,A1,24,,0.350,0.345\nMuestra,B1,0,,0.180,0.190\nMuestra,B1,48,,0.400,0.395</pre>")),
      hr(),
      h4("2. Parámetros de Cálculo"),
      numericInput(ns("DF"), "Factor de dilución (DF):", value = 1, min = 1, step = 1),
      actionButton(ns("calcular"), "Calcular y Graficar", class = "btn-primary btn-lg", icon = icon("cogs"))
    ),
    mainPanel(
      tabsetPanel(
        id = ns("tabs"),
        tabPanel("Resultados Tabulados", h4("Resultados de Concentración de Glucosa por Grupo"), uiOutput(ns("resultados_tabulados_ui"))),
        tabPanel("Parámetros Cinéticos", h4("Parámetros Cinéticos de Consumo de Glucosa por Grupo"), verbatimTextOutput(ns("parametros_cineticos"))),
        tabPanel("Gráficas",
                 # Gráfica de Curva de Calibración
                 h4("1. Curva de Calibración de Glucosa"),
                 plotlyOutput(ns("plot_calibracion"), height = "400px"), # Usando plotly para interactividad aquí
                 hr(),
                 # Opciones para Gráfica de Concentración vs. Absorbancia Neta
                 fluidRow(
                   column(12, h5("Opciones para 'Glucosa vs. Absorbancia Neta'"))
                 ),
                 fluidRow(
                   column(6, checkboxGroupInput(ns("plotGlucosaAbs_groups"), "Seleccionar Grupo(s):", choices = NULL))
                 ),
                 h4("2. Glucosa vs. Absorbancia Neta"),
                 plotOutput(ns("plotGlucosaAbs"), height = "400px"),
                 hr(),
                 # Opciones para Gráfica de Concentración vs. Tiempo
                 fluidRow(
                   column(12, h5("Opciones para 'Glucosa vs. Tiempo'"))
                 ),
                 fluidRow(
                   column(6, checkboxGroupInput(ns("plotGlucosaTiempo_groups"), "Seleccionar Grupo(s):", choices = NULL)),
                   column(6, checkboxInput(ns("plotGlucosaTiempo_show_sd"), "Mostrar Desviación Estándar", value = FALSE))
                 ),
                 h4("3. Concentración de Glucosa vs. Tiempo"),
                 plotOutput(ns("plotGlucosaTiempo"), height = "400px"),
                 hr(),
                 # Opciones para Gráfica de Cinética de Consumo
                 fluidRow(
                   column(12, h5("Opciones para 'Cinética de Consumo de Glucosa (Orden 1)'"))
                 ),
                 fluidRow(
                   column(6, checkboxGroupInput(ns("plotGlucosaConsumo_groups"), "Seleccionar Grupo(s):", choices = NULL))
                 ),
                 h4("4. Cinética de Consumo de Glucosa (Orden 1)"),
                 plotOutput(ns("plotGlucosaConsumo"), height = "400px")
        )
      ),
      hr(),
      uiOutput(ns("download_button_ui")) # Botón de descarga
    )
  )
}

glucosaServer <- function(id, datos_crudos_r = reactive(NULL)) { # Ahora recibe datos_crudos_r, aunque aquí aún se usa fileInput
  moduleServer(id, function(input, output, session) {
    
    # Reactive value to store processed data
    datos_procesados <- eventReactive(input$calcular, {
      req(input$file_datos)
      
      df_raw <- read.csv(input$file_datos$datapath, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
      cols_requeridas <- c("Tipo", "Muestra_ID", "Tiempo_fermentacion", "Concentracion", "OD1", "OD2")
      
      validate(need(all(cols_requeridas %in% names(df_raw)), paste("El archivo CSV debe contener las columnas:", paste(cols_requeridas, collapse = ", "))))
      
      # --- 1. Procesamiento de Blancos ---
      od_blanco_df <- df_raw %>% pivot_longer(cols = c(OD1, OD2), names_to = "replica", values_to = "OD") %>%
        filter(Tipo == "Blanco")
      validate(need(nrow(od_blanco_df) > 0, "No hay datos de 'Blanco'."))
      od_blanco <- mean(od_blanco_df$OD, na.rm = TRUE)
      validate(need(!is.na(od_blanco) && !is.infinite(od_blanco), "El OD del blanco es NA o Infinito."))
      
      # --- 2. Procesamiento de Estándares y Curva de Calibración ---
      df_estandares <- df_raw %>%
        filter(Tipo == "Estandar") %>%
        mutate(OD_prom = rowMeans(select(., OD1, OD2), na.rm = TRUE),
               OD_corregido = OD_prom - od_blanco)
      
      validate(need(nrow(df_estandares) >= 2, "Se necesitan al menos 2 estándares para la curva de calibración."))
      
      # Asegurarse de que no haya NaN/Inf en los datos para el modelo lineal
      df_estandares_clean <- df_estandares %>%
        filter(is.finite(Concentracion) & is.finite(OD_corregido))
      
      validate(need(nrow(df_estandares_clean) >= 2, "Datos de estándares insuficientes o inválidos para la calibración después de la limpieza."))
      
      # El modelo es Concentracion ~ OD_corregido - 1 (sin intercepto)
      modelo_lm <- tryCatch(
        lm(Concentracion ~ OD_corregido - 1, data = df_estandares_clean),
        error = function(e) {
          shiny::validate(paste("Error al crear el modelo de calibración:", e$message))
        }
      )
      
      validate(need(!is.null(modelo_lm), "No se pudo crear el modelo de calibración."))
      
      slope <- tryCatch(
        coef(modelo_lm)[["OD_corregido"]],
        error = function(e) NA_real_
      )
      
      validate(need(!is.na(slope) && is.finite(slope) && slope != 0, "El slope de la curva de calibración es inválido (NA, Inf, o cero)."),
               need(slope > 0, "El slope de la curva de calibración debe ser positivo para que la concentración aumente con la absorbancia."))
      
      # --- 3. Procesamiento de Muestras ---
      df_muestras <- df_raw %>% filter(Tipo == "Muestra")
      validate(need(nrow(df_muestras) > 0, "No se encontraron filas con Tipo 'Muestra'."))
      
      df_final_glucosa <- df_muestras %>%
        mutate(Grupo = str_extract(Muestra_ID, "^[A-Za-z]+")) %>% # Extraer el prefijo del grupo
        # Calcular absorbancias netas para cada réplica
        mutate(
          Absorbancia_Neta_1 = OD1 - od_blanco,
          Absorbancia_Neta_2 = OD2 - od_blanco
        ) %>%
        # Calcular concentraciones de glucosa para cada réplica
        mutate(
          Glucosa_uM_1 = pmax(0, (Absorbancia_Neta_1 / slope) * input$DF),
          Glucosa_uM_2 = pmax(0, (Absorbancia_Neta_2 / slope) * input$DF)
        ) %>%
        # Calcular promedio y desviación estándar para las réplicas
        rowwise() %>%
        mutate(
          # Lista de concentraciones válidas (no NA ni Inf)
          valid_concentrations = list(c(Glucosa_uM_1, Glucosa_uM_2) %>% na.omit() %>% .[is.finite(.)]),
          Promedio_Glucosa = if(length(valid_concentrations) > 0) mean(valid_concentrations) else NA_real_,
          SD_Glucosa = case_when(
            length(valid_concentrations) >= 2 ~ sd(valid_concentrations),
            length(valid_concentrations) == 1 ~ 0,
            TRUE ~ NA_real_
          ),
          # Absorbancia neta promedio para la gráfica de diagnóstico
          Absorbancia_Neta_Promedio = mean(c(Absorbancia_Neta_1, Absorbancia_Neta_2), na.rm = TRUE)
        ) %>%
        ungroup() %>%
        # Convertir tiempo a horas
        mutate(Tiempo_fermentacion_h = Tiempo_fermentacion / 60) %>%
        arrange(Grupo, Tiempo_fermentacion) # Organizar para consistencia
      
      list(
        estandares = df_estandares,
        modelo_calibracion = modelo_lm,
        slope = slope,
        df_final_glucosa = df_final_glucosa,
        od_blanco = od_blanco
      )
    })
    
    # Actualizar las opciones de los selectores de grupos (checkboxGroupInput)
    observeEvent(datos_procesados(), {
      grupos_disponibles <- unique(datos_procesados()$df_final_glucosa$Grupo)
      updateCheckboxGroupInput(session, "plotGlucosaAbs_groups", choices = grupos_disponibles, selected = grupos_disponibles)
      updateCheckboxGroupInput(session, "plotGlucosaTiempo_groups", choices = grupos_disponibles, selected = grupos_disponibles)
      updateCheckboxGroupInput(session, "plotGlucosaConsumo_groups", choices = grupos_disponibles, selected = grupos_disponibles)
    })
    
    # --- RESULTADOS TABULADOS (por grupo) ---
    output$resultados_tabulados_ui <- renderUI({
      req(datos_procesados())
      df <- datos_procesados()$df_final_glucosa
      grupos <- unique(df$Grupo)
      
      lapply(grupos, function(g) {
        df_grupo <- df %>%
          filter(Grupo == g) %>%
          select(Muestra_ID, Tiempo_fermentacion_h, Promedio_Glucosa, SD_Glucosa) %>%
          rename(
            `Muestra ID` = Muestra_ID,
            `Tiempo (h)` = Tiempo_fermentacion_h,
            `Glucosa Promedio (µM)` = Promedio_Glucosa,
            `Desviación Estándar (µM)` = SD_Glucosa
          ) %>%
          mutate(
            `Tiempo (h)` = floor(`Tiempo (h)`), # Sin decimales para Tiempo (h)
            `Glucosa Promedio (µM)` = round(`Glucosa Promedio (µM)`, 2), # Dos decimales
            `Desviación Estándar (µM)` = round(`Desviación Estándar (µM)`, 2) # Dos decimales
          )
        
        tagList(
          h5(paste("Grupo de Muestras:", g), style = "font-weight: bold;"),
          renderTable(df_grupo, striped = TRUE, hover = TRUE, bordered = TRUE)
        )
      })
    })
    
    # --- PARÁMETROS CINÉTICOS de Consumo (por grupo) ---
    parametros_cineticos_glucosa_por_grupo <- reactive({
      req(datos_procesados())
      df <- datos_procesados()$df_final_glucosa
      
      df %>%
        group_by(Grupo) %>%
        do({
          df_grupo <- . # Los datos del grupo actual
          text_output <- paste0("\n--- Grupo: ", unique(df_grupo$Grupo), " ---\n")
          
          # Ordenar por tiempo para asegurar la secuencia
          df_grupo <- df_grupo %>% arrange(Tiempo_fermentacion_h)
          
          # Asegurarse de que haya al menos un punto inicial y final
          if (nrow(df_grupo) < 2) {
            text_output <- paste0(text_output, "Se necesitan al menos 2 puntos de datos para el análisis cinético.\n")
          } else {
            glucosa_inicial <- df_grupo$Promedio_Glucosa[df_grupo$Tiempo_fermentacion_h == min(df_grupo$Tiempo_fermentacion_h, na.rm = TRUE)]
            glucosa_final <- tail(df_grupo$Promedio_Glucosa, 1)
            tiempo_total_h <- tail(df_grupo$Tiempo_fermentacion_h, 1) - min(df_grupo$Tiempo_fermentacion_h, na.rm = TRUE)
            
            # Consumo total
            consumo_total <- glucosa_inicial - glucosa_final
            
            # Tasa de consumo promedio (si hay duración)
            tasa_consumo_promedio <- if (tiempo_total_h > 0) consumo_total / tiempo_total_h else 0
            
            text_output <- paste0(text_output,
                                  sprintf("Glucosa Inicial (µM)         = %.2f\n", glucosa_inicial),
                                  sprintf("Glucosa Final (µM)           = %.2f\n", glucosa_final),
                                  sprintf("Consumo Total (µM)           = %.2f\n", consumo_total),
                                  sprintf("Tasa Consumo Promedio (µM/h) = %.2f\n", tasa_consumo_promedio))
            
            # Ajuste a cinética de primer orden (ln(C) vs Tiempo)
            df_log <- df_grupo %>% filter(Promedio_Glucosa > 0) %>%
              mutate(log_Glucosa = log(Promedio_Glucosa))
            
            if (nrow(df_log) >= 2) {
              model_1st_order <- tryCatch(
                lm(log_Glucosa ~ Tiempo_fermentacion_h, data = df_log),
                error = function(e) NULL
              )
              
              if (!is.null(model_1st_order) && !is.na(coef(model_1st_order)[2])) {
                k_obs <- -coef(model_1st_order)[2] # La constante de velocidad k
                r_squared <- summary(model_1st_order)$r.squared
                
                text_output <- paste0(text_output,
                                      sprintf("Constante velocidad (k, 1er orden) = %.4f h⁻¹ (R² = %.3f)\n", k_obs, r_squared))
                if (k_obs > 0) {
                  t_half <- log(2) / k_obs
                  text_output <- paste0(text_output,
                                        sprintf("Vida media (t1/2)                 = %.2f h\n", t_half))
                } else {
                  text_output <- paste0(text_output, "No se calculó t1/2 (k no positivo).\n")
                }
              } else {
                text_output <- paste0(text_output, "No se pudo ajustar el modelo de primer orden para la cinética de consumo.\n")
              }
            } else {
              text_output <- paste0(text_output, "Datos insuficientes para el ajuste de primer orden.\n")
            }
          }
          data.frame(text_output = text_output)
        }) %>%
        pull(text_output) %>%
        paste(collapse = "\n")
    })
    
    output$parametros_cineticos <- renderPrint({
      cat(parametros_cineticos_glucosa_por_grupo())
    })
    
    # --- GRÁFICAS ---
    
    # Gráfica 1: Curva de Calibración
    output$plot_calibracion <- renderPlotly({
      req(datos_procesados())
      datos <- datos_procesados()
      df_estandares <- datos$estandares
      modelo <- datos$modelo_calibracion
      r2 <- summary(modelo)$r.squared
      
      p <- ggplot(df_estandares, aes(x=OD_corregido, y=Concentracion,
                                     text=paste("Abs:",round(OD_corregido,3),"<br>Conc:",round(Concentracion,2),"µM"))) +
        geom_point(color="blue",size=3,alpha=0.8) +
        labs(title="Curva de Calibración de Glucosa",
             x="Absorbancia Corregida (OD muestra - OD blanco)",
             y="Conc. Glucosa (µM)") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5))
      
      fig <- ggplotly(p, tooltip="text")
      fig <- fig %>% add_lines(x=~OD_corregido, y=~predict(modelo), data=df_estandares,
                               name='Regresión', line=list(color='red'))
      
      formula_t <- sprintf("[Glucosa]=%.3f*OD",coef(modelo)[["OD_corregido"]])
      r2_t <- sprintf("R<sup>2</sup>=%.4f",r2)
      
      fig %>% layout(annotations=list(x=0.95,y=0.05,xref="paper",yref="paper",text=paste(formula_t,r2_t,sep="<br>"),
                                      showarrow=F,xanchor='right',yanchor='bottom')) %>%
        config(displaylogo=F)
    })
    
    # Gráfica 2: Glucosa vs. Absorbancia Neta (Muestras)
    output$plotGlucosaAbs <- renderPlot({
      req(datos_procesados(), input$plotGlucosaAbs_groups)
      df_plot <- datos_procesados()$df_final_glucosa %>% filter(Grupo %in% input$plotGlucosaAbs_groups)
      
      if (nrow(df_plot) == 0) {
        return(ggplot() + annotate("text", x=0.5, y=0.5, label="Selecciona grupo(s) para visualizar.", size=6, color="gray") + theme_void())
      }
      
      df_plot_ordered <- df_plot %>% arrange(Grupo, Absorbancia_Neta_Promedio)
      
      ggplot(df_plot_ordered, aes(x = Absorbancia_Neta_Promedio, y = Promedio_Glucosa, color = Grupo)) +
        geom_point(size = 4, alpha = 0.8) +
        geom_line(aes(group = Grupo), linetype = "dotted", size = 0.8) +
        labs(title = "Glucosa vs. Absorbancia Neta en Muestras",
             x = "Absorbancia Neta Promedio (OD muestra - OD blanco)",
             y = "Glucosa Promedio (µM)") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
    })
    
    # Gráfica 3: Concentración de Glucosa vs. Tiempo (con opción SD)
    output$plotGlucosaTiempo <- renderPlot({
      req(datos_procesados(), input$plotGlucosaTiempo_groups)
      df_plot <- datos_procesados()$df_final_glucosa %>% filter(Grupo %in% input$plotGlucosaTiempo_groups)
      
      if (nrow(df_plot) == 0) {
        return(ggplot() + annotate("text", x=0.5, y=0.5, label="Selecciona grupo(s) para visualizar.", size=6, color="gray") + theme_void())
      }
      
      p <- ggplot(df_plot, aes(x = Tiempo_fermentacion_h, y = Promedio_Glucosa, color = Grupo, group = Grupo)) +
        geom_line(size = 1) +
        geom_point(size = 4, alpha = 0.8) +
        labs(title = "Concentración de Glucosa vs. Tiempo",
             x = "Tiempo (horas)",
             y = "Glucosa Promedio (µM)") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
      
      if (input$plotGlucosaTiempo_show_sd) {
        p <- p + geom_errorbar(data = df_plot %>% filter(!is.na(SD_Glucosa) & SD_Glucosa >= 0),
                               aes(ymin = Promedio_Glucosa - SD_Glucosa, ymax = Promedio_Glucosa + SD_Glucosa), width = 0.2, alpha = 0.5)
      }
      p
    })
    
    # Gráfica 4: Cinética de Consumo de Glucosa (Orden 1)
    output$plotGlucosaConsumo <- renderPlot({
      req(datos_procesados(), input$plotGlucosaConsumo_groups)
      df_plot <- datos_procesados()$df_final_glucosa %>% filter(Grupo %in% input$plotGlucosaConsumo_groups)
      
      if (nrow(df_plot) == 0) {
        return(ggplot() + annotate("text", x=0.5, y=0.5, label="Selecciona grupo(s) para visualizar la cinética de consumo.", size=6, color="gray") + theme_void())
      }
      
      list_of_consumption_plots <- list()
      grupos_seleccionados <- unique(df_plot$Grupo)
      
      for (g in grupos_seleccionados) {
        df_grupo <- df_plot %>% filter(Grupo == g) %>% arrange(Tiempo_fermentacion_h)
        
        plot_cons <- NULL
        # Solo puntos con glucosa > 0 para logaritmo
        df_log <- df_grupo %>% filter(Promedio_Glucosa > 0) %>%
          mutate(log_Glucosa = log(Promedio_Glucosa))
        
        if (nrow(df_log) >= 2) {
          plot_cons <- ggplot(df_log, aes(x = Tiempo_fermentacion_h, y = log_Glucosa)) +
            geom_point(color = "purple", size = 4, alpha = 0.8) +
            geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
            labs(title = paste("Cinética de Consumo (Orden 1) - Grupo", g),
                 x = "Tiempo (horas)", y = "ln(Glucosa Promedio (µM))") +
            theme_minimal(base_size = 14) +
            theme(plot.title = element_text(hjust = 0.5))
        }
        
        if (is.null(plot_cons)) {
          plot_cons <- ggplot() +
            annotate("text", x=0.5, y=0.5, label=paste("No hay datos para cinética de consumo\n(ln) en el Grupo", g), size=5, color="gray") +
            theme_void() +
            ggtitle(paste("Cinética de Consumo (Orden 1) - Grupo", g)) +
            theme(plot.title = element_text(hjust = 0.5))
        }
        list_of_consumption_plots[[g]] <- plot_cons
      }
      
      if (length(list_of_consumption_plots) > 0) {
        wrap_plots(list_of_consumption_plots, ncol = ceiling(sqrt(length(list_of_consumption_plots))))
      } else {
        ggplot() + annotate("text", x=0.5, y=0.5, label="No hay datos de cinética de consumo para los grupos seleccionados.", size=6, color="gray") + theme_void()
      }
    })
    
    # --- Descarga de Reporte ---
    output$download_button_ui <- renderUI({
      req(datos_procesados())
      downloadButton(session$ns("downloadReport"), "Descargar Reporte Completo (PDF)", class = "btn-success")
    })
    
    output$downloadReport <- downloadHandler(
      filename = function() { paste0("reporte_glucosa_", Sys.Date(), ".pdf") },
      content = function(file) {
        showNotification("Generando reporte en PDF, por favor espere...", duration = 10, type = "message")
        
        tabla_data <- datos_procesados()$df_final_glucosa
        params_texto <- parametros_cineticos_glucosa_por_grupo()
        grupos_disponibles_rep <- unique(tabla_data$Grupo)
        df_estandares_rep <- datos_procesados()$estandares
        modelo_calibracion_rep <- datos_procesados()$modelo_calibracion
        
        temp_dir <- tempdir()
        temp_template_path <- file.path(temp_dir, "reporte_template_glucosa.Rmd")
        # Asegúrate de que 'reporte_template_glucosa.Rmd' esté en el mismo directorio que app.R
        file.copy("reporte_template_glucosa.Rmd", temp_template_path, overwrite = TRUE)
        
        params <- list(
          datos_completos = tabla_data,
          resultados_cineticos = params_texto,
          grupos_disponibles_rep = grupos_disponibles_rep,
          nombre_archivo_original = input$file_datos$name,
          factor_dilucion = input$DF,
          estandares_data = df_estandares_rep,
          calibracion_model = modelo_calibracion_rep
        )
        
        rmarkdown::render(
          input = temp_template_path,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
        
        showNotification("Reporte PDF generado exitosamente!", duration = 5, type = "success")
      }
    )
    
    # Devolver los datos procesados para el módulo integrado
    return(reactive({
      req(datos_procesados())
      datos_procesados()$df_final_glucosa %>%
        select(Grupo, Tiempo_fermentacion, Valor = Promedio_Glucosa) %>%
        # Añadir una columna TipoMedicion para que el módulo integrado sepa qué es
        mutate(TipoMedicion = "Glucosa")
    }))
  })
}