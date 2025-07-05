

glucosaUI <- function(id) {
  ns <- NS(id) # Crear el namespace para los IDs

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
        id = ns("tabs"), # ID del tabsetPanel
        tabPanel("Resultados Tabulados",
                 h4("Concentración de Glucosa por Grupo"),
                 uiOutput(ns("tablas_resultados"))),
        tabPanel("Análisis de Consumo",
                 h4("Parámetros de Glucosa por Grupo"),
                 uiOutput(ns("analisis_por_grupo"))),
        tabPanel("Gráficos de Análisis",
                 h4("1. Curva de Calibración (Estándares)"),
                 plotlyOutput(ns("plot_calibracion")),
                 hr(),
                 h4("2. Diagnóstico: Absorbancia vs. Glucosa Calculada (Muestras)"),
                 uiOutput(ns("diagnostic_controls_ui")),
                 plotlyOutput(ns("plot_diagnostico_muestras")),
                 hr(),
                 h4("3. Concentración de Glucosa vs. Tiempo (con Desviación Estándar)"),
                 uiOutput(ns("consumption_controls_ui")),
                 plotlyOutput(ns("plot_consumo_con_sd"))
        )
      )
    )
  )
}

glucosaServer <- function(id, datos_crudos_r = reactive(NULL)) { # datos_crudos_r se mantiene para compatibilidad, pero no se usa con fileInput directo.
  moduleServer(id, function(input, output, session) {

    datos_analizados <- eventReactive(input$calcular, {
      req(input$file_datos);
      validate(need(tools::file_ext(input$file_datos$name) == "csv", "Por favor, carga un archivo .csv"))

      df_raw <- tryCatch(
        read.csv(input$file_datos$datapath, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e) {
          shiny::validate("No se pudo leer el archivo CSV. Asegúrate de que el formato es correcto.")
        }
      )

      cols_requeridas <- c("Tipo", "Muestra_ID", "Tiempo_fermentacion", "Concentracion", "OD1", "OD2")
      validate(
        need(all(cols_requeridas %in% names(df_raw)),
             paste("Faltan una o más columnas requeridas:", paste(cols_requeridas, collapse = ", ")))
      )

      # 1. Calcular OD del blanco
      od_blanco_df <- df_raw %>%
        filter(Tipo == "Blanco") %>%
        pivot_longer(cols = c(OD1, OD2), names_to = "replica", values_to = "OD")

      validate(need(nrow(od_blanco_df) > 0, "No se encontraron datos de 'Blanco' en la columna 'Tipo'."))

      od_blanco <- mean(od_blanco_df$OD, na.rm = TRUE)
      validate(need(!is.na(od_blanco) && is.finite(od_blanco), "El OD del blanco no pudo ser calculado. Asegúrate que los valores de OD1/OD2 del blanco no son NA/Inf."))

      # 2. Curva de Calibración con Estándares
      df_estandares <- df_raw %>%
        filter(Tipo == "Estandar") %>%
        mutate(OD_prom = rowMeans(select(., OD1, OD2), na.rm = TRUE),
               OD_corregido = OD_prom - od_blanco)

      validate(
        need(nrow(df_estandares) >= 2, "Se necesitan al menos 2 estándares para construir la curva de calibración.")
      )

      # Modelo lineal: Concentracion = slope * OD_corregido (forzando intercepto a 0)
      # 'slope' ahora tendrá unidades de µM/OD
      modelo_lm <- tryCatch(
        lm(Concentracion ~ OD_corregido - 1, data = df_estandares),
        error = function(e) {
          shiny::validate(paste("Error al crear el modelo de calibración:", e$message))
        }
      )

      validate(need(!is.null(modelo_lm), "No se pudo crear el modelo de calibración."))

      slope <- tryCatch(
        coef(modelo_lm)[["OD_corregido"]],
        error = function(e) NA_real_
      )

      validate(
        need(!is.na(slope) && is.finite(slope) && slope != 0, "El slope de la curva de calibración es inválido (podría ser 0, NA o Inf)."),
        # Para que la glucosa aumente con la absorbancia, el slope debe ser positivo.
        need(slope > 0, "El slope de la curva de calibración es negativo. Esto es inusual para un ensayo de glucosa donde concentración aumenta con OD.")
      )

      # 3. Procesar Muestras
      df_muestras_long <- df_raw %>%
        filter(Tipo == "Muestra") %>%
        pivot_longer(
          cols = c(OD1, OD2),
          names_to = "replica",
          values_to = "OD_raw"
        ) %>%
        mutate(
          Grupo = substr(Muestra_ID, 1, 1),
          OD_corregida = OD_raw - od_blanco,
          Glucosa_uM = pmax(0, (OD_corregida / slope) * input$DF)
        )

      validate(
        need(nrow(df_muestras_long) > 0, "No se encontraron datos de tipo 'Muestra' en el archivo.")
      )

      # Sumarizar datos de muestras por grupo y tiempo (media y desviación estándar)
      df_muestras_sumarizado <- df_muestras_long %>%
        group_by(Grupo, Tiempo_fermentacion) %>%
        summarise(
          Glucosa_uM_mean = mean(Glucosa_uM, na.rm = TRUE),
          Glucosa_uM_sd = sd(Glucosa_uM, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        arrange(Grupo, Tiempo_fermentacion)

      list(estandares = df_estandares, modelo_calibracion = modelo_lm,
           muestras_long = df_muestras_long, muestras_sumarizado = df_muestras_sumarizado)
    })

    # --- Pestaña: Resultados Tabulados ---
    output$tablas_resultados <- renderUI({
      req(datos_analizados())
      lapply(unique(datos_analizados()$muestras_sumarizado$Grupo), function(g) {
        # Usar session$ns para los IDs de las tablas dinámicas
        tagList(h5(paste("Concentración de Glucosa - Grupo", g)), tableOutput(session$ns(paste0("tabla_res_", g))), hr())
      })
    })

    observe({
      req(datos_analizados())
      df_sumarizado <- datos_analizados()$muestras_sumarizado
      for (g in unique(df_sumarizado$Grupo)) {
        local({
          grupo_local <- g
          # Usar session$ns para asignar el output a la tabla dinámica
          output[[paste0("tabla_res_", grupo_local)]] <- renderTable({
            df_sumarizado %>%
              filter(Grupo == grupo_local) %>%
              # Convertir minutos a horas para la tabla de resultados
              mutate(Tiempo_h = Tiempo_fermentacion / 60) %>%
              # Seleccionar y renombrar las columnas deseadas
              select(
                `Tiempo (h)` = Tiempo_h,
                `Glucosa Media (µM)` = Glucosa_uM_mean,
                `Desv. Est. (µM)` = Glucosa_uM_sd
              ) %>%
              # Formatear todos los números a 3 decimales
              mutate(across(where(is.numeric), ~ sprintf("%.3f", .)))
          }, striped = TRUE, hover = TRUE, bordered = TRUE)
        })
      }
    })

    # --- Pestaña: Análisis de Consumo ---
    output$analisis_por_grupo <- renderUI({
      req(datos_analizados());

      tagList(
        lapply(unique(datos_analizados()$muestras_sumarizado$Grupo), function(g) {
          # Usar session$ns para los IDs de las tablas dinámicas
          tagList(h5(paste("Parámetros de Glucosa - Grupo", g)), tableOutput(session$ns(paste0("tabla_analisis_", g))), hr())
        })
      )
    })

    observe({
      req(datos_analizados());
      df_sumarizado <- datos_analizados()$muestras_sumarizado;

      for (g in unique(df_sumarizado$Grupo)) {
        local({
          grupo_local <- g;
          datos_grupo <- df_sumarizado %>% filter(Grupo == grupo_local) %>% arrange(Tiempo_fermentacion);

          # Usar session$ns para asignar el output a la tabla dinámica
          output[[paste0("tabla_analisis_", grupo_local)]] <- renderTable({
            if (nrow(datos_grupo) < 2) {
              return(data.frame(Parámetro = "Error", Valor = "Se necesitan al menos 2 puntos de tiempo para el cálculo de los parámetros."))
            };

            glucosa_inicial_medida <- datos_grupo$Glucosa_uM_mean[1]
            glucosa_final_medida <- tail(datos_grupo$Glucosa_uM_mean, 1)
            cambio_neto_total_glucosa <- glucosa_final_medida - glucosa_inicial_medida
            duracion_total <- tail(datos_grupo$Tiempo_fermentacion, 1) - datos_grupo$Tiempo_fermentacion[1]

            min_glucosa_val <- min(datos_grupo$Glucosa_uM_mean, na.rm = TRUE)
            max_glucosa_val <- max(datos_grupo$Glucosa_uM_mean, na.rm = TRUE)
            time_at_max_glucosa <- datos_grupo$Tiempo_fermentacion[which.max(datos_grupo$Glucosa_uM_mean)]

            glucosa_producida_inicial_a_max <- max_glucosa_val - glucosa_inicial_medida
            glucosa_consumida_max_a_final <- max_glucosa_val - glucosa_final_medida

            data.frame(
              Parámetro = c(
                "Glucosa Inicial Media (µM)",
                "Glucosa Final Media (µM)",
                "Glucosa Mínima Observada (µM)",
                "Glucosa Máxima Observada (µM)",
                "Tiempo a la Glucosa Máxima (min)",
                "Glucosa Producida (Inicial a Máx) (µM)",
                "Glucosa Consumida (Máx a Final) (µM)",
                "Cambio Neto Total Glucosa (µM)",
                "Duración del Experimento (min)"
              ),
              Valor = c(
                sprintf("%.3f", glucosa_inicial_medida),
                sprintf("%.3f", glucosa_final_medida),
                sprintf("%.3f", min_glucosa_val),
                sprintf("%.3f", max_glucosa_val),
                sprintf("%.3f", time_at_max_glucosa),
                sprintf("%.3f", glucosa_producida_inicial_a_max),
                sprintf("%.3f", glucosa_consumida_max_a_final),
                sprintf("%.3f", cambio_neto_total_glucosa),
                sprintf("%.3f", duracion_total)
              )
            )
          }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'l')
        })
      }
    })

    # --- Pestaña: Gráficos de Análisis ---
    grupos_disponibles_reactivos <- reactive({ req(datos_analizados()); unique(datos_analizados()$muestras_sumarizado$Grupo) })

    output$diagnostic_controls_ui <- renderUI({
      grupos <- grupos_disponibles_reactivos();
      # Usar session$ns para los IDs de los inputs
      checkboxGroupInput(session$ns("selected_groups_diagnostic"), "Seleccionar Grupos:", choices = grupos, selected = grupos, inline = TRUE)
    })

    output$consumption_controls_ui <- renderUI({
      grupos <- grupos_disponibles_reactivos();
      fluidRow(
        # Usar session$ns para los IDs de los inputs
        column(8, checkboxGroupInput(session$ns("selected_groups_consumption"), "Seleccionar Grupos:", choices = grupos, selected = grupos, inline = TRUE)),
        column(4, checkboxInput(session$ns("show_sd"), "Desv. Estándar", value = TRUE))
      )
    })

    output$plot_calibracion <- renderPlotly({
      req(datos_analizados());
      datos <- datos_analizados();
      df_estandares <- datos$estandares;
      modelo <- datos$modelo_calibracion;

      # Obtener el R^2 del modelo
      r2_valor <- summary(modelo)$r.squared

      p <- ggplot(df_estandares, aes(x = OD_corregido, y = Concentracion,
                                     text = paste("Abs Corregida:", round(OD_corregido, 3),
                                                  "<br>Concentración:", round(Concentracion, 2), "µM"))) +
        geom_point(color = "blue", size = 3, alpha = 0.8) +
        labs(title = "Curva de Calibración",
             x = "Absorbancia Corregida (OD Muestra - OD Blanco)",
             y = "Concentración de Glucosa (µM)") +
        theme_minimal(base_size = 10) +
        theme(plot.title = element_text(size = 12, hjust = 0.5));

      fig <- ggplotly(p, tooltip = "text");

      # Añadir la línea de regresión usando el modelo ajustado
      fig <- fig %>%
        add_lines(x = ~OD_corregido, y = ~predict(modelo), data = df_estandares,
                  name = 'Regresión', line = list(color = 'red', width = 2), inherit = FALSE);

      # Actualizar la fórmula mostrada en el gráfico
      formula_texto <- sprintf("[Glucosa] = %.3f * OD", coef(modelo)[["OD_corregido"]]);
      r2_texto <- sprintf("R<sup>2</sup> = %.4f", r2_valor);

      fig <- fig %>%
        layout(annotations = list(x = 0.95, y = 0.05, xref = "paper", yref = "paper",
                                  text = paste(formula_texto, r2_texto, sep = "<br>"),
                                  showarrow = FALSE, xanchor = 'right', yanchor = 'bottom',
                                  font = list(size=12))) %>%
        config(displaylogo = FALSE)
      fig
    })

    output$plot_diagnostico_muestras <- renderPlotly({
      req(datos_analizados(), input$selected_groups_diagnostic) # input está en el namespace del módulo
      df_muestras_filtrado <- datos_analizados()$muestras_long %>%
        filter(Grupo %in% input$selected_groups_diagnostic) %>%
        group_by(Grupo, Muestra_ID, Tiempo_fermentacion) %>%
        summarise(
          OD_prom_corregida = mean(OD_corregida, na.rm = TRUE),
          Glucosa_uM_mean = mean(Glucosa_uM, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(Muestra_ID, Tiempo_fermentacion)

      # Calcular rangos dinámicos para los ejes X y Y
      x_min_data <- min(df_muestras_filtrado$OD_prom_corregida, na.rm = TRUE)
      x_max_data <- max(df_muestras_filtrado$OD_prom_corregida, na.rm = TRUE)
      y_min_data <- min(df_muestras_filtrado$Glucosa_uM_mean, na.rm = TRUE)
      y_max_data <- max(df_muestras_filtrado$Glucosa_uM_mean, na.rm = TRUE)

      # Añadir un pequeño margen para que los puntos no toquen los bordes
      x_lower_bound <- max(0, x_min_data - (x_max_data - x_min_data) * 0.1)
      x_upper_bound <- x_max_data + (x_max_data - x_min_data) * 0.1
      y_lower_bound <- max(0, y_min_data - (y_max_data - y_min_data) * 0.1)
      y_upper_bound <- y_max_data + (y_max_data - y_min_data) * 0.1

      # Manejar el caso donde el rango de datos es casi cero
      if ((x_upper_bound - x_lower_bound) < 1e-6) { x_upper_bound <- x_lower_bound + 0.1 }
      if ((y_upper_bound - y_lower_bound) < 1e-6) { y_upper_bound <- y_lower_bound + 0.1 }

      plot_ly(data = df_muestras_filtrado, x = ~OD_prom_corregida, y = ~Glucosa_uM_mean,
              type = 'scatter',
              mode = 'lines+markers',
              group = ~Muestra_ID,
              line = list(dash = 'dot'),
              color = ~Grupo,
              text = ~paste("Muestra:", Muestra_ID,
                            "<br>Grupo:", Grupo,
                            "<br>Tiempo:", Tiempo_fermentacion, "min",
                            "<br>Abs Corregida:", round(OD_prom_corregida, 3),
                            "<br>Glucosa:", round(Glucosa_uM_mean, 2), "µM"),
              hoverinfo = 'text') %>%
        layout(title = "Diagnóstico: Relación Absorbancia vs. Glucosa en Muestras",
               xaxis = list(title = "Absorbancia Promedio Corregida", range = c(x_lower_bound, x_upper_bound)),
               yaxis = list(title = "Glucosa Calculada (µM)", range = c(y_lower_bound, y_upper_bound)),
               legend = list(title = list(text = 'Grupo'))) %>%
        config(displaylogo = FALSE)
    })


    output$plot_consumo_con_sd <- renderPlotly({
      req(datos_analizados(), input$selected_groups_consumption); # input está en el namespace del módulo
      df_filtrado <- datos_analizados()$muestras_sumarizado %>% filter(Grupo %in% input$selected_groups_consumption);

      fig <- plot_ly();
      for(g in unique(df_filtrado$Grupo)) {
        df_grupo <- filter(df_filtrado, Grupo == g);
        fig <- fig %>% add_trace(data = df_grupo, x = ~Tiempo_fermentacion, y = ~Glucosa_uM_mean,
                                 type = 'scatter', mode = 'lines+markers', name = g,
                                 text = ~paste("Grupo:", Grupo,
                                               "<br>Tiempo:", Tiempo_fermentacion, "min",
                                               "<br>Glucosa (media):", round(Glucosa_uM_mean, 2), "µM"),
                                 hoverinfo = 'text',
                                 error_y = if(input$show_sd) { list(type = "data", array = ~Glucosa_uM_sd, visible=TRUE) } else { NULL }) # input está en el namespace del módulo
      };
      fig %>% layout(title = "Concentración de Glucosa Durante la Fermentación",
                     xaxis = list(title = "Tiempo (min)"),
                     yaxis = list(title = "Concentración de Glucosa (µM)"),
                     legend = list(title=list(text='Grupo'))) %>%
        config(displaylogo = FALSE)
    })

    # --- Dado que no se solicitó la funcionalidad de descarga de reporte PDF en este código ---
    # La parte de UI para el botón de descarga no se renderiza.
    output$download_button_ui <- renderUI({
      NULL # No hay botón de descarga en este código fuente
    })
    # La lógica para downloadHandler no está presente en el código fuente proporcionado.

    # Devolver los datos procesados para el módulo integrado
    return(reactive({
      req(datos_analizados())
      datos_analizados()$muestras_sumarizado %>%
        select(Grupo, Tiempo_fermentacion, Valor = Glucosa_uM_mean) %>%
        # Añadir una columna TipoMedicion para que el módulo integrado sepa qué es
        mutate(TipoMedicion = "Glucosa")
    }))
  })
}