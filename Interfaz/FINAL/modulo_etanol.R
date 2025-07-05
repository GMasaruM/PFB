
# modulo_etanol.R
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
# rmarkdown y knitr no son necesarios porque se eliminó el downloadHandler en los módulos anteriores.
# library(rmarkdown)
# library(knitr)
library(stringr) # Necesario para str_extract

etanolUI <- function(id) {
  # <<< 1. Se crea el namespace para los IDs >>>
  ns <- NS(id)
  
  # <<< 2. La UI original se pega aquí, sin fluidPage/titlePanel >>>
  # Se envuelven todos los IDs de input/output con ns()
  sidebarLayout(
    sidebarPanel(
      h4("1. Carga de Datos de Etanol"),
      fileInput(ns("file_datos"), "Selecciona archivo CSV único:", accept = c(".csv")),
      helpText(HTML("El CSV debe contener: <b>Tipo, Muestra_ID, Tiempo_fermentacion, concentracion_etanol, OD1, OD2</b>.<br>
                     - <b>Tipo</b>: 'Muestra', 'Blanco', o 'Estandar'.<br>
                     - <b>concentracion_etanol</b>: Solo para 'Estandar' (concentración conocida de etanol).<br>
                     - <b>Muestra_ID</b> debe identificar los grupos con una letra inicial (ej: A1, B2).<br>
                     - <b>OD1 y OD2</b> son los duplicados de la absorbancia.")),
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
        tabPanel("Parámetros Cinéticos", h4("Parámetros por Grupo"), uiOutput(ns("parametros_ui"))), # CAMBIO: de verbatimTextOutput a uiOutput
        tabPanel("Gráficas",
                 h4("Curva de Calibración"), plotOutput(ns("plotCurvaCalibracion")),
                 hr(),
                 h4("Absorbancia vs Concentración"),
                 fluidRow(
                   column(4, checkboxInput(ns("show_prom_abs"), "Promedio", value = TRUE)),
                   column(4, checkboxInput(ns("show_od1_abs"), "OD1", value = FALSE)),
                   column(4, checkboxInput(ns("show_od2_abs"), "OD2", value = FALSE)),
                   column(4, checkboxInput(ns("show_desv_abs"), "DE", value = TRUE))
                 ),
                 plotOutput(ns("plotAbsorbanciaMuestras")),
                 hr(),
                 h4("Concentración de Etanol vs Tiempo"),
                 fluidRow(
                   column(4, checkboxInput(ns("show_prom_conc"), "Promedio", value = TRUE)),
                   column(4, checkboxInput(ns("show_od1_conc"), "OD1", value = FALSE)),
                   column(4, checkboxInput(ns("show_od2_conc"), "OD2", value = FALSE)),
                   column(4, checkboxInput(ns("show_desv_conc"), "DE", value = TRUE))
                 ),
                 plotOutput(ns("plotConcentracionEtanolTiempo"))
        )
      )
    )
  )
}


etanolServer <- function(id) {
  # <<< 3. Toda la lógica del servidor se envuelve en moduleServer >>>
  moduleServer(id, function(input, output, session) {
    
    # --- Tu código de servidor original, pegado directamente ---
    
    datos_procesados <- eventReactive(input$calcular, {
      req(input$file_datos)
      df_raw <- read.csv(input$file_datos$datapath, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
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
      
      # Asegurar que hay suficientes puntos para el modelo lineal y que no son todos el mismo valor
      validate(
        need(nrow(curva_calib) >= 2, "Se necesitan al menos 2 puntos para la curva de calibración (incluyendo el blanco)."),
        need(sd(curva_calib$OD_promedio, na.rm = TRUE) > 1e-6, "Las absorbancias de los estándares son todas iguales. No se puede formar una curva de calibración.")
      )
      
      modelo_calib <- tryCatch(
        lm(OD_promedio ~ concentracion_etanol, data = curva_calib),
        error = function(e) {
          shiny::validate(paste("Error al crear el modelo de calibración:", e$message))
        }
      )
      
      validate(need(!is.null(modelo_calib), "No se pudo crear el modelo de calibración."))
      
      Slope <- coef(modelo_calib)[2]
      R_squared <- summary(modelo_calib)$r.squared
      
      validate(
        need(!is.na(Slope) && is.finite(Slope) && Slope != 0, "El slope de la curva de calibración es inválido (podría ser 0, NA o Inf)."),
        need(Slope > 0, "El slope de la curva de calibración es negativo. Esto es inusual para la concentración de etanol.")
      )
      
      muestras <- df_raw %>% filter(Tipo == "Muestra")
      validate(need(nrow(muestras) > 0, "Error: No se encontraron 'Muestras'."))
      
      df_resultados <- muestras %>%
        mutate(
          Grupo = gsub("[0-9].*$", "", Muestra_ID),
          # Asegurarse de que el cálculo no genere NaN/Inf si el Slope es problemático
          Concentracion_OD1 = pmax(0, (OD1 - OD_blanco_promedio) / Slope * input$DF),
          Concentracion_OD2 = pmax(0, (OD2 - OD_blanco_promedio) / Slope * input$DF),
          Concentracion_promedio = (Concentracion_OD1 + Concentracion_OD2) / 2,
          # Asegurarse de que sd() maneje NA si solo hay una replica
          Concentracion_desv_std = apply(cbind(Concentracion_OD1, Concentracion_OD2), 1, function(x) if(sum(!is.na(x)) > 1) sd(x, na.rm = TRUE) else 0),
          OD_promedio = (OD1 + OD2) / 2,
          OD_desv_std = apply(cbind(OD1, OD2), 1, function(x) if(sum(!is.na(x)) > 1) sd(x, na.rm = TRUE) else 0)
        ) %>%
        arrange(Grupo, Tiempo_fermentacion, Muestra_ID)
      
      validate(need(length(unique(df_resultados$Grupo)) > 0, "No se pudo identificar ningún grupo a partir de los prefijos de Muestra_ID."))
      
      return(list(df_final = df_resultados, curva_calib = curva_calib, Slope = Slope, R_squared = R_squared))
    })
    
    output$tablas_resultados_ui <- renderUI({
      req(datos_procesados())
      nombres_grupos <- unique(datos_procesados()$df_final$Grupo)
      # Se usa session$ns() para los IDs dinámicos
      lapply(nombres_grupos, function(nombre_grupo) {
        ns <- session$ns
        tagList(h4(paste("Resultados para el Grupo:", nombre_grupo)), tableOutput(ns(paste0("tabla_", nombre_grupo))), hr())
      })
    })
    
    observe({
      req(datos_procesados())
      df_final <- datos_procesados()$df_final
      nombres_grupos <- unique(df_final$Grupo)
      
      lapply(nombres_grupos, function(nombre_grupo) {
        # Se usa session$ns() para los IDs dinámicos
        output[[paste0("tabla_", nombre_grupo)]] <- renderTable({
          df_final %>%
            filter(Grupo == nombre_grupo) %>%
            # Convertir Tiempo_fermentacion a horas para la tabla
            mutate(`Tiempo (h)` = Tiempo_fermentacion / 60) %>%
            select(Muestra_ID, `Tiempo (h)`, OD1, OD2, OD_promedio, OD_desv_std,
                   Concentracion_OD1, Concentracion_OD2, Concentracion_promedio, Concentracion_desv_std) %>%
            rename("Muestra" = Muestra_ID, "Abs. OD1" = OD1, "Abs. OD2" = OD2, "Abs. Promedio" = OD_promedio, "DE Abs." = OD_desv_std, "Conc. OD1 (%)" = Concentracion_OD1, "Conc. OD2 (%)" = Concentracion_OD2, "Conc. Promedio (%)" = Concentracion_promedio, "DE Conc. (%)" = Concentracion_desv_std) %>%
            mutate(across(where(is.numeric), ~round(., 3)))
        }, striped = TRUE, hover = TRUE, bordered = TRUE)
      })
    })
    
    # --- PARÁMETROS CINÉTICOS (ahora en formato de tabla) ---
    parametros_cineticos_df <- reactive({
      req(datos_procesados())
      df_final_etanol <- datos_procesados()$df_final
      
      all_params_df <- df_final_etanol %>%
        group_by(Grupo) %>%
        do({
          df_grupo <- . # Datos del grupo actual
          grupo_actual <- unique(df_grupo$Grupo)
          
          # Inicializar un data.frame temporal para los parámetros de este grupo
          params_df_temp <- data.frame(Parámetro = character(), Valor = character(), stringsAsFactors = FALSE)
          
          # Función auxiliar para añadir filas de parámetros
          add_param_row <- function(df, param_name, param_value, format_string = "%.3f") {
            new_row <- data.frame(Parámetro = param_name, Valor = sprintf(format_string, param_value), stringsAsFactors = FALSE)
            bind_rows(df, new_row)
          }
          
          # Asegurarse de que haya al menos 3 puntos para el ajuste cuadrático
          if(nrow(df_grupo) < 3) {
            params_df_temp <- add_param_row(params_df_temp, "Nota", "Se necesitan al menos 3 puntos de datos para el ajuste cinético de A_max y t_opt.", "%s")
            # Devolver el data.frame con la nota
            return(data.frame(Grupo = grupo_actual, params_df_temp, stringsAsFactors = FALSE))
          }
          
          # Calcular A_max (máxima concentración de etanol)
          A_max_val <- max(df_grupo$Concentracion_promedio, na.rm = TRUE)
          params_df_temp <- add_param_row(params_df_temp, "Concentración Máxima de Etanol (A_max, %)", A_max_val)
          
          # Ajuste del modelo cuadrático para t_opt
          modelo_q <- tryCatch(
            lm(Concentracion_promedio ~ poly(Tiempo_fermentacion, 2, raw = TRUE), data = df_grupo),
            error = function(e) NULL
          )
          
          if (!is.null(modelo_q) && length(coef(modelo_q)) >= 3 && !is.na(coef(modelo_q)[3])) {
            coefs <- coef(modelo_q)
            # El coeficiente para el término cuadrático (x^2) es coefs[3]
            # El coeficiente para el término lineal (x) es coefs[2]
            # Si el coeficiente del término cuadrático (a) es positivo, la parábola abre hacia arriba, no tiene máximo.
            if (coefs[3] < 0) {
              t_opt_val <- -coefs[2] / (2 * coefs[3])
              params_df_temp <- add_param_row(params_df_temp, "Tiempo Óptimo (t_opt, min)", t_opt_val)
              params_df_temp <- add_param_row(params_df_temp, "Tiempo Óptimo (t_opt, h)", t_opt_val / 60)
            } else {
              params_df_temp <- add_param_row(params_df_temp, "Nota", "No se pudo determinar t_opt (modelo no tiene máximo o es lineal).", "%s")
            }
          } else {
            params_df_temp <- add_param_row(params_df_temp, "Nota", "No se pudo ajustar un modelo cuadrático para t_opt.", "%s")
          }
          
          # Calcular Rendimiento (yield)
          # Asumiendo que 7.85 es un factor de conversión para el rendimiento a partir de glucosa
          # Es importante que el usuario entienda qué significa este "rendimiento"
          Rendimiento_val <- (A_max_val * input$DF * 7.85 / input$cantidad_sustrato) * 100
          params_df_temp <- add_param_row(params_df_temp, "Rendimiento (%)", Rendimiento_val)
          
          # Retornar el data.frame con los parámetros de este grupo, incluyendo la columna Grupo
          data.frame(Grupo = grupo_actual, params_df_temp, stringsAsFactors = FALSE)
        }) %>%
        ungroup() # Quitar la agrupación para que sea un solo dataframe
      
      return(all_params_df)
    })
    
    # --- UI para Parámetros Cinéticos (ahora con tablas) ---
    output$parametros_ui <- renderUI({
      req(parametros_cineticos_df())
      df_params <- parametros_cineticos_df() # Esto ahora es un solo data.frame combinado
      
      if (nrow(df_params) == 0) {
        return(tags$p("No hay parámetros cinéticos disponibles para mostrar."))
      }
      
      grupos <- unique(df_params$Grupo)
      lapply(grupos, function(g) {
        df_grupo_params <- df_params %>% filter(Grupo == g) %>% select(-Grupo)
        
        tagList(
          h5(paste("Parámetros Cinéticos - Grupo", g), style = "font-weight: bold;"),
          # Usar session$ns para los IDs de las tablas dinámicas
          tableOutput(session$ns(paste0("tabla_parametros_", g))),
          hr()
        )
      })
    })
    
    # --- Lógica para renderizar cada tabla de parámetros ---
    observe({
      req(parametros_cineticos_df())
      df_params <- parametros_cineticos_df()
      for (g in unique(df_params$Grupo)) {
        local({
          grupo_local <- g
          output[[paste0("tabla_parametros_", grupo_local)]] <- renderTable({
            df_params %>% filter(Grupo == grupo_local) %>% select(-Grupo)
          }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'l')
        })
      }
    })
    
    output$plotCurvaCalibracion <- renderPlot({ req(datos_procesados()); curva <- datos_procesados()$curva_calib; R_squared <- round(datos_procesados()$R_squared, 4); ggplot(curva, aes(x = concentracion_etanol, y = OD_promedio)) + geom_point(size = 3, color = "blue") + geom_smooth(method = "lm", se = FALSE, color="red") + annotate("text", x = max(curva$concentracion_etanol, na.rm=T) * 0.8, y = max(curva$OD_promedio, na.rm=T) * 0.9, label = paste("R² =", R_squared), size = 5) + labs(title = "Curva de Calibración", x = "Concentración de Etanol (%)", y = "Absorbancia (OD)") + theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) })
    
    output$plotAbsorbanciaMuestras <- renderPlot({ req(datos_procesados()); datos_wide <- datos_procesados()$df_final; datos_long <- datos_wide %>% pivot_longer(cols = c("OD_promedio", "OD1", "OD2"), names_to = "Tipo_Medida_Abs", values_to = "Valor_Abs"); medidas_a_mostrar <- c(); if(input$show_prom_abs) medidas_a_mostrar <- c(medidas_a_mostrar, "OD_promedio"); if(input$show_od1_abs) medidas_a_mostrar <- c(medidas_a_mostrar, "OD1"); if(input$show_od2_abs) medidas_a_mostrar <- c(medidas_a_mostrar, "OD2"); datos_filtrados <- datos_long %>% filter(Tipo_Medida_Abs %in% medidas_a_mostrar); p <- ggplot(); if(input$show_desv_abs && input$show_prom_abs){ p <- p + geom_ribbon(data = datos_wide, aes(x = Concentracion_promedio, ymin = OD_promedio - OD_desv_std, ymax = OD_promedio + OD_desv_std, fill = Grupo, group = Grupo), alpha = 0.2) }; p <- p + geom_line(data = datos_filtrados, aes(x = Concentracion_promedio, y = Valor_Abs, color = Grupo, linetype = Tipo_Medida_Abs, group = interaction(Grupo, Tipo_Medida_Abs)), size=1) + geom_point(data = datos_filtrados, aes(x = Concentracion_promedio, y = Valor_Abs, color = Grupo, shape = Tipo_Medida_Abs, group = interaction(Grupo, Tipo_Medida_Abs)), size=2.5); p + labs(title = "Absorbancia vs Concentración de Etanol", x = "Concentración de Etanol Promedio (%)", y = "Absorbancia (OD)", color = "Grupo", fill = "Grupo", linetype = "Medida", shape = "Medida") + scale_linetype_manual(values = c("OD_promedio" = "solid", "OD1" = "dotted", "OD2" = "dashed"), labels=c("Promedio", "Réplica OD1", "Réplica OD2")) + scale_shape_manual(values = c("OD_promedio" = 16, "OD1" = 1, "OD2" = 2), labels=c("Promedio", "Réplica OD1", "Réplica OD2")) + theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom", legend.box = "vertical") })
    
    output$plotConcentracionEtanolTiempo <- renderPlot({ req(datos_procesados()); datos_wide <- datos_procesados()$df_final; datos_long <- datos_wide %>% pivot_longer(cols = c("Concentracion_promedio", "Concentracion_OD1", "Concentracion_OD2"), names_to = "Tipo_Medida_Conc", values_to = "Valor_Conc"); medidas_a_mostrar <- c(); if(input$show_prom_conc) medidas_a_mostrar <- c(medidas_a_mostrar, "Concentracion_promedio"); if(input$show_od1_conc) medidas_a_mostrar <- c(medidas_a_mostrar, "Concentracion_OD1"); if(input$show_od2_conc) medidas_a_mostrar <- c(medidas_a_mostrar, "Concentracion_OD2"); datos_filtrados <- datos_long %>% filter(Tipo_Medida_Conc %in% medidas_a_mostrar); p <- ggplot(); if(input$show_desv_conc && input$show_prom_conc){ p <- p + geom_ribbon(data = datos_wide, aes(x = Tiempo_fermentacion, ymin = Concentracion_promedio - Concentracion_desv_std, ymax = Concentracion_promedio + Concentracion_desv_std, fill = Grupo, group = Grupo), alpha = 0.2) }; p <- p + geom_line(data = datos_filtrados, aes(x = Tiempo_fermentacion, y = Valor_Conc, color = Grupo, linetype = Tipo_Medida_Conc, group = interaction(Grupo, Tipo_Medida_Conc)), size=1) + geom_point(data = datos_filtrados, aes(x = Tiempo_fermentacion, y = Valor_Conc, color = Grupo, shape = Tipo_Medida_Conc, group = interaction(Grupo, Tipo_Medida_Conc)), size=2.5); p + labs(title = "Concentración de Etanol vs Tiempo", x = "Tiempo (min)", y = "Concentración de Etanol (%)", color = "Grupo", fill = "Grupo", linetype = "Medida", shape = "Medida") + scale_linetype_manual(values = c("Concentracion_promedio" = "solid", "Concentracion_OD1" = "dotted", "Concentracion_OD2" = "dashed"), labels = c("Promedio", "Réplica OD1", "Réplica OD2")) + scale_shape_manual(values = c("Concentracion_promedio" = 16, "Concentracion_OD1" = 1, "Concentracion_OD2" = 2), labels = c("Promedio", "Réplica OD1", "Réplica OD2")) + theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom", legend.box = "vertical") })
    
    # <<< 4. Devolver un data.frame estandarizado para la integración >>>
    datos_para_integracion <- reactive({
      req(datos_procesados())
      datos_procesados()$df_final %>%
        # Agrupamos para obtener un valor promedio por grupo y tiempo, si hay múltiples Muestra_ID por grupo
        group_by(Grupo, Tiempo_fermentacion) %>%
        summarise(Valor = mean(Concentracion_promedio, na.rm = TRUE), .groups = "drop") %>%
        # Añadir una columna TipoMedicion para que el módulo integrado sepa qué es
        mutate(TipoMedicion = "Etanol")
    })
    
    return(datos_para_integracion)
  })
}
