# modulo_amilasa.R
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr) # Necesario para str_extract
library(patchwork) # Para combinar gráficos de decaimiento si hay múltiples grupos

amilasaUI <- function(id) {
  ns <- NS(id) # Crear el namespace para los IDs
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Carga de Datos"),
      fileInput(ns("file_datos"), "Selecciona archivo CSV único:", accept = c(".csv")),
      helpText(HTML("El CSV debe contener: <b>Tipo, Muestra_ID, Tiempo_fermentacion, OD1, OD2</b>.<br>
                     - <b>Tipo</b>: 'Muestra', 'Blanco', o 'Estandar'.<br>
                     - Para 'Blanco'/'Estandar', deje Muestra_ID y Tiempo_fermentacion en blanco.<br>
                     - Cada fila representa un punto de datos con sus dos réplicas.<br>
                     - <b>Muestra_ID</b> debe tener un prefijo de grupo (ej. A, B) y un sufijo numérico (ej. A1, B2).<br>
                     <b>Ejemplo de formato:</b>
                     <pre style='font-size: 10px;'>Tipo,Muestra_ID,Tiempo_fermentacion,OD1,OD2\nBlanco,,,0.127,0.111\nEstandar,,,0.600,0.614\nMuestra,A1,0,0.150,0.155\nMuestra,A2,30,0.350,0.345\nMuestra,B1,0,0.180,0.190\nMuestra,B2,45,0.400,0.395</pre>")),
      hr(),
      h4("2. Parámetros de Cálculo"),
      numericInput(ns("DF"), "Factor de dilución (DF):", value = 50, min = 1, step = 1),
      numericInput(ns("T"), "Tiempo de reacción (min):", value = 20, min = 1, step = 1),
      actionButton(ns("calcular"), "Calcular y Graficar", class = "btn-primary btn-lg", icon = icon("cogs"))
    ),
    mainPanel(
      tabsetPanel(
        id = ns("tabs"), # ID del tabsetPanel
        tabPanel("Resultados Tabulados", h4("Resultados de Actividad Enzimática por Grupo"), uiOutput(ns("resultados_tabulados_ui"))),
        tabPanel("Parámetros Cinéticos", h4("Parámetros Cinéticos Estimados por Grupo"), uiOutput(ns("parametros_cineticos_ui"))), # Cambiado a uiOutput
        tabPanel("Gráficas",
                 # Controles específicos para cada gráfica
                 fluidRow(
                   column(12, h5("Opciones para 'Gráfica de Actividad vs. Absorbancia Neta'"))
                 ),
                 fluidRow(
                   column(6, checkboxGroupInput(ns("plotConc_groups"), "Seleccionar Grupo(s):", choices = NULL)) # Checkbox
                 ),
                 h4("Gráfica de Actividad vs. Absorbancia Neta"), # Título de la primera gráfica
                 plotOutput(ns("plotConcentration"), height = "400px"),
                 hr(),
                 fluidRow(
                   column(12, h5("Opciones para 'Gráfica de Actividad vs. Tiempo'"))
                 ),
                 fluidRow(
                   column(6, checkboxGroupInput(ns("plotAct_groups"), "Seleccionar Grupo(s):", choices = NULL)), # Checkbox
                   column(6, checkboxInput(ns("plotAct_show_sd"), "Mostrar Desviación Estándar", value = FALSE)) # SD solo aquí
                 ),
                 h4("Gráfica de Actividad vs. Tiempo"),
                 plotOutput(ns("plotActividad"), height = "400px"),
                 hr(),
                 fluidRow(
                   column(12, h5("Opciones para 'Gráfica de Decaimiento Enzimático'"))
                 ),
                 fluidRow(
                   column(6, checkboxGroupInput(ns("plotDecay_groups"), "Seleccionar Grupo(s):", choices = NULL)) # Checkbox
                 ),
                 h4("Gráfica de Decaimiento Enzimático"),
                 plotOutput(ns("plotDecay"), height = "400px")
        )
      )
    )
  )
}

amilasaServer <- function(id, datos_crudos_r = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive value to store processed data
    datos_procesados <- eventReactive(input$calcular, {
      req(input$file_datos)
      
      df_raw <- read.csv(input$file_datos$datapath, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
      cols_requeridas <- c("Tipo", "Muestra_ID", "Tiempo_fermentacion", "OD1", "OD2")
      
      validate(need(all(cols_requeridas %in% names(df_raw)), paste("El archivo CSV debe contener las columnas:", paste(cols_requeridas, collapse = ", "))))
      
      controles <- df_raw %>% filter(Tipo %in% c("Blanco", "Estandar"))
      validate(need(nrow(controles) > 0, "No se encontraron filas con Tipo 'Blanco' o 'Estandar'."))
      
      od_blancos <- controles %>% filter(Tipo == "Blanco") %>% pivot_longer(cols = c(OD1, OD2), names_to = "replica", values_to = "od")
      od_estandares <- controles %>% filter(Tipo == "Estandar") %>% pivot_longer(cols = c(OD1, OD2), names_to = "replica", values_to = "od")
      
      OD_blanco_promedio  <- mean(od_blancos$od, na.rm = TRUE)
      OD_estandar_promedio <- mean(od_estandares$od, na.rm = TRUE)
      
      validate(
        need(!is.nan(OD_blanco_promedio) && is.finite(OD_blanco_promedio), "No se encontraron datos válidos para 'Blanco' en OD1/OD2."),
        need(!is.nan(OD_estandar_promedio) && is.finite(OD_estandar_promedio), "No se encontraron datos válidos para 'Estandar' en OD1/OD2."),
        need((OD_estandar_promedio - OD_blanco_promedio) != 0, "La diferencia entre el estándar y el blanco es cero. No se puede calcular la actividad.")
      )
      
      df_muestras <- df_raw %>% filter(Tipo == "Muestra")
      validate(need(nrow(df_muestras) > 0, "No se encontraron filas con Tipo 'Muestra'."))
      
      # Function to calculate activity
      calc_act <- function(OD_muestra, OD_blanco, OD_estandar, T_reaccion, DF) {
        if ((OD_estandar - OD_blanco) == 0) return(NA) # Evitar división por cero
        actividad <- ((OD_muestra - OD_blanco) / (OD_estandar - OD_blanco)) * (400 / T_reaccion) * DF
        return(actividad)
      }
      
      df_final <- df_muestras %>%
        # Extraer el prefijo del grupo del Muestra_ID (ej. "A" de "A1")
        mutate(Grupo = str_extract(Muestra_ID, "^[A-Za-z]+")) %>% # Nueva columna Grupo
        
        # Calcular actividades para cada réplica
        mutate(U_L_1 = calc_act(OD1, OD_blanco_promedio, OD_estandar_promedio, input$T, input$DF),
               U_L_2 = calc_act(OD2, OD_blanco_promedio, OD_estandar_promedio, input$T, input$DF)) %>%
        
        # Calcular promedio y desviación estándar para las réplicas
        rowwise() %>% # Aplicar operaciones por fila
        mutate(
          # Crear una lista de actividades válidas (no NA) para el cálculo de promedio y SD
          valid_activities = list(c(U_L_1, U_L_2) %>% na.omit() %>% .[is.finite(.)]),
          Promedio_U_L = if(length(valid_activities) > 0) mean(valid_activities) else NA_real_,
          SD_U_L = case_when(
            length(valid_activities) >= 2 ~ sd(valid_activities), # SD si hay 2 o más valores válidos
            length(valid_activities) == 1 ~ 0,                    # SD es 0 si solo hay 1 valor válido
            TRUE ~ NA_real_                                       # SD es NA si no hay valores válidos
          ),
          # Calcular absorbancia neta promedio para la nueva gráfica
          Absorbancia_Neta = mean(c(OD1, OD2), na.rm = TRUE) - OD_blanco_promedio
        ) %>%
        ungroup() %>% # Quitar la agrupación por fila
        
        # Convertir tiempo de minutos a horas para la tabla y gráficas
        mutate(Tiempo_fermentacion_h = Tiempo_fermentacion / 60) %>%
        
        # Organizar por grupo y luego por tiempo
        arrange(Grupo, Tiempo_fermentacion)
      
      return(df_final)
    })
    
    # Actualizar las opciones de los selectores de grupos (checkboxGroupInput)
    observeEvent(datos_procesados(), {
      grupos_disponibles <- unique(datos_procesados()$Grupo)
      updateCheckboxGroupInput(session, "plotConc_groups", choices = grupos_disponibles, selected = grupos_disponibles)
      updateCheckboxGroupInput(session, "plotAct_groups", choices = grupos_disponibles, selected = grupos_disponibles)
      updateCheckboxGroupInput(session, "plotDecay_groups", choices = grupos_disponibles, selected = grupos_disponibles)
    })
    
    # --- RESULTADOS TABULADOS (por grupo) ---
    output$resultados_tabulados_ui <- renderUI({
      req(datos_procesados())
      df <- datos_procesados()
      grupos <- unique(df$Grupo)
      
      # Crear una lista de tagLists, cada uno conteniendo un encabezado y una tabla
      lapply(grupos, function(g) {
        df_grupo <- df %>%
          filter(Grupo == g) %>%
          select(Muestra_ID, Tiempo_fermentacion_h, Promedio_U_L, SD_U_L) %>%
          rename(
            `Muestra ID` = Muestra_ID,
            `Tiempo (h)` = Tiempo_fermentacion_h, # Ya está en horas
            `Actividad Promedio (U/L)` = Promedio_U_L,
            `Desviación Estándar (U/L)` = SD_U_L
          ) %>%
          mutate(
            `Tiempo (h)` = floor(`Tiempo (h)`), # Sin decimales para Tiempo (h)
            `Actividad Promedio (U/L)` = round(`Actividad Promedio (U/L)`, 2), # Dos decimales
            `Desviación Estándar (U/L)` = round(`Desviación Estándar (U/L)`, 2) # Dos decimales
          )
        
        tagList(
          h5(paste("Grupo de Muestras:", g), style = "font-weight: bold;"),
          renderTable(df_grupo, striped = TRUE, hover = TRUE, bordered = TRUE) # Añadidas opciones de tabla
        )
      })
    })
    
    # --- PARÁMETROS CINÉTICOS (por grupo) ---
    parametros_cineticos_por_grupo <- reactive({
      req(datos_procesados())
      df <- datos_procesados()
      
      # Calcular parámetros cinéticos por cada grupo
      all_params_df <- df %>%
        group_by(Grupo) %>%
        do({
          df_grupo <- . # Los datos del grupo actual
          grupo_actual <- unique(df_grupo$Grupo)
          
          # Inicializar un data.frame temporal para los parámetros de este grupo
          params_df_temp <- data.frame(Parámetro = character(), Valor = character(), stringsAsFactors = FALSE)
          
          # Función auxiliar para añadir filas de parámetros
          add_param_row <- function(df, param_name, param_value, format_string = "%.3f") {
            new_row <- data.frame(Parámetro = param_name, Valor = sprintf(format_string, param_value), stringsAsFactors = FALSE)
            bind_rows(df, new_row)
          }
          
          if(nrow(df_grupo) < 3) {
            params_df_temp <- add_param_row(params_df_temp, "Nota", "Se necesitan al menos 3 puntos de datos para el ajuste cinético en este grupo.", "%s")
          } else {
            # Modelo cuadrático
            modelo_q <- tryCatch(
              lm(Promedio_U_L ~ poly(Tiempo_fermentacion, 2, raw = TRUE), data = df_grupo),
              error = function(e) NULL
            )
            
            if (is.null(modelo_q) || length(coef(modelo_q)) < 3 || is.na(coef(modelo_q)[3])) {
              params_df_temp <- add_param_row(params_df_temp, "Nota", "No se pudo ajustar un modelo cuadrático (datos insuficientes o no lineales).", "%s")
            } else {
              coefs <- coef(modelo_q); c0 <- coefs[1]; b <- coefs[2]; a <- coefs[3]
              
              if(abs(a) < 1e-9) { # Si 'a' es casi cero, es lineal
                params_df_temp <- add_param_row(params_df_temp, "Nota", "Los datos no se ajustan a un modelo cuadrático significativo (se asemeja a lineal).", "%s")
              } else {
                t_opt <- -b / (2 * a)
                A_max <- c0 + b * t_opt + a * t_opt^2
                
                params_df_temp <- add_param_row(params_df_temp, "Tiempo óptimo (min)", t_opt)
                params_df_temp <- add_param_row(params_df_temp, "Tiempo óptimo (h)", t_opt / 60)
                params_df_temp <- add_param_row(params_df_temp, "Actividad máxima (U/L)", A_max)
                
                # Cálculo de decaimiento (solo if t_opt es positivo y dentro del rango de datos)
                if (!is.na(t_opt) && t_opt >= min(df_grupo$Tiempo_fermentacion, na.rm = TRUE) && A_max > 0) {
                  decay_data <- subset(df_grupo, Tiempo_fermentacion >= t_opt & Promedio_U_L > 0)
                  if(nrow(decay_data) > 1) {
                    decay_data$invA <- 1 / decay_data$Promedio_U_L
                    mod2o <- tryCatch(
                      lm(invA ~ Tiempo_fermentacion, data = decay_data),
                      error = function(e) NULL
                    )
                    
                    if (!is.null(mod2o) && !is.na(coef(mod2o)[2])) {
                      k <- coef(mod2o)[2]
                      # t_half solo if k y A_max son positivos
                      if (k > 0 && A_max > 0) {
                        t_half <- 1 / (k * A_max)
                        params_df_temp <- add_param_row(params_df_temp, "Constante decaimiento 2º orden (L·U⁻¹·min⁻¹)", k, "%.5f")
                        params_df_temp <- add_param_row(params_df_temp, "Vida media (t1/2) a A_max (min)", t_half)
                        params_df_temp <- add_param_row(params_df_temp, "Vida media (t1/2) a A_max (h)", t_half / 60)
                      } else {
                        params_df_temp <- add_param_row(params_df_temp, "Nota", "No se pudo calcular k y t1/2 (k o A_max no son positivos).", "%s")
                      }
                    } else {
                      params_df_temp <- add_param_row(params_df_temp, "Nota", "No se pudo ajustar el modelo de decaimiento de 2º orden.", "%s")
                    }
                  } else {
                    params_df_temp <- add_param_row(params_df_temp, "Nota", "No hay suficientes datos en la fase de decaimiento para calcular k y t1/2.", "%s")
                  }
                } else {
                  params_df_temp <- add_param_row(params_df_temp, "Nota", "Tiempo óptimo no válido o Actividad Máxima no positiva, no se calcula decaimiento.", "%s")
                }
              }
            }
          }
          # Retornar el data.frame con los parámetros de este grupo, incluyendo la columna Grupo
          data.frame(Grupo = grupo_actual, params_df_temp, stringsAsFactors = FALSE)
        }) %>%
        ungroup() # Quitar la agrupación para que sea un solo dataframe
      
      return(all_params_df)
    })
    
    # --- UI para Parámetros Cinéticos (ahora con tablas) ---
    output$parametros_cineticos_ui <- renderUI({
      req(parametros_cineticos_por_grupo())
      df_params <- parametros_cineticos_por_grupo() # Esto ahora es un solo data.frame combinado
      
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
      req(parametros_cineticos_por_grupo())
      df_params <- parametros_cineticos_por_grupo()
      for (g in unique(df_params$Grupo)) {
        local({
          grupo_local <- g
          output[[paste0("tabla_parametros_", grupo_local)]] <- renderTable({
            df_params %>% filter(Grupo == grupo_local) %>% select(-Grupo)
          }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'l')
        })
      }
    })
    
    # --- GRÁFICAS ---
    
    # Gráfica 1: Gráfica de Actividad vs. Absorbancia Neta
    output$plotConcentration <- renderPlot({
      req(datos_procesados(), input$plotConc_groups)
      df_plot <- datos_procesados() %>% filter(Grupo %in% input$plotConc_groups)
      
      if (nrow(df_plot) == 0) {
        return(ggplot() + annotate("text", x=0.5, y=0.5, label="Selecciona grupo(s) para visualizar.", size=6, color="gray") + theme_void())
      }
      
      # Ordenar los datos por Grupo y luego por Absorbancia_Neta para que la línea se dibuje correctamente
      df_plot_ordered <- df_plot %>% arrange(Grupo, Absorbancia_Neta)
      
      ggplot(df_plot_ordered, aes(x = Absorbancia_Neta, y = Promedio_U_L, color = Grupo)) +
        geom_point(size = 4, alpha = 0.8) +
        geom_line(aes(group = Grupo), linetype = "dotted", size = 0.8) + # Línea punteada por grupo
        labs(title = "Gráfica de Actividad vs. Absorbancia Neta",
             x = "Absorbancia Neta (OD muestra - OD blanco)",
             y = "Actividad Promedio (U/L)") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
    })
    
    # Gráfica 2: Gráfica de Actividad vs. Tiempo (con opción SD)
    output$plotActividad <- renderPlot({
      req(datos_procesados(), input$plotAct_groups)
      df_plot <- datos_procesados() %>% filter(Grupo %in% input$plotAct_groups)
      
      if (nrow(df_plot) == 0) {
        return(ggplot() + annotate("text", x=0.5, y=0.5, label="Selecciona grupo(s) para visualizar.", size=6, color="gray") + theme_void())
      }
      
      p <- ggplot(df_plot, aes(x = Tiempo_fermentacion_h, y = Promedio_U_L, color = Grupo, group = Grupo)) + # Tiempo en horas
        geom_line(size = 1) +
        geom_point(size = 4, alpha = 0.8) +
        labs(title = "Gráfica de Actividad vs. Tiempo",
             x = "Tiempo (horas)", # Cambiado a horas
             y = "Actividad Promedio (U/L)") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
      
      # Mostrar desviación estándar si el usuario lo desea y si hay datos válidos para SD
      if (input$plotAct_show_sd) {
        # Filtra NAs para SD, ya que ggplot lanzaría warnings si hay NA en SD_U_L
        p <- p + geom_errorbar(data = df_plot %>% filter(!is.na(SD_U_L) & SD_U_L >= 0),
                               aes(ymin = Promedio_U_L - SD_U_L, ymax = Promedio_U_L + SD_U_L), width = 0.2, alpha = 0.5)
      }
      
      p # Devuelve la gráfica
    })
    
    # Gráfica 3: Gráfica de Decaimiento Enzimático (por grupo)
    output$plotDecay <- renderPlot({
      req(datos_procesados(), input$plotDecay_groups)
      df_plot <- datos_procesados() %>% filter(Grupo %in% input$plotDecay_groups)
      
      if (nrow(df_plot) == 0) {
        return(ggplot() + annotate("text", x=0.5, y=0.5, label="Selecciona grupo(s) para visualizar el decaimiento.", size=6, color="gray") + theme_void())
      }
      
      list_of_decay_plots <- list()
      grupos_seleccionados <- unique(df_plot$Grupo)
      
      for (g in grupos_seleccionados) {
        df_grupo <- df_plot %>% filter(Grupo == g) %>% arrange(Tiempo_fermentacion)
        
        plot_dec <- NULL
        # Recalcular t_opt para cada grupo (para el decaimiento)
        if(nrow(df_grupo) >= 3) {
          modelo_q <- tryCatch(lm(Promedio_U_L ~ poly(Tiempo_fermentacion, 2, raw = TRUE), data = df_grupo),
                               error = function(e) NULL)
          
          if (!is.null(modelo_q) && length(coef(modelo_q)) >= 3 && !is.na(coef(modelo_q)[3]) && abs(coef(modelo_q)[3]) >= 1e-9) {
            t_opt <- -coef(modelo_q)[2] / (2 * coef(modelo_q)[3])
            
            # Solo si t_opt es positivo y A_max es positiva para el decaimiento
            if (!is.na(t_opt) && t_opt >= min(df_grupo$Tiempo_fermentacion, na.rm = TRUE)) {
              A_max_val <- coef(modelo_q)[1] + coef(modelo_q)[2] * t_opt + coef(modelo_q)[3] * t_opt^2
              if (A_max_val > 0) {
                decay_data <- subset(df_grupo, Tiempo_fermentacion >= t_opt & Promedio_U_L > 0)
                if(nrow(decay_data) > 1) {
                  decay_data$invA <- 1 / decay_data$Promedio_U_L
                  mod2o <- tryCatch(
                    lm(invA ~ Tiempo_fermentacion, data = decay_data),
                    error = function(e) NULL
                  )
                  
                  if (!is.null(mod2o) && !is.na(coef(mod2o)[2])) {
                    k <- coef(mod2o)[2]
                    # t_half solo si k y A_max son positivos
                    if (k > 0 && A_max > 0) {
                      t_half <- 1 / (k * A_max) # Esto es para decaimiento de segundo orden
                      plot_dec <- ggplot(decay_data, aes(x = Tiempo_fermentacion_h, y = invA)) + # Usar h
                        geom_point(color = "red", size = 4, alpha = 0.8) +
                        geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
                        labs(title = paste("Decaimiento 2º Orden - Grupo", g),
                             x = "Tiempo (horas)", y = "1 / Actividad (L/U)") + # Eje y acorde
                        theme_minimal(base_size = 14) +
                        theme(plot.title = element_text(hjust = 0.5))
                    } else {
                      plot_dec <- ggplot() + annotate("text", x=0.5, y=0.5, label="No se pudo calcular k o t1/2 (k o A_max no positivos).", size=4, color="gray") + theme_void() + ggtitle(paste("Decaimiento 2º Orden - Grupo", g)) + theme(plot.title = element_text(hjust = 0.5))
                    }
                  } else {
                    plot_dec <- ggplot() + annotate("text", x=0.5, y=0.5, label="No se pudo ajustar el modelo de decaimiento de 2º orden.", size=4, color="gray") + theme_void() + ggtitle(paste("Decaimiento 2º Orden - Grupo", g)) + theme(plot.title = element_text(hjust = 0.5))
                  }
                } else {
                  plot_dec <- ggplot() + annotate("text", x=0.5, y=0.5, label="No hay suficientes datos en la fase de decaimiento para calcular k y t1/2.", size=4, color="gray") + theme_void() + ggtitle(paste("Decaimiento 2º Orden - Grupo", g)) + theme(plot.title = element_text(hjust = 0.5))
                }
              }
            }
          }
        }
        
        if (is.null(plot_dec)) {
          plot_dec <- ggplot() +
            annotate("text", x=0.5, y=0.5, label=paste("No hay datos para decaimiento\nen el Grupo", g), size=5, color="gray") +
            theme_void() +
            ggtitle(paste("Decaimiento 2º Orden - Grupo", g)) +
            theme(plot.title = element_text(hjust = 0.5))
        }
        list_of_decay_plots[[g]] <- plot_dec
      }
      
      if (length(list_of_decay_plots) > 0) {
        # Usamos patchwork para mostrar múltiples gráficos, uno por cada grupo seleccionado
        wrap_plots(list_of_decay_plots, ncol = ceiling(sqrt(length(list_of_decay_plots))))
      } else {
        ggplot() + annotate("text", x=0.5, y=0.5, label="No hay datos de decaimiento para los grupos seleccionados.", size=6, color="gray") + theme_void()
      }
    })
    
    # --- La sección de Descarga de Reporte ha sido eliminada por completo ---
    # output$download_button_ui <- renderUI({ ... })
    # output$downloadReport <- downloadHandler({ ... })
    
    # Devolver los datos procesados para el módulo integrado
    return(reactive({
      req(datos_procesados())
      datos_procesados() %>%
        select(Grupo, Tiempo_fermentacion, Valor = Promedio_U_L) %>%
        # Añadir una columna TipoMedicion para que el módulo integrado sepa qué es
        mutate(TipoMedicion = "Amilasa")
    }))
  })
}