library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rmarkdown)
library(knitr)

ui <- fluidPage(
  titlePanel("Análisis de Actividad de Etanol"),
  sidebarLayout(
    sidebarPanel(
      h4("1. Carga de Datos"),
      fileInput("file_datos", "Selecciona archivo CSV único:", accept = c(".csv")),
      helpText(HTML("El CSV debe contener: <b>Tipo, Muestra_ID, Tiempo_fermentacion, OD1, OD2</b>.<br>
                     - <b>Tipo</b>: 'Muestra', 'Blanco', o 'Estandar'.<br>
                     - Para 'Blanco'/'Estandar', deje Muestra_ID y Tiempo_fermentacion en blanco.<br>
                     - Cada fila representa un punto de datos con sus dos réplicas.<br>
                     <b>Ejemplo de formato:</b>
                     <pre style='font-size: 10px;'>Tipo,Muestra_ID,Tiempo_fermentacion,OD1,OD2\nBlanco,,,0.127,0.111\nEstandar,,,0.600,0.614\nMuestra,M1,0,0.150,0.155\nMuestra,M2,30,0.350,0.345</pre>")),
      hr(),
      h4("2. Parámetros de Cálculo"),
      numericInput("DF", "Factor de dilución (DF):", value = 50, min = 1, step = 1),
      numericInput("T", "Tiempo de reacción (min):", value = 20, min = 1, step = 1),
      actionButton("calcular", "Calcular y Graficar", class = "btn-primary btn-lg", icon = icon("cogs"))
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Resultados Tabulados", h4("Resultados de Actividad Enzimática"), tableOutput("tablaResultados")),
        tabPanel("Parámetros Cinéticos", h4("Parámetros Cinéticos Estimados"), verbatimTextOutput("parametros")),
        tabPanel("Gráficas", h4("Gráfica de Actividad vs. Tiempo"), plotOutput("plotActividad"), hr(), h4("Gráfica de Decaimiento Enzimático"), plotOutput("plotDecay"))
      ),
      hr(),
      uiOutput("download_button_ui")
    )
  )
)

server <- function(input, output, session) {
  datos_procesados <- eventReactive(input$calcular, {
    req(input$file_datos)
    df_raw <- read.csv(input$file_datos$datapath, header = TRUE, stringsAsFactors = FALSE)
    cols_requeridas <- c("Tipo", "Muestra_ID", "Tiempo_fermentacion", "OD1", "OD2")
    validate(need(all(cols_requeridas %in% names(df_raw)), paste("El archivo CSV debe contener las columnas:", paste(cols_requeridas, collapse = ", "))))
    controles <- df_raw %>% filter(Tipo %in% c("Blanco", "Estandar"))
    validate(need(nrow(controles) > 0, "No se encontraron filas con Tipo 'Blanco' o 'Estandar'."))
    od_blancos <- controles %>% filter(Tipo == "Blanco") %>% pivot_longer(cols = c(OD1, OD2), names_to = "replica", values_to = "od")
    od_estandares <- controles %>% filter(Tipo == "Estandar") %>% pivot_longer(cols = c(OD1, OD2), names_to = "replica", values_to = "od")
    OD_blanco_promedio  <- mean(od_blancos$od, na.rm = TRUE)
    OD_estandar_promedio <- mean(od_estandares$od, na.rm = TRUE)
    validate(need(!is.nan(OD_blanco_promedio), "No se encontraron datos válidos para 'Blanco' en OD1/OD2."), need(!is.nan(OD_estandar_promedio), "No se encontraron datos válidos para 'Estandar' en OD1/OD2."))
    df_muestras <- df_raw %>% filter(Tipo == "Muestra")
    validate(need(nrow(df_muestras) > 0, "No se encontraron filas con Tipo 'Muestra'."))
    calc_act <- function(OD_muestra, OD_blanco, OD_estandar, T_reaccion, DF) {
      if ((OD_estandar - OD_blanco) == 0) return(NA)
      actividad <- ((OD_muestra - OD_blanco) / (OD_estandar - OD_blanco)) * (400 / T_reaccion) * DF
      return(actividad)
    }
    df_final <- df_muestras %>%
      mutate(U_L_1 = calc_act(OD1, OD_blanco_promedio, OD_estandar_promedio, input$T, input$DF), U_L_2 = calc_act(OD2, OD_blanco_promedio, OD_estandar_promedio, input$T, input$DF)) %>%
      mutate(Promedio_U_L = rowMeans(select(., U_L_1, U_L_2), na.rm = TRUE)) %>%
      arrange(Tiempo_fermentacion)
    return(df_final)
  })
  
  resultados_formateados <- reactive({
    req(datos_procesados())
    datos_procesados() %>%
      select(Muestra_ID, Tiempo_fermentacion, U_L_1, U_L_2, Promedio_U_L) %>%
      rename(Muestra = Muestra_ID, `Tiempo (min)` = Tiempo_fermentacion, `Actividad R1 (U/L)` = U_L_1, `Actividad R2 (U/L)` = U_L_2, `Actividad Promedio (U/L)` = Promedio_U_L)
  })
  
  analisis_completo <- reactive({
    req(datos_procesados())
    df <- datos_procesados()
    plot_act <- ggplot(df, aes(x = Tiempo_fermentacion, y = Promedio_U_L)) + geom_line(color = "darkblue", size = 1) + geom_point(color = "orange", size = 4, alpha = 0.8) + labs(title = "Actividad de α-amilasa vs Tiempo de fermentación", x = "Tiempo (min)", y = "Actividad (U/L)") + theme_minimal(base_size = 14) + theme(plot.title = element_text(hjust = 0.5))
    texto_params <- ""
    plot_dec <- NULL
    if(nrow(df) < 3) { texto_params <- "Se necesitan al menos 3 puntos de datos para el ajuste."
    } else {
      modelo_q <- lm(Promedio_U_L ~ poly(Tiempo_fermentacion, 2, raw = TRUE), data = df)
      coefs <- coef(modelo_q); c0 <- coefs[1]; b <- coefs[2]; a <- coefs[3]
      if(abs(a) < 1e-9) { texto_params <- "Los datos no se ajustan a un modelo cuadrático.\n"
      } else {
        t_opt <- -b / (2 * a); A_max <- c0 + b * t_opt + a * t_opt^2
        texto_params <- paste(sprintf("Tiempo óptimo (t_opt)    = %.1f min (%.1f h)\n", t_opt, t_opt / 60), sprintf("Actividad máxima (A_max) = %.1f U/L\n", A_max))
        if (!is.na(t_opt)) {
          decay <- subset(df, Tiempo_fermentacion >= t_opt & Promedio_U_L > 0)
          if(nrow(decay) > 1) {
            decay$invA <- 1 / decay$Promedio_U_L
            mod2o <- lm(invA ~ Tiempo_fermentacion, data = decay)
            k <- coef(mod2o)[2]; t_half <- 1 / (k * A_max)
            texto_params <- paste(texto_params, sprintf("Constante decaimiento 2º orden (k) = %.5f L·U⁻¹·min⁻¹\n", k), sprintf("Vida media (t1/2) a A_max        = %.1f min (%.1f h)\n", t_half, t_half / 60))
            plot_dec <- ggplot(decay, aes(x = Tiempo_fermentacion, y = invA)) + geom_point(color = "red", size = 4, alpha = 0.8) + geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") + labs(title = "Linealización de Decaimiento de 2º Orden", x = "Tiempo (min)", y = "1 / Actividad (L/U)") + theme_minimal(base_size = 14) + theme(plot.title = element_text(hjust = 0.5))
          } else { texto_params <- paste(texto_params, "No hay suficientes datos en decaimiento para calcular k y t1/2.\n") }
        }
      }
    }
    list(plot_actividad = plot_act, texto_parametros = texto_params, plot_decaimiento = plot_dec)
  })
  
  output$tablaResultados <- renderTable({ resultados_formateados() %>% mutate(across(where(is.numeric), ~ round(., 2))) })
  output$parametros <- renderPrint({ cat(analisis_completo()$texto_parametros) })
  output$plotActividad <- renderPlot({ analisis_completo()$plot_actividad })
  output$plotDecay <- renderPlot({ p <- analisis_completo()$plot_decaimiento; if(is.null(p)){ plot(1, type="n", axes=F, xlab="", ylab=""); title("No hay datos para graficar decaimiento") } else { print(p) } })
  output$download_button_ui <- renderUI({ req(datos_procesados()); downloadButton("downloadReport", "Descargar Reporte Completo (PDF)", class = "btn-success") })
  
  output$downloadReport <- downloadHandler(
    filename = function() { paste0("reporte_actividad_amilasa_", Sys.Date(), ".pdf") },
    content = function(file) {
      # Muestra notificación al usuario
      showNotification("Generando reporte en PDF, por favor espere...", duration = 10, type = "message")
      
      # 1. CAPTURAR todos los datos en variables locales INMEDIATAMENTE.
      # Esto evita problemas de alcance con las funciones reactivas.
      tabla_data <- resultados_formateados()
      analisis_data <- analisis_completo()
      params_texto <- analisis_data$texto_parametros
      plot_act <- analisis_data$plot_actividad
      plot_dec <- analisis_data$plot_decaimiento
      
      # 2. CREAR un directorio temporal para TODOS los archivos (plantilla e imágenes).
      # Esto asegura que R Markdown pueda encontrar todo sin problemas de rutas.
      temp_dir <- tempdir()
      
      # 3. COPIAR la plantilla a este directorio temporal.
      temp_template_path <- file.path(temp_dir, "reporte_template.Rmd")
      file.copy("reporte_template.Rmd", temp_template_path, overwrite = TRUE)
      
      # 4. GUARDAR las imágenes en el mismo directorio temporal.
      plot_act_path <- file.path(temp_dir, "plot_actividad.png")
      ggsave(filename = plot_act_path, plot = plot_act, device = "png", width = 7, height = 5, dpi = 300)
      
      plot_dec_path <- NA # Inicializar como NA
      if (!is.null(plot_dec)) {
        plot_dec_path_temp <- file.path(temp_dir, "plot_decaimiento.png")
        ggsave(filename = plot_dec_path_temp, plot = plot_dec, device = "png", width = 7, height = 5, dpi = 300)
        plot_dec_path <- plot_dec_path_temp
      }
      
      # --- INICIO DE LA DEPURACIÓN (Mira la consola de R cuando descargues) ---
      print("--- Información de Depuración para el Reporte ---")
      print(paste("Directorio Temporal:", temp_dir))
      print(paste("Ruta a la plantilla:", temp_template_path))
      print(paste("Ruta al gráfico de actividad:", plot_act_path))
      print(paste("Ruta al gráfico de decaimiento:", plot_dec_path))
      print("¿Existe el gráfico de actividad? ")
      print(file.exists(plot_act_path))
      print("Primeras filas de la tabla de datos:")
      print(head(tabla_data))
      # --- FIN DE LA DEPURACIÓN ---
      
      # 5. PREPARAR la lista de parámetros para el reporte.
      # Las rutas a las imágenes ahora son relativas al directorio temporal.
      params <- list(
        tabla_resultados = tabla_data,
        texto_parametros = params_texto,
        # Ya no pasamos la ruta completa, solo el nombre del archivo,
        # porque la plantilla y las imágenes están en el mismo lugar.
        plot_actividad_file = "plot_actividad.png", 
        plot_decaimiento_file = if (!is.na(plot_dec_path)) "plot_decaimiento.png" else NA,
        nombre_archivo_original = input$file_datos$name,
        factor_dilucion = input$DF,
        tiempo_reaccion = input$T
      )
      
      # 6. RENDERIZAR el reporte, especificando el directorio de trabajo.
      rmarkdown::render(
        input = temp_template_path,       # La plantilla en el dir temporal
        output_file = file,               # El archivo final de salida
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui = ui, server = server)

