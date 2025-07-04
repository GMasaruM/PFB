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
      helpText(HTML("El CSV debe contener: <b>Tipo, Muestra_ID, Tiempo_fermentacion,concentracion_etanol, OD1, OD2</b>.<br>
                     - <b>Tipo</b>: 'Muestra', 'Blanco', o 'Estandar'.<br>
                     - Para 'Blanco'/'Estandar', deje Muestra_ID y Tiempo_fermentacion en blanco.<br>
                     - Cada fila representa un punto de datos con sus dos réplicas.<br>
                     - No colocar nada en los espacios de las muestras en la columna de concentracion_etanol.<br>
                     <b>Ejemplo de formato:</b>
                     <pre style='font-size: 10px;'>Tipo,Muestra_ID,Tiempo_fermentacion,concentracion_etanol,OD1,OD2\nBlanco,,,0.127,0.111\nEstandar,,,0.600,0.614\nMuestra,M1,0,0.150,0.155\nMuestra,M2,30,0.350,0.345</pre>")),
      hr(),
      h4("2. Parámetros de Cálculo"),
      numericInput("DF", "Factor de dilución (DF):", value = 50, min = 1, step = 1),
      actionButton("calcular", "Calcular y Graficar", class = "btn-primary btn-lg", icon = icon("cogs"))
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Resultados Tabulados", h4("Resultados de Actividad Enzimática"), tableOutput("tablaResultados")),
        tabPanel("Parámetros Cinéticos", h4("Parámetros Cinéticos Estimados"), verbatimTextOutput("parametros")),
        tabPanel("Gráficas", 
                 h4("Gráfica de Absorbancia vs. %Etanol"), plotOutput("plotAbsorbanciaEtanol"), 
                 hr(), 
                 h4("Gráfica de Absorbancia vs Tiempo"), plotOutput("plotAbsorbanciaTiempo"))
      ),
      hr(),
      uiOutput("download_button_ui")
    )
  )
)

server <- function(input, output, session) {
  datos_procesados <- eventReactive(input$calcular, {
    req(input$file_datos)
    
    # Leer archivo CSV
    df_raw <- read.csv(input$file_datos$datapath, header = TRUE, stringsAsFactors = FALSE)
    
    # Validar columnas
    cols_requeridas <- c("Tipo", "Muestra_ID", "Tiempo_fermentacion", "Concentracion_etanol", "OD1", "OD2")
    validate(need(all(cols_requeridas %in% names(df_raw)), 
                  paste("Error: El archivo CSV debe contener las columnas:", paste(cols_requeridas, collapse = ", "))))
    
    # Procesar blancos
    blancos <- df_raw %>% filter(Tipo == "Blanco")
    validate(need(nrow(blancos) > 0, "Error: No se encontraron filas con Tipo 'Blanco'."))
    OD_blanco_promedio <- mean(c(blancos$OD1, blancos$OD2), na.rm = TRUE)
    
    # Procesar estándares
    estandares <- df_raw %>% filter(Tipo == "Estandar")
    validate(need(nrow(estandares) > 0, "Error: No se encontraron filas con Tipo 'Estandar'."))
    
    # Crear dataframe para la curva de calibración
    curva_calib <- estandares %>% 
      pivot_longer(cols = c(OD1, OD2), names_to = "Replica", values_to = "OD") %>%
      group_by(Concentracion_etanol) %>%
      summarise(OD_promedio = mean(OD, na.rm = TRUE))
    
    # Calcular slope (pendiente) de la curva de calibración
    modelo_calib <- lm(Concentracion_etanol ~ OD_promedio, data = curva_calib)
    Slope <- coef(modelo_calib)[2]
    
    # Procesar muestras
    muestras <- df_raw %>% filter(Tipo == "Muestra")
    validate(need(nrow(muestras) > 0, "Error: No se encontraron filas con Tipo 'Muestra'."))
    
    # Función para cálculo de concentración
    calc_concentracion <- function(OD_muestra, OD_blanco, Slope, DF) {
      concentracion <- ((OD_muestra - OD_blanco) / Slope) * DF
      return(concentracion)
    }
    
    # Calcular concentraciones para muestras
    df_resultados <- muestras %>%
      mutate(
        Concentracion_1 = calc_concentracion(OD1, OD_blanco_promedio, Slope, input$DF),
        Concentracion_2 = calc_concentracion(OD2, OD_blanco_promedio, Slope, input$DF),
        Promedio_Concentracion = (Concentracion_1 + Concentracion_2)/2
      ) %>%
      arrange(Tiempo_fermentacion, Muestra_ID)
    
    return(list(
      df_final = df_resultados,
      estandares = estandares,
      blancos = blancos,
      Slope = Slope,
      curva_calib = curva_calib
    ))
  })
    
  
  resultados_formateados <- reactive({
    req(datos_procesados())
    datos_procesados()$df_final %>%
      select(Muestra_ID, Tiempo_fermentacion, Concentracion_1, Concentracion_2, Promedio_Concentracion) %>%
      rename(Muestra = Muestra_ID, `Tiempo (min)` = Tiempo_fermentacion, `Concentración R1 (g/L)` = Concentracion_1, `Concentración R2 (g/L)` = Concentracion_2, `Concentración Promedio (g/L)` = Promedio_Concentracion)
  })
  
  output$tablaResultados <- renderTable({ resultados_formateados() %>% mutate(across(where(is.numeric), ~ round(., 2))) })
  
  
  output$parametros <- renderPrint({
    req(datos_procesados())
    df_final <- datos_procesados()$df_final  # Acceder correctamente al data frame
    
    if(nrow(df_final) < 3) {
      return("Se necesitan al menos 3 puntos para calcular parámetros cinéticos")
    }
    
    modelo_q <- lm(Promedio_Concentracion ~ poly(Tiempo_fermentacion, 2), data = df_final)
    coefs <- coef(modelo_q)
    t_opt <- -coefs[2]/(2*coefs[3])
    A_max <- predict(modelo_q, data.frame(Tiempo_fermentacion = t_opt))
    
    cat(sprintf("Actividad máxima (A_max) = %.2f g/L\n", A_max))
    cat(sprintf("Tiempo óptimo (t_opt) = %.1f min\n", t_opt))
    
    # Verificar que hay datos para decaimiento
    if(t_opt < max(df_final$Tiempo_fermentacion)) {
      decay <- subset(df_final, Tiempo_fermentacion >= t_opt)
      if(nrow(decay) > 1) {
        modelo_decay <- lm(log(Promedio_Concentracion) ~ Tiempo_fermentacion, data = decay)
        k <- -coef(modelo_decay)[2]
        t_mitad <- log(2)/k
        cat(sprintf("\nConstante de decaimiento (k) = %.4f min⁻¹\n", k))
        cat(sprintf("Tiempo de vida media (t½) = %.1f min\n", t_mitad))
      }
    }
  })
  
  output$plotAbsorbanciaEtanol <- renderPlot({
    req(datos_procesados())
    curva <- datos_procesados()$curva_calib
    
    ggplot(curva, aes(x = OD_promedio, y = Concentracion_etanol)) +
      geom_point(size = 3, color = "blue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = "Curva de Calibración: Concentración vs Absorbancia",
           x = "Absorbancia Promedio", 
           y = "Concentración Etanol (% o g/L)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plotAbsorbanciaTiempo <- renderPlot({
    req(datos_procesados())
    df <- datos_procesados()$df_final
    
    # Calcular absorbancia promedio para las muestras
    df_plot <- df %>%
      mutate(Absorbancia_promedio = (OD1 + OD2)/2)
    
    ggplot(df_plot, aes(x = Tiempo_fermentacion, y = Absorbancia_promedio)) +
      geom_line(color = "darkblue") +
      geom_point(aes(color = Muestra_ID), size = 3) +
      labs(title = "Absorbancia vs Tiempo de Fermentación",
           x = "Tiempo (min)", 
           y = "Absorbancia Promedio") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
  })
  
  
  output$download_button_ui <- renderUI({
    req(datos_procesados())
    downloadButton("downloadReport", "Descargar Reporte Completo (PDF)", class = "btn-success")
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("reporte_analisis_etanol_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      showNotification("Generando reporte en PDF, por favor espere...", duration = 10, type = "message")
      
      
      tempReport <- file.path(tempdir(), "Plantilla_etanol.Rmd")
      file.copy("Plantilla_etanol.Rmd", tempReport, overwrite = TRUE)
      
      
      # Capturar gráficas temporalmente
      plot_abs_etanol <- output$plotAbsorbanciaEtanol()
      plot_abs_tiempo <- output$plotAbsorbanciaTiempo()
      
      tempPlot1 <- tempfile(fileext = ".png")
      tempPlot2 <- tempfile(fileext = ".png")
      
      ggsave(tempPlot1, plot = plot_abs_etanol, device = "png", width = 8, height = 6, dpi = 300)
      ggsave(tempPlot2, plot = plot_abs_tiempo, device = "png", width = 8, height = 6, dpi = 300)
      
      params <- list(
        tabla_resultados = resultados_formateados(),
        texto_parametros = capture.output(output$parametros()),
        plot_absorbancia_etanol_file = tempPlot1,
        plot_absorbancia_tiempo_file = tempPlot2,
        DF = input$DF
      )
      
      withProgress(message = 'Generando reporte', {
        rmarkdown::render(
          input = tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      })
    }
  )
}

  shinyApp(ui = ui, server = server)