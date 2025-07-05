library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rmarkdown)
library(knitr)
library(kableExtra)

# ==============================================================================
# Interfaz de Usuario (UI)
# ==============================================================================
ui <- fluidPage(
  titlePanel("Análisis Comparativo de Actividad de Etanol"),
  sidebarLayout(
    sidebarPanel(
      # --- Grupo de Controles para el Dataset 1 ---
      h4("Dataset 1"),
      wellPanel(
        textInput("nombre_dataset1", "Nombre del Dataset 1:", value = "Experimento A"), 
        fileInput("file_datos1", "Selecciona archivo CSV:", accept = c(".csv")),
        numericInput("DF1", "Factor de dilución (DF):", value = 50, min = 1, step = 1),
        numericInput("cantidad_sustrato1", "Cantidad de Sustrato (g):", value = 100, min = 1, step = 1)
      ),
      
      hr(),
      
      # --- Grupo de Controles para el Dataset 2 (Opcional) ---
      h4("Dataset 2 (Opcional)"),
      wellPanel(
        textInput("nombre_dataset2", "Nombre del Dataset 2:", value = "Experimento B"), 
        fileInput("file_datos2", "Selecciona archivo CSV (opcional):", accept = c(".csv")),
        numericInput("DF2", "Factor de dilución (DF):", value = 50, min = 1, step = 1),
        numericInput("cantidad_sustrato2", "Cantidad de Sustrato (g):", value = 100, min = 1, step = 1)
      ),
      
      helpText(HTML("El CSV debe contener: <b>Tipo, Muestra_ID, Tiempo_fermentacion, concentracion_etanol, OD1, OD2</b>.")),
      
      hr(),
      
      actionButton("calcular", "Calcular y Graficar", class = "btn-primary btn-lg", icon = icon("cogs"))
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Resultados Tabulados", 
                 h4(textOutput("titulo_tabla1")),
                 tableOutput("tablaResultados1"),
                 hr(),
                 uiOutput("output_tabla2_ui")
        ),
        tabPanel("Parámetros Cinéticos", 
                 h4("Parámetros Cinéticos Estimados por Dataset"), 
                 verbatimTextOutput("parametros")
        ),
        tabPanel("Gráficas Superpuestas", 
                 h4("Gráfica de Curva de Calibración"), 
                 plotOutput("plotCurvaCalibracion"), 
                 hr(),
                 
                 h4("Gráfica de Absorbancia de Muestras"),
                 fluidRow(
                   column(4, checkboxInput("show_prom_abs", "Mostrar Promedio", value = TRUE)),
                   column(4, checkboxInput("show_od1_abs", "Mostrar OD1 (réplica)", value = FALSE)),
                   column(4, checkboxInput("show_od2_abs", "Mostrar OD2 (réplica)", value = FALSE)),
                   column(4, checkboxInput("show_desv_abs", "Mostrar Desviación", value = TRUE))
                 ),
                 plotOutput("plotAbsorbanciaMuestras"),
                 hr(),
                 
                 h4("Gráfica de Concentración de Etanol vs Tiempo"),
                 fluidRow(
                   column(4, checkboxInput("show_prom_conc", "Mostrar Promedio", value = TRUE)),
                   column(4, checkboxInput("show_od1_conc", "Mostrar Conc. OD1 (réplica)", value = FALSE)),
                   column(4, checkboxInput("show_od2_conc", "Mostrar Conc. OD2 (réplica)", value = FALSE)),
                   column(4, checkboxInput("show_desv_conc", "Mostrar Desviación", value = TRUE))
                 ),
                 plotOutput("plotConcentracionEtanolTiempo"))
      ),
      hr(),
      uiOutput("download_button_ui")
    )
  )
)

# ==============================================================================
# Lógica del Servidor (Server)
# ==============================================================================
server <- function(input, output, session) {
  
  procesar_datos_individuales <- function(file_info, df_val, sustrato_val, nombre_dataset) {
    if (is.null(file_info) || file_info$size == 0) return(NULL)
    df_raw <- read.csv(file_info$datapath, header = TRUE, stringsAsFactors = FALSE)
    cols_requeridas <- c("Tipo", "Muestra_ID", "Tiempo_fermentacion", "concentracion_etanol", "OD1", "OD2")
    validate(need(all(cols_requeridas %in% names(df_raw)), paste("Error en", nombre_dataset, ": Faltan columnas.")))
    blancos <- df_raw %>% filter(Tipo == "Blanco")
    validate(need(nrow(blancos) > 0, paste("Error en", nombre_dataset, ": No hay 'Blancos'.")))
    OD_blanco_promedio <- mean(c(blancos$OD1, blancos$OD2), na.rm = TRUE)
    estandares <- df_raw %>% filter(Tipo == "Estandar")
    validate(need(nrow(estandares) > 0, paste("Error en", nombre_dataset, ": No hay 'Estandares'.")))
    curva_calib <- estandares %>% 
      pivot_longer(cols = c(OD1, OD2), names_to = "Replica", values_to = "OD") %>%
      group_by(concentracion_etanol) %>%
      summarise(OD_promedio = mean(OD, na.rm = TRUE), .groups = 'drop') %>%
      bind_rows(data.frame(concentracion_etanol = 0, OD_promedio = OD_blanco_promedio))
    modelo_calib <- lm(OD_promedio ~ concentracion_etanol, data = curva_calib)
    Slope <- coef(modelo_calib)[2]
    R_squared <- summary(modelo_calib)$r.squared
    muestras <- df_raw %>% filter(Tipo == "Muestra")
    validate(need(nrow(muestras) > 0, paste("Error en", nombre_dataset, ": No hay 'Muestras'.")))
    df_resultados <- muestras %>%
      mutate(
        Concentracion_OD1 = ((OD1 - OD_blanco_promedio) / Slope) * df_val,
        Concentracion_OD2 = ((OD2 - OD_blanco_promedio) / Slope) * df_val,
        Concentracion_promedio = (Concentracion_OD1 + Concentracion_OD2) / 2,
        Concentracion_desv_std = apply(cbind(Concentracion_OD1, Concentracion_OD2), 1, sd, na.rm = TRUE),
        OD_promedio = (OD1 + OD2) / 2,
        OD_desv_std = apply(cbind(OD1, OD2), 1, sd, na.rm = TRUE),
        Dataset = nombre_dataset 
      ) %>%
      arrange(Tiempo_fermentacion, Muestra_ID)
    curva_calib$Dataset <- nombre_dataset
    return(list(df_final = df_resultados, curva_calib = curva_calib, Slope = Slope, R_squared = R_squared, df_val = df_val, sustrato_val = sustrato_val, nombre_dataset = nombre_dataset))
  }
  
  datos_combinados <- eventReactive(input$calcular, {
    req(input$file_datos1, input$nombre_dataset1)
    res1 <- procesar_datos_individuales(input$file_datos1, input$DF1, input$cantidad_sustrato1, input$nombre_dataset1)
    res2 <- procesar_datos_individuales(input$file_datos2, input$DF2, input$cantidad_sustrato2, input$nombre_dataset2)
    lista_resultados <- list(res1, res2)
    lista_resultados <- lista_resultados[!sapply(lista_resultados, is.null)]
    validate(need(length(lista_resultados) > 0, "Carga al menos un archivo CSV válido."))
    df_final_combinado <- bind_rows(lapply(lista_resultados, `[[`, "df_final"))
    curvas_combinadas <- bind_rows(lapply(lista_resultados, `[[`, "curva_calib"))
    return(list(df_combinado = df_final_combinado, curvas_combinadas = curvas_combinadas, resultados_individuales = lista_resultados))
  })
  
  # --- Salidas para Tablas Tabuladas ---
  
  output$titulo_tabla1 <- renderText({ req(datos_combinados()); datos_combinados()$resultados_individuales[[1]]$nombre_dataset })
  
  output$tablaResultados1 <- renderTable({
    req(datos_combinados())
    df <- datos_combinados()$resultados_individuales[[1]]$df_final
    df %>% 
      select(Muestra_ID, Tiempo_fermentacion, OD1, OD2, OD_promedio, 
             Concentracion_OD1, Concentracion_OD2, Concentracion_promedio, 
             Concentracion_desv_std) %>% # <<< CAMBIO AQUÍ: Columna añadida a la selección
      rename("Muestra" = Muestra_ID, "Tiempo (min)" = Tiempo_fermentacion,
             "Abs. OD1" = OD1, "Abs. OD2" = OD2, "Abs. Promedio" = OD_promedio,
             "Conc. OD1 (%)" = Concentracion_OD1, "Conc. OD2 (%)" = Concentracion_OD2, 
             "Conc. Promedio (%)" = Concentracion_promedio, 
             "DE Conc. (%)" = Concentracion_desv_std) %>% # <<< CAMBIO AQUÍ: Nuevo nombre para la columna
      mutate(across(where(is.numeric), ~round(., 3)))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$output_tabla2_ui <- renderUI({
    req(datos_combinados())
    if (length(datos_combinados()$resultados_individuales) > 1) {
      tagList(h4(textOutput("titulo_tabla2")), tableOutput("tablaResultados2"))
    }
  })
  
  output$titulo_tabla2 <- renderText({ req(datos_combinados()); if (length(datos_combinados()$resultados_individuales) > 1) { datos_combinados()$resultados_individuales[[2]]$nombre_dataset } })
  
  output$tablaResultados2 <- renderTable({
    req(datos_combinados())
    if (length(datos_combinados()$resultados_individuales) > 1) {
      df <- datos_combinados()$resultados_individuales[[2]]$df_final
      df %>% 
        select(Muestra_ID, Tiempo_fermentacion, OD1, OD2, OD_promedio, 
               Concentracion_OD1, Concentracion_OD2, Concentracion_promedio, 
               Concentracion_desv_std) %>% # <<< CAMBIO AQUÍ: Columna añadida a la selección
        rename("Muestra" = Muestra_ID, "Tiempo (min)" = Tiempo_fermentacion,
               "Abs. OD1" = OD1, "Abs. OD2" = OD2, "Abs. Promedio" = OD_promedio,
               "Conc. OD1 (%)" = Concentracion_OD1, "Conc. OD2 (%)" = Concentracion_OD2, 
               "Conc. Promedio (%)" = Concentracion_promedio,
               "DE Conc. (%)" = Concentracion_desv_std) %>% # <<< CAMBIO AQUÍ: Nuevo nombre para la columna
        mutate(across(where(is.numeric), ~round(., 3)))
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$parametros <- renderPrint({
    req(datos_combinados())
    text_output <- lapply(datos_combinados()$resultados_individuales, function(res) {
      df_final <- res$df_final
      if(nrow(df_final) < 3) return(paste0("--- ", res$nombre_dataset, " ---\nDatos insuficientes.\n"))
      A_max <- max(df_final$Concentracion_promedio, na.rm = TRUE)
      modelo_q <- lm(Concentracion_promedio ~ poly(Tiempo_fermentacion, 2, raw = TRUE), data = df_final)
      coefs <- coef(modelo_q)
      t_opt <- if (length(coefs) >= 3 && !is.na(coefs[3]) && coefs[3] < 0) -coefs[2] / (2 * coefs[3]) else NA
      cantidad_etanol <- A_max * res$df_val * 7.85
      rendimiento <- (cantidad_etanol / res$sustrato_val) * 100
      paste0("--- ", res$nombre_dataset, " ---\n", sprintf("A_max = %.2f %%\n", A_max), sprintf("t_opt = %.1f min\n", t_opt), sprintf("Rendimiento = %.2f %%\n", rendimiento), "--------------------------\n")
    })
    cat(paste(text_output, collapse = ""))
  })
  
  output$plotCurvaCalibracion <- renderPlot({
    req(datos_combinados())
    ggplot(datos_combinados()$curvas_combinadas, aes(x = concentracion_etanol, y = OD_promedio, color = Dataset)) +
      geom_point(size = 3, alpha = 0.8) + geom_smooth(method = "lm", se = FALSE, size=1.2) +
      labs(title = "Curva de Calibración Comparativa", x = "Concentración de Etanol (%)", y = "Absorbancia (OD)", color = "Dataset") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom")
  })
  
  output$plotAbsorbanciaMuestras <- renderPlot({
    req(datos_combinados())
    datos_wide <- datos_combinados()$df_combinado
    datos_long <- datos_wide %>%
      pivot_longer(cols = c("OD_promedio", "OD1", "OD2"), names_to = "Tipo_Medida_Abs", values_to = "Valor_Abs")
    medidas_a_mostrar <- c()
    if(input$show_prom_abs) medidas_a_mostrar <- c(medidas_a_mostrar, "OD_promedio")
    if(input$show_od1_abs) medidas_a_mostrar <- c(medidas_a_mostrar, "OD1")
    if(input$show_od2_abs) medidas_a_mostrar <- c(medidas_a_mostrar, "OD2")
    datos_filtrados <- datos_long %>% filter(Tipo_Medida_Abs %in% medidas_a_mostrar)
    p <- ggplot()
    if(input$show_desv_abs && input$show_prom_abs){
      p <- p + geom_ribbon(data = datos_wide, aes(x = Concentracion_promedio, ymin = OD_promedio - OD_desv_std, 
                                                  ymax = OD_promedio + OD_desv_std, fill = Dataset, group = Dataset), alpha = 0.2)
    }
    p <- p + geom_line(data = datos_filtrados, aes(x = Concentracion_promedio, y = Valor_Abs, color = Dataset, linetype = Tipo_Medida_Abs, group = interaction(Dataset, Tipo_Medida_Abs)), size=1) +
      geom_point(data = datos_filtrados, aes(x = Concentracion_promedio, y = Valor_Abs, color = Dataset, shape = Tipo_Medida_Abs, group = interaction(Dataset, Tipo_Medida_Abs)), size=2.5)
    p + labs(title = "Absorbancia Comparativa de Muestras", x = "Concentración de Etanol Promedio (%)", y = "Absorbancia (OD)", color = "Dataset", fill = "Dataset", linetype = "Medida", shape = "Medida") +
      scale_linetype_manual(values = c("OD_promedio" = "solid", "OD1" = "dotted", "OD2" = "dashed")) +
      scale_shape_manual(values = c("OD_promedio" = 16, "OD1" = 1, "OD2" = 2)) +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom", legend.box = "vertical")
  })
  
  output$plotConcentracionEtanolTiempo <- renderPlot({
    req(datos_combinados())
    datos_wide <- datos_combinados()$df_combinado
    datos_long <- datos_wide %>%
      pivot_longer(cols = c("Concentracion_promedio", "Concentracion_OD1", "Concentracion_OD2"), names_to = "Tipo_Medida_Conc", values_to = "Valor_Conc")
    medidas_a_mostrar <- c()
    if(input$show_prom_conc) medidas_a_mostrar <- c(medidas_a_mostrar, "Concentracion_promedio")
    if(input$show_od1_conc) medidas_a_mostrar <- c(medidas_a_mostrar, "Concentracion_OD1")
    if(input$show_od2_conc) medidas_a_mostrar <- c(medidas_a_mostrar, "Concentracion_OD2")
    datos_filtrados <- datos_long %>% filter(Tipo_Medida_Conc %in% medidas_a_mostrar)
    p <- ggplot()
    if(input$show_desv_conc && input$show_prom_conc){
      p <- p + geom_ribbon(data = datos_wide, aes(x = Tiempo_fermentacion, ymin = Concentracion_promedio - Concentracion_desv_std, 
                                                  ymax = Concentracion_promedio + Concentracion_desv_std, fill = Dataset, group = Dataset), alpha = 0.2)
    }
    p <- p + geom_line(data = datos_filtrados, aes(x = Tiempo_fermentacion, y = Valor_Conc, color = Dataset, linetype = Tipo_Medida_Conc, group = interaction(Dataset, Tipo_Medida_Conc)), size=1) +
      geom_point(data = datos_filtrados, aes(x = Tiempo_fermentacion, y = Valor_Conc, color = Dataset, shape = Tipo_Medida_Conc, group = interaction(Dataset, Tipo_Medida_Conc)), size=2.5)
    p + labs(title = "Concentración de Etanol vs Tiempo (Comparativo)", x = "Tiempo (min)", y = "Concentración de Etanol (%)", color = "Dataset", fill = "Dataset", linetype = "Medida", shape = "Medida") +
      scale_linetype_manual(values = c("Concentracion_promedio" = "solid", "Concentracion_OD1" = "dotted", "Concentracion_OD2" = "dashed"), 
                            labels = c("Promedio", "Réplica OD1", "Réplica OD2")) +
      scale_shape_manual(values = c("Concentracion_promedio" = 16, "Concentracion_OD1" = 1, "Concentracion_OD2" = 2),
                         labels = c("Promedio", "Réplica OD1", "Réplica OD2")) +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom", legend.box = "vertical")
  })
  
  output$download_button_ui <- renderUI({ req(datos_combinados()); downloadButton("downloadReport", "Descargar Reporte (PDF)", class = "btn-success") })
  
  output$downloadReport <- downloadHandler(
    filename = function() { paste0("reporte_etanol_comparativo_", format(Sys.Date(), "%Y%m%d"), ".pdf") },
    content = function(file) {
      id <- showNotification("Generando reporte PDF...", duration = NULL, closeButton = FALSE); on.exit(removeNotification(id), add = TRUE)
      temp_report <- file.path(tempdir(), "reporte.Rmd")
      rmd_content <- "
---
title: 'Reporte Comparativo de Actividad de Etanol'
output: pdf_document
params:
  resultados: !r list()
  plot_cinetica: !r ggplot()
  fecha: ''
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(ggplot2); library(knitr); library(kableExtra)

print(params$plot_cinetica)

for (res in params$resultados) {
  cat(paste0('#### Resultados para: ', res$nombre_dataset, '\\n\\n'))
  tabla_df <- res$df_final %>% 
    select(Muestra_ID, Tiempo_fermentacion, OD_promedio, Concentracion_promedio, Concentracion_desv_std) %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  print(kable(tabla_df, 'latex', booktabs = TRUE) %>% kable_styling(latex_options = c('striped', 'scale_down')))
  cat('\\n\\n')
}

"
    writeLines(rmd_content, temp_report)
    g_cinetica_combinada <- datos_combinados()
    
    }
  )
}
    
shinyApp(ui = ui, server = server)