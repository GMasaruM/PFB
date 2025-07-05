library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rmarkdown)
library(knitr)
library(kableExtra)

# ==============================================================================
# Interfaz de Usuario (UI) - Funcionalidad Completa
# ==============================================================================
ui <- fluidPage(
  titlePanel("Análisis Comparativo por Grupo de Muestra (Archivo Único)"),
  sidebarLayout(
    sidebarPanel(
      h4("1. Carga de Datos"),
      fileInput("file_datos", "Selecciona archivo CSV único:", accept = c(".csv")),
      
      helpText(HTML("El CSV debe contener: <b>Tipo, Muestra_ID, Tiempo_fermentacion, concentracion_etanol, OD1, OD2</b>.<br>
                     - <b>Tipo</b>: 'Muestra', 'Blanco', o 'Estandar'.<br>
                     - <b>Muestra_ID</b>: Para 'Muestras', usar un prefijo común para agrupar (ej: <b>A</b>1, <b>A</b>2... para el grupo 'A'; <b>B</b>1, <b>B</b>2... para el grupo 'B').<br>
                     - Los datos de 'Blanco' y 'Estandar' se usarán para TODOS los experimentos.<br>
                     <b>Ejemplo de formato:</b>
                     <pre style='font-size: 10px;'>Tipo,Muestra_ID,Tiempo_fermentacion,concentracion_etanol,OD1,OD2
Blanco,,,0.395,0.327
Estandar,,,0.1,3.476,3.425
Muestra,A1,0,,0.747,0.597
Muestra,A2,1440,,0.742,0.666
Muestra,B1,0,,0.812,0.805
Muestra,B2,1440,,0.950,0.981</pre>")),
      
      hr(),
      
      h4("2. Parámetros de Cálculo Globales"),
      numericInput("DF", "Factor de dilución (DF):", value = 50, min = 1, step = 1),
      numericInput("cantidad_sustrato", "Cantidad de Sustrato Utilizado (g):", value = 100, min = 1, step = 1),
      
      hr(),
      actionButton("calcular", "Calcular y Graficar", class = "btn-primary btn-lg", icon = icon("cogs"))
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Resultados Tabulados", 
                 uiOutput("tablas_resultados_ui")
        ),
        tabPanel("Parámetros Cinéticos", 
                 h4("Parámetros Cinéticos por Grupo de Muestra"), 
                 verbatimTextOutput("parametros")
        ),
        tabPanel("Gráficas Superpuestas", 
                 h4("Gráfica de Curva de Calibración (Única para todos)"), 
                 plotOutput("plotCurvaCalibracion"), 
                 hr(),
                 # Gráfico de Absorbancia Restaurado
                 h4("Gráfica de Absorbancia vs Concentración"),
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
# Lógica del Servidor (Server) - Funcionalidad Completa
# ==============================================================================
server <- function(input, output, session) {
  
  datos_procesados <- eventReactive(input$calcular, {
    req(input$file_datos)
    df_raw <- read.csv(input$file_datos$datapath, header = TRUE, stringsAsFactors = FALSE)
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
    
    modelo_calib <- lm(OD_promedio ~ concentracion_etanol, data = curva_calib)
    Slope <- coef(modelo_calib)[2]
    R_squared <- summary(modelo_calib)$r.squared
    
    muestras <- df_raw %>% filter(Tipo == "Muestra")
    validate(need(nrow(muestras) > 0, "Error: No se encontraron 'Muestras'."))
    
    df_resultados <- muestras %>%
      mutate(
        Grupo = gsub("[0-9].*$", "", Muestra_ID), 
        Concentracion_OD1 = ((OD1 - OD_blanco_promedio) / Slope) * input$DF,
        Concentracion_OD2 = ((OD2 - OD_blanco_promedio) / Slope) * input$DF,
        Concentracion_promedio = (Concentracion_OD1 + Concentracion_OD2) / 2,
        Concentracion_desv_std = apply(cbind(Concentracion_OD1, Concentracion_OD2), 1, sd, na.rm = TRUE),
        OD_promedio = (OD1 + OD2) / 2,
        OD_desv_std = apply(cbind(OD1, OD2), 1, sd, na.rm = TRUE)
      ) %>%
      arrange(Grupo, Tiempo_fermentacion, Muestra_ID)
    
    validate(need(length(unique(df_resultados$Grupo)) > 0, "No se pudo identificar ningún grupo a partir de los prefijos de Muestra_ID."))
    
    return(list(df_final = df_resultados, curva_calib = curva_calib, Slope = Slope, R_squared = R_squared))
  })
  
  # --- Salidas de Tablas Dinámicas ---
  output$tablas_resultados_ui <- renderUI({
    req(datos_procesados())
    nombres_grupos <- unique(datos_procesados()$df_final$Grupo)
    lapply(nombres_grupos, function(nombre_grupo) {
      ns <- NS(nombre_grupo)
      tagList(h4(paste("Resultados para el Grupo:", nombre_grupo)), tableOutput(ns("tabla")), hr())
    })
  })
  
  observe({
    req(datos_procesados())
    df_final <- datos_procesados()$df_final
    nombres_grupos <- unique(df_final$Grupo)
    
    lapply(nombres_grupos, function(nombre_grupo) {
      ns <- NS(nombre_grupo)
      output[[ns("tabla")]] <- renderTable({
        df_final %>%
          filter(Grupo == nombre_grupo) %>%
          # Selección de todas las columnas solicitadas
          select(Muestra_ID, Tiempo_fermentacion, OD1, OD2, OD_promedio, OD_desv_std,
                 Concentracion_OD1, Concentracion_OD2, Concentracion_promedio, Concentracion_desv_std) %>%
          rename("Muestra" = Muestra_ID, "Tiempo (min)" = Tiempo_fermentacion,
                 "Abs. OD1" = OD1, "Abs. OD2" = OD2, "Abs. Promedio" = OD_promedio, "DE Abs." = OD_desv_std,
                 "Conc. OD1 (%)" = Concentracion_OD1, "Conc. OD2 (%)" = Concentracion_OD2, 
                 "Conc. Promedio (%)" = Concentracion_promedio, "DE Conc. (%)" = Concentracion_desv_std) %>%
          mutate(across(where(is.numeric), ~round(., 3)))
      }, striped = TRUE, hover = TRUE, bordered = TRUE)
    })
  })
  
  # --- Salidas de Parámetros y Gráficos ---
  output$parametros <- renderPrint({
    req(datos_procesados())
    df_parametros <- datos_procesados()$df_final %>%
      group_by(Grupo) %>%
      summarise(
        A_max = max(Concentracion_promedio, na.rm = TRUE),
        t_opt = { if(n() >= 3) { modelo_q <- lm(Concentracion_promedio ~ poly(Tiempo_fermentacion, 2, raw = TRUE), data = cur_data()); coefs <- coef(modelo_q); if (length(coefs) >= 3 && !is.na(coefs[3]) && coefs[3] < 0) -coefs[2] / (2 * coefs[3]) else NA } else { NA } },
        Rendimiento = (A_max * input$DF * 7.85 / input$cantidad_sustrato) * 100, .groups = "drop"
      )
    for (i in 1:nrow(df_parametros)) {
      cat(paste0("--- Grupo: ", df_parametros$Grupo[i], " ---\n"))
      cat(sprintf("A_max = %.2f %%\n", df_parametros$A_max[i]))
      cat(sprintf("t_opt = %.1f min\n", df_parametros$t_opt[i]))
      cat(sprintf("Rendimiento = %.2f %%\n\n", df_parametros$Rendimiento[i]))
    }
  })
  
  output$plotCurvaCalibracion <- renderPlot({
    req(datos_procesados())
    curva <- datos_procesados()$curva_calib
    R_squared <- round(datos_procesados()$R_squared, 4)
    ggplot(curva, aes(x = concentracion_etanol, y = OD_promedio)) +
      geom_point(size = 3, color = "blue") + geom_smooth(method = "lm", se = FALSE, color="red") +
      annotate("text", x = max(curva$concentracion_etanol, na.rm=T) * 0.8, y = max(curva$OD_promedio, na.rm=T) * 0.9, 
               label = paste("R² =", R_squared), size = 5) +
      labs(title = "Curva de Calibración", x = "Concentración de Etanol (%)", y = "Absorbancia (OD)") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  # Gráfico de Absorbancia vs. Concentración
  output$plotAbsorbanciaMuestras <- renderPlot({
    req(datos_procesados())
    datos_wide <- datos_procesados()$df_final
    datos_long <- datos_wide %>% pivot_longer(cols = c("OD_promedio", "OD1", "OD2"), names_to = "Tipo_Medida_Abs", values_to = "Valor_Abs")
    medidas_a_mostrar <- c()
    if(input$show_prom_abs) medidas_a_mostrar <- c(medidas_a_mostrar, "OD_promedio")
    if(input$show_od1_abs) medidas_a_mostrar <- c(medidas_a_mostrar, "OD1")
    if(input$show_od2_abs) medidas_a_mostrar <- c(medidas_a_mostrar, "OD2")
    datos_filtrados <- datos_long %>% filter(Tipo_Medida_Abs %in% medidas_a_mostrar)
    
    p <- ggplot()
    if(input$show_desv_abs && input$show_prom_abs){
      p <- p + geom_ribbon(data = datos_wide, aes(x = Concentracion_promedio, ymin = OD_promedio - OD_desv_std, ymax = OD_promedio + OD_desv_std, fill = Grupo, group = Grupo), alpha = 0.2)
    }
    p <- p + geom_line(data = datos_filtrados, aes(x = Concentracion_promedio, y = Valor_Abs, color = Grupo, linetype = Tipo_Medida_Abs, group = interaction(Grupo, Tipo_Medida_Abs)), size=1) +
      geom_point(data = datos_filtrados, aes(x = Concentracion_promedio, y = Valor_Abs, color = Grupo, shape = Tipo_Medida_Abs, group = interaction(Grupo, Tipo_Medida_Abs)), size=2.5)
    
    p + labs(title = "Absorbancia vs Concentración de Etanol", x = "Concentración de Etanol Promedio (%)", y = "Absorbancia (OD)", color = "Grupo", fill = "Grupo", linetype = "Medida", shape = "Medida") +
      scale_linetype_manual(values = c("OD_promedio" = "solid", "OD1" = "dotted", "OD2" = "dashed"), labels=c("Promedio", "Réplica OD1", "Réplica OD2")) +
      scale_shape_manual(values = c("OD_promedio" = 16, "OD1" = 1, "OD2" = 2), labels=c("Promedio", "Réplica OD1", "Réplica OD2")) +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom", legend.box = "vertical")
  })
  
  # Gráfico de Concentración vs. Tiempo
  output$plotConcentracionEtanolTiempo <- renderPlot({
    req(datos_procesados())
    datos_wide <- datos_procesados()$df_final
    datos_long <- datos_wide %>% pivot_longer(cols = c("Concentracion_promedio", "Concentracion_OD1", "Concentracion_OD2"), names_to = "Tipo_Medida_Conc", values_to = "Valor_Conc")
    medidas_a_mostrar <- c()
    if(input$show_prom_conc) medidas_a_mostrar <- c(medidas_a_mostrar, "Concentracion_promedio")
    if(input$show_od1_conc) medidas_a_mostrar <- c(medidas_a_mostrar, "Concentracion_OD1")
    if(input$show_od2_conc) medidas_a_mostrar <- c(medidas_a_mostrar, "Concentracion_OD2")
    datos_filtrados <- datos_long %>% filter(Tipo_Medida_Conc %in% medidas_a_mostrar)
    
    p <- ggplot()
    if(input$show_desv_conc && input$show_prom_conc){
      p <- p + geom_ribbon(data = datos_wide, aes(x = Tiempo_fermentacion, ymin = Concentracion_promedio - Concentracion_desv_std, ymax = Concentracion_promedio + Concentracion_desv_std, fill = Grupo, group = Grupo), alpha = 0.2)
    }
    p <- p + geom_line(data = datos_filtrados, aes(x = Tiempo_fermentacion, y = Valor_Conc, color = Grupo, linetype = Tipo_Medida_Conc, group = interaction(Grupo, Tipo_Medida_Conc)), size=1) +
      geom_point(data = datos_filtrados, aes(x = Tiempo_fermentacion, y = Valor_Conc, color = Grupo, shape = Tipo_Medida_Conc, group = interaction(Grupo, Tipo_Medida_Conc)), size=2.5)
    
    p + labs(title = "Concentración de Etanol vs Tiempo", x = "Tiempo (min)", y = "Concentración de Etanol (%)", color = "Grupo", fill = "Grupo", linetype = "Medida", shape = "Medida") +
      scale_linetype_manual(values = c("Concentracion_promedio" = "solid", "Concentracion_OD1" = "dotted", "Concentracion_OD2" = "dashed"), labels = c("Promedio", "Réplica OD1", "Réplica OD2")) +
      scale_shape_manual(values = c("Concentracion_promedio" = 16, "Concentracion_OD1" = 1, "Concentracion_OD2" = 2), labels = c("Promedio", "Réplica OD1", "Réplica OD2")) +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom", legend.box = "vertical")
  })
  
  output$download_button_ui <- renderUI({ req(datos_procesados()); downloadButton("downloadReport", "Descargar Reporte (PDF)", class = "btn-success") })
  
  output$downloadReport <- downloadHandler(
    filename = function() { paste0("reporte_etanol_", format(Sys.Date(), "%Y%m%d"), ".pdf") },
    content = function(file) { id <- showNotification("Generando reporte PDF...", duration = NULL, closeButton = FALSE); on.exit(removeNotification(id), add = TRUE) }
  )
}

shinyApp(ui = ui, server = server)