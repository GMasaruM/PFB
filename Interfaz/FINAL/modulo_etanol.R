# modulo_etanol.R

etanolUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Análisis de Producción de Etanol"),
    sidebarLayout(
      sidebarPanel(
        h4("1. Carga de Datos de Etanol"),
        fileInput(ns("file_datos"), "Selecciona archivo CSV:", accept = c(".csv")),
        helpText(HTML("El CSV debe contener: <b>Tipo, Muestra_ID, Tiempo_fermentacion, Concentracion, OD1, OD2</b>.")),
        hr(),
        h4("2. Configuración"),
        numericInput(ns("DF"), "Factor de dilución (DF):", value = 50),
        hr(),
        actionButton(ns("calcular"), "Calcular Etanol", class = "btn-primary", icon = icon("cogs"))
      ),
      mainPanel(
        tabsetPanel(id = ns("tabs"),
                    tabPanel("Resultados Tabulados", uiOutput(ns("tablas_resultados"))),
                    tabPanel("Análisis", uiOutput(ns("analisis_por_grupo"))),
                    tabPanel("Gráficos",
                             h4("Curva de Calibración"), plotlyOutput(ns("plot_calibracion")),
                             h4("Concentración vs. Tiempo"), plotlyOutput(ns("plot_consumo_con_sd"))
                    )
        )
      )
    )
  )
}

etanolServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    datos_analizados <- eventReactive(input$calcular, {
      req(input$file_datos)
      df_raw <- read.csv(input$file_datos$datapath, header = TRUE, stringsAsFactors = FALSE)
      
      blancos <- df_raw %>% filter(Tipo == "Blanco")
      od_blanco <- mean(c(blancos$OD1, blancos$OD2), na.rm = TRUE)
      
      estandares <- df_raw %>% filter(Tipo == "Estandar") %>%
        mutate(OD_prom = rowMeans(select(., OD1, OD2), na.rm = TRUE), 
               OD_corregido = OD_prom - od_blanco)
      
      modelo_lm <- lm(Concentracion ~ OD_corregido - 1, data = estandares) 
      slope <- coef(modelo_lm)[["OD_corregido"]]
      
      muestras_long <- df_raw %>% filter(Tipo == "Muestra") %>% 
        pivot_longer(cols = c(OD1, OD2), names_to = "replica", values_to = "OD_raw") %>% 
        mutate(
          Grupo = substr(Muestra_ID, 1, 1), 
          OD_corregida = OD_raw - od_blanco,
          Valor = pmax(0, (OD_corregida * slope) * input$DF) # Asumiendo que para etanol también es OD * slope
        )
      
      muestras_sumarizado <- muestras_long %>% group_by(Grupo, Tiempo_fermentacion) %>% 
        summarise(
          Valor_mean = mean(Valor, na.rm = TRUE), 
          Valor_sd = sd(Valor, na.rm = TRUE), .groups = 'drop'
        ) %>%
        arrange(Grupo, Tiempo_fermentacion)
      
      list(estandares = estandares, modelo_calibracion = modelo_lm, muestras_sumarizado = muestras_sumarizado)
    })
    
    # --- Outputs para UI de Etanol ---
    output$tablas_resultados <- renderUI({ req(datos_analizados()); lapply(unique(datos_analizados()$muestras_sumarizado$Grupo), function(g) { tagList(h5(paste("Resultados Etanol - Grupo", g)), tableOutput(session$ns(paste0("tabla_res_", g))), hr()) }) })
    observe({ req(datos_analizados()); df <- datos_analizados()$muestras_sumarizado; for(g in unique(df$Grupo)) { local({ g_local <- g; output[[paste0("tabla_res_", g_local)]] <- renderTable({ df %>% filter(Grupo == g_local) %>% select(Tiempo_fermentacion, `Etanol Medio` = Valor_mean, `DE` = Valor_sd) }) }) } })
    output$plot_calibracion <- renderPlotly({ req(datos_analizados()); p <- ggplot(datos_analizados()$estandares, aes(x = OD_corregido, y = Concentracion)) + geom_point(); ggplotly(p) })
    output$plot_consumo_con_sd <- renderPlotly({ req(datos_analizados()); p <- ggplot(datos_analizados()$muestras_sumarizado, aes(x=Tiempo_fermentacion, y=Valor_mean, color=Grupo)) + geom_line() + geom_point(); ggplotly(p) })
    
    # Devolver un data.frame estandarizado
    datos_para_integracion <- reactive({
      req(datos_analizados())
      datos_analizados()$muestras_sumarizado %>%
        select(Grupo, Tiempo_fermentacion, Valor = Valor_mean)
    })
    
    return(datos_para_integracion)
  })
}