# modulo_glucosa.R

glucosaUI <- function(id) {
  # <<< 1. Se crea el namespace para los IDs >>>
  ns <- NS(id)
  
  # <<< 2. La UI original se pega aquí, sin fluidPage/titlePanel >>>
  sidebarLayout(
    sidebarPanel(
      h4("1. Carga de Datos de Glucosa"),
      fileInput(ns("file_datos"), "Selecciona archivo CSV único:", accept = c(".csv", "text/csv")),
      helpText(HTML("El CSV debe contener: <b>Tipo, Muestra_ID, Tiempo_fermentacion, Concentracion, OD1, OD2</b>.")),
      hr(),
      h4("2. Configuración"),
      numericInput(ns("DF"), "Factor de dilución (DF):", value = 1, min = 1, step = 1),
      hr(),
      actionButton(ns("calcular"), "Calcular Glucosa", class = "btn-primary", icon = icon("cogs"))
    ),
    mainPanel(
      tabsetPanel(
        id = ns("tabs"),
        tabPanel("Resultados Tabulados", 
                 h4("Concentración de Glucosa por Grupo"),
                 uiOutput(ns("tablas_resultados"))),
        tabPanel("Análisis de Consumo",
                 h4("Parámetros de Glucosa por Grupo"), 
                 uiOutput(ns("analisis_por_grupo"))), 
        tabPanel("Gráficos de Análisis",
                 h4("1. Curva de Calibración"), plotlyOutput(ns("plot_calibracion")),
                 hr(),
                 h4("2. Diagnóstico: Absorbancia vs. Glucosa"), uiOutput(ns("diagnostic_controls_ui")), plotlyOutput(ns("plot_diagnostico_muestras")),
                 hr(),
                 h4("3. Concentración de Glucosa vs. Tiempo"), uiOutput(ns("consumption_controls_ui")), plotlyOutput(ns("plot_consumo_con_sd"))
        )
      )
    )
  )
}

glucosaServer <- function(id) {
  # <<< 3. Toda la lógica del servidor se envuelve en moduleServer >>>
  moduleServer(id, function(input, output, session) {
    
    # --- Tu código de servidor original, pegado directamente ---
    datos_analizados <- eventReactive(input$calcular, {
      req(input$file_datos)
      validate(need(tools::file_ext(input$file_datos$name) == "csv", "Por favor, carga un archivo .csv"))
      df_raw <- tryCatch(read.csv(input$file_datos$datapath, header=TRUE, stringsAsFactors=FALSE, check.names=FALSE), error=function(e) { shiny::validate("No se pudo leer el archivo CSV.") })
      cols_requeridas <- c("Tipo", "Muestra_ID", "Tiempo_fermentacion", "Concentracion", "OD1", "OD2")
      validate(need(all(cols_requeridas %in% names(df_raw)), paste("Faltan columnas:", paste(cols_requeridas, collapse=", "))))
      
      od_blanco_df <- df_raw %>% filter(Tipo == "Blanco") %>% pivot_longer(cols = c(OD1, OD2), names_to = "replica", values_to = "OD")
      validate(need(nrow(od_blanco_df) > 0, "No hay datos de 'Blanco'."))
      od_blanco <- mean(od_blanco_df$OD, na.rm = TRUE)
      validate(need(!is.na(od_blanco), "El OD del blanco es NA."))
      
      df_estandares <- df_raw %>% filter(Tipo == "Estandar") %>% mutate(OD_prom = rowMeans(select(., OD1, OD2), na.rm = TRUE), OD_corregido = OD_prom - od_blanco)
      validate(need(nrow(df_estandares) >= 2, "Se necesitan al menos 2 estándares."))
      
      modelo_lm <- lm(Concentracion ~ OD_corregido - 1, data = df_estandares) 
      slope <- coef(modelo_lm)[["OD_corregido"]]
      validate(need(!is.na(slope) && slope != 0, "El slope de la curva de calibración es inválido."), need(slope > 0, "El slope de la curva de calibración debe ser positivo."))
      
      df_muestras_long <- df_raw %>% filter(Tipo == "Muestra") %>% 
        pivot_longer(cols = c(OD1, OD2), names_to = "replica", values_to = "OD_raw") %>% 
        mutate(Grupo = substr(Muestra_ID, 1, 1), OD_corregida = OD_raw - od_blanco, Glucosa_uM = pmax(0, (OD_corregida / slope) * input$DF))
      validate(need(nrow(df_muestras_long) > 0, "No se encontraron datos de 'Muestra'."))
      
      df_muestras_sumarizado <- df_muestras_long %>% group_by(Grupo, Tiempo_fermentacion) %>% 
        summarise(Glucosa_uM_mean = mean(Glucosa_uM, na.rm=TRUE), Glucosa_uM_sd = sd(Glucosa_uM, na.rm=TRUE), .groups = 'drop') %>%
        arrange(Grupo, Tiempo_fermentacion)
      
      list(estandares = df_estandares, modelo_calibracion = modelo_lm, muestras_long = df_muestras_long, muestras_sumarizado = df_muestras_sumarizado)
    })
    
    # --- Tus outputs originales, con session$ns() para IDs dinámicos ---
    output$tablas_resultados <- renderUI({ req(datos_analizados()); lapply(unique(datos_analizados()$muestras_sumarizado$Grupo), function(g) { tagList(h5(paste("Concentración de Glucosa - Grupo", g)), tableOutput(session$ns(paste0("tabla_res_", g))), hr()) }) })
    observe({ req(datos_analizados()); df_sumarizado <- datos_analizados()$muestras_sumarizado; for (g in unique(df_sumarizado$Grupo)) { local({ grupo_local <- g; output[[paste0("tabla_res_", grupo_local)]] <- renderTable({ df_sumarizado %>% filter(Grupo == grupo_local) %>% mutate(Tiempo_h = Tiempo_fermentacion / 60) %>% select(`Tiempo (h)` = Tiempo_h, `Glucosa Media (µM)` = Glucosa_uM_mean, `Desv. Est. (µM)` = Glucosa_uM_sd) %>% mutate(across(where(is.numeric), ~ sprintf("%.3f", .))) }, striped = TRUE, hover = TRUE, bordered = TRUE) }) } })
    output$analisis_por_grupo <- renderUI({ req(datos_analizados()); tagList(lapply(unique(datos_analizados()$muestras_sumarizado$Grupo), function(g) { tagList(h5(paste("Parámetros de Glucosa - Grupo", g)), tableOutput(session$ns(paste0("tabla_analisis_", g))), hr()) })) })
    observe({ req(datos_analizados()); df_sumarizado <- datos_analizados()$muestras_sumarizado; for (g in unique(df_sumarizado$Grupo)) { local({ grupo_local <- g; datos_grupo <- df_sumarizado %>% filter(Grupo == grupo_local) %>% arrange(Tiempo_fermentacion); output[[paste0("tabla_analisis_", grupo_local)]] <- renderTable({ if (nrow(datos_grupo) < 2) { return(data.frame(Parámetro = "Error", Valor = "Se necesitan >= 2 puntos")) }; g_i <- datos_grupo$Glucosa_uM_mean[1]; g_f <- tail(datos_grupo$Glucosa_uM_mean, 1); cambio_neto <- g_f - g_i; duracion <- tail(datos_grupo$Tiempo_fermentacion, 1) - datos_grupo$Tiempo_fermentacion[1]; g_min <- min(datos_grupo$Glucosa_uM_mean, na.rm=T); g_max <- max(datos_grupo$Glucosa_uM_mean, na.rm=T); t_max <- datos_grupo$Tiempo_fermentacion[which.max(datos_grupo$Glucosa_uM_mean)]; g_prod <- g_max - g_i; g_cons <- g_max - g_f; data.frame(Parámetro = c("Glucosa Inicial (µM)", "Glucosa Final (µM)", "Glucosa Mín (µM)", "Glucosa Máx (µM)", "Tiempo a Máx (min)", "Producida (µM)", "Consumida (µM)", "Cambio Neto (µM)", "Duración (min)"), Valor = c(sprintf("%.3f",g_i), sprintf("%.3f",g_f), sprintf("%.3f",g_min), sprintf("%.3f",g_max), sprintf("%.3f",t_max), sprintf("%.3f",g_prod), sprintf("%.3f",g_cons), sprintf("%.3f",cambio_neto), sprintf("%.3f",duracion))) }, striped=TRUE, hover=TRUE, bordered=TRUE, align='l') }) } })
    grupos_disponibles <- reactive({ req(datos_analizados()); unique(datos_analizados()$muestras_sumarizado$Grupo) })
    output$diagnostic_controls_ui <- renderUI({ grupos <- grupos_disponibles(); checkboxGroupInput(session$ns("selected_groups_diagnostic"), "Grupos:", choices=grupos, selected=grupos, inline=TRUE) })
    output$consumption_controls_ui <- renderUI({ grupos <- grupos_disponibles(); fluidRow(column(8, checkboxGroupInput(session$ns("selected_groups_consumption"), "Grupos:", choices=grupos, selected=grupos, inline=TRUE)), column(4, checkboxInput(session$ns("show_sd"), "DE", value=TRUE))) })
    output$plot_calibracion <- renderPlotly({ req(datos_analizados()); datos <- datos_analizados(); df_estandares <- datos$estandares; modelo <- datos$modelo_calibracion; r2 <- summary(modelo)$r.squared; p <- ggplot(df_estandares, aes(x=OD_corregido, y=Concentracion, text=paste("Abs:",round(OD_corregido,3),"<br>Conc:",round(Concentracion,2),"µM"))) + geom_point(color="blue",size=3,alpha=0.8) + labs(title="Curva de Calibración",x="Abs Corregida",y="Conc Glucosa (µM)") + theme_minimal(); fig <- ggplotly(p, tooltip="text"); fig <- fig %>% add_lines(x=~OD_corregido, y=~predict(modelo), data=df_estandares, name='Regresión', line=list(color='red')); formula_t <- sprintf("[Glucosa]=%.3f*OD",coef(modelo)[["OD_corregido"]]); r2_t <- sprintf("R<sup>2</sup>=%.4f",r2); fig %>% layout(annotations=list(x=0.95,y=0.05,xref="paper",yref="paper",text=paste(formula_t,r2_t,sep="<br>"),showarrow=F,xanchor='right',yanchor='bottom')) %>% config(displaylogo=F) })
    output$plot_diagnostico_muestras <- renderPlotly({ req(datos_analizados(), input$selected_groups_diagnostic); df_filtrado <- datos_analizados()$muestras_long %>% filter(Grupo %in% input$selected_groups_diagnostic) %>% group_by(Grupo,Muestra_ID,Tiempo_fermentacion) %>% summarise(OD_prom=mean(OD_corregida,na.rm=T),Glucosa_mean=mean(Glucosa_uM,na.rm=T),.groups="drop") %>% arrange(Muestra_ID,Tiempo_fermentacion); plot_ly(data=df_filtrado,x=~OD_prom,y=~Glucosa_mean,type='scatter',mode='lines+markers',group=~Muestra_ID,line=list(dash='dot'),color=~Grupo,text=~paste("Muestra:",Muestra_ID,"<br>Tiempo:",Tiempo_fermentacion,"min","<br>Abs:",round(OD_prom,3),"<br>Glucosa:",round(Glucosa_mean,2),"µM"),hoverinfo='text') %>% layout(title="Diagnóstico: Abs vs Glucosa en Muestras",xaxis=list(title="Abs Promedio Corregida"),yaxis=list(title="Glucosa Calculada (µM)"),legend=list(title=list(text='Grupo'))) %>% config(displaylogo=F) })
    output$plot_consumo_con_sd <- renderPlotly({ req(datos_analizados(), input$selected_groups_consumption); df_filtrado <- datos_analizados()$muestras_sumarizado %>% filter(Grupo %in% input$selected_groups_consumption); fig <- plot_ly(); for(g in unique(df_filtrado$Grupo)) { df_grupo <- filter(df_filtrado, Grupo == g); fig <- fig %>% add_trace(data=df_grupo, x=~Tiempo_fermentacion, y=~Glucosa_uM_mean, type='scatter', mode='lines+markers', name=g, text=~paste("Grupo:",Grupo,"<br>Tiempo:",Tiempo_fermentacion,"min","<br>Glucosa:",round(Glucosa_uM_mean,2),"µM"),hoverinfo='text', error_y=if(input$show_sd){list(type="data",array=~Glucosa_uM_sd,visible=T)}else{NULL}) }; fig %>% layout(title="Consumo de Glucosa vs. Tiempo", xaxis=list(title="Tiempo (min)"), yaxis=list(title="Conc Glucosa (µM)"), legend=list(title=list(text='Grupo'))) %>% config(displaylogo=F) })
    
    # <<< 4. Devolver un data.frame estandarizado para la integración >>>
    datos_para_integracion <- reactive({
      req(datos_analizados())
      datos_analizados()$muestras_sumarizado %>%
        select(Grupo, Tiempo_fermentacion, Valor = Glucosa_uM_mean)
    })
    
    return(datos_para_integracion)
  })
}