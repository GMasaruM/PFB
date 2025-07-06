# Cargar librerías
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggiraph)

# -----------------------------------------------------------------------------
# Interfaz de Usuario (UI) para el Módulo Integrado
# -----------------------------------------------------------------------------
integradoUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Análisis Integrado de Parámetros de Fermentación"),
    
    fluidRow(
      column(12,
             helpText("Este gráfico combina los resultados de Glucosa, Amilasa y Etanol. Asegúrate de presionar 'Calcular' en cada pestaña individual para que los datos estén disponibles aquí."),
             hr(),
             
             wellPanel(
               fluidRow(
                 
                 column(6,
                        selectInput(ns("parametro_filtro"),
                                    h5(strong("Mostrar Parámetro:")),
                                    choices = c("Todos", "Glucosa", "Amilasa", "Etanol"), 
                                    selected = "Todos") 
                 ),
                 column(6,
                        checkboxGroupInput(ns("grupo_filtro"),
                                           h5(strong("Mostrar Grupo(s):")),
                                           choices = c("Grupo A" = "A", "Grupo B" = "B"),
                                           selected = c("A", "B"),
                                           inline = TRUE)
                 )
               )
             ),
             
             plotlyOutput(ns("grafico_combinado"), height = "700px"), 
             hr(),
             h4("Modelo Gráfico de Estados de Fermentación"),
             p("Pasa el ratón sobre un punto para ver un resumen. ¡Haz clic para más detalles!"),
             girafeOutput(ns("grafico_estados_interactivo"), height = "400px"),
             hr(),
             
             h4("Análisis Detallado por Parámetro"),
             p("Explora la evolución de cada parámetro individualmente:"),
             br(),
             fluidRow(
               column(4, style="text-align: center;", actionButton(ns("analizar_glucosa"), "¿Qué pasa con la Glucosa?", icon = icon("dna"), class = "btn-primary")),
               column(4, style="text-align: center;", actionButton(ns("analizar_amilasa"), "¿Qué pasa con la Amilasa?", icon = icon("flask"), class = "btn-warning")),
               column(4, style="text-align: center;", actionButton(ns("analizar_etanol"), "¿Qué pasa con el Etanol?", icon = icon("wine-bottle"), class = "btn-danger"))
             ),
             hr(),
             uiOutput(ns("contenedor_analisis_individual"))
      )
    )
  )
}

# -----------------------------------------------------------------------------
# Lógica del Servidor para el Módulo Integrado
# -----------------------------------------------------------------------------
integradoServer <- function(id, datos_etanol_r, datos_glucosa_r, datos_amilasa_r) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Combinar datos
    datos_combinados_crudos <- reactive({
      cols_expected <- c("Grupo", "Tiempo_fermentacion", "Valor", "Valor_sd", "TipoMedicion")
      standardize_df <- function(df, type_name) { if (!is.null(df) && nrow(df) > 0) { if (!"Valor_sd" %in% names(df)) df$Valor_sd <- NA_real_; df_s <- df %>% select(any_of(cols_expected)) %>% mutate(TipoMedicion = type_name); missing_cols <- setdiff(cols_expected, names(df_s)); for(col in missing_cols) df_s[[col]] <- NA; df_s %>% select(all_of(cols_expected)) } else { empty_df <- setNames(data.frame(matrix(ncol=5, nrow=0)), cols_expected); empty_df$TipoMedicion <- type_name; type.convert(empty_df, as.is=TRUE) } }
      bind_rows(standardize_df(datos_etanol_r(), "Etanol"), standardize_df(datos_glucosa_r(), "Glucosa"), standardize_df(datos_amilasa_r(), "Amilasa")) %>% filter(!is.na(Valor))
    })
    
    # 2. Filtrar datos
    datos_filtrados <- reactive({
      req(input$parametro_filtro, input$grupo_filtro)
      df <- datos_combinados_crudos()
      df <- df %>% filter(Grupo %in% input$grupo_filtro)
      
      # Condición para filtrar por parámetro
      if (input$parametro_filtro != "Todos") {
        df <- df %>% filter(TipoMedicion == input$parametro_filtro)
      }
      
      if (nrow(df) > 0) {
        df$TipoMedicion <- factor(df$TipoMedicion, levels = c("Glucosa", "Amilasa", "Etanol"))
        df$Grupo <- factor(df$Grupo, levels = c("A", "B"))
      }
      return(df)
    })
    
    # 3. Gráfico Combinado 
    y_axis_labels <- c("Glucosa"="Concentración de Glucosa (µM)", "Amilasa"="Actividad de la Amilasa (U/L)", "Etanol"="Concentración de Etanol (%)")
    
    output$grafico_combinado <- renderPlotly({
      df_plot <- datos_filtrados()
      if (nrow(df_plot) == 0) {
        return(plotly_empty() %>% layout(title = "Selecciona al menos un grupo para mostrar."))
      }
      
      # Gráfico base
      p <- ggplot(df_plot, aes(x = Tiempo_fermentacion, y = Valor, color = Grupo, group = interaction(Grupo, TipoMedicion))) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        theme_bw(base_size = 14) +
        theme(
          legend.position = "top",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          strip.text = element_text(face="bold")
        )
      
      # Etiquetas condicionales
      if (input$parametro_filtro == "Todos") {
        p <- p + 
          facet_wrap(~ TipoMedicion, scales = "free_y", ncol = 1, labeller = as_labeller(y_axis_labels)) +
          labs(
            title = "Evolución de Parámetros Clave",
            x = "Tiempo (min)",
            y = "Valor" 
          )
      } else {
        p <- p + 
          labs(
            title = paste("Evolución de", input$parametro_filtro),
            x = "Tiempo (min)",
            y = y_axis_labels[input$parametro_filtro] 
          )
      }
      
      ggplotly(p) %>% config(displaylogo = FALSE)
    })
    
    # --- MODELO DE ESTADOS ---
    puntos_data_interactive <- reactive({
tiempos <- c(0, 1440, 2880, 4320); data.frame(label=c("A1","A2","A3","A4","B1","B2","B3","B4"), x=rep(tiempos, 2), y=c(rep(2,4), rep(1,4)), tooltip_text=c("A1: La maquinaria enzimática se pone en marcha.","A2: Máxima producción enzimática; inicio fermentación.","A3: Pleno apogeo fermentativo; consumo máximo.","A4: Agotamiento de sustrato; cese de actividad.","B1: Adición de azúcar; amilasa inactiva.","B2: Fermentación rápida basada en azúcar añadido.","B3: Pico de etanol.","B4: Colapso  Acético"), data_id=c("A1","A2","A3","A4","B1","B2","B3","B4"))
})
    
    
    output$grafico_estados_interactivo <- renderGirafe({
      tiempos <- c(0,1440,2880,4320); estados_data <- data.frame(id_color=1:6, Grupo=c(rep("A",3),rep("B",3)), y_pos=c(rep(2,3),rep(1,3)), xmin=c(0,1440,2880,0,1,2880), xmax=c(1440,2880,4320,1,2880,4320), label_especifico=c("Sacarificación y Arranque","Fermentación Plena","Agotamiento","Adición Directa","Fermentación Rápida","Agotamiento")); titulos_generales_data <- data.frame(label=c("Estado 1","Estado 2","Estado 3"),x=c(720,2160,3600),y=3.0); puntos_df <- puntos_data_interactive()
      gg <- ggplot() + geom_rect(data=estados_data,aes(xmin=xmin,xmax=xmax,ymin=y_pos-0.3,ymax=y_pos+0.3,fill=as.factor(id_color)),alpha=0.4) + geom_segment(data=data.frame(y=c(1,2)),aes(x=0,xend=4320,y=y,yend=y),color="gray30",size=1.0) + geom_segment(data=data.frame(x=tiempos),aes(x=x,xend=x,y=0.5,yend=2.5),linetype="dashed",color="gray50") + geom_text(data=titulos_generales_data,aes(x=x,y=y,label=label),fontface="bold",size=6,color="black") + geom_text(data=estados_data,aes(x=(xmin+xmax)/2,y=y_pos+0.1,label=label_especifico),fontface="italic",size=4,vjust=0) + geom_point_interactive(data=puntos_df,aes(x=x,y=y,tooltip=tooltip_text,data_id=data_id),size=6,shape=21,fill="white",color="black",stroke=1.2) + geom_text(data=puntos_df,aes(x=x,y=y,label=label),color="black",size=3.5,fontface="bold") + annotate("text",x=-250,y=2,label="Grupo A",fontface="bold",size=5,hjust=1) + annotate("text",x=-250,y=1,label="Grupo B",fontface="bold",size=5,hjust=1) + scale_fill_viridis_d(guide="none") + scale_x_continuous(breaks=tiempos,labels=tiempos) + scale_y_continuous(limits=c(0.5,3.5)) + coord_cartesian(xlim=c(-300,4500),clip="off") + labs(x="Tiempo de Fermentación (min)",y="") + theme_void(base_size=14) + theme(axis.text.x=element_text(color="black",size=12,face="bold"),axis.title.x=element_text(color="black",size=14,face="bold",margin=margin(t=15)),plot.margin=margin(20,20,20,100))
      girafe(ggobj=gg, width_svg=12, height_svg=6, options=list(opts_hover(css="fill:yellow;stroke:black;stroke-width:2px;cursor:pointer;"),opts_selection(type="none"),opts_toolbar(saveaspng=FALSE)))
    })
    observeEvent(input$grafico_estados_interactivo_selected, {
      punto_id_sel <- input$grafico_estados_interactivo_selected; if(is.null(punto_id_sel)||punto_id_sel==""){return()}; puntos_df <- puntos_data_interactive(); texto_info <- puntos_df$tooltip_text[puntos_df$data_id==punto_id_sel]; showModal(modalDialog(title=paste("Detalle del Punto",punto_id_sel),p(strong(texto_info)),easyClose=TRUE,footer=modalButton("Cerrar")))
    })
    textos_individuales <- list(
      Glucosa = setNames(c("El combustible (chancaca) se incorporó al inicio, mientras que el combustible (glucosa) empezó a acumularse gracias a la intensa actividad de la obrera (amilasa)", "La producción de Combustible sigue superando al consumo por levaduras, por lo que la concentración aumenta.", "La acumulación de glucosa (combustible) llega a su máximo. A partir de este momento, la levadura empieza a consumir el azúcar más rápido de lo que se produce.", "El Combustible es casi totalmente consumido por las levaduras para producir el producto final.", "El Combustible (chancaca) fue añadido al inicio; la producción nueva es casi nula.", "El Combustible inicial se consume a gran velocidad por las levaduras activas.", "Se alcanza un pequeño pico de Combustible, que se consume inmediatamente.", "Queda poco Combustible disponible."), paste0(rep(c("A","B"), each=4), "_", rep(c(0, 1440, 2880, 4320), 2))),

      Amilasa = setNames(c("La Obrera (amilasa) está muy activa, rompiendo el almidón para generar el combustible.", "Actividad máxima de la Obrera; es el punto de mayor eficiencia en la producción de azúcar.", "La Obrera empieza a decaer, probablemente por cambios de pH o inhibición del producto.", "La Obrera ha terminado su rol principal en el proceso; su actividad es mínima.", "La actividad de amilasa es mínima, sugiriendo que los ingredientes adicionales de la receta inhiben la enzima o diluyen su efecto desde el comienzo del proceso.", "La actividad de la Obrera es extremadamente baja, incapaz de generar suficiente combustible.", "Un ligero y tardío pico de actividad de la Obrera, pero es demasiado bajo para impactar.", "La Obrera está completamente inactiva; no hay producción endógena de combustible."), paste0(rep(c("A","B"), each=4), "_", rep(c(0, 1440, 2880, 4320), 2))),
      
      Etanol =  setNames(c("Las levaduras aún no han comenzado a generar el Producto Final (etanol) de forma significativa.", "La producción del Producto Final comienza lentamente a medida que las levaduras se activan.", "A pesar de tener el máximo nivel de combustible disponible, la producción de etanol está estancada, lo que evidencia el fracaso de la levadura para fermentar eficientemente.", "El declive final del etanol confirma el cese de la fermentación y sugiere pérdidas por evaporación o por un leve consumo microbiano, cuyo impacto (avinagramiento) fue mínimo debido al bajo grado alcohólico.", "La producción del Producto Final es inmediata, la levadura es extremadamente activa desde el primer momento.", "La caída de etanol representa un valle metabólico transitorio, donde la levadura pausa su producción para adaptarse y puede pasar de consumir azúcares simples a fermentar los carbohidratos más complejos de la receta.", "Pico máximo del Producto Final (alta eficiencia fermentativa).", "El Producto Final es consumido por otras bacterias (indicio del avinagramiento)."), paste0(rep(c("A","B"), each=4), "_", rep(c(0, 1440, 2880, 4320), 2)))
    )
    parametro_seleccionado <- reactiveVal(NULL)
    observeEvent(input$analizar_glucosa, { parametro_seleccionado("Glucosa") })
    observeEvent(input$analizar_amilasa, { parametro_seleccionado("Amilasa") })
    observeEvent(input$analizar_etanol,  { parametro_seleccionado("Etanol") })
    observeEvent(input$cerrar_analisis, { parametro_seleccionado(NULL) })
    output$contenedor_analisis_individual <- renderUI({
      param <- parametro_seleccionado(); if (is.null(param)) { return(NULL) }
      ns <- session$ns
      wellPanel(
        h4(paste("Análisis Detallado de", param)),
        p("Pasa el ratón sobre los puntos para un resumen, o haz clic para abrir un pop-up con detalles."),
        girafeOutput(ns(paste0("grafico_", tolower(param)))),
        actionButton(ns("cerrar_analisis"), "Ocultar Gráfico", icon=icon("eye-slash"))
      )
    })
    for (param in c("Glucosa", "Amilasa", "Etanol")) {
      local({
        parametro_local <- param
        output[[paste0("grafico_", tolower(parametro_local))]] <- renderGirafe({
          req(parametro_seleccionado() == parametro_local)
          datos_parametro <- datos_combinados_crudos() %>% filter(TipoMedicion == parametro_local) %>% mutate(data_id = paste0(Grupo, "_", Tiempo_fermentacion), tooltip_text = textos_individuales[[parametro_local]][data_id])
          gg <- ggplot(datos_parametro, aes(x = Tiempo_fermentacion, y = Valor, color = Grupo, group = Grupo)) + geom_line(size = 1.2) + geom_point_interactive(aes(tooltip = tooltip_text, data_id = data_id), size = 5) + labs(x = "Tiempo (min)", y = y_axis_labels[parametro_local]) + theme_bw(base_size = 14) + theme(legend.position = "top")
          girafe(ggobj = gg, width_svg = 10, height_svg = 5, options = list(opts_hover(css="fill:orange;stroke:black;"), opts_selection(type="none"), opts_toolbar(saveaspng=FALSE)))
        })
        observeEvent(input[[paste0("grafico_", tolower(parametro_local), "_selected")]], {
          punto_sel_id <- input[[paste0("grafico_", tolower(parametro_local), "_selected")]]
          if (!is.null(punto_sel_id) && punto_sel_id != "") {
            texto_info <- textos_individuales[[parametro_local]][punto_sel_id]
            showModal(modalDialog(title = paste("Detalle del Punto para", parametro_local), p(strong(texto_info)), easyClose = TRUE, footer = modalButton("Cerrar")))
          }
        }, ignoreInit = TRUE)
      })
    }
  })
}
