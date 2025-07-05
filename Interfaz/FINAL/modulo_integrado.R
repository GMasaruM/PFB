# modulo_integrado.R
integradoUI <- function(id) {ns <- NS(id); fluidPage(titlePanel("Análisis Integrado de Parámetros"), mainPanel(width = 12, helpText("Este gráfico combina los resultados. Presiona 'Calcular' en cada pestaña para ver los datos."), hr(), plotlyOutput(ns("grafico_combinado"), height = "600px")))}
integradoServer <- function(id, datos_etanol_r, datos_glucosa_r, datos_amilasa_r) {
  moduleServer(id, function(input, output, session) {
    datos_combinados <- reactive({
      req(datos_etanol_r(), datos_glucosa_r(), datos_amilasa_r())
      df_etanol <- datos_etanol_r() %>% mutate(Parametro = "Etanol")
      df_glucosa <- datos_glucosa_r() %>% mutate(Parametro = "Glucosa")
      df_amilasa <- datos_amilasa_r() %>% mutate(Parametro = "Amilasa")
      bind_rows(df_etanol, df_glucosa, df_amilasa)
    })
    output$grafico_combinado <- renderPlotly({
      req(datos_combinados())
      p <- ggplot(datos_combinados(), aes(x = Tiempo_fermentacion, y = Valor, color = Grupo, group = Grupo)) +
        geom_line(size = 1) + geom_point(size = 2.5) +
        facet_wrap(~ Parametro, scales = "free_y", ncol = 1) + 
        labs(title = "Evolución de Parámetros por Grupo", x = "Tiempo (min)", y = "Valor (Unidades Correspondientes)") +
        theme_bw(base_size = 14) + theme(strip.text = element_text(face = "bold"), legend.position = "top")
      ggplotly(p) %>% config(displaylogo = FALSE)
    })
  })
}