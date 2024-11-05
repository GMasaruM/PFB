
## Librerias 

# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("tidyr")


# Cargar librerías necesarias
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(renz)




## Data
data <- read_excel("datos_absorbancia.xlsx")



## Funciones

# Definir la función para el análisis de la curva de calibración
calibracion_glucosa_etanol <- function(concentraciones, absorbancias_calibracion, absorbancias_muestras) {
  
  # Verificar que los vectores de concentración y absorbancia de calibración tengan la misma longitud
  if (length(concentraciones) != length(absorbancias_calibracion)) {
    stop("Los vectores de 'concentraciones' y 'absorbancias_calibracion' deben tener la misma longitud.")
  }
  
  # Crear el gráfico de la relación entre concentración y absorbancia de calibración
  plot(concentraciones, absorbancias_calibracion,
       main = "Curva de Calibración",
       xlab = "Concentración (mg/L)",  # Especificar la unidad de concentración
       ylab = "Absorbancia",
       pch = 19, col = "darkslategray", # Cambiar a color gris oscuro
       cex = 1.2) # Ajustar tamaño de los puntos
  
  # Ajustar un modelo de regresión lineal
  modelo <- lm(absorbancias_calibracion ~ concentraciones)
  
  # Añadir la línea de regresión al gráfico
  abline(modelo, col = "steelblue", lwd = 2) # Cambiar a azul acero
  
  # Obtener los coeficientes de la regresión y mostrar la fórmula en formato matemático
  coeficientes <- coef(modelo)
  eq_texto <- bquote(y == .(round(coeficientes[1], 3)) + .(round(coeficientes[2], 3)) * x)
  text(x = max(concentraciones) * 0.6, y = max(absorbancias_calibracion) * 0.9, eq_texto, col = "steelblue", cex = 0.9)
  
  # Mostrar los parámetros ajustados del modelo
  print(summary(modelo))
  
  # Generar gráficos de diagnóstico
  par(mfrow = c(2, 2)) # Dividir la ventana gráfica en 2x2
  plot(modelo)
  
  # Calcular concentraciones desconocidas a partir de absorbancias de las muestras
  concentraciones_predichas <- (absorbancias_muestras - coeficientes[1]) / coeficientes[2]
  
  # Volver a la configuración de gráfico original
  par(mfrow = c(1, 1))
  
  # Agregar puntos de las muestras desconocidas al gráfico de calibración
  points(concentraciones_predichas, absorbancias_muestras, pch = 17, col = "darkolivegreen", cex = 1.2) 
  
  # Devolver las concentraciones predichas
  return(concentraciones_predichas)
}





# Definir la función para calcular las velocidades iniciales y graficar cada ensayo
calcular_velocidades_iniciales_por_ensayo <- function(df) {
  # Detectar automáticamente los nombres de las columnas de concentración
  columnas_concentracion <- names(df)[-1]  # Excluir la columna de Tiempo
  
  # Convertir los datos a formato largo
  data_long <- df %>%
    pivot_longer(cols = all_of(columnas_concentracion), 
                 names_to = "Ensayo", values_to = "Concentracion") %>%
    mutate(Ensayo = factor(Ensayo, levels = columnas_concentracion))
  
  # Crear un data frame para almacenar las velocidades iniciales
  velocidades_iniciales <- data.frame(Ensayo = character(), Velocidad_Inicial = numeric())
  
  # Gráfico combinado de todos los ensayos con líneas de regresión
  p_combinado <- ggplot(data_long, aes(x = Tiempo, y = Concentracion, color = Ensayo)) +
    geom_point() +
    labs(title = "Concentración vs Tiempo para los Ensayos",
         x = "Tiempo (min)",
         y = "Concentración (mg/L)") +
    theme_minimal() +
    scale_color_brewer(palette = "Set2") 
  
  # Graficar y calcular velocidades iniciales para cada ensayo
  for (ensayo in unique(data_long$Ensayo)) {
    ensayo_data <- data_long %>%
      filter(Ensayo == ensayo, !is.na(Concentracion))
    
    if (nrow(ensayo_data) > 1) {  # Asegurarse de que haya suficientes puntos para la regresión
      modelo <- lm(Concentracion ~ Tiempo, data = ensayo_data)
      velocidad_inicial <- coef(modelo)[2]
      
      # Almacenar la velocidad inicial
      velocidades_iniciales <- rbind(velocidades_iniciales, data.frame(Ensayo = ensayo, Velocidad_Inicial = velocidad_inicial))
      
      # Crear los datos de predicción con los mismos puntos de tiempo para asegurar compatibilidad
      prediccion_data <- data.frame(Tiempo = ensayo_data$Tiempo, Concentracion = predict(modelo, newdata = ensayo_data))
      
      # Gráfico del ensayo individual
      p_individual <- ggplot(ensayo_data, aes(x = Tiempo, y = Concentracion)) +
        geom_point(color = "steelblue") +
        geom_line(data = prediccion_data, aes(y = Concentracion), linetype = "dotted", color = "darkblue") +
        labs(title = paste("Concentración vs Tiempo -", ensayo),
             x = "Tiempo (min)",
             y = "Concentración (mg/L)") +
        annotate("text", x = max(df$Tiempo) * 0.6, 
                 y = max(ensayo_data$Concentracion, na.rm = TRUE) * 1.1, 
                 label = paste0("y = ", round(coef(modelo)[1], 3), " + ", round(velocidad_inicial, 3), " * x"), 
                 color = "black", size = 4, hjust = 0) +  # Ajustar posición de la ecuación
        theme_minimal()
      
      print(p_individual)  # Mostrar el gráfico individual
      
      # Añadir la línea de regresión al gráfico combinado
      p_combinado <- p_combinado + 
        geom_line(data = prediccion_data, aes(y = Concentracion), linetype = "dotted", color = "grey50")
    }
  }
  
  # Mostrar el gráfico combinado
  print(p_combinado)
  
  # Devolver las velocidades iniciales
  return(velocidades_iniciales)
}








# Definir la función para el análisis de cinética de Michaelis-Menten
analizar_cinetica_MM <- function(sustrato, velocidades) { 
  # Verificar que ambos vectores tengan la misma longitud
  if (length(sustrato) != length(velocidades)) {
    stop("Los vectores de 'sustrato' y 'velocidades' deben tener la misma longitud.")
  }
  
  # Crear un data frame a partir de los vectores de sustrato y velocidades
  data_velocidades <- data.frame(Concentracion = sustrato, Velocidad_Inicial = velocidades)
  
  # Verificar que el data frame contiene las columnas necesarias
  if (!all(c("Concentracion", "Velocidad_Inicial") %in% colnames(data_velocidades))) { 
    stop("El data frame debe contener las columnas 'Concentracion' y 'Velocidad_Inicial'.") 
  }
  
  # Comprobar si hay valores NA y lanzar advertencia si existen
  if (any(is.na(data_velocidades))) {
    warning("Se encontraron valores NA en los datos. Los cálculos se realizarán con los valores disponibles.")
  }
  
  # Convertir el data frame a formato compatible con renz
  datos <- data_velocidades %>% 
    rename(S = Concentracion, v = Velocidad_Inicial) 
  
  # Calcular los parámetros cinéticos usando Michaelis-Menten
  resultados_MM <- dir.MM(datos, unit_S = 'mg/L', unit_v = 'mg/L*min') 
  
  # Verificar si se obtuvieron resultados válidos
  if (is.null(resultados_MM$parameters)) {
    stop("No se pudieron calcular Vmax y Km. Verifica los datos de entrada.")
  }
  
  # Devolver los resultados en una lista
  return(resultados_MM$parameters) 
}







# Generar datos de ejemplo para la curva de calibración
set.seed(123) # Para reproducibilidad
concentraciones <- c(0, 50, 100, 150, 200, 250) # Concentraciones en mg/L

# Generar absorbancias de calibración como un vector
absorbancias_calibracion <- c(0.02, 0.12, 0.21, 0.31, 0.4, 0.52) # Absorbancias correspondientes

# Generar absorbancias de muestras desconocidas como un vector
absorbancias_muestras <- c(0.45, 0.65, 0.35, 0.75)

# Ejecutar la función con los datos de prueba
concentraciones_muestras<- calibracion_glucosa_etanol(concentraciones, absorbancias_calibracion, absorbancias_muestras)

# Ver las concentraciones predichas
print(concentraciones_muestras)



# Datos de entrada para probar la función
Tiempo <- c(0, 30, 60, 90, 120, 150)  # Tiempo en minutos
concentraciones_2 <- c(0.391, 9.211, 12.488, 13.675, NA, 19.095)  # Concentraciones ensayo 1
concentraciones_7 <- c(0.173, NA, NA, 30.644, 32.156, 35.285)     # Concentraciones ensayo 2
concentraciones_13 <- c(3.107, NA, NA, 39.466, 43.469, 53.671)    # Concentraciones ensayo 3

df <- data.frame(
  Tiempo = Tiempo,
  Concentraciones_2 = concentraciones_2,
  Concentraciones_7 = concentraciones_7,
  Concentraciones_13 = concentraciones_13
)

# Llamada a la función
velocidades <- calcular_velocidades_iniciales_por_ensayo(df)

# Ver el resultado de las velocidades iniciales
print(velocidades)



Sustrato <- c(20, 70, 130)

# Llamar a la función con los vectores sustrato y velocidades
resultados <- analizar_cinetica_MM(Sustrato, velocidades$Velocidad_Inicial)

# Mostrar los resultados
print(resultados)


sE.progress(So = 20, time = 150, Km = 82.202, Vm = 0.544, unit_S = 'mg', unit_t = 'min')

data <- sE.progress(So = 20, time = 150, Km = 82.202, Vm = 0.544, unit_S = 'mg', unit_t = 'min', plot = FALSE)
fE.progress(data)


# Nuestra salvacion 
nuevo_dataframe <- data[, 1:2]
fE.progress(nuevo_dataframe)




# Validación

# Vectores de sustrato y velocidades
sustrato <- c(0.010, 0.015, 0.020, 0.025, 0.030)
v1 <- c(0.00002134, 0.00002997, 0.00003981, 0.00005494, 0.00005996)
v2 <- c(0.0000764, 0.0000995, 0.0001231, 0.0001452, 0.0001769)
v3 <- c(0.0000858, 0.0000920, 0.0000955, 0.0000978, 0.0000980)


