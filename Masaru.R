
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
  resultados_MM <- dir.MM(datos, unit_S = 'mM', unit_v = 'mM/min') 
  
  # Verificar si se obtuvieron resultados válidos
  if (is.null(resultados_MM$parameters)) {
    stop("No se pudieron calcular Vmax y Km. Verifica los datos de entrada.")
  }
  
  # Devolver los resultados en una lista
  return(resultados_MM$parameters) 
}

# Validación

# Vectores de sustrato y velocidades
sustrato <- c(0.05, 0.10, 0.25, 0.50, 1.00, 2.50, 5.00, 8.00, 20.0, 30.0)
sustrato_p <- c(0.05, 0.10, 0.25, 0.50, 1.00, 2.50, 5.00, 20.0, 30.0)
v1 <- c(2.3, 5.5, 13.4, 24.7, 40.9, 62.3, 94.3, 105.0, 133.0, 144.0)
v2 <- c(1.3, 3.3, 11.8, 22.8, 35.2, 39.9, 73.5, 112.0, 120.0)
v3 <- c(4.0, 8.0, 20.0, 35.0, 60.0, 110.0, 138.0, 154.0, 179.0, 200.0)
v4 <- c(4.0, 7.0, 20.0, 35.0, 56.0, 104.0, 138.0, 150.0, 179.0, 200.0)
v5 <- c(4.0, 7.0, 16.0, 32.0, 50.0, 90.0, 115.0, 119.0, 142.0, 166.0)
v6 <- c(3.0, 6.0, 17.0, 31.0, 48.0, 101.0, 121.0, 139.0, 152.0, 181.0)
v7 <- c(1.8, 5.2, 15.0, 28.3, 51.0, 75.4, 112.7, 126.1, 154.9, 168.8)
v8 <- c(3.0, 5.2, 14.4, 30.3, 49.0, 86.3, 112.6, 136.2, 170.0, 177.7)



analizar_cinetica_MM(sustrato,v1)

analizar_cinetica_MM(sustrato_p,v2)

analizar_cinetica_MM(sustrato,v3)

analizar_cinetica_MM(sustrato,v4)
analizar_cinetica_MM(sustrato,v5)
analizar_cinetica_MM(sustrato,v6)
analizar_cinetica_MM(sustrato,v7)
analizar_cinetica_MM(sustrato,v8)

