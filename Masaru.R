
# Cosas de masaru

library(ggplot2)
library(readxl)

data <- read_excel("datos_absorbancia.xlsx")

# Función para filtrar los datos

filtrar_datos <- function(data, variable) {
  # Verificar que la variable está en el dataframe
  if (!variable %in% colnames(data)) {
    stop(paste("La variable", variable, "no se encuentra en el dataframe."))
  }
  
  # Seleccionar solo la columna "Tiempo" y la variable especificada
  datos_filtrados <- data[, c("Tiempo", variable)]  # Asegúrate de que "Tiempo" también esté presente
  
  # Eliminar filas con NA
  datos_filtrados <- na.omit(datos_filtrados)  # Eliminar filas con NA
  
  # Eliminar valores que son mayores que el siguiente
  i <- 1
  while (i < nrow(datos_filtrados)) {
    if (datos_filtrados[[variable]][i] > datos_filtrados[[variable]][i + 1]) {
      datos_filtrados <- datos_filtrados[-i, ]  # Eliminar el valor actual
    } else {
      i <- i + 1
    }
  }
  
  # Inicializar el índice
  i <- 1
  
  # Iterar sobre los valores
  while (i < nrow(datos_filtrados)) {
    # Compara el valor actual con el siguiente
    if (i < nrow(datos_filtrados) - 1 && 
        (datos_filtrados[[variable]][i + 1] - datos_filtrados[[variable]][i]) > 30) {
      # Si hay un salto mayor a 30, eliminar el siguiente valor
      datos_filtrados <- datos_filtrados[-(i + 1), ]
      i <- i + 1
      # No incrementar i, ya que hemos eliminado un elemento
    } else {
      i <- i + 1  # Solo incrementar si no se elimina
    }
  }
  
  return(datos_filtrados)  # Devolver el dataframe filtrado
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






# Configurar la disposición de los gráficos en 2x2
par(mfrow = c(2, 2), mar = c(2, 2, 2, 1))

# Generar las 4 gráficas de Michaelis-Menten
analizar_cinetica_MM(sustrato, v5)
analizar_cinetica_MM(sustrato, v6)
analizar_cinetica_MM(sustrato, v7)
analizar_cinetica_MM(sustrato, v8)

# Crear un data frame con los resultados simulados y experimentales
resultados_df <- data.frame(
  Grupo = 1:8,
  Km_simulado = NA,
  Vmax_simulado = NA,
  Km_experimental = c(3.2, 3.1, 2.4, 2.7, 2.4, 2.5, 3.0, 3.2),
  Vmax_experimental = c(155.7, 129.8, 208.2, 209.6, 169.6, 186.4, 180.3, 195.2)
)

# Generar los resultados simulados usando la función analizar_cinetica_MM()
for (i in 1:8) {
  if (i == 2) {
    resultados <- analizar_cinetica_MM(sustrato_p, get(paste0("v", i)))
  } else {
    resultados <- analizar_cinetica_MM(sustrato, get(paste0("v", i)))





filtrar_datos_multi <- function(data, variable1, variable2) {
  # Verificar que las variables están en el dataframe
  if (!variable1 %in% colnames(data)) {
    stop(paste("La variable", variable1, "no se encuentra en el dataframe."))

  }
  if (!variable2 %in% colnames(data)) {
    stop(paste("La variable", variable2, "no se encuentra en el dataframe."))
  }
  
  # Seleccionar solo la columna "Tiempo" y las variables especificadas
  datos_filtrados <- data[, c("Tiempo", variable1, variable2)]
  
  # Eliminar filas con NA
  datos_filtrados <- na.omit(datos_filtrados)
  
  # Eliminar valores que son mayores que el siguiente para variable1
  i <- 1
  while (i < nrow(datos_filtrados)) {
    if (datos_filtrados[[variable1]][i] > datos_filtrados[[variable1]][i + 1]) {
      datos_filtrados <- datos_filtrados[-i, ]
      i <- i + 1
    } else {
      i <- i + 1
    }
  }
  
  # Eliminar valores que son mayores que el siguiente para variable2
  i <- 1
  while (i < nrow(datos_filtrados)) {
    if (datos_filtrados[[variable2]][i] > datos_filtrados[[variable2]][i + 1]) {
      datos_filtrados <- datos_filtrados[-i, ]
      i <- i + 1
    } else {
      i <- i + 1
    }
  }
  
  # Inicializar el índice para la comparación de saltos
  i <- 1
  
  # Iterar sobre los valores de variable1
  while (i < nrow(datos_filtrados)) {
    if (i < nrow(datos_filtrados) - 1 && 
        (datos_filtrados[[variable1]][i + 1] - datos_filtrados[[variable1]][i]) > 30) {
      datos_filtrados <- datos_filtrados[-(i + 1), ]
      i <- i + 1
      # No incrementar i, ya que hemos eliminado un elemento
    } else {
      i <- i + 1
    }
  }
  
  # Reiniciar el índice para la variable2
  i <- 1
  
  # Iterar sobre los valores de variable2
  while (i < nrow(datos_filtrados)) {
    if (i < nrow(datos_filtrados) - 1 && 
        (datos_filtrados[[variable2]][i + 1] - datos_filtrados[[variable2]][i]) > 30) {
      datos_filtrados <- datos_filtrados[-(i + 1), ]
      i <- i + 1
      # No incrementar i, ya que hemos eliminado un elemento
    } else {
      i <- i + 1
    }
  }
  
  return(datos_filtrados)  # Devolver el dataframe filtrado
}

###

# Filtrar los datos 
datos_filtrados_eta2 <- filtrar_datos(data, "Abs Etanol 2%")
datos_filtrados_eta7 <- filtrar_datos(data, "Abs Etanol 7%")
datos_filtrados_eta13 <- filtrar_datos(data, "Abs Etanol 13%")

# Filtrar los datos para "AR 2%", "AR 7%", o "AR 13%"
datos_filtrados_ar2 <- filtrar_datos(data, "AR 2%")
datos_filtrados_ar7 <- filtrar_datos(data, "AR 7%")
datos_filtrados_ar13 <- filtrar_datos(data, "AR 13%")

Multi2 <- filtrar_datos_multi(data,"AR 2%","Abs Etanol 2%")
Multi7 <- filtrar_datos_multi(data,"AR 7%","Abs Etanol 7%")
Multi13 <- filtrar_datos_multi(data,"AR 13%","Abs Etanol 13%")

# obtenemos la produccion de etanol g/l

Etanol_2 <- 14.89 * Multi2$`Abs Etanol 2%` + 0.1563
Etanol_7 <- 14.89 * Multi7$`Abs Etanol 7%` + 0.1563
Etanol_13 <- 14.89 * Multi13$`Abs Etanol 13%` + 0.1563

modelo_eta2 <- lm(`Abs Etanol 2%` ~ Tiempo, data = Multi2)
modelo_eta7 <- lm(`Abs Etanol 7%` ~ Tiempo, data = Multi7)
modelo_eta13 <- lm(`Abs Etanol 13%` ~ Tiempo, data = Multi13)


# Calculamos el "Ethanol yield"



Ethanol_yield2 <- (Etanol_2 / Multi2$`AR 2%`)

Ethanol_yield7 <-(Etanol_7 / Multi7$`AR 7%`)

Ethanol_yield13 <- (Etanol_13 / Multi13$`AR 13%`) 

# Calculamos el "Ethanol productivity"


Ethanol_productivity2 <- (Etanol_2 / Multi2$Tiempo)

Ethanol_productivity7 <- (Etanol_7 / Multi7$Tiempo)

Ethanol_productivity13 <- (Etanol_13 / Multi13$Tiempo) 


