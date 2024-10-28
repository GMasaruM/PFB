
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


