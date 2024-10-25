
# Cosas de masaru

library(readxl)

data <- read_excel("Libro1.xlsx")

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

# Cargar los datos (asegúrate de que los datos están en un dataframe llamado 'data')
# data <- read.csv("tu_archivo.csv")
data <- read_excel("Libro1.xlsx")


# Filtrar los datos para "AR 2%", "AR 7%", o "AR 13%"
datos_filtrados_ar2 <- filtrar_datos(data, "AR 2%")
datos_filtrados_ar7 <- filtrar_datos(data, "AR 7%")
datos_filtrados_ar13 <- filtrar_datos(data, "AR 13%")

## AR 2%

# Graficar los datos sin outliers
plot(datos_filtrados_ar2$Tiempo, datos_filtrados_ar2$`AR 2%`, 
     main = "Modelo Lineal de AR 2% vs Tiempo (sin outliers)", 
     xlab = "Tiempo", 
     ylab = "AR 2%", 
     pch = 19)

# Ajustar el modelo lineal
modelo_sin_outliers <- lm(`AR 2%` ~ Tiempo, data = datos_filtrados_ar2)

# Agregar la línea de tendencia
abline(modelo_sin_outliers, col = "blue")

# Agregar la ecuación de la recta al gráfico
eq_sin_outliers <- paste("y =", round(coef(modelo_sin_outliers)[2], 2), "x +", round(coef(modelo_sin_outliers)[1], 2))
legend("topleft", legend = eq_sin_outliers, bty = "n", col = "blue", lwd = 2)

## AR 7%

# Graficar los datos sin outliers
plot(datos_filtrados_ar7$Tiempo, datos_filtrados_ar7$`AR 7%`, 
     main = "Modelo Lineal de AR 7% vs Tiempo (sin outliers)", 
     xlab = "Tiempo", 
     ylab = "AR 7%", 
     pch = 19)

# Ajustar el modelo lineal
modelo_filtrado <- lm(`AR 7%` ~ Tiempo, data = datos_filtrados_ar7)

# Agregar la línea de tendencia
abline(modelo_filtrado, col = "blue")

# Agregar la ecuación de la recta al gráfico
eq_filtrado <- paste("y =", round(coef(modelo_filtrado)[2], 2), "x +", round(coef(modelo_filtrado)[1], 2))
legend("topleft", legend = eq_filtrado, bty = "n", col = "blue", lwd = 2)


## AR 13%


# Cargar los datos (asegúrate de que los datos están en un dataframe llamado 'data')
# data <- read.csv("tu_archivo.csv")


# Ajustar el modelo lineal a los datos filtrados
modelo_filtrado <- lm(`AR 13%` ~ Tiempo, data = datos_filtrados_ar13)

# Graficar los datos filtrados
plot(datos_filtrados_ar13$Tiempo, datos_filtrados_ar13$`AR 13%`, 
     main = "Modelo Lineal de AR 13% vs Tiempo (sin outliers)", 
     xlab = "Tiempo", 
     ylab = "AR 13%", 
     pch = 19)

# Agregar la línea de tendencia
abline(modelo_filtrado, col = "blue")

# Crear la ecuación de la recta
eq_filtrado <- paste("y =", round(coef(modelo_filtrado)[2], 2), "x +", round(coef(modelo_filtrado)[1], 2))

# Agregar la ecuación al gráfico
legend("topleft", legend = eq_filtrado, bty = "n", col = "blue", lwd = 2)
