
## Librerias y datos

# install.packages("ggplot2")
# install.packages("readxl")

# Cargar librerías necesarias
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
###


## Parte de Azucares reductores

# Convertir absorbancia a concentración de azúcares reductores usando una fórmula ficticia
# Aquí asumimos que la relación es [Azúcares Reductores] = m * Abs + c
# Ajustar los valores de 'm' y 'c' según la curva de calibración de tu experimento
m <- 1.7713
c <- -0.0478

# Calcular concentración de azúcares reductores para cada absorbancia
ar_e1 <- m * data$`Absorbancia 1` + c
ar_e2 <- m * data$`Absorbancia 2` + c
ar_e3 <- m * data$`Absorbancia 3` + c

# Crear un dataframe con los datos

AR <- data.frame(
  Tiempo = data$Tiempo,
  AR_Ensayo_1 = ar_e1,
  AR_Ensayo_2 = ar_e2,
  AR_Ensayo_3 = ar_e3
)

print(AR)

# Filtrar los datos para "AR 2%", "AR 7%", o "AR 13%"
datos_filtrados_ar2 <- filtrar_datos(data, "AR 2%")
datos_filtrados_ar7 <- filtrar_datos(data, "AR 7%")
datos_filtrados_ar13 <- filtrar_datos(data, "AR 13%")



# Graficar los datos de azúcares reductores vs tiempo para las tres concentraciones de almidón
#ggplot() +
 # geom_point(data = AR, aes(x = Tiempo, y = AR_Ensayo_1), color = "blue", size = 3, label="2%") +
  #geom_smooth(data = AR, aes(x = Tiempo, y = AR_Ensayo_1), method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  #geom_point(data = AR, aes(x = Tiempo, y = AR_Ensayo_2), color = "red", size = 3, label="7%") +
  #geom_smooth(data = AR, aes(x = Tiempo, y = AR_Ensayo_2), method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  #geom_point(data = AR, aes(x = Tiempo, y = AR_Ensayo_3), color = "green", size = 3, label="13%") +
  #geom_smooth(data = AR, aes(x = Tiempo, y = AR_Ensayo_3), method = "lm", se = FALSE, color = "green", linetype = "dashed") +
  #labs(title = "Concentración de Azúcares Reductores vs Tiempo para diferentes concentraciones de almidón",
  #     x = "Tiempo (minutos)",
  #    y = "Concentración de Azúcares Reductores (g/L)") +
  #theme_minimal()


# Calcular la pendiente usando un modelo lineal para cada concentración
#modelo_2 <- lm(AR_Ensayo_1 ~ Tiempo, data = AR)
#modelo_7 <- lm(AR_Ensayo_2 ~ Tiempo, data = AR)
#modelo_13 <- lm(AR_Ensayo_3 ~ Tiempo, data = AR)

modelo_2 <- lm(`AR 2%` ~ Tiempo, data = datos_filtrados_ar2)
modelo_7 <- lm(`AR 7%` ~ Tiempo, data = datos_filtrados_ar7)
modelo_13 <- lm(`AR 13%` ~ Tiempo, data = datos_filtrados_ar13)


# 2 %
plot(datos_filtrados_ar2$Tiempo, datos_filtrados_ar2$`AR 2%`, 
     main = "Modelo Lineal de AR 2% vs Tiempo (sin outliers)", 
     xlab = "Tiempo", 
     ylab = "AR 2%", 
     pch = 19)

# Agregar la línea de tendencia
abline(modelo_2, col = "blue")

# Agregar la ecuación de la recta al gráfico
eq_sin_outliers <- paste("y =", round(coef(modelo_2)[2], 2), "x +", round(coef(modelo_2)[1], 2))
legend("topleft", legend = eq_sin_outliers, bty = "n", col = "blue", lwd = 2)

# Calcular R^2
r_squared <- summary(modelo_2)$r.squared
r_squared_text <- paste("R² =", round(r_squared, 3))

# Agregar solo R² al gráfico
legend("bottomright", legend = r_squared_text, bty = "n", col = "blue", lwd = 2, cex = 0.8)

# 7 %
# Graficar los datos sin outliers
plot(datos_filtrados_ar7$Tiempo, datos_filtrados_ar7$`AR 7%`, 
     main = "Modelo Lineal de AR 7% vs Tiempo (sin outliers)", 
     xlab = "Tiempo", 
     ylab = "AR 7%", 
     pch = 19)

# Agregar la línea de tendencia
abline(modelo_7, col = "blue")

# Agregar la ecuación de la recta al gráfico
eq_filtrado <- paste("y =", round(coef(modelo_7)[2], 2), "x +", round(coef(modelo_7)[1], 2))
legend("topleft", legend = eq_filtrado, bty = "n", col = "blue", lwd = 2)

# Calcular R^2
r_squared <- summary(modelo_7)$r.squared
r_squared_text <- paste("R² =", round(r_squared, 3))

# Agregar solo R² al gráfico
legend("bottomright", legend = r_squared_text, bty = "n", col = "blue", lwd = 2, cex = 0.8)

# 13 %

# Graficar los datos filtrados
plot(datos_filtrados_ar13$Tiempo, datos_filtrados_ar13$`AR 13%`, 
     main = "Modelo Lineal de AR 13% vs Tiempo (sin outliers)", 
     xlab = "Tiempo", 
     ylab = "AR 13%", 
     pch = 19)

# Agregar la línea de tendencia
abline(modelo_13, col = "blue")

# Crear la ecuación de la recta
eq_filtrado <- paste("y =", round(coef(modelo_13)[2], 2), "x +", round(coef(modelo_13)[1], 2))

# Agregar la ecuación al gráfico
legend("topleft", legend = eq_filtrado, bty = "n", col = "blue", lwd = 2)

# Calcular R^2
r_squared <- summary(modelo_13)$r.squared
r_squared_text <- paste("R² =", round(r_squared, 3))

# Agregar solo R² al gráfico
legend("bottomright", legend = r_squared_text, bty = "n", col = "blue", lwd = 2, cex = 0.8)

# Mostrar los coeficientes del modelo (pendiente y ordenada al origen)
summary(modelo_2)
summary(modelo_7)
summary(modelo_13)

# Agregar el tercer conjunto de datos
ggplot() +
  geom_point(data = datos_filtrados_ar2, aes(x = Tiempo, y = `AR 2%`), color = "blue", size = 3) +
  geom_smooth(data = datos_filtrados_ar2, aes(x = Tiempo, y = `AR 2%`), method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  geom_text(data = datos_filtrados_ar2, aes(x = Tiempo, y = `AR 2%`, label = "2%"), vjust = -1, color = "blue") +
  
  geom_point(data = datos_filtrados_ar7, aes(x = Tiempo, y = `AR 7%`), color = "red", size = 3) +
  geom_smooth(data = datos_filtrados_ar7, aes(x = Tiempo, y = `AR 7%`), method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  geom_text(data = datos_filtrados_ar7, aes(x = Tiempo, y = `AR 7%`, label = "7%"), vjust = -1, color = "red") +
  
  geom_point(data = datos_filtrados_ar13, aes(x = Tiempo, y = `AR 13%`), color = "green", size = 3) +
  geom_smooth(data = datos_filtrados_ar13, aes(x = Tiempo, y = `AR 13%`), method = "lm", se = FALSE, color = "green", linetype = "dashed") +
  geom_text(data = datos_filtrados_ar13, aes(x = Tiempo, y = `AR 13%`, label = "13%"), vjust = -1, color = "green") +
  
  labs(title = "Concentración de Azúcares Reductores vs Tiempo",
       x = "Tiempo (minutos)",
       y = "Concentración de Azúcares Reductores (g/L)") +
  theme_minimal()




## Parte para la actividad enzimatica

# Extraer la pendiente para cada concentración
pendiente_2 <- coef(modelo_2)[2]
pendiente_7 <- coef(modelo_7)[2]
pendiente_13 <- coef(modelo_13)[2]

# Parámetros adicionales para la actividad enzimática
volumen_ensayo <- 1  # Volumen del ensayo en mL (ajustar si es diferente)
coef_extincion_molar <- 6220  # Coeficiente de extinción molar para NADH (M^-1 cm^-1)
distancia_celda <- 1  # Distancia de la celda del espectrofotómetro (cm)

# Calcular la actividad enzimática (U.I.) para cada concentración
actividad_enzimatica_2 <- (pendiente_2 * volumen_ensayo) / (coef_extincion_molar * distancia_celda)
actividad_enzimatica_7 <- (pendiente_7 * volumen_ensayo) / (coef_extincion_molar * distancia_celda)
actividad_enzimatica_13 <- (pendiente_13 * volumen_ensayo) / (coef_extincion_molar * distancia_celda)

# Mostrar el valor de la actividad enzimática
cat("Actividad enzimática para 2%: ", actividad_enzimatica_2, "U.I.\n")
cat("Actividad enzimática para 7%: ", actividad_enzimatica_7, "U.I.\n")
cat("Actividad enzimática para 13%: ", actividad_enzimatica_13, "U.I.\n")

### Agregar modelo Michaelis-Menten

# Supongamos datos de concentración de sustrato [S] (en mM)
concentracion_sustrato <- c(0.1, 0.5, 1, 5, 10, 20, 50, 100, 200, 500)

# Datos simulados de velocidad de reacción (en µM/min)
velocidad <- c(0.5, 2.5, 4.8, 23, 37, 50, 57, 60, 61, 62)

# Ajustar el modelo de Michaelis-Menten usando nls
modelo_mm <- nls(velocidad ~ Vmax * concentracion_sustrato / (Km + concentracion_sustrato), 
                 start = list(Vmax = 65, Km = 10))

# Mostrar los resultados del modelo
summary(modelo_mm)

# Extraer Vmax y Km
Vmax <- coef(modelo_mm)["Vmax"]
Km <- coef(modelo_mm)["Km"]

cat("Vmax:", Vmax, "\n")
cat("Km:", Km, "\n")

# Graficar la curva de Michaelis-Menten
ggplot(data = data.frame(concentracion_sustrato, velocidad), aes(x = concentracion_sustrato, y = velocidad)) +
  geom_point(color = "blue", size = 3) +
  stat_smooth(method = "nls", formula = y ~ Vmax * x / (Km + x),
              method.args = list(start = list(Vmax = 65, Km = 10)), se = FALSE, color = "red") +
  labs(title = "Curva de Michaelis-Menten",
       x = "[S] (mM)",
       y = "Velocidad de reacción (µM/min)") +
  theme_minimal()

cat("Actividad enzimática para 13%: ", actividad_enzimatica_13, "U.I.\n")

