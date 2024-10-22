# Instala ggplot2 si aún no lo tienes
# install.packages("ggplot2")

# Cargar librerías necesarias
library(ggplot2)

# Datos de tiempo (minutos)
tiempo <- c(0, 30, 60, 90, 120, 150, 180)  # Tiempos en minutos

# Absorbancia para distintas concentraciones de almidón (2%, 7%, 13%)
abs_2 <- c(0.248, 0.287, 0.380, 0.413, 2.093, 0.566, 0.833)
abs_7 <- c(0.125, 2.342, 0.914, 0.892, 0.935, 1.023, 0.817)
abs_13 <- c(1.781, 0.555, 0.646, 0.584, 0.641, 0.785, 0.562)

# Convertir absorbancia a concentración de azúcares reductores usando una fórmula ficticia
# Aquí asumimos que la relación es [Azúcares Reductores] = m * Abs + c
# Ajustar los valores de 'm' y 'c' según la curva de calibración de tu experimento
m <- 1.7713
c <- -0.0478

# Calcular concentración de azúcares reductores para cada absorbancia
azucares_reductores_2 <- m * abs_2 + c
azucares_reductores_7 <- m * abs_7 + c
azucares_reductores_13 <- m * abs_13 + c

# Crear un dataframe con los datos
datos_2 <- data.frame(tiempo, azucares_reductores_2)
datos_7 <- data.frame(tiempo, azucares_reductores_7)
datos_13 <- data.frame(tiempo, azucares_reductores_13)

# Graficar los datos de azúcares reductores vs tiempo para las tres concentraciones de almidón
ggplot() +
  geom_point(data = datos_2, aes(x = tiempo, y = azucares_reductores_2), color = "blue", size = 3, label="2%") +
  geom_smooth(data = datos_2, aes(x = tiempo, y = azucares_reductores_2), method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  geom_point(data = datos_7, aes(x = tiempo, y = azucares_reductores_7), color = "red", size = 3, label="7%") +
  geom_smooth(data = datos_7, aes(x = tiempo, y = azucares_reductores_7), method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  geom_point(data = datos_13, aes(x = tiempo, y = azucares_reductores_13), color = "green", size = 3, label="13%") +
  geom_smooth(data = datos_13, aes(x = tiempo, y = azucares_reductores_13), method = "lm", se = FALSE, color = "green", linetype = "dashed") +
  labs(title = "Concentración de Azúcares Reductores vs Tiempo para diferentes concentraciones de almidón",
       x = "Tiempo (minutos)",
       y = "Concentración de Azúcares Reductores (g/L)") +
  theme_minimal()

# Calcular la pendiente usando un modelo lineal para cada concentración
modelo_2 <- lm(azucares_reductores_2 ~ tiempo, data = datos_2)
modelo_7 <- lm(azucares_reductores_7 ~ tiempo, data = datos_7)
modelo_13 <- lm(azucares_reductores_13 ~ tiempo, data = datos_13)

# Mostrar los coeficientes del modelo (pendiente y ordenada al origen)
summary(modelo_2)
summary(modelo_7)
summary(modelo_13)

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


