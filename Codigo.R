
# Instala ggplot2 si aún no lo tienes
# install.packages("ggplot2")
# install.packages("readxl")

# Cargar librerías necesarias
library(ggplot2)
library(readxl)

datos <- read_excel("datos_absorbancia.xlsx", col_types = c("text", "numeric", "numeric", "numeric"))


# Convertir absorbancia a concentración de azúcares reductores usando una fórmula ficticia
# Aquí asumimos que la relación es [Azúcares Reductores] = m * Abs + c
# Ajustar los valores de 'm' y 'c' según la curva de calibración de tu experimento
m <- 1.7713
c <- -0.0478

# Calcular concentración de azúcares reductores para cada absorbancia
ar_e1 <- m * datos$`Absorbancia 1` + c
ar_e2 <- m * datos$`Absorbancia 2` + c
ar_e3 <- m * datos$`Absorbancia 3` + c

# Crear un dataframe con los datos

AR <- data.frame(
  Tiempo = datos$Tiempo,
  AR_Ensayo_1 = ar_e1,
  AR_Ensayo_2 = ar_e2,
  AR_Ensayo_3 = ar_e3
)

print(AR)


# Graficar los datos de azúcares reductores vs tiempo para las tres concentraciones de almidón
ggplot() +
  geom_point(data = AR, aes(x = Tiempo, y = AR_Ensayo_1), color = "blue", size = 3, label="2%") +
  geom_smooth(data = AR, aes(x = Tiempo, y = AR_Ensayo_1), method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  geom_point(data = AR, aes(x = Tiempo, y = AR_Ensayo_2), color = "red", size = 3, label="7%") +
  geom_smooth(data = AR, aes(x = Tiempo, y = AR_Ensayo_2), method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  geom_point(data = AR, aes(x = Tiempo, y = AR_Ensayo_3), color = "green", size = 3, label="13%") +
  geom_smooth(data = AR, aes(x = Tiempo, y = AR_Ensayo_3), method = "lm", se = FALSE, color = "green", linetype = "dashed") +
  labs(title = "Concentración de Azúcares Reductores vs Tiempo para diferentes concentraciones de almidón",
       x = "Tiempo (minutos)",
       y = "Concentración de Azúcares Reductores (g/L)") +
  theme_minimal()

# Calcular la pendiente usando un modelo lineal para cada concentración
modelo_2 <- lm(AR_Ensayo_1 ~ Tiempo, data = AR)
modelo_7 <- lm(AR_Ensayo_2 ~ Tiempo, data = AR)
modelo_13 <- lm(AR_Ensayo_3 ~ Tiempo, data = AR)

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

