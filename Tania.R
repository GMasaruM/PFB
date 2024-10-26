# Datos
Abs_2 <- c(0.248, 0.287, 0.38, 0.413, 0.566)
AR_2 <- c(0.391, 9.211, 12.488, 13.675, 19.095)
t_2 <- c(0, 30, 60, 90, 150)

# Crear el modelo de regresión lineal para AR 2% vs Tiempo
modelo_ar_2 <- lm(AR_2 ~ t_2)

# Graficar AR 2% vs Tiempo
plot(t_2, AR_2, main = "Gráfico de AR 2% vs Tiempo", xlab = "Tiempo", ylab = "AR 2%", pch = 16, col = "green")
abline(modelo_ar_2, col = "red") # Añadir la línea de regresión

# Obtener la ecuación de la recta para AR 2%
coeficientes_ar_2 <- coef(modelo_ar_2)
cat("La ecuación de la recta para AR 2% es: AR 2% =", coeficientes_ar_2[1], "+", coeficientes_ar_2[2], "* Tiempo\n")



# Datos
Abs_7 <- c(0.125, 0.892, 0.935, 1.023)
AR_7 <- c(0.173, 30.644, 32.156, 35.285)
t_7 <- c(0, 90, 120, 150)


# Graficar AR 7% vs Tiempo
plot(t_7, AR_7, main = "Gráfico de AR 7% vs Tiempo", xlab = "Tiempo", ylab = "AR 7%", pch = 16, col = "green")
abline(modelo_ar_7, col = "red") # Añadir la línea de regresión

# Obtener la ecuación de la recta para AR 7%
coeficientes_ar_7 <- coef(modelo_ar_7)
cat("La ecuación de la recta para AR 7% es: AR 7% =", coeficientes_ar_7[1], "+", coeficientes_ar_7[2], "* Tiempo\n")



# Datos
Abs_13 <- c(1.781, 0.584, 0.641, 0.785)
AR_13 <- c(3.107, 39.466, 43.469, 53.671)
t_13 <- c(0, 90, 120, 150)

# Graficar AR 13% vs Tiempo
plot(t_13, AR_13, main = "Gráfico de AR 13% vs Tiempo", xlab = "Tiempo", ylab = "AR 13%", pch = 16, col = "green")
abline(modelo_ar_13, col = "red") # Añadir la línea de regresión

# Obtener la ecuación de la recta para AR 13%
coeficientes_ar_13 <- coef(modelo_ar_13)
cat("La ecuación de la recta para AR 13% es: AR 13% =", coeficientes_ar_13[1], "+", coeficientes_ar_13[2], "* Tiempo\n")


# Obtener las pendientes (coeficiente)
pendiente_2 <- coef(modelo_ar_2)[2]
pendiente_7 <- coef(modelo_ar_7)[2]
pendiente_13 <- coef(modelo_ar_13)[2]





# Datos de las velocidades iniciales
velocidades_iniciales <- c(pendiente_2, pendiente_7, pendiente_13)

# Vector de sustratos
sustrato <- c(20, 70, 130)






# Asegúrate de tener el paquete 'renz' instalado y cargado
# install.packages("renz")  # Si aún no está instalado
library(renz)

# Datos de concentración de sustrato y velocidades iniciales (pendientes)
datos <- data.frame(
  sustrato = c(20, 70, 130),                       # Concentraciones de sustrato en g/L
  velocidad = c(pendiente_2, pendiente_7, pendiente_13)  # Velocidades iniciales en g/L*min
)

# Aplicar la función lb() para obtener Vmax y Km usando regresión ponderada
resultados_lb <- lb(datos, unit_S = 'g/L', unit_v = 'g/L*min', weighting = TRUE, plot = TRUE)

# Imprimir los resultados
cat("Vmax estimado:", resultados_lb$Vm, "\n")
cat("Km estimado:", resultados_lb$Km, "\n")


