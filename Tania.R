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




# Calcular los valores de 1/V y 1/[S] para el gráfico de Lineweaver-Burk
inv_velocidades <- 1 / velocidades_iniciales
inv_sustrato <- 1 / sustrato

# Ordenar los datos en función de los valores de 1/[S]
orden <- order(inv_sustrato)
inv_sustrato <- inv_sustrato[orden]
inv_velocidades <- inv_velocidades[orden]

# Realizar la regresión lineal de 1/V vs 1/[S]
modelo_lineweaver_burk <- lm(inv_velocidades ~ inv_sustrato)

# Obtener la pendiente (Km/Vmax) y la ordenada al origen (1/Vmax)
pendiente <- coef(modelo_lineweaver_burk)[2] # Km/Vmax
ordenada_origen <- coef(modelo_lineweaver_burk)[1] # 1/Vmax

# Calcular Vmax y Km a partir de la pendiente y la ordenada al origen
Vmax <- 1 / ordenada_origen
Km <- pendiente * Vmax

# Mostrar los resultados con las unidades
cat("Vmax:", Vmax, "g/L*min\n")
cat("Km:", Km, "g/L\n")

# Generar la gráfica de Lineweaver-Burk
plot(inv_sustrato, inv_velocidades, 
     xlab = "1/[S] (1/g/L)", 
     ylab = "1/V (min/L/g)", 
     main = "Gráfica de Lineweaver-Burk", 
     pch = 19, 
     col = "blue")

# Agregar la línea de regresión al gráfico
abline(modelo_lineweaver_burk, col = "red")

# Agregar leyenda con los valores de Vmax y Km
legend("topright", legend = c(paste("Vmax:", round(Vmax, 4), "g/L*min"), paste("Km:", round(Km, 4), "g/L")),
       col = c("red", "red"), lty = 1, bty = "n")








# Asegúrate de tener el paquete 'renz' instalado y cargado
# install.packages("renz")  # Si aún no está instalado
library(renz)

# Datos de concentración de sustrato y velocidades iniciales (pendientes)
datos <- data.frame(
  sustrato = c(20, 70, 130),                       # Concentraciones de sustrato en g/L
  velocidad = c(pendiente_2, pendiente_7, pendiente_13)  # Velocidades iniciales en g/L*min
)

# Aplicar la función lb() para obtener Vmax y Km usando regresión ponderada
resultados_lb <- lb(datos, unit_S = 'g/L', unit_v = 'g/L*min',  plot = TRUE)

# Imprimir los resultados
cat("Vmax estimado:", resultados_lb$Vm, "\n")
cat("Km estimado:", resultados_lb$Km, "\n")


