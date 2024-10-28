
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

# Mezcla de las tres graficas
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

### Agregar modelo Lineweaver - Burk

sustrato <- data$Sustrato

# Datos de las velocidades iniciales
velocidades_iniciales <- c(pendiente_2, pendiente_7, pendiente_13)

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

sustrato_limpio <- na.omit(data$Sustrato)



# Datos de concentración de sustrato y velocidades iniciales (pendientes)
datos <- data.frame(
  sustrato = sustrato_limpio,                       # Concentraciones de sustrato en g/L
  velocidad = c(pendiente_2, pendiente_7, pendiente_13)  # Velocidades iniciales en g/L*min
)




# Lineweaver - Burk
# Aplicar la función lb() para obtener Vmax y Km usando regresión ponderada
resultados_lb <- lb(datos, unit_S = 'g/L', unit_v = 'g/L*min')

# Imprimir los resultados
cat("Vmax estimado:", resultados_lb$Vm, "\n")
cat("Km estimado:", resultados_lb$Km, "\n")





# Eadie-Hofstee
# Aplicar la función eh() para obtener Vmax y Km usando regresión ponderada
resultados_eh <- eh(datos, unit_S = 'g/L', unit_v = 'g/L*min', plot = FALSE) # Generar sin gráfica automática

# Calcular los valores de v y v/[S]
v <- datos$v
v_over_S <- v / datos$s

# Calcular los límites automáticos basados en los valores mínimos y máximos de v/[S] y v
xlim_vals <- range(v_over_S) * c(0.9, 1.1)  # Margen del 10% para el eje x
ylim_vals <- range(v) * c(0.9, 1.1)  # Margen del 10% para el eje y

# Graficar manualmente los puntos y la línea de regresión
plot(v_over_S, v, 
     xlab = "v/[S] (g/L)", 
     ylab = "v (g/L*min)", 
     pch = 19, col = "blue", xlim = xlim_vals, ylim = ylim_vals)

# Ajustar un modelo de regresión lineal
modelo_eh <- lm(v ~ v_over_S)

# Extraer los coeficientes
intercepto <- coef(modelo_eh)[1]
pendiente <- coef(modelo_eh)[2]

# Agregar la línea de regresión al gráfico
abline(modelo_eh, col = "red")

# Agregar título usando mtext()
mtext("Gráfica de Eadie-Hofstee", side = 3, line = 3, cex = 1.5, col = "black")

# Imprimir Vmax y Km en la parte superior del gráfico
mtext(paste("Km:", round(resultados_eh$Km, 2)), side = 3, line = 1, adj = 0, cex = 1.2, col = "black")
mtext(paste("Vm:", round(resultados_eh$Vm, 2)), side = 3, line = 1, adj = 1, cex = 1.2, col = "black")

# Mostrar la ecuación de la recta en el gráfico
eq_text <- paste("y =", round(intercepto, 2), "+", round(pendiente, 2), "* x")
text(x = mean(v_over_S), y = max(v) * 0.9, labels = eq_text, col = "black")

# Mostrar los resultados calculados
cat("Ecuación de la recta: ", eq_text, "\n")
cat("Vmax estimado:", resultados_eh$Vm, "g/L*min\n")
cat("Km estimado:", resultados_eh$Km, "g/L\n")



# Hanes-Woolf
resultados_hw <- hw(datos, unit_S = 'g/L', unit_v = 'g/L*min')

# Imprimir los resultados
cat("Vmax estimado:", resultados_hw$Vm, "\n")
cat("Km estimado:", resultados_hw$Km, "\n")



resultados_MM <- dir.MM(datos, unit_S = 'g/L', unit_v = 'g/L*min')

cat("Vmax estimado:", resultados_MM$Vm, "\n")
cat("Km estimado:", resultados_MM$Km, "\n")


# Etanol

# Filtrar los datos 
datos_filtrados_eta2 <- filtrar_datos(data, "Abs Etanol 2%")
datos_filtrados_eta7 <- filtrar_datos(data, "Abs Etanol 7%")
datos_filtrados_eta13 <- filtrar_datos(data, "Abs Etanol 13%")

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



# Calcular la correlación entre AR y Etanol 
cor_2 <- cor(datos_filtrados_ar2$`AR 2%`, Etanol_2, use = "complete.obs")
cor_7 <- cor(datos_filtrados_ar7$`AR 7%`, Etanol_7, use = "complete.obs")
cor_13 <- cor(datos_filtrados_ar13$`AR 13%`,Etanol_13, use = "complete.obs")


#install.packages("tidyr")
library(tidyr)

#install.packages(dplyr)
library(dplyr)



# Etanol
data$Etanol_2 <- 14.89 * data$`Abs Etanol 2%` + 0.1563
data$Etanol_7 <- 14.89 * data$`Abs Etanol 7%` + 0.1563
data$Etanol_13 <- 14.89 * data$`Abs Etanol 13%` + 0.1563



# Calcular la correlación entre AR y Etanol 
cor_2 <- cor(data$`AR 2%`, data$Etanol_2, use = "complete.obs")
cor_7 <- cor(data$`AR 7%`, data$Etanol_7, use = "complete.obs")
cor_13 <- cor(data$`AR 7%`, data$Etanol_13, use = "complete.obs")




#install.packages("tidyr")
library(tidyr)

# Etanol
data$Etanol_2 <- 14.89 * data$`Abs Etanol 2%` + 0.1563
data$Etanol_7 <- 14.89 * data$`Abs Etanol 7%` + 0.1563
data$Etanol_13 <- 14.89 * data$`Abs Etanol 13%` + 0.1563


# Crear un dataframe largo para el gráfico
datos_long <- pivot_longer(data, 
                           cols = c(`AR 2%`, `AR 7%`, `AR 13%`, Etanol_2, Etanol_7, Etanol_13), 
                           names_to = "Variable", 
                           values_to = "Concentracion")


# Crear el gráfico con dos ejes Y
ggplot(datos_long, aes(x = Tiempo)) +
  geom_line(data = subset(datos_long, grepl("AR", Variable)), 
            aes(y = Concentracion, color = Variable), linetype = "solid") +
  geom_line(data = subset(datos_long, grepl("Etanol", Variable)), 
            aes(y = Concentracion, color = Variable), linetype = "dashed") +
  scale_color_manual(values = c('AR 2%' = 'blue', 
                                'AR 7%' = 'orange', 
                                'AR 13%' = 'red', 
                                'Etanol_2' = 'green', 
                                'Etanol_7' = 'purple', 
                                'Etanol_13' = 'brown'),
                     name = "Variables") +
  scale_y_continuous(name = "Concentración de AR", 
                     sec.axis = sec_axis(~., name = "Concentración de Etanol")) +
  labs(title = "Concentraciones de AR y Etanol en función del Tiempo",
       x = "Tiempo (min)") +
  theme_minimal() +
  theme(legend.position = "right")





