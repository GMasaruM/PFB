# Datos corregidos: tiempo de fermentación y tiempo de incubación fijo
datos <- data.frame(
  Muestra = c("A1", "A2", "A3", "A4"),
  Tiempo_fermentacion = c(0, 1440, 2880, 4320),
  OD1 = c(0.420, 0.476, 0.454, 0.136),
  OD2 = c(0.373, 0.515, 0.342, 0.106),
  ODblank1 = 0.127,
  ODblank2 = 0.111,
  ODstd1 = 0.600,
  ODstd2 = 0.614,
  DF = 50,
  Tiempo_reaccion = 20 # constante para todos
)

# Función para calcular actividad
calc_actividad <- function(OD, ODblank, ODstd, T, DF) {
  ((OD - ODblank) / (ODstd - ODblank)) * (400 / T) * DF
}

# Aplicar fórmula a cada réplica
datos$U_L_1 <- mapply(calc_actividad, datos$OD1, datos$ODblank1, datos$ODstd1, datos$Tiempo_reaccion, datos$DF)
datos$U_L_2 <- mapply(calc_actividad, datos$OD2, datos$ODblank2, datos$ODstd2, datos$Tiempo_reaccion, datos$DF)

# Promedio
datos$Promedio_U_L <- rowMeans(datos[, c("U_L_1", "U_L_2")], na.rm = TRUE)

# Mostrar resultados
print(datos[, c("Muestra", "Tiempo_fermentacion", "U_L_1", "U_L_2", "Promedio_U_L")])




# Construir dataframe largo con los datos duplicados
od_vs_actividad <- data.frame(
  OD = c(datos$OD1, datos$OD2),
  Actividad = c(datos$U_L_1, datos$U_L_2),
  Replica = rep(c("Replica 1", "Replica 2"), each = nrow(datos)),
  Muestra = rep(datos$Muestra, 2)
)

# Graficar
library(ggplot2)

ggplot(od_vs_actividad, aes(x = OD, y = Actividad, color = Replica)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray40") +
  labs(title = "Relación entre OD y Actividad de α-amilasa (Set A)",
       x = "OD (absorbancia)",
       y = "Actividad α-amilasa (U/L)") +
  theme_minimal()

# Datos Set A
tiempo <- c(0, 1440, 2880, 4320)
actividad <- c(539.65, 796.88, 541.55, 47.33)
datos <- data.frame(t = tiempo, A = actividad)

ggplot(datos, aes(x = tiempo, y = actividad)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(size = 3, color = "orange") +
  labs(title = "Actividad de α-amilasa durante la fermentación (Set A)",
       x = "Tiempo de fermentación (min)",
       y = "Actividad enzimática (U/L)") +
  theme_minimal()



###




# Filtrar fase de decaimiento (t >= t_opt ≈ 1496)
t_opt <- 1496
decay <- subset(datos, t >= t_opt)

# Calcular 1/A y ajustar ley integrada de 2º orden: 1/A = 1/A0 + k * t
decay$invA <- 1 / decay$A
modelo2o <- lm(invA ~ t, data = decay)

# Extraer parámetros
intercept <- coef(modelo2o)[1]  # 1/A0
k <- coef(modelo2o)[2]          # constante k
A0_est <- 1 / intercept

cat("Constante k (L·U⁻¹·min⁻¹):", signif(k, 3), "\n")
cat("A0 estimada (U/L):", signif(A0_est, 3), "\n")

# Crear un nuevo data.frame para predicciones
pred <- data.frame(t_pred = seq(min(decay$t), max(decay$t), length.out = 100))
pred$A_pred <- 1 / (intercept + k * pred$t_pred)

# Gráfico 1: 1/A vs t
plot(decay$t, decay$invA, pch = 19,
     xlab = "Tiempo de fermentación (min)",
     ylab = "1 / Actividad (L/U)",
     main = "Ley integrada 2º orden (fase de decaimiento)")
abline(modelo2o, col = "red", lwd = 2)
legend("topleft", legend = c("Datos 1/A", "Ajuste lineal"),
       col = c("black", "red"), pch = c(19, NA), lty = c(NA, 1))

# Gráfico 2: A(t) vs t con datos y modelo
plot(decay$t, decay$A, pch = 19,
     xlab = "Tiempo de fermentación (min)",
     ylab = "Actividad (U/L)",
     main = "Modelo 2º orden: A(t) vs t")
lines(pred$t_pred, pred$A_pred, col = "blue", lwd = 2)
legend("topright", legend = c("Datos", "Modelo 2º orden"),
       col = c("black", "blue"), pch = c(19, NA), lty = c(NA, 1))



###
# 1. Datos de actividad vs tiempo
tiempo <- c(0, 1440, 2880, 4320)      # min de fermentación
actividad <- c(539.65, 796.88, 541.55, 47.33)  # U/L
datos <- data.frame(t = tiempo, A = actividad)

# 2. Ajuste cuadrático (modelo empírico) para obtener t_opt y A_max
modelo_quad <- lm(A ~ I(t^2) + t, data = datos)
coefs <- coef(modelo_quad)
a <- coefs["I(t^2)"]
b <- coefs["t"]
c <- coefs["(Intercept)"]

t_opt   <- -b / (2 * a)               # Tiempo óptimo
A_max   <- a * t_opt^2 + b * t_opt + c # Actividad máxima

# 3. Tasa inicial de “activación” (pendiente entre los dos primeros puntos)
v_ini <- (actividad[2] - actividad[1]) / (tiempo[2] - tiempo[1])

# 4. Constante de decaimiento (fase de caída) usando modelo de primer orden
#    1ª orden: A(t) = A3 * exp(-k*(t - t3)), se calcula entre puntos 3 y 4
k <- -log(actividad[4] / actividad[3]) / (tiempo[4] - tiempo[3])

# 5. Vida media en la fase de decaimiento
t_half <- log(2) / k

# 6. Mostrar resultados
cat("Parámetros cinéticos (Set A):\n")
cat(sprintf("  • Tiempo óptimo (t_opt)    = %.1f min (%.1f h)\n", t_opt, t_opt/60))
cat(sprintf("  • Actividad máxima (A_max) = %.1f U/L\n", A_max))
cat(sprintf("  • Tasa inicial (v_ini)     = %.4f U/L·min⁻¹\n", v_ini))
cat(sprintf("  • Constante de decaimiento (k) = %.5f min⁻¹\n", k))
cat(sprintf("  • Vida media (t1/2)        = %.1f min (%.1f h)\n", t_half, t_half/60))

