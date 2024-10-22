#install.packages("renz")
library(renz)
#install.packages("EKA")
#library(EKA)
library(ggplot2)

# Datos originales
tiempo <- c(0, 30, 60, 90, 120, 150, 180)
ensayo_1 <- c(0.248, 0.287, 0.38, 0.413, 2.093, 0.566, 0.833)
ensayo_2 <- c(0.125, 2.342, 0.914, 0.892, 0.935, 1.023, 0.817)
ensayo_3 <- c(1.781, 0.555, 0.646, 0.584, 0.641, 0.785, 0.562)

# Función para eliminar outliers
remove_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  return(x[x >= lower_bound & x <= upper_bound])
}

# Eliminar outliers de cada ensayo
ensayo_1_clean <- remove_outliers(ensayo_1)
ensayo_2_clean <- remove_outliers(ensayo_2)
ensayo_3_clean <- remove_outliers(ensayo_3)

# Actualizar los tiempos según los datos limpios
tiempo_1_clean <- tiempo[which(ensayo_1 %in% ensayo_1_clean)]
tiempo_2_clean <- tiempo[which(ensayo_2 %in% ensayo_2_clean)]
tiempo_3_clean <- tiempo[which(ensayo_3 %in% ensayo_3_clean)]

# Calcular la pendiente de la curva (velocidad de formación de producto) para los datos limpios
pendiente_2 <- coef(lm(ensayo_1_clean ~ tiempo_1_clean))[2]
pendiente_7 <- coef(lm(ensayo_2_clean ~ tiempo_2_clean))[2]
pendiente_13 <- coef(lm(ensayo_3_clean ~ tiempo_3_clean))[2]


print(pendiente_2)

print(pendiente_7)

print(pendiente_13)
