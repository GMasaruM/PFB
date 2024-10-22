
# Cosas de masaru

# Ejemplo paper

library(readxl)

datos <- read_excel("datos_absorbancia.xlsx", col_types = c("text", "numeric", "numeric", "numeric"))


# Calcular los nuevos valores de AR para cada absorbancia
# Aplicamos la fórmula a cada columna de absorbancia
ar_e1 <- 1.7713 * datos$`Absorbancia 1` - 0.0478
ar_e2 <- 1.7713 * datos$`Absorbancia 2` - 0.0478
ar_e3 <- 1.7713 * datos$`Absorbancia 3` - 0.0478

# Crear un data frame con los nuevos valores
AR <- data.frame(
  Tiempo = datos$Tiempo,
  AR_Ensayo_1 = ar_e1,
  AR_Ensayo_2 = ar_e2,
  AR_Ensayo_3 = ar_e3
)

# Mostrar los resultados
print(AR)
