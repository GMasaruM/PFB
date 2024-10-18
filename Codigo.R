# Instalar las bibliotecas necesarias si aún no las tienes
#install.packages("ggplot2")
#install.packages("manipulate")

library(ggplot2)
library(manipulate)

# Definir la función de Michaelis-Menten
V <- function(S, Km, Vmax) {
  return(Vmax * S / (Km + S))
}

# Función para graficar V vs [S]
plot_V <- function(Km, Vmax) {
  S <- seq(0, 1000, by = 1)  # Crear una secuencia de concentraciones de sustrato
  V_values <- V(S, Km, Vmax)  # Calcular la velocidad para cada valor de S
  
# Crear el gráfico usando ggplot2
  ggplot(data = data.frame(S = S, V = V_values), aes(x = S, y = V)) +
    geom_line(color = "red") +
    ylim(0, 100) +
    labs(x = "[S] (mM)", y = expression(V~(mu*M/min)), title = "V vs [S]") +
    theme_minimal()
}

# Crear la interfaz interactiva con sliders para Km y Vmax
manipulate(
  plot_V(Km, Vmax),
  Km = slider(0, 500, step = 1, initial = 100),
  Vmax = slider(0, 100, step = 1, initial = 50)
)


