# Cargar las bibliotecas necesarias
library(dplyr)
library(ggplot2)
library(broom)

setwd("/Users/ctejada/Desktop/AED/proyecto/src/")

# Leer el archivo CSV
data <- read.csv("../input/food.csv")

# Ver las primeras filas de los datos
head(data)

# Ver la estructura de los datos
str(data)

# Resumen de los datos
summary(data)


# 2: Describir las variables ----------------------------------------------

# Seleccionar las variables relevantes para el análisis
variables <- c('calories', 'protein', 'fat', 'sodium', 'rating')

# Filtrar los datos para excluir valores extremos y eliminar filas con valores faltantes
filtered_data <- data %>%
  filter(calories <= 3000) %>%
  select(all_of(variables)) %>%
  na.omit()

# Obtener estadísticas descriptivas
summary(filtered_data)




# 3: Realizar un análisis de regresión ------------------------------------

# Ajustar el modelo de regresión lineal
model <- lm(rating ~ calories + protein + fat + sodium, data = filtered_data)

# Resumen del modelo
model_summary <- summary(model)
print(model_summary)

# Extraer los coeficientes del modelo
coefficients <- tidy(model)

# Excluir el término de la constante (intercepto) para la visualización
coefficients <- coefficients %>% filter(term != "(Intercept)")


# 4: Visualizar los resultados --------------------------------------------
# Visualizar la influencia de las variables en un gráfico de barras
ggplot(coefficients, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Influencia de las Variables en la Calificación", x = "Variables", y = "Coeficiente de Regresión") +
  theme_minimal() +
  coord_flip()


