library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(broom)

setwd("/Users/ctejada/Desktop/AED/proyecto/src/")

# Leer el archivo CSV
data <- read.csv("../input/food.csv")

# Mostrar las primeras filas del dataframe
head(data)

# Generar estadísticas descriptivas iniciales
initial_stats <- summary(data)

# Visualizar la distribución de calorías
ggplot(data, aes(x = calories)) + 
  geom_histogram(binwidth = 50, fill = "blue", color = "black") +
  scale_y_log10() +
  labs(title = "Distribution of Calories", x = "Calories", y = "Frequency")

# Filtrar valores de calorías superiores a 3000
filtered_data <- data %>% filter(calories <= 3000)

# Recalcular las estadísticas descriptivas después de filtrar los valores extremos
filtered_stats <- summary(filtered_data)

# Visualizar la distribución de calorías después del filtrado
ggplot(filtered_data, aes(x = calories)) + 
  geom_histogram(binwidth = 50, fill = "blue", color = "black") +
  labs(title = "Distribution of Calories (Filtered)", x = "Calories", y = "Frequency")





ggplot(filtered_data, aes(x = rating)) + 
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Distribución de las Calificaciones", x = "Calificación", y = "Frecuencia")

# Gráfico de dispersión de calorías vs calificación
ggplot(filtered_data, aes(x = calories, y = rating)) + 
  geom_point(alpha = 0.5) +
  labs(title = "Calorías vs. Calificación", x = "Calorías", y = "Calificación")

# Diagrama de cajas del contenido de proteína
ggplot(filtered_data, aes(y = protein)) + 
  geom_boxplot() +
  labs(title = "Diagrama de Cajas del Contenido de Proteína", y = "Proteína")

# Histograma del contenido de sodio
ggplot(filtered_data, aes(x = sodium)) + 
  geom_histogram(binwidth = 50, fill = "blue", color = "black") +
  labs(title = "Distribución del Contenido de Sodio", x = "Sodio", y = "Frecuencia")

correlation <- cor(filtered_data$sodium, filtered_data$calories, use = "complete.obs")
print(paste("Correlación entre sodio y calorías:", correlation))



# Seleccionar las variables relevantes para el análisis de regresión
variables <- c('calories', 'protein', 'fat', 'sodium')

# Preparar los datos para el análisis de regresión, eliminando filas con valores faltantes
data_for_regression <- filtered_data %>% select(rating, all_of(variables)) %>% na.omit()

# Ajustar el modelo de regresión lineal
model <- lm(rating ~ calories + protein + fat + sodium, data = data_for_regression)

# Resumen del modelo
model_summary <- summary(model)
print(model_summary)

# Visualización de la relación entre sodio y calorías
ggplot(data_for_regression, aes(x = calories, y = sodium)) + 
  geom_point(alpha = 0.5) +
  labs(title = paste("Sodio vs Calorías (Correlación:", round(cor(data_for_regression$sodium, data_for_regression$calories), 2), ")"),
       x = "Calorías", y = "Sodio") +
  geom_smooth(method = "lm", color = "red")




variables <- c('calories', 'protein', 'fat', 'sodium')

# Preparar los datos para el análisis de regresión, eliminando filas con valores faltantes
data_for_regression <- filtered_data %>% select(rating, all_of(variables)) %>% na.omit()

# Ajustar el modelo de regresión lineal
model <- lm(rating ~ calories + protein + fat + sodium, data = data_for_regression)

# Resumen del modelo
model_summary <- summary(model)
print(model_summary)

# Extraer los coeficientes del modelo
coefficients <- tidy(model)

# Excluir el término de la constante (intercepto) para la visualización
coefficients <- coefficients %>% filter(term != "(Intercept)")

# Visualizar la influencia de las variables en un gráfico de barras
ggplot(coefficients, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Influencia de las Variables en la Calificación", x = "Variables", y = "Coeficiente de Regresión") +
  theme_minimal() +
  coord_flip()  # Para una mejor visualización, giramos el gráfico de barras

ggplot(filtered_data, aes(x = calories, y = protein)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Proteína vs Calorías", x = "Calorías", y = "Proteína") +
  theme_minimal()

# Gráfico de dispersión de grasa vs calorías
ggplot(filtered_data, aes(x = calories, y = fat)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Grasa vs Calorías", x = "Calorías", y = "Grasa") +
  theme_minimal()
