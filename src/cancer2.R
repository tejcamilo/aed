# Cargar los datos desde el archivo CSV
data <- read.csv("../input/cancer.csv")
data <- subset(data, Jurisdiction != "Maryland")

library(dplyr)

# Verificar datos faltantes
missing_data <- sapply(data, function(x) sum(is.na(x)))
print(missing_data)

# Obtener estadísticas descriptivas
summary(data)

# Instalar y cargar el paquete 'ggplot2' para visualizaciones
library(ggplot2)

# Transformar los datos a formato largo para ggplot2
data_long <- reshape2::melt(data, id.vars = "Jurisdiction", measure.vars = c("Males", "Females"))

# Crear el gráfico de barras agrupadas
ggplot(data_long, aes(x = Jurisdiction, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Number of Cases", x = "Jurisdiction", fill = "Gender", title = "Distribution of Cancer Cases by Gender and Jurisdiction") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Distribución de casos por raza
data_long <- data %>%
  gather(key = "Race", value = "Cases", Whites:Unknown.Race)

ggplot(data_long, aes(x = Jurisdiction, y = Cases, fill = Race)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(y = "Number of Cases", title = "Distribution of Cancer Cases by Race") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


library(ggcorrplot)
library(ggplot2)
library(corrplot)



# Seleccionar solo las columnas numéricas para el análisis de correlación
numeric_data <- data[, sapply(data, is.numeric)]

# Calcular la matriz de correlación
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Imprimir la matriz de correlación
print(correlation_matrix)

# Visualizar la matriz de correlación usando una gráfica de calor
corrplot(correlation_matrix, method = "color", col = colorRampPalette(c("red", "white", "blue"))(200), 
         type = "lower", tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, title = "Matriz de Correlación")


# Seleccionar las columnas numéricas
numeric_columns <- c("Total", "Males", "Females", "Whites", "Blacks", "Other.Race", "Unknown.Race")
data_numeric <- data[, numeric_columns]

# Calcular la matriz de correlación
correlation_matrix <- cor(data_numeric, use="complete.obs")

# Instalar y cargar la librería corrplot
library(corrplot)

# Visualizar la matriz de correlación
corrplot(correlation_matrix, method="color", type="upper", tl.col="black", tl.srt=45, addCoef.col = "black")
