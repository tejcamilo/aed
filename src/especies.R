library(ggplot2)

# Cargar los datos desde un archivo CSV
especies_data <- read.csv("../input/especies.csv")


# 2. Preparar los datos ---------------------------------------------------

# Distribución por Reino con nombres comunes
reino_distribution <- table(especies_data$REINO)
reino_names <- c("Plantas", "Animales", "Hongos")
names(reino_distribution) <- reino_names

# Distribución por Filo con nombres comunes
filo_distribution <- table(especies_data$FILO)
filo_names <- c("Plantas Vasculares", "Cordados", "Artrópodos", "Hongos Ascomicetos", 
                "Musgos", "Hongos Basidiomicetos", "Hepáticas", "Moluscos", "Cnidarios", "Equinodermos")
names(filo_distribution) <- filo_names

# Distribución por Clase con nombres comunes
clase_distribution <- table(especies_data$CLASE)
clase_names <- c("Plantas con Flores", "Anfibios", "Aves", "Peces con Aletas Radiadas", "Mamíferos", 
                 "Helechos", "Hongos Líquenes", "Reptiles", "Crustáceos", "Insectos", 
                 "Hongos Basidiomicetos", "Hepáticas Foliosas", "Tiburones y Rayas", "Musgos Esfagnos", 
                 "Coníferas", "Musgos Verdaderos", "Anthozoarios", "Arácnidos", "Bivalvos", 
                 "Hongos Líquenes", "Caracoles y Babosas", "Pepinos de Mar", "Monocotiledóneas", 
                 "Ofiuras", "Hepáticas Talosas", "Hongos Ascomicetos")
names(clase_distribution) <- clase_names

# Distribución por Orden (Top 10) con nombres comunes
orden_distribution <- sort(table(especies_data$ORDEN), decreasing = TRUE)[1:10]
orden_names <- c("Ranas y Sapos", "Plantas Monocotiledóneas", "Gramíneas", "Plantas Compuestas", 
                 "Plantas Dicotiledóneas", "Plantas con Flores", "Plantas con Flores", 
                 "Aves Paseriformes", "Plantas con Flores", "Palmas")
names(orden_distribution) <- orden_names

# Distribución por Familia (Top 10) con nombres comunes
familia_distribution <- sort(table(especies_data$FAMILIA), decreasing = TRUE)[1:10]
familia_names <- c("Orquídeas", "Bromelias", "Ranas Terrestres", "Plantas Compuestas", 
                   "Plantas con Flores", "Palmas", "Sapos", "Plantas con Flores", 
                   "Helechos Arborescentes", "Ranas Venenosas")
names(familia_distribution) <- familia_names



# 3. Crear los gráficos ---------------------------------------------------

# Crear un dataframe para ggplot2
reino_df <- as.data.frame(reino_distribution)
colnames(reino_df) <- c("Reino", "Frecuencia")

# Gráfico
ggplot(reino_df, aes(x = Reino, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Distribución de Especies Amenazadas por Reino", 
       x = "Reino", y = "Número de Especies") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Crear un dataframe para ggplot2
filo_df <- as.data.frame(filo_distribution)
colnames(filo_df) <- c("Filo", "Frecuencia")

# Gráfico
ggplot(filo_df, aes(x = Filo, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Distribución de Especies Amenazadas por Tipo de Filo", 
       x = "Tipo de Filo", y = "Número de Especies") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Crear un dataframe para ggplot2
clase_df <- as.data.frame(clase_distribution)
colnames(clase_df) <- c("Clase", "Frecuencia")

# Gráfico
ggplot(clase_df, aes(x = Clase, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "coral") +
  theme_minimal() +
  labs(title = "Distribución de Especies Amenazadas por Tipo de Clase", 
       x = "Tipo de Clase", y = "Número de Especies") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Crear un dataframe para ggplot2
orden_df <- as.data.frame(orden_distribution)
colnames(orden_df) <- c("Orden", "Frecuencia")

# Gráfico
ggplot(orden_df, aes(x = Orden, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  theme_minimal() +
  labs(title = "Distribución de Especies Amenazadas por Orden (Top 10)", 
       x = "Tipo de Orden", y = "Número de Especies") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Crear un dataframe para ggplot2
familia_df <- as.data.frame(familia_distribution)
colnames(familia_df) <- c("Familia", "Frecuencia")

# Gráfico
ggplot(familia_df, aes(x = Familia, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "gold") +
  theme_minimal() +
  labs(title = "Distribución de Especies Amenazadas por Familia (Top 10)", 
       x = "Tipo de Familia", y = "Número de Especies") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ACM ---------------------------------------------------------------------

library(FactoMineR)
library(factoextra)

# 2. Preparar los datos ---------------------------------------------------

# Seleccionar columnas categóricas relevantes para el ACM
# Por ejemplo, seleccionamos 'REINO', 'FILO', 'CLASE', 'ORDEN', 'FAMILIA'
acm_data <- especies_data[, c("REINO", "FILO", "CLASE", "ORDEN", "FAMILIA")]  


# 3: Realizar el ACM ------------------------------------------------------

# Realizar el Análisis de Correspondencias Múltiples
acm_result <- MCA(acm_data, graph = FALSE)


#  4: Visualizar los resultados -------------------------------------------

# Visualizar los individuos (especies) en el plano factorial
fviz_mca_ind(acm_result, 
             label = "none", # No mostrar etiquetas de los individuos
             habillage = "REINO", # Color según la variable 'REINO'
             addEllipses = TRUE, ellipse.type = "confidence",
             palette = "jco",
             ggtheme = theme_minimal())

# Visualizar las variables (categorías) en el plano factorial
fviz_mca_var(acm_result, 
             repel = TRUE, # Evitar la superposición de etiquetas
             palette = "jco",
             ggtheme = theme_minimal())

# Visualizar tanto individuos como variables en el plano factorial
fviz_mca_biplot(acm_result, 
                repel = TRUE, # Evitar la superposición de etiquetas
                habillage = "REINO", # Color según la variable 'REINO'
                addEllipses = TRUE, ellipse.type = "confidence",
                palette = "jco",
                ggtheme = theme_minimal())


