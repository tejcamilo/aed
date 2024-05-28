library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(corrplot)
library(stringr)

setwd("/Users/ctejada/Desktop/AED/proyecto/src/")
original <- read_csv("../input/verdes.csv")
data <- read_csv("../input/verdes.csv")

# Clean up the column names
names(data) <- gsub("\\n|\\s+", "", names(data))

# Check for missing values
missing_values <- colSums(is.na(data))
print(missing_values)

# Display the first few rows of the data
head(data)

# Summary statistics for numerical columns
summary(data)

# Summary statistics for categorical columns
data %>%
  summarise_all(~n_distinct(.))

arreglar <- function(name) { # quitar los paréntesis
  str_split(name, "\\(")[[1]][1] %>% str_trim()
}

names(data) <- sapply(names(data), arreglar)

print(names(data))


# Distribution by Region
ggplot(data, aes(x = factor(REGION))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Green Businesses by Region",
       x = "Region",
       y = "Number of Businesses")

# Distribution by Category
ggplot(data, aes(x = factor(CATEGORÍA))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Green Businesses by Category",
       x = "Category",
       y = "Number of Businesses")

# Distribution by Sector
ggplot(data, aes(x = factor(SECTOR))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Green Businesses by Sector",
       x = "Sector",
       y = "Number of Businesses")

# Convert year column to numeric
data$ANO <- as.numeric(data$AÑO)

# Time Trend of Registrations
ggplot(data, aes(x = AÑO)) +
  geom_line(stat = "count") +
  geom_point(stat = "count") +
  labs(title = "Time Trend of Green Business Registrations",
       x = "Year",
       y = "Number of Registrations")

# Trend of Top Categories
top_categories <- data %>%
  group_by(CATEGORÍA) %>%
  summarise(count = n()) %>%
  top_n(5, wt = count) %>%
  pull(CATEGORÍA)

category_trend <- data %>%
  filter(CATEGORÍA %in% top_categories) %>%
  group_by(AÑO, CATEGORÍA) %>%
  summarise(count = n()) %>%
  spread(CATEGORÍA, count, fill = 0)

ggplot(category_trend, aes(x = AÑO)) +
  geom_line(aes(y = category_trend$`1. Bienes y servicios sostenibles provenientes de los Recursos Naturales`)) +
  geom_line(aes(y = category_trend$`2. Ecoproductos Industriales`)) +
  geom_line(aes(y = category_trend$`3. Mercados de Carbono`)) +
  labs(title = "Top Green Business Categories Over the Years",
       x = "Year",
       y = "Number of Registrations") +
  theme(legend.position = "top")


data$AÑO <- as.numeric(data$AÑO)

# Identify numerical columns
numerical_columns <- data %>% select_if(is.numeric)

# Calculate correlation matrix
correlation_matrix <- cor(numerical_columns, use = "complete.obs")

# Display the correlation matrix
print(correlation_matrix)
# Convert the correlation matrix to a long format
correlation_long <- as.data.frame(as.table(correlation_matrix))

# Plot the correlation matrix using ggplot2
ggplot(correlation_long, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Matrix",
       x = "Variables",
       y = "Variables")

