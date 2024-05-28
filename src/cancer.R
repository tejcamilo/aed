library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

setwd("/Users/ctejada/Desktop/AED/proyecto/src/")

cancer_data <- read_csv("../input/cancer.csv")

summary(cancer_data)

na_remove <- function(x) {
  return(mean(x, na.rm = TRUE)) #na.rem quitamos los na
}

clean <- cancer_data %>%  
  group_by(Jurisdiction) %>% 
  mutate(across(everything(), na_remove)) %>% 
  distinct()

clean <- clean %>% ungroup()

# quitar valor atípico
filtr <- clean %>% filter(Total < 20000)

# casos por ubicación
ggplot(filtr, aes(x = Jurisdiction, y = Total)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Jurisdicción", y = "Casos", title = "Casos por ubicación") +
  theme_minimal()

data_long <- pivot_longer(filtr, cols = c(Males, Females), names_to = "Gender", values_to = "Count")


ggplot(data_long, aes(x = Jurisdiction, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Males vs Females by Jurisdiction",
       x = "Jurisdiction", y = "Total Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Males" = "steelblue", "Females" = "coral"))

gender <- data.frame(
  Category = c("Males", "Females"),
  Count = c(sum(filtr$Males), sum(filtr$Females))
)

# agregar %
gender$Percentage <- (gender$Count / sum(gender$Count)) * 100 

ggplot(gender, aes(x = Category, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage Distribution of Males vs. Females",
       x = "Gender",
       y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("Males" = "steelblue", "Females" = "coral")) +
  theme(legend.position = "none") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5)

summary(clean)

