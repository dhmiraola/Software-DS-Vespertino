library(MASS)
library(dplyr)
library(e1071)

data("Boston") # Cargo la base de datos
data <- Boston # Asignar el nombre data a la base de datos
head(data)

# Extraer solo columnas numericas
columnas_numericas <- data %>%
  select_if(is.numeric)

# Función para calcular la moda, ya que R no la trae predefinida
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calcular estadísticos
estadisticos <- columnas_numericas %>%
  summarise(across(everything(), list(
    count = ~sum(!is.na(.)),                # Cantidad de datos
    min = ~min(., na.rm = TRUE),            # Mínimo
    max = ~max(., na.rm = TRUE),            # Máximo
    mean = ~mean(., na.rm = TRUE),          # Media
    median = ~median(., na.rm = TRUE),      # Mediana
    moda = ~moda(.),                        # Moda, usando la función personalizada
    variance = ~var(., na.rm = TRUE),       # Varianza
    sd = ~sd(., na.rm = TRUE),              # Desviación estándar
    cv = ~sd(., na.rm = TRUE) / mean(., na.rm = TRUE),  # Coeficiente de variación
    Q1 = ~quantile(., 0.25, na.rm = TRUE),  # Cuartil 1
    Q3 = ~quantile(., 0.75, na.rm = TRUE),  # Cuartil 3
    IQR = ~IQR(., na.rm = TRUE),            # Rango intercuartílico
    MAD = ~mad(., constant = 1, na.rm = TRUE), # MAD
    skewness = ~e1071::skewness(., na.rm = TRUE),  # Asimetría
    kurtosis = ~e1071::kurtosis(., na.rm = TRUE)   # Curtosis
  )))

print(estadisticos)


# Ejemplo con variable categórica
data <- data %>%
  mutate(chas = as.factor(chas)) # Convertir chas a factor si no lo es

dummies <- model.matrix(~chas - 1, data = data)
head(dummies)


# Función para detectar outliers
detectar_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  return(x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR))
}

outliers <- columnas_numericas %>%
  mutate(across(everything(), detectar_outliers))
print(outliers)

# Convertir columnas categóricas a factores
data <- data %>%
  mutate(across(where(is.character), as.factor))

str(data) # Verificar la estructura de los datos

# Crear una nueva variable que es la suma de otras dos variables
data <- data %>%
  mutate(nueva_variable = medv + rm) 
head(data)

# Encontrar y contar duplicados
duplicados <- data[duplicated(data), ]
num_duplicados <- nrow(duplicados)
print(num_duplicados)

