data <- Boston
head(data)


# Identificar los outliers mediante histograma y boxplot
library(ggplot2)

# Boxplot
ggplot(data, aes(x = "", y = crim)) +
  geom_boxplot(fill = "orange", color = "black") +
  ggtitle("Boxplot de 'crim' (Tasa de Criminalidad)") +
  ylab("Crim (Tasa de Criminalidad)") +
  theme_minimal()


# Histograma
ggplot(data, aes(x = crim)) +
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  ggtitle("Histograma de 'crim' (Tasa de Criminalidad)") +
  xlab("Crim (Tasa de Criminalidad)") +
  ylab("Frecuencia") +
  theme_classic()


# Función para detectar outliers usando el método del IQR
outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outliers <- data[[column]] < lower_bound | data[[column]] > upper_bound
  
  return(outliers)
}

# Aplicar la función de outliers en las columnas 'medv' y 'crim'
outliers_medv <- outliers(data, "medv")
outliers_crim <- outliers(data, "crim")
eliminar_outliers <- outliers_medv | outliers_crim
data_limpia <- data[!eliminar_outliers, ]


# Verificamos si los outliers han sido eliminados
ggplot(data_limpia, aes(x = "", y = medv)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  ggtitle("Boxplot de 'medv' sin outliers") +
  ylab("Medv (Valor medio de las viviendas)") +
  theme_minimal()


ggplot(data_limpia, aes(x = "", y = crim)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  ggtitle("Boxplot de 'crim' sin outliers") +
  ylab("Crim (Tasa de Criminalidad)") +
  theme_minimal()


# Función para reemplazar outliers con la mediana
mediana <- function(data, column) {
  outliers_mediana <- outliers(data, column)
  data[[column]][outliers_mediana] <- median(data[[column]], na.rm = TRUE)
  return(data)
}
data <- mediana(data, "crim")


# Función para imputar outliers con el promedio
imputar_outliers_promedio <- function(data, column) {
  outliers_promedio <- outliers(data, column)  
  promedio <- mean(data[[column]], na.rm = TRUE)  
  data[[column]][outliers_promedio] <- promedio  
  return(data)
}
data <- imputar_outliers_promedio(data, "crim")

# Verificar si los outliers fueron imputados
summary(data$crim)





