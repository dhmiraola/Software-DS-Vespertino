library(MASS)
library(dplyr)
library(ggplot2)


data("Boston") # Cargo la base de datos
data <- Boston # Asignar el nombre data a la base de datos
head(data)

# Histograma
hist(Boston$crim, main="Tasa de Criminalidad", xlab="Tasa de Criminalidad", col="lightblue")

# Boxplor
boxplot(Boston$medv, main="Boxplot de Valor Medio de las Casas", ylab="Valor Medio de las Casas ($1000)")

# Grafico de barras
Boston$chas <- as.factor(Boston$chas)
barplot(table(Boston$chas), main="Gráfico de Barras", xlab="chas", col="lightgreen")

# Grafico de dispersion
plot(Boston$rm, Boston$medv, main="Relación entre Número de Habitaciones y medv", xlab="Número de Habitaciones", ylab="medv", pch=19, col="blue")

# Pairplot
pairs(Boston[, c("crim", "rm", "medv", "tax")], main="Gráfico de Pares o Pairplot")

# Grafico de densidad
plot(density(Boston$medv), main="Gráfico de Densidad de medv", xlab="medv", col="purple")

# Series de tiempo
plot(ts(Boston$medv), main="Serie de Tiempo de medv", ylab="medv", xlab="Índice", col="darkred")

# Grafico de violin
ggplot(Boston, aes(x=factor(chas), y=medv)) + 
  geom_violin(fill="lightblue") +
  labs(title="Gráfico de Violin de Valor Medio de las Casas por Proximidad al Río", x="Cerca del Río Charles (1=Sí, 0=No)", y="Valor Medio de las Casas ($1000)")


# Crear una matriz de correlación
correlation_matrix <- cor(Boston)
heatmap(correlation_matrix, main="Mapa de Calor de la Matriz de Correlación", col=topo.colors(10), symm=TRUE)

# Grafico de lineas
ggplot(Boston, aes(x=1:nrow(Boston), y=medv)) + 
  geom_line(col="darkgreen") +
  labs(title="Gráfico de Línea de medv", x="Índice", y="medv)")


# Grafico de dispersion 3d
# Instalar scatterplot3d
# install.packages("scatterplot3d")
library(scatterplot3d)
# Gráfico 3D de 'crim', 'rm', y 'medv'
scatterplot3d(Boston$crim, Boston$rm, Boston$medv, main="Gráfico 3D", xlab="Tasa de Criminalidad", ylab="Número de Habitaciones", zlab="Valor Medio de las Casas ($1000)", color="darkred", pch=19)

# Grafico de barras
ggplot(Boston, aes(x=factor(chas))) + 
  geom_bar(fill="lightcoral") +
  labs(title="Gráfico de Barras de Proximidad al Río Charles", x="Cerca del Río Charles (1=Sí, 0=No)", y="Frecuencia")


# Mapa de calor (Primero debemos seleccionar las columnas numericas)
Boston_numeric <- Boston[, sapply(Boston, is.numeric)]
correlation_matrix <- cor(Boston_numeric)
heatmap(correlation_matrix, main="Mapa de Calor de la Matriz de Correlación", col=topo.colors(10), symm=TRUE)


