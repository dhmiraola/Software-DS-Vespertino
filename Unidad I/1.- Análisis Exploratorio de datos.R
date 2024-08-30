# Instalar y cargar el paquete MASS que contiene el dataset de Boston
# install.packages("MASS")
library(MASS)

data("Boston")
data <- Boston
head(data)

# EDA Análisis exploratorio de datos

# Estructura del dataset
str(data)

# Resumen estadístico de todas las columnas
summary(data)

# Desviación estándar de cada variable
sapply(data, sd)

# Matriz de correlación
correlacion <- cor(Boston)
correlacion

# Verificar si hay valores perdidos
valores_perdidos <- sapply(data, function(x) sum(is.na(x)))
valores_perdidos

# Media de la variable 'crim' 
mean(data$crim)

# Mediana de la variable 'crim'
median(data$crim)


# Transformacion de datos con dplyr

# Instalar dplyr
# install.packages("dplyr")
library(dplyr)

# Seleccionamos columnas
seleccionar <- data %>% select(crim, rm, medv)
head(seleccionar)

# Filtrar filas donde el valor de las casas es mayor a 25 por ejemplo
mayor <- data %>% filter(medv > 25)
head(mayor)

# Crear una nueva columna con la relación crimen-rm
relacion <- data %>%mutate(crim_rm_ratio = crim / rm)
head(relacion)
