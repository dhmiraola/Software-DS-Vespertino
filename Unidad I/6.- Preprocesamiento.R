# Cargar el paquete y los datos del Titanic
library(titanic)
data <- titanic_train
head(data)

# Convertir la variable categórica 'Sex' a variables dummy
sex_dummy <- model.matrix(~Sex-1, data=titanic_train)
head(sex_dummy)

# Tratamiento de duplicados
duplicados <- duplicated(titanic_train)
numero_duplicados <- sum(duplicados) # Contar el número de duplicados
titanic_sin_duplicados <- titanic_train[!duplicated(titanic_train), ] # Eliminar duplicados

library(dplyr)
# Convertir variables categóricas a numéricas con la función recode
data <- data %>%
  mutate(Sex_numeric = recode(Sex, 
                              'male' = 0, 
                              'female' = 1))


head(data)

# Convertir la columna 'Sex' a una columna numérica con la función as.numeric
data$Sex_numeric <- as.numeric(factor(data$Sex))
head(data)


# Renombrar las columnas usando colnames
colnames(data)[colnames(data) == "PassengerId"] <- "ID"
colnames(data)[colnames(data) == "Pclass"] <- "Clase"
colnames(data)[colnames(data) == "Survived"] <- "Sobrevivencia"
colnames(data)

# Renombrar columnas usando la librería dyplr
library(dplyr)
data <- data %>%
  rename(
    ID = PassengerId,
    Clase = Pclass,
    Sobrevivio = Survived
  )

head(data)

# Renombrar usando la funcion data.table
install.packages("data.table")
library(data.table)
data <- as.data.table(data)
setnames(data, old = c("ID", "Clase", "Sobrevivio"), 
         new = c("ID", "Class", "Survival_Status"))
colnames(data)

# Subconjunto de datos
subset_data <- data[, c("ID", "Class", "Survival_Status")]
head(subset_data)

# Subconjunto con condiciones
subset_data <- data[data$Class == 1, ]
# Crear un subconjunto que incluya solo las filas donde los pasajeros son de clase 1 y sobrevivieron
subset_data <- data[data$Class == 1 & data$Survival_Status == 1, ]
head(subset_data)


# Crear un subconjunto con muestras aleatorias
set.seed(123)
sample_size <- floor(0.10 * nrow(data)) # usando el 10% de los datos




