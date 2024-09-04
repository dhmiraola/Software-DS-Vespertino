data <- titanic3
head(data)


# Verificar los valores faltantes
colSums(is.na(data))


# Eliminar las filas que contienen valores faltantes
eliminar <- titanic3
titanic_no_na <- na.omit(eliminar)
colSums(is.na(titanic_no_na))


# Imputar por la media en la columna 'age'
media <- titanic3
media$age[is.na(media$age)] <- mean(media$age, na.rm = TRUE)
colSums(is.na(media))
summary(media$age)


# Imputar la mediana en la columna Age
mediana <- titanic3
mediana$age[is.na(mediana$age)] <- median(mediana$age, na.rm = TRUE)
colSums(is.na(mediana))
summary(mediana$age)

# Calcular la moda de la columna Embarked
moda <- titanic3  # Crear una copia del dataframe original
moda_embarked <- names(which.max(table(moda$embarked)))
moda$embarked[is.na(moda$embarked)] <- moda_embarked
table(moda$embarked)


# Imputar variables categoricas mediante 'Sin Informacion'
imputar <- titanic3
imputar$embarked[is.na(imputar$embarked)] <- "Sin Información"
table(imputar$embarked)


# Imputar mediante método knn
install.packages("VIM")
library(VIM)
knn <- titanic3
imputed_data_knn <- kNN(knn, variable = c("embarked"), k = 5)
table(imputed_data_knn$embarked)
sum(is.na(imputed_data_knn$embarked))


# Imputar mediante la librería 'mice'
install.packages("mice")
library(mice)
mice_ <- titanic3
# Determinar los métodos de imputación adecuados para cada tipo de variable
# Ejemplo:
# - 'logreg' para variables binarias (dos categorías)
# - 'polyreg' para variables categóricas con más de dos categorías
# - 'pmm' para variables numéricas
imputation_methods <- make.method(mice_)
imputation_methods["embarked"] <- "polyreg"  # Embarked tiene más de dos categorías
imputation_methods["sex"] <- "logreg"        # Sex es binaria
imputed_data <- mice(mice_, m = 5, method = imputation_methods, maxit = 5, seed = 500)
complete_data <- complete(imputed_data)
colSums(is.na(complete_data))






