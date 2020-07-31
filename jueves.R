## Limpiar el entorno de trabajo
rm(list = ls())
#### Cargar  los datos y limpiar los datos ####
cleve <- read.csv("cleve.csv",header = FALSE)
head(cleve)
cleve.names <- c("age", "sex", "cp", "trestbps", "chol","fbs", "restcg", "thalac",
                 "exang","oldpeak","slope", "ca",
                 "thal", "diagnostic")

names(cleve) <- cleve.names
class(cleve)
head(cleve)
## Vistazo a los datos
summary(cleve)
## Hay dos variables de tipo caractéres, "ca", "thal"

## Usar as.data.frame para definir el resultado como dataframe
## La función "apply()" a las dos variables para cambiarlas a 
## tipo numérico
cleve <- as.data.frame((apply(cleve, 2, as.numeric)))

## adjuntar los datos
attach(cleve)

summary(cleve)
## Las dos variables tienen NAs (valores ausentes)

#### Análisis de algunas variables ####

## Seleccionar individuos con edad >= 50
age[age >= 50]
## Cuántos individuos hay con edad >= 50
length(age[age >= 50])

# El número de individuos que tienen diagnóstico negativo
length(sex[diagnostic == 0])

## Cálculo de sd cuando hay NAs presentes
sd(thal, na.rm = TRUE)

apply(cleve[,11:13], 2, sd) # NAs presentes
sum(is.na(cleve))
apply(na.omit(cleve[,11:13]), 2, sd) # omitir NAs (6/303 <= 2%)

# La variable "slope" tiene sd un poco más grande.


#### Contingency tables ####

table(sex,diagnostic)

## Estimated frequencies
table(sex,diagnostic)/nrow(cleve)

## Redefinir "diagnostic = 1" si se detecta algún tipo 
## de problema de corazón

diagnostic[diagnostic > 0 ] <- 1

## Tabla de contingencia con frecuencias estimadas
table(sex,diagnostic)/nrow(cleve)

detach(cleve)
