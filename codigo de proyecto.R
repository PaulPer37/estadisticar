library(readxl)
file.choose()
datos <- "C:\\Users\\RUCO HOUSE\\OneDrive\\Desktop\\pipo\\espol\\s3\\esta\\estadisticar\\Proyecto_Estadística1.xlsx"
excel_sheets(datos)
#lo que sea que sea

datos1 <- read_excel(datos)
head(datos1)
library(ggplot2)
library(dplyr)
?"$"

hoja <- datos1$hojas
riego <- datos1$Riego
min(hoja)
max(hoja)
mean(hoja) #media de hojas = 2.48
sd(hoja) #desviacion estandar de hojas = 1.12
hist(hoja)
boxplot(hoja)
chisq.test(hoja)

#Independencia del sustrato
table(datos1$hojas, datos1$Sustrato)


prop.table(table(datos1$hojas, datos1$Sustrato))

ggplot(data= datos1, aes(x= hojas, fill= Sustrato)) + 
  geom_bar()
chisq.test(x= datos1$hojas, y= datos1$Sustrato)

#Independecia del riego
table(datos1$hojas, datos1$Riego)


prop.table(table(datos1$hojas, datos1$Riego))

ggplot(data= datos1, aes(x= hojas, fill= Riego)) + 
  geom_bar()
?chisq.test
chisq.test(x= datos1$hojas, y= datos1$Riego)

#Creacion de tablas para la comparacion de medias y varianzas

#comprobar binomial
n_total <- 300 * 5  # Total de plantas (300 grupos, 5 plantas por grupo)
x_exitos <- sum(datos1$hojas == 3)  # Total de plantas con 3 hojas

# Probabilidad teórica de éxito (aquí debes colocar la probabilidad teórica esperada)
p_teorica <- 0.85  # Ejemplo, reemplaza con la probabilidad teórica real

# Realizar la prueba binomial
prueba_binomial <- binom.test(x = 1300, n = 1500, p = 0.85)

# Resultados de la prueba
print(prueba_binomial)
print(prueba_binomial$p.value)
# Interpretar resultados
if (prueba_binomial$p.value < 0.05) {
  cat("Los datos no se ajustan bien a la distribución binomial con la probabilidad teórica.\n")
} else {
  cat("Los datos se ajustan bien a la distribución binomial con la probabilidad teórica.\n")
}


            