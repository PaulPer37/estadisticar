library(readxl)

datos <- "~/Proyecto_Estadística1.xlsx"
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
#Prueba de medias

#Independecia del riego
table(datos1$hojas, datos1$Riego)


prop.table(table(datos1$hojas, datos1$Riego))

ggplot(data= datos1, aes(x= hojas, fill= Riego)) + 
  geom_bar()
?chisq.test
chisq.test(x= datos1$hojas, y= datos1$Riego)

#Creacion de tablas para la comparacion de medias y varianzas
#Prueba de varianzas

#H0: var_st == var_sa
#Ha: var_st != var_sa
st <- datos1 %>%
  filter(datos1$Sustrato == "T") %>%
  select(hojas) %>%
  unlist()

sa <- datos1 %>%
  filter(datos1$Sustrato == "A") %>%
  select(hojas) %>%
  unlist()
?var.test
var.test(x = st, y = sa,
         ratio = 1, 
         alternative = "two.sided",
         conf.level = .95)
#Prueba de dos medias
prueba_t <- t.test(st, sa, var.equal = TRUE)  # Usar var.equal=TRUE si se asume igualdad de varianzas

print(prueba_t)
#comprobar binomial
n_total <- 300 * 5  # Total de plantas (300 grupos, 5 plantas por grupo)
x_exitos <- sum(datos1$hojas == 3)  # Total de plantas con 3 hojas

# Probabilidad teórica de éxito (aquí debes colocar la probabilidad teórica esperada)
p_teorica <- 0.85  # Ejemplo, reemplaza con la probabilidad teórica real

# Realizar la prueba binomial
prueba_binomial <- binom.test(x = 1330, n = 1500, p = 0.85)

# Resultados de la prueba
print(prueba_binomial)
print(prueba_binomial$p.value)
# Interpretar resultados
if (prueba_binomial$p.value < 0.05) {
  cat("Los datos no se ajustan bien a la distribución binomial con la probabilidad teórica.\n")
} else {
  cat("Los datos se ajustan bien a la distribución binomial con la probabilidad teórica.\n")
}


            