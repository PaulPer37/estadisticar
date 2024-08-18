library(readxl)
library(readr)
datos<- "~/datasets/Datos1.csv"
datos1 <- read_csv("Datos1.csv")
head(datos1)
library(ggplot2)
library(dplyr)
?"$"

hoja <- datos1$hojas
riego <- datos1$riego
sustrato <- datos1$sustrato
min(hoja)
max(hoja)
mean(hoja) #media de hojas = 2.48
sd(hoja) #desviacion estandar de hojas = 1.12
hist(hoja)
boxplot(hoja)
chisq.test(hoja)

#Independencia del sustrato
table(hoja, sustrato)


prop.table(table(hoja, sustrato))

ggplot(data= datos1, aes(x= hoja, fill= sustrato)) + 
  geom_bar()
chisq.test(x= hoja, y= sustrato)


#Independecia del riego
table(hoja, riego)


prop.table(table(hoja, riego))

ggplot(data= datos1, aes(x= hojas, fill= Riego)) + 
  geom_bar()
?chisq.test
chisq.test(x= hoja, y= riego)

#Creacion de tablas para la comparacion de medias y varianzas
#Prueba de varianzas

#H0: var_st == var_sa
#Ha: var_st != var_sa
st <- datos1 %>%
  filter(sustrato == "T") %>%
  select(hojas) %>%
  unlist()

sa <- datos1 %>%
  filter(sustrato == "A") %>%
  select(hojas) %>%
  unlist()
?var.test
var.test(x = st, y = sa,
         ratio = 1, 
         alternative = "two.sided",
         conf.level = .95)
#Prueba de dos medias
prueba_t <- t.test(st, sa, var.equal = FALSE)  

print(prueba_t)
#comprobar binomial
n_total <- 300 * 5  # Total de plantas (300 grupos, 5 plantas por grupo)
x_exitos <- sum(hoja == 3)  # Total de plantas con 3 hojas
print(x_exitos/n_total)
# Probabilidad teórica de éxito (aquí debes colocar la probabilidad teórica esperada)
p_teorica <- 0.1  # Ejemplo, reemplaza con la probabilidad teórica real

# Realizar la prueba binomial
prueba_binomial <- binom.test(x = x_exitos, n = 1500, p = 0.1)

# Resultados de la prueba
print(prueba_binomial)
print(prueba_binomial$p.value)
# Interpretar resultados
if (prueba_binomial$p.value < 0.05) {
  cat("Los datos no se ajustan bien a la distribución binomial con la probabilidad teórica.\n")
} else {
  cat("Los datos se ajustan bien a la distribución binomial con la probabilidad teórica.\n")
}
p_teorica <- 0.1  # Ejemplo, ajusta según sea necesario

# Inicializar el dataframe de resultados
resultados <- data.frame(Muestra = integer(), P_Valor = numeric(), Ajuste = character(), stringsAsFactors = FALSE)

# Obtener los valores únicos de "muestra"
grupos <- unique(datos1$muestra)

# Iterar sobre cada grupo
for (grupo in grupos) {
  # Filtrar los datos para el grupo actual
  datos_grupo <- datos1 %>% filter(muestra == grupo)
  
  # Contar el número de éxitos (plantas con 3 hojas)
  x_exitos <- sum(datos_grupo$hojas == 3)
  
  # Realizar la prueba binomial
  prueba_binomial <- binom.test(x = x_exitos, n = 5, p = 0.1)

  resultados <- rbind(resultados, data.frame(
    Muestra = grupo,
    P_Valor = prueba_binomial$p.value,
    Ajuste = ifelse(prueba_binomial$p.value < 0.05, "No ajusta", "Ajusta"),
    stringsAsFactors = FALSE
  ))
}
datos_grupo1 <- datos1 %>% filter(muestra == 1)

# Contar el número de éxitos (plantas con 3 hojas)
x_exitos1 <- sum(datos_grupo1$hojas == 3)
probabilidad_0_exitos <- dbinom(x=0, size = 5, prob = 0.1)
print(probabilidad_0_exitos)
prueba_binomial1 <- binom.test(x = 0, n = 5, p = 0.1)
print(prueba_binomial1)
P_Valor1 = prueba_binomial$p.value
print(P_Valor1)
# Mostrar los resultados
print(resultados)
