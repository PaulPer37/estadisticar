library(readxl)
library(readr)
datos<- "~/datasets/Datos1.csv"
datos1 <- read_csv("C:/Users/RUCO HOUSE/OneDrive/Desktop/pipo/espol/s3/esta/estadisticar/Datos1.csv")
head(datos1)
library(ggplot2)
library(dplyr)
?"$"

hoja <- datos1$hojas
riego <- datos1$riego
sustrato <- datos1$sustrato
min(hoja)
max(hoja)
mean(hoja) 
sd(hoja) 
hist(hoja)
boxplot(hoja)


#Independencia del sustrato
table(hoja, sustrato)


prop.table(table(hoja, sustrato))

ggplot(data= datos1, aes(x= hoja, fill= sustrato)) + 
  geom_bar()
chisq.test(x= hoja, y= sustrato)


#Independecia del riego
table(hoja, riego)


prop.table(table(hoja, riego))

ggplot(data= datos1, aes(x= hoja, fill= riego)) + 
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
#Prueba de varianza para riego
ra <- datos1 %>%
  filter(riego == "A") %>%
  select(hojas) %>%
  unlist()

rb <- datos1 %>%
  filter(riego == "B") %>%
  select(hojas) %>%
  unlist()
?var.test
var.test(x = ra, y = rb,
         ratio = 1, 
         alternative = "two.sided",
         conf.level = .95)
#Prueba de dos medias riego
prueba_t2 <- t.test(ra, rb, var.equal = TRUE)  

print(prueba_t2)
#Prueba de dos medias sustrato
prueba_t <- t.test(st, sa, var.equal = FALSE)  

print(prueba_t)
#comprobar binomial para muestra de 1500
n_total <- 300 * 5  
x_exitos <- sum(hoja == 3)
print(x_exitos/n_total)

p_teorica <- 0.1  


prueba_binomial <- binom.test(x = x_exitos, n = 1500, p = 0.1)


print(prueba_binomial)
print(prueba_binomial$p.value)

if (prueba_binomial$p.value < 0.05) {
  cat("Los datos no se ajustan bien a la distribución binomial con la probabilidad teórica.\n")
} else {
  cat("Los datos se ajustan bien a la distribución binomial con la probabilidad teórica.\n")
}
#Prueba binomial para cada grupo, ver si cumple distribución
resultados <- data.frame(Muestra = integer(), P_Valor = numeric(), Ajuste = character(), stringsAsFactors = FALSE)

grupos <- unique(datos1$muestra)

# Iterar sobre cada grupo
for (grupo in grupos) {
  datos_grupo <- datos1 %>% filter(muestra == grupo)
  x_exitos <- sum(datos_grupo$hojas == 3)
  
  prueba_binomial <- binom.test(x = x_exitos, n = 5, p = 0.1)

  resultados <- rbind(resultados, data.frame(
    Muestra = grupo,
    P_Valor = prueba_binomial$p.value,
    Ajuste = ifelse(prueba_binomial$p.value < 0.05, "No ajusta", "Ajusta"),
    stringsAsFactors = FALSE
  ))
}
conteos_ajustes <- table(resultados$Ajuste)
print(conteos_ajustes)
print(resultados)
#Normal

observados <- table(datos1$hojas)
observados <- as.numeric(observados)
names(observados) <- as.character(names(table(datos1$hojas)))
print(observados)

media <- mean(datos1$hojas)
sd <- sd(datos1$hojas)
print(media)
print(sd)

intervalos <- c(0.5, 1.5, 2.5, 3.5)

esperados <- sapply(1:(length(intervalos) - 1), function(i) {
  pnorm(intervalos[i+1], mean = media, sd = sd) - pnorm(intervalos[i], mean = media, sd = sd)
})
esperados <- esperados * sum(observados)
print(esperados)

prueba_chisq <- chisq.test(x = observados, p = esperados / sum(esperados))
print(prueba_chisq)

if (prueba_chisq$p.value < 0.05) {
  cat("Los datos no se ajustan a la distribución normal")
} else {
  cat("Los datos se ajustan a la distribución normal")
}

#Tabla
datos <- data.frame(
  hojas = hoja,
  riego = riego,
  sustrato = sustrato
)

datos <- datos %>%
  mutate(exito = ifelse(hojas == 3, 1, 0))

resumen <- datos %>%
  group_by(riego, sustrato) %>%
  summarise(
    Total = n(),
    Exitos = sum(exito),
    Tasa_Exito = Exitos / Total
  )

print(resumen)

ggplot(resumen, aes(x = interaction(riego, sustrato), y = Tasa_Exito, fill = interaction(riego, sustrato))) +
  geom_bar(stat = "identity") +
  labs(x = "Combinación de Riego y Sustrato", y = "Tasa de Éxito", title = "Tasa de Éxito por Combinación de Riego y Sustrato") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


