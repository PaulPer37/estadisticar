library(readxl)
library(readr)
datos<- "~/datasets/Datos1.csv"
datos1 <- read_csv("C:/Users/RUCO HOUSE/OneDrive/Desktop/pipo/espol/s3/esta/estadisticar/Datos1.csv")
head(datos1)
library(ggplot2)
library(dplyr)
?"$"
datosp <- read_excel("C:/Users/RUCO HOUSE/OneDrive/Desktop/pipo/espol/s3/esta/estadisticar/Prueba piloto.xlsx")
head(datosp)
hojap <- datosp$hojas
frecuencias <- table(hojap)
frecuencias_interes <- frecuencias[names(frecuencias) %in% c("1", "2", "3")]
total <- sum(frecuencias)
probabilidades <- frecuencias_interes / total
frecuencias_interes
probabilidades
prob <- 0.1167
promedio_teorico <- mean(hojap)


hoja <- datos1$hojas
riego <- datos1$riego
sustrato <- datos1$sustrato
min(hoja)
max(hoja)
mean(hoja) 
sd(hoja) 
hist(hoja)
boxplot(hoja)


#Prueba de hipotesis igualdad de medias: poblacional, muestral
prueba_varianza <- var.test(hoja, hojap)
prueba_varianza
promedio_hoja <- mean(hoja, na.rm = TRUE)
promedio_hojap <- mean(hojap, na.rm = TRUE)
promedio_hoja
promedio_hojap
prueba_t <- t.test(hoja, hojap, var.equal = TRUE)
prueba_t

#Frecuencias poblacinal con muestral
frecuencias_hoja <- table(datos1$hojas)
frecuencias_hojap <- table(hojap)

# Convertir las frecuencias absolutas en frecuencias relativas
frecuencia_relativa_hoja <- frecuencias_hoja / sum(frecuencias_hoja)
frecuencia_relativa_hojap <- frecuencias_hojap / sum(frecuencias_hojap)

# Crear un dataframe con las frecuencias relativas
df_frecuencias <- data.frame(
  Valor = as.numeric(names(frecuencia_relativa_hoja)),
  Frecuencia_Relativa_hoja = as.numeric(frecuencia_relativa_hoja),
  Frecuencia_Relativa_hojap = as.numeric(frecuencia_relativa_hojap)
)
df_frecuencias
# Reorganizar datos para facilitar el gráfico
df_frecuencias_long <- df_frecuencias %>%
  pivot_longer(cols = c(Frecuencia_Relativa_hoja, Frecuencia_Relativa_hojap), 
               names_to = "Variable", 
               values_to = "Frecuencia_Relativa")

# Crear el gráfico de barras de frecuencias relativas
ggplot(df_frecuencias_long, aes(x = as.factor(Valor), y = Frecuencia_Relativa, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Número de Hojas", y = "Frecuencia Relativa", 
       title = "Comparación de Frecuencias Relativas de Hojas entre 'hoja' y 'hojap'",
       fill = "Variable") +
  theme_minimal()

#Tabla de combinaciones
library(dplyr)

# Crear un dataframe con las variables relevantes
datos <- data.frame(hojas = datos1$hojas, 
                    riego = datos1$riego, 
                    sustrato = datos1$sustrato)

# Crear una tabla con los valores de hojas para cada combinación de riego y sustrato
tabla_combinaciones <- datos %>%
  group_by(riego, sustrato) %>%
  summarize(valores = list(hojas), .groups = 'drop')

#Independienza sustrato
table(hoja, sustrato)
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

prob <- 0.1167  


prueba_binomial <- binom.test(x = x_exitos, n = 1500, p = 0.1167)


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
  
  prueba_binomial <- binom.test(x = x_exitos, n = 5, p = prob)

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


valores <- unique(datos1$hojas)

# Calcula la frecuencia de cada valor
frecuencias <- table(datos1$hojas)
frecuencias
probabilidades <- frecuencias / sum(frecuencias)
probabilidades


atos <- data.frame(hojas = datos1$hojas, 
                    riego = datos1$riego, 
                    sustrato = datos1$sustrato)

# Crear los boxplots para cada combinación de riego y sustrato  
ggplot(datos, aes(x = interaction(riego, sustrato), y = hojas, fill = interaction(riego, sustrato))) +
  geom_boxplot() +
  labs(x = "Combinación de Riego y Sustrato", y = "Número de Hojas", fill = "Combinación") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Prueba de Hipotesis de Superioridad Combinazión de Arena con Riego distribuido
# Filtrar la combinación sustrato A y riego B
comb_ab <- datos %>% filter(sustrato == "A" & riego == "B")
otras_combinaciones <- datos %>% filter(!(sustrato == "A" & riego == "B"))
prueba_varianza <- var.test(comb_ab$hojas, otras_combinaciones$hojas)
prueba_varianza
prueba_t_combinaciones <- t.test(comb_ab$hojas, otras_combinaciones$hojas, var.equal = FALSE)
print(prueba_t_combinaciones)

#Medias entre cada subgrupo
# Crear todas las combinaciones de riego y sustrato
combinaciones <- unique(datos %>% select(riego, sustrato))

# Crear un dataframe para almacenar los resultados
resultados_ttest <- data.frame(
  Combinacion1 = character(),
  Combinacion2 = character(),
  Media1 = numeric(),
  Media2 = numeric(),
  P_Valor = numeric(),
  Diferencia_Significativa = character(),
  stringsAsFactors = FALSE
)

# Realizar pruebas t entre cada par de combinaciones
for (i in 1:nrow(combinaciones)) {
  for (j in (i+1):nrow(combinaciones)) {
    comb1 <- combinaciones[i, ]
    comb2 <- combinaciones[j, ]
    
    # Filtrar los datos para las combinaciones
    grupo1 <- datos %>% filter(riego == comb1$riego & sustrato == comb1$sustrato) %>% select(hojas)
    grupo2 <- datos %>% filter(riego == comb2$riego & sustrato == comb2$sustrato) %>% select(hojas)
    
    # Realizar la prueba t
    prueba_t <- t.test(grupo1$hojas, grupo2$hojas, var.equal = FALSE)
    
    # Almacenar los resultados
    resultados_ttest <- rbind(resultados_ttest, data.frame(
      Combinacion1 = paste(comb1$riego, comb1$sustrato, sep = " - "),
      Combinacion2 = paste(comb2$riego, comb2$sustrato, sep = " - "),
      Media1 = mean(grupo1$hojas, na.rm = TRUE),
      Media2 = mean(grupo2$hojas, na.rm = TRUE),
      P_Valor = prueba_t$p.value,
      Diferencia_Significativa = ifelse(prueba_t$p.value < 0.05, "Sí", "No")
    ))
  }
}

# Mostrar los resultados
print(resultados_ttest)

#Probabilididades binomial

n <- 5  
p <- prob  

probabilidades <- dbinom(0:n, size = n, prob = p)

tabla_probabilidades <- data.frame(
  Exitos = 0:n,
  Probabilidad = probabilidades
)

# Mostrar la tabla
print(tabla_probabilidades)

