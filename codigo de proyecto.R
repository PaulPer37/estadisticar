library(readxl)
file.choose()
datos <- "C:\\Users\\RUCO HOUSE\\OneDrive\\Desktop\\pipo\\espol\\s3\\esta\\Proyecto_Estadística1.xlsx"
excel_sheets(datos)
#lo que sea

datos1 <- read_excel(datos)
head(datos1)
library(ggplot2)
library(dplyr)
?"$"

hoja <- datos1$hojas
riego1 <- datos1$Riego
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




            