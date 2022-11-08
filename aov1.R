# Análisis de varianza de 1 factor con R
## link :: https://www.youtube.com/watch?v=sg1Nw7wCLgo

library(readxl)
library(stats)

df <- readxl::read_xlsx("aov_taller.xlsx")
tratamientos <- read_excel("aov_taller.xlsx")


tratamientos$Muestra <- as.factor(tratamientos$Tratamiento)

tratamientos$Rendimiento<- as.numeric(tratamientos$Respuesta)

tratamientos

modelo_1 <- lm(Rendimiento ~ Muestra ,data = tratamientos)

anova(modelo_1)

#Analysis of variance aov <- anova
tratamientos_aov <- aov(Rendimiento ~ Muestra , data = tratamientos)
tratamientos_aov

summary(tratamientos_aov)

#Usando R Console
# Mostrar varias graficas a la vez con la interfaz dividida
par(mfrow = c(2,2))

plot(tratamientos_aov)

boxplot(Rendimiento ~ Muestra , data = tratamientos)
