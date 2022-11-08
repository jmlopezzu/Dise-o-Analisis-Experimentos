# Análisis de varianza de 1 factor con R
## link :: https://www.youtube.com/watch?v=sg1Nw7wCLgo

library(readxl)
library(stats)

df <- readxl::read_xlsx("taller_anova.xlsx")
tratamientos <- read_excel("taller_anova.xlsx")


tratamientos$Tratamiento <- as.character(tratamientos$Tratamiento)

tratamientos$Respuesta <- as.numeric(tratamientos$Respuesta)

tratamientos

modelo_1 <- lm(Respuesta ~ Tratamiento ,data = tratamientos)

anova(modelo_1)

#Analysis of variance aov <- anova
tratamientos_aov <- aov(Respuesta ~ Tratamiento , data = tratamientos)
tratamientos_aov

summary(tratamientos_aov)

#Usando R Console
# Mostrar varias graficas a la vez con la interfaz dividida
par(mfrow = c(2,2))

plot(tratamientos_aov)

boxplot(Respuesta ~ Tratamiento , data = tratamientos)
