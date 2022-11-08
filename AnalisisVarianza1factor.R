# Análisis de varianza de 1 factor con R
## link :: https://www.youtube.com/watch?v=sg1Nw7wCLgo

library(readxl)
library(stats)

df <- readxl::read_xlsx("cereales.xlsx")
cereales <- read_excel("cereales.xlsx")

cereales

modelo_1 <- lm(Tiamina ~ Grupo,data = cereales)

anova(modelo_1)

#Analysis of variance aov <- anova
cereales_aov <- aov(Tiamina ~ Grupo, data = cereales)
cereales_aov

summary(cereales_aov)

#Usando R Console
# Mostrar varias graficas a la vez con la interfaz dividida
par(mfrow = c(2,2))

plot(cereales_aov)

boxplot(Tiamina ~ Grupo, data = cereales)

