library(agricolae)
library(readxl)


df <- readxl::read_xlsx("cereales.xlsx")
cereales <- read_excel("cereales.xlsx")

cereales

cereales_aov <- aov(Tiamina ~ Grupo, data = cereales)

summary(cereales_aov)
TukeyHSD(cereales_aov)

# Honest Significant difference == TUKEY
HSD.test(cereales_aov, trt = "Grupo")
cereales_hsd <- HSD.test(cereales_aov, trt = "Grupo")
summary(cereales_hsd)
cereales_hsd


## LSD method :: DMS METODO minima diferencia significativa


cereales_lsd <- LSD.test(y=cereales_aov, trt = "Grupo")
cereales_lsd

su