# Diseños anidados

anidado <- read_excel("anidado.xlsx")
anidado$Operador <- as.factor(anidado$Operador)
anidado$Maquina <- as.factor(anidado$Maquina)
anidado$Muestra <- as.factor(anidado$Muestra)
anidado_aov <- aov(Acabado ~ Maquina + Maquina/Operador, data = anidado)
summary(anidado_aov)

# Taller dis. anidado 

## Punto 1 

anidado <- read_excel("tall_diseños_anidados.xlsx", sheet ="Hoja1")
anidado

anidado$Metodo <- as.factor(anidado$Metodo)
anidado$Proceso <- as.factor(anidado$Proceso)
anidado_aov <- aov(Eficiencia ~ Metodo + Metodo/Proceso, data = anidado)
summary(anidado_aov)


## Punto 2

anidado <- read_excel("tall_diseños_anidados.xlsx", sheet ="Hoja2")
anidado

anidado$Mezcla <- as.factor(anidado$Mezcla)
anidado$Catalizador <- as.factor(anidado$Catalizador)

anidado_aov <- aov(Concentracion ~  Catalizador + Catalizador/Mezcla, data = anidado)
summary(anidado_aov)

# Parcial

parcial <- read_excel("parcial_diseño_exp-2.xlsx", sheet="Hoja 1")
parcial

parcial$Fabrica <- as.factor(parcial$Fabrica)
parcial$Modulo <- as.factor(parcial$Modulo)

parcial_aov <- aov(Eficiencia ~ Fabrica * Fabrica/Modulo, data=parcial)
summary(parcial_aov)




