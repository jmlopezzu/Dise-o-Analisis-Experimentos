# Bloques aleatorizados

library(agricolae)

# Randomized complete block design

bloque <- design.rcbd(trt=1:4, r=4, seed=42, serie=0)
bloque$sketch

#check :: Numeros Pseudoaleatorios

# Analysis

# ANOVA

bloques1 <- read_excel("bloques_alea.xlsx", sheet="Ejemplo1")
bloques1$Rollo <- as.factor(bloques1$Rollo)
bloque1_aov <- aov(Resistencia ~ Quimico + Rollo, data=bloques1)

summary(bloque1_aov)

bloque1_aov

# Cuadrados Latinos

## EX.
latino <- design.lsd(LETTERS[1:5], serie = 2, seed = 42)
latino$sketch

# EX.2.
latinos <- read_excel("cuad_latinos.xlsx")
latinos$Lote <- as.factor(latinos$Lote)
latinos$Dia <- as.factor(latinos$Dia)

latinos_aov <- aov(Tiempo ~ Lote + Dia + Ingrediente, data=latinos)
summary(latinos_aov)

# Cuadrados Grecolatinos

## EX.
greco <- design.graeco(LETTERS[1:5],1:5, seed = 42, serie=2)
greco$sketch

## EX. 2.

grecolat <- read_excel("cuad_grecolat.xlsx")
grecolat$Lote <- as.factor(grecolat$Lote)
grecolat$Acido <- as.factor(grecolat$Acido)
greco_aov <- aov(Rendimiento ~ Ingrediente+ Tiempo + Acido + Lote, data=grecolat)
summary()


# DF :: Orden Operador Metodo Sitio Tiempo

grecolat <- read_excel("cuad_grecolat.xlsx", sheet = "Ejemplo1")
grecolat
grecolat$Orden <- as.factor(grecolat$Orden)
grecolat$Operador <- as.factor(grecolat$Operador)
greco_aov <- aov(Tiempo ~ Orden + Operador + Metodo + Sitio, data=grecolat)
summary(greco_aov)

# DF :: Lote Acido Tiempo Ingrediente Rendimiento

greco <- read_excel("cuad_grecolat.xlsx", sheet = "Ejemplo2")
greco$Lote <- as.factor(greco$Lote)
greco$Acido <- as.factor(greco$Acido)
greco_aov <- aov(Rendimiento ~ Ingrediente + Tiempo + Acido + Lote, data = greco)
summary(greco_aov)


greco <- read_excel("cuad_grecolat.xlsx", sheet = "Ejemplo2")
greco
greco$Lote <- as.factor(greco$Lote)
greco$Acido <- as.factor(greco$Acido)
greco_aov <- aov(Rendimiento ~ Ingrediente + Tiempo + Acido + Lote, data = greco)
summary(greco_aov)


df3 <- read_excel("taller_bloques.xlsx", sheet="Hoja 2")
summary(df3)
df3$sueno <- as.factor(df3$sueno)
df3$suplemento <- as.factor(df3$suplemento)
taller_aov <- aov(Peso_perdido ~ dieta + ejercicio + suplemento + sueno, data=df3)
summary(taller_aov)


df4 <- read_excel("taller_bloques.xlsx", sheet="Hoja 1")
summary(df4)
df4$Percent_mutacion<- as.factor(df4$Percent_mutacion)
df4$puntos_cruce <- as.factor(df4$puntos_cruce)
taller2_aov <- aov(Rendimiento ~ Percent_mutacion + puntos_cruce + Criterio_parada + Poblacion, data=df4)
summary(taller2_aov)

# Parcial


parc3 <- read_excel("parcial_diseno.xlsx", sheet="H3")
parc3

parc3$pan <-as.factor(parc3$pan)
parc3$queso <-as.factor(parc3$queso)
parc3_aov <- aov(Calidad ~ pan + queso + carne + tocineta, data=parc3)
summary(parc3_aov)


#Punto 2 
parc2 <- read_excel("parcial_diseno.xlsx", sheet="H2")
parc2

parc2$Día <-as.factor(parc2$Día)
parc2$Operario<-as.factor(parc2$Operario)

parc2_aov <- aov(Rendimiento ~ Proceso + Método + Operario + Día, data=parc2)
summary(parc2_aov)




### PROYECTO

# EJ
greco <- read_excel("cuad_grecolat.xlsx", sheet = "Ejemplo2")
greco
greco$Lote <- as.factor(greco$Lote)
greco$Acido <- as.factor(greco$Acido)
greco_aov <- aov(Rendimiento ~ Ingrediente + Tiempo + Acido + Lote, data = greco)
summary(greco_aov)

# SLN
#write.csv(greco, "greco.csv")

Proyecto_Diseño <- read_excel("proyecto_dis_exp.xlsx")
Proyecto_Diseño 
Proyecto_Diseño$Lote <- as.factor(Proyecto_Diseño$Lote)
Proyecto_Diseño$Concentracion_nButanol<- as.factor(Proyecto_Diseño$Concentracion_nButanol)
Proyecto_Diseño_aov <- aov(Rendimiento ~ Color + Tiempo + Concentracion_nButanol+ Lote, data = Proyecto_Diseño)
summary(Proyecto_Diseño_aov )




