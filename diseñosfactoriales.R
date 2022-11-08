library(readxl)
library(broom)

# Diseño factorial 2^2
factorial1 <- read_excel("factorial_2_a_la_2.xlsx", sheet = "ejemplo1")
factorial1

factorial1_aov <- aov(Rendimiento ~ Reactivo * Catalizador, data=factorial1)
summary(factorial1_aov)


factorial2 <- read_xlsx("factorial_2_a_la_2.xlsx", sheet="ejemplo2")
factorial2


factorial2_aov <- aov(Vibracion ~ Velocidad*Ranura, data=factorial2)
summary(factorial2_aov)

??aov

# Diseño factorial 2^3

factorial3 <- read_xlsx("factorial_2_a_la_3.xlsx", sheet="ejemplo1")
factorial3


factorial3_aov <- aov(Vida ~ Velocidad*Geometria*Angulo, data=factorial3)
summary(factorial3_aov)




factorial4 <- read_xlsx("factorial_2_a_la_3.xlsx", sheet="ejemplo2")
factorial4


factorial4_aov <- tidy.aov(exquisitez ~ Molde*Herramienta*Harina, data=factorial4)
summary(factorial4_aov)

# Taller Diseños factoriales 2^3


Tdis <- read_xlsx("Diseños2^k.xlsx", sheet="Hoja1")
Tdis


tdis_aov <- aov(resultado ~ a*b*c, data=Tdis)
summary(tdis_aov)


# punto 2


Tdis2 <- read_xlsx("Diseños2^k.xlsx", sheet="Hoja2")
Tdis2


tdis_aov2 <- aov(resultado ~ a*b*c, data=Tdis2)
summary(tdis_aov2)

# Parcial Diseños Factoriales

parc <- read_excel("parcial_diseno.xlsx", sheet="H1")
parc


parc_aov <- aov(tiempo ~ a*b*c, data=parc)
summary(parc_aov)


#Punto 2


parc1 <- read_excel("parcial_diseno.xlsx", sheet="H4")
parc1


parc1_aov <- aov(rendimiento ~ a*b*c, data=parc1)
summary(parc1_aov)
