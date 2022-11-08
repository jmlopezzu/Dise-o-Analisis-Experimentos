library(rsm)
library(readxl)

# Ej 1
ej1 <- read_excel("nivel.xlsx", sheet="ejemplo1")
ej1_lm <- lm(Rendimiento ~ Catalizador * Reactivo, data = ej1)
par(mfrow = c(1,3))
image(ej1_lm, Catalizador ~ Reactivo)
contour(ej1_lm, Catalizador ~ Reactivo)
persp(ej1_lm, Catalizador ~ Reactivo, zlab = "Rendimiento")

# Ej 2

ej2 <- read_excel("nivel.xlsx", sheet="ejemplo2")
ej2_lm <- lm(Vibracion ~ Ranura * Velocidad, data = ej2)
image(ej2_lm, Velocidad ~ Ranura)
contour(ej2_lm, Velocidad ~ Ranura)
persp(ej2_lm, Velocidad ~ Ranura, zlab = "Vibracion")


# TALLER 

tall <- read_excel("taller_curvas.xlsx", sheet = "H1")
tall

tall_lm <- lm(Espesor ~ Rapidez_f * Tiempo_d, data = tall)
image(tall_lm, Tiempo_d~Rapidez_f)
contour(tall_lm, Tiempo_d~Rapidez_f)
persp(tall_lm, Tiempo_d~Rapidez_f, zlab="Espesor")

summary(tall_lm)

# Taller 2


tall2 <- read_excel("taller_curvas.xlsx", sheet = "H2")
tall2

tall2_lm <- lm(Crecimiento ~ M_cultivo * Tiempo, data = tall2)
tall2_lm

par(mfrow = c(1,3))
image(tall2_lm, Tiempo ~ M_cultivo)
contour(tall2_lm, Tiempo ~ M_cultivo)
persp(tall2_lm,Tiempo ~ M_cultivo, zlab="Crecimiento")





# Parcf

parcf <- read_excel("parcial_diseño_exp.xlsx", sheet="Hoja 2")
parcf

parcf_lm <- lm(tiempo ~ material * diseño, data = parcf)
summary(parcf_lm)

parcf_aov <- aov(tiempo ~ material + diseño + material/diseño, data=parcf)
summary(parcf_aov)
parcf$material <- as.factor(parcf$material)
parcf$diseño <- as.factor(parcf$diseño)

parcf_lm
par(mfrow = c(1,3))
image(parcf_lm, material ~ diseño)
contour(parcf_lm, material ~ diseño)
persp(parcf_lm, material ~ diseño, zlab="tiempo")



# Punto 3 

parcial3 <- read_excel("parcial_diseño_exp.xlsx", sheet="Hoja 3")
parcial3

parcial3_lm <- lm(desperdicio ~ metodo * equipo, data=parcial3)
summary(parcial3_lm)

parcial3_aov <- aov(desperdicio ~ metodo * equipo, data=parcial3)
summary(parcial3_aov)
parcial3$metodo <- as.factor(parcial3$metodo)
parcial3$equipo <- as.factor(parcial3$equipo)


image(parcial3_lm, metodo ~ equipo)
contour(parcial3_lm, metodo ~ equipo)
persp(parcial3_lm, metodo ~ equipo, zlab ="desperdicio")

