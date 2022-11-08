library(agricolae)
library(stats)

# Duncan test 

radon <- read_excel("radon.xlsx")
radon$diametro <- as.factor(radon$diametro)

# Crear aov
radon_aov <- aov(radon ~ diametro, data = radon)
summary(radon_aov)

### NULLL --- RUN THE NEXT SECTION ON R deskstop
# Aplicar prueba

radon_duncan <- duncan.test(y=radon_aov, trt = "diámetro")
radon_duncan

# boxplot

boxplot(radon$radon ~ radon$diametro, main="boxplot chart", xlab="Diametro", ylab="Radon")

# Test de Newman keuls

radon_nk <- SNK.test(y = radon_aov, trt = "diámetro")
radon_nk


# Scheffe Test

radon_scheffe <- scheffe.test(y=radon_aov,group=TRUE,console=TRUE, alpha = 0.05, trt = "diámetro")
radon_scheffe
