
# IMPORT DATA -------------------------------------------------------------

# Importar datos
datos <- read.csv("datos encuestas.csv", na.strings = "NA")
# Corregir clase de las variables
sapply(datos, class)
datos$razon.deuda.ingreso <- as.numeric(as.character(datos$razon.deuda.ingreso))


# LOGIT: PREDICTORS OF OVERINDEBTEDNESS -----------------------------------
names(datos)

# Regresión
logit.reg <- glm(sobreendeud ~ deuda + razon.deuda.ingreso + deudas + 
                      mora + tamano.hogar + ingreso.mensual, 
                 family = "binomial", data = datos)
# Reporte de resultados
summary(logit.reg)


# RELATIONSHIP WITH FINANCIAL HEALTH --------------------------------------

library(MASS)

# Financial health vs. food security (periodic)

# Frequency table
saludfin.fsperiod <- table(datos$salud.fciera, datos$inseg.alim.periodica)
orden.saludfin <- c("excelente", "bueno", "mas o menos", "pobre")
saludfin.fsperiod <- saludfin.fsperiod[orden.saludfin, ]
# Relative frequency table
r.saludfin.fsperiod <- scale(saludfin.fsperiod, center = FALSE,
                           scale = colSums(saludfin.fsperiod))
# Chi-squre test (null hypothesis: variables are independent)
chisq.test(saludfin.fsperiod)


# Financial health vs. food security (chronic)

# Frequency table
saludfin.fschron <- table(datos$salud.fciera, datos$inseg.alim.cronica)
saludfin.fschron <- saludfin.fschron[orden.saludfin, ]
# Relative frequency table
r.saludfin.fschron <- scale(saludfin.fschron, center = FALSE,
                             scale = colSums(saludfin.fschron))
# Chi-squre test (null hypothesis: variables are independent)
chisq.test(saludfin.fschron)


# Financial health vs. poverty

# Create poverty variable
ppiNac.avg <- mean(datos$PPI.nac)/100
datos <- datos[order(datos$ppi), ]
porc.ppi <- c()
for (i in 1:nrow(datos)) {
      porc.ppi[i] <- i / nrow(datos)
}
datos <- data.frame(datos, porc.ppi)
ppi.nac.cat <- ifelse(porc.ppi < ppiNac.avg, "Pobre", "No pobre")
datos <- data.frame(datos, ppi.nac.cat)

# Frequency table
saludfin.pov <- table(datos$salud.fciera, datos$ppi.nac.cat)
saludfin.pov <- saludfin.pov[orden.saludfin, ]
# Relative frequency table
r.saludfin.pov <- scale(saludfin.pov, center = FALSE,
                            scale = colSums(saludfin.pov))
# Chi-squre test (null hypothesis: variables are independent)
chisq.test(saludfin.pov)


# Financial health vs.sacrifices
saludfin.sacrif <- table(datos$salud.fciera, datos$sacrificios.inacept)
saludfin.sacrif <- saludfin.sacrif[orden.saludfin ,]
# Relative frequencies (relative to No or Si)
r.saludfin.sacrif <- scale(saludfin.sacrif, center = FALSE,
                           scale = colSums(saludfin.sacrif))
# Chi-square test (null hypothesis: variables are independent)
chisq.test(saludfin.sacrif)


# Financial health vs. sacrifice
saludfin.sacrif <- table(datos$salud.fciera, datos$sacrificios.inacept)
saludfin.sacrif <- saludfin.sacrif[orden.saludfin ,]
# Relative frequencies (relative to No or Si)
r.saludfin.sacrif <- scale(saludfin.sacrif, center = FALSE,
                           scale = colSums(saludfin.sacrif))
# Chi-square test (null hypothesis: variables are independent)
chisq.test(saludfin.sacrif)


# Financial health vs. overindebtedness
saludfin.oi <- table(datos$salud.fciera, datos$Sobreendeudamiento2)
orden.oi <- c("Green", "Yellow", "Red")
saludfin.oi <- saludfin.oi[orden.saludfin , orden.oi]
# Relative frequencies (relative to No or Si)
r.saludfin.or <- scale(saludfin.oi, center = FALSE,
                           scale = colSums(saludfin.oi))
# Chi-square test (null hypothesis: variables are independent)
chisq.test(saludfin.oi)
