################################################################################
'
Proyecto: Econometria 2 - INTEC
Que hace: Estima educacion como señalizacion
Autores: Carlos Contreras 1099739, Mario Avila
Ultima actualizacion: 10 Julio 2022
'
################################################################################

#Importar y limpiar la base de datos

##establece carpeta de trabajo
## cambiar \ a / para computadoras windows
setwd(dir = "~/Econometría/Trabajo Final Ecuación de Mincer/R script final")

## importar base de datos
library(readxl)
base1 <- read_excel("ENFT_abril2021_Mincer_sheepskin.xlsx")



################################################################################

#Ecuacion Mincer

##ingreso

###histograma con valor absoluto
options(scipen=999)
jpeg(filename = "histogram_ingreso_absoluto.jpeg")
hist(base1$EFT_INGRESO_MENSUAL, main = "", ylab = "Frecuencia", xlab = "Ingreso mensual",
     col = "firebrick")
dev.off()

### crear logaritmo natural de ingreso
base1$ln_ingreso <- log(base1$EFT_INGRESO_MENSUAL)

### personas que gana cero NA
base1$ln_ingreso[base1$EFT_INGRESO_MENSUAL<1] <- NA

### histograma del ln ingreso
jpeg(filename = "histogram_ingreso_ln.jpeg")
hist(base1$ln_ingreso, main = "", ylab = "Frecuencia", xlab = "Ln(ingreso mensual)",
     col = "firebrick")
dev.off()

# el logaritmo natural del ingreso tiene una distribución normal.
## anos educativos

### tabla de frecuencia del nivel educativo maximo alcazando y ultimo ano alcanzado en ese nivel educativo
table(base1$EFT_ULT_NIVEL_ALCANZADO,base1$EFT_ULT_ANO_APROBADO)

### computando obervacion vacia hay para cada variable (TRUE)
table(is.na(base1$EFT_ULT_NIVEL_ALCANZADO),is.na(base1$EFT_ULT_ANO_APROBADO)) #1864 TRUE

#### sacar todos los individuos que no tenga informacion de educacion
library(dplyr)
base2 <- filter(base1,!is.na(base1$EFT_ULT_NIVEL_ALCANZADO))


### computando obervacion vacia hay para cada variable (TRUE)
table(is.na(base2$EFT_ULT_NIVEL_ALCANZADO),is.na(base2$EFT_ULT_ANO_APROBADO)) #0

### crear anos educativos
base2$educ <- NA
base2$educ[base2$EFT_ULT_NIVEL_ALCANZADO==2] <- base2[base2$EFT_ULT_NIVEL_ALCANZADO==2,]$EFT_ULT_ANO_APROBADO #primaria
base2$educ[base2$EFT_ULT_NIVEL_ALCANZADO==3] <- base2[base2$EFT_ULT_NIVEL_ALCANZADO==3,]$EFT_ULT_ANO_APROBADO + 8 #secundaria
base2$educ[base2$EFT_ULT_NIVEL_ALCANZADO==4] <- base2[base2$EFT_ULT_NIVEL_ALCANZADO==4,]$EFT_ULT_ANO_APROBADO + 8 #vocaional
base2$educ[base2$EFT_ULT_NIVEL_ALCANZADO==5] <- base2[base2$EFT_ULT_NIVEL_ALCANZADO==5,]$EFT_ULT_ANO_APROBADO + 12 #unversitario
base2$educ[base2$EFT_ULT_NIVEL_ALCANZADO==6] <- base2[base2$EFT_ULT_NIVEL_ALCANZADO==6,]$EFT_ULT_ANO_APROBADO + 16 #postuniversitario
base2$educ[base2$EFT_ULT_NIVEL_ALCANZADO==7] <- 0 #ninguno
base2$educ[base2$EFT_ULT_NIVEL_ALCANZADO==1] <- 0 #ninguno
## años de experiencia laboral con edad de retiro a las 65 años e inicio a la 
## educacion formal a las 5 años
base2$exp <- base2$EFT_EDAD - base2$educ - 5
base2$exp <- ifelse(base2$EFT_EDAD>65,65-base2$educ-5,base2$EFT_EDAD-base2$educ-5)
base2$exp[base2$exp<0]<- 0
base2$exp_2 <- base2$exp^2 #experiencia cuadratica


################################################################################

## Crear variables con fines comparativos

base2$casado <- 0
base2$casado[base2$EFT_ESTADO_CIVIL==2] <- 1
base2$casado[base2$EFT_ESTADO_CIVIL==6] <- 0

base2$divorciado <- 0
base2$divorciado[base2$EFT_ESTADO_CIVIL==3] <- 1
base2$divorciado[base2$EFT_ESTADO_CIVIL==6] <- 0

base2$viudo <- 0
base2$viudo[base2$EFT_ESTADO_CIVIL==5] <- 1
base2$viudo[base2$EFT_ESTADO_CIVIL==6] <- 0

base2$separado <- 0
base2$separado[base2$EFT_ESTADO_CIVIL==4] <- 1
base2$separado[base2$EFT_ESTADO_CIVIL==6] <- 0

base2$unionlibre <- 0
base2$unionlibre[base2$EFT_ESTADO_CIVIL==1] <- 1
base2$unionlibre[base2$EFT_ESTADO_CIVIL==6] <- 0

base2$mujer <- 0
base2$mujer[base2$EFT_SEXO==1] <- 0
base2$mujer[base2$EFT_SEXO==2] <- 1

### tabla de frencuencia ano educacion
table(base2$educ)


### grafico 
hist(base2$exp) 
plot(base2$EFT_EDAD,base2$EFT_EDAD)

################################################################################
#Estimacion ecuacion Mincer Weights

##regresion
summary(lm(ln_ingreso ~ educ + exp + exp_2, data = base2))
summary(lm(ln_ingreso ~ educ + exp + exp_2, data = base2, weights = EFT_FACTOR_EXP_ANUAL)) #con factor de expansion

modelomincerw <- lm(ln_ingreso ~ educ + exp + exp_2, data = base2, weights = EFT_FACTOR_EXP_ANUAL)

##importar regresion a html
library(stargazer)
stargazer(modelomincerw, type = "html", out= "Regresion_Mincerw.html",
          dep.var.labels = "Ln(ingreso laboral)",
          covariate.labels = c("Años educativos","Experiencia laboral", 
                               "Experiencia laboral 2", "Constante"))

################################################################################
# Variables para efecto sheepskin o efecto diploma

## primaria
table(base2$COMPLETO_EDUCACION_PRIMARIA)
table(is.na(base2$COMPLETO_EDUCACION_PRIMARIA)) #7492 vacia
base2$dip_primaria <- NA
base2$dip_primaria[base2$COMPLETO_EDUCACION_PRIMARIA==2] <- 0
base2$dip_primaria[base2$COMPLETO_EDUCACION_PRIMARIA==1] <- 1 #9114
table(base2$dip_primaria)
sum(table(base2$dip_primaria)) #17369

## secundaria
table(base2$COMPLETO_EDUCACION_SECUNDARIA)
table(is.na(base2$COMPLETO_EDUCACION_SECUNDARIA)) #15748 vacias
base2$dip_secundaria <- NA
base2$dip_secundaria[base2$COMPLETO_EDUCACION_SECUNDARIA==2 | !is.na(base2$dip_primaria)] <- 0
base2$dip_secundaria[base2$COMPLETO_EDUCACION_SECUNDARIA==1] <- 1 #5284
table(base2$dip_secundaria)
sum(table(base2$dip_secundaria)) #17371

## tecnico superior
table(base2$ESTA_INSCRITO_EN,base2$COMPLETO_PROGRAMA_INSCRITO)
table(is.na(base2$ESTA_INSCRITO_EN),is.na(base2$COMPLETO_PROGRAMA_INSCRITO)) #19559 vacias
base2$dip_tec_sup <- NA
base2$dip_tec_sup[!is.na(base2$dip_primaria)] <- 0
base2$dip_tec_sup[base2$ESTA_INSCRITO_EN==2 & base2$COMPLETO_PROGRAMA_INSCRITO==1] <- 1
base2$dip_tec_sup[base2$ESTA_INSCRITO_EN==3 & base2$COMPLETO_PROGRAMA_INSCRITO==1] <- 1  #100
table(base2$dip_tec_sup)
sum(table(base2$dip_tec_sup)) #17369

## carrera universitaria
base2$dip_licenciatura <- NA
base2$dip_licenciatura[!is.na(base2$dip_primaria)] <- 0
base2$dip_licenciatura[base2$ESTA_INSCRITO_EN==1 & base2$COMPLETO_PROGRAMA_INSCRITO==1] <- 1  #1293
table(base2$dip_licenciatura)
sum(table(base2$dip_licenciatura)) #17369

################################################################################
#Estimacion efecto sheepskin o efecto diploma

##regresion
summary(lm(ln_ingreso ~ educ + exp + exp_2 + dip_primaria + dip_secundaria + dip_tec_sup + dip_licenciatura, data = base2))
summary(lm(ln_ingreso ~ educ + exp + exp_2 + dip_primaria + dip_secundaria + dip_tec_sup + dip_licenciatura, data = base2, weights = EFT_FACTOR_EXP_ANUAL)) #con factor de expansion

## guardar regresiones
modelospencew <- lm(ln_ingreso ~ educ + exp + exp_2 + dip_primaria + dip_secundaria + dip_tec_sup + dip_licenciatura, data = base2, weights = EFT_FACTOR_EXP_ANUAL)

modelospencecivil <- lm(ln_ingreso ~ educ + exp + exp_2 + dip_primaria + dip_secundaria + dip_tec_sup + dip_licenciatura+ divorciado + unionlibre + viudo + separado + casado, data = base2, weights = EFT_FACTOR_EXP_ANUAL)

modelospencemujer<- lm(ln_ingreso ~ educ + exp + exp_2 + dip_primaria + dip_secundaria + dip_tec_sup + dip_licenciatura + mujer, data = base2, weights = EFT_FACTOR_EXP_ANUAL)

#heterocedasticidad

library(lmtest)
library(car)

bptest(modelomincerw)
#se rechaza la hipotesis nula de la homocedasticidad por lo que, indica que hay problemas
# de heterocedasticidad
bptest(modelospencew)
#se rechaza la hipotesis nula de la homocedasticidad por lo que, indica que hay problemas
# de heterocedasticidad
bptest(modelospencecivil)
#se rechaza la hipotesis nula de la homocedasticidad por lo que, indica que hay problemas
# de heterocedasticidad
bptest(modelospencemujer)
#se rechaza la hipotesis nula de la homocedasticidad por lo que, indica que hay problemas
# de heterocedasticidad

################################################################################

##Analisis descriptivo

#ln ingreso
summary(base1$ln_ingreso)
options(scipen=999)
jpeg(filename = "Histograma_del_cambio_en_el_ingreso_absoluto.jpeg")
hist(base2$ln_ingreso, xlab = "Ln del ingreso", ylab = "Frecuencia", main = "Histograma del ln del ingreso")
dev.off()

#educ
jpeg(filename = "Grafico de dispersion1.jpeg")
plot(base2$educ, base2$ln_ingreso, main = "Relacion entre el cambio en el ingreso y los años educativos", xlab = "Años educativos", ylab = "Cambio en el ingreso", pch = 19, frame = FALSE)
abline(lm(base2$ln_ingreso ~ base2$educ), col = "red")
dev.off()
jpeg(filename = "Histograma1.jpeg")
hist(base2$educ, xlab = "Años educativos", ylab = "Frecuencia", main = "Histograma años educativos")
dev.off()

# exp
jpeg(filename = "Grafico de dispersion2.jpeg")
plot(base2$exp, base2$ln_ingreso, main = "Relacion entre el cambio en el ingreso y los años de experiencia laboral", xlab = "Años laborales", ylab = "Cambio en el ingreso", pch = 19, frame = FALSE)
abline(lm(base2$ln_ingreso ~ base2$exp), col = "red")
dev.off()
jpeg(filename = "Histograma2.jpeg")
hist(base2$exp, xlab = "Años laborales", ylab = "Frecuencia", main = "Histograma años laborales")
dev.off()

#dip primaria
table(base2$dip_primaria)
#dip secundria
table(base2$dip_secundaria)
#dip tecnico superior
table(base2$dip_tec_sup)
#dip licenciatura
table(base2$dip_licenciatura)
#divorciado
table(base2$divorciado)
#union libre
table(base2$unionlibre)
#viudo
table(base2$viudo)
#separado
table(base2$separado)
#casado
table(base2$casado)
#mujer
table(base2$mujer)


#Resolviendo los problemas de heterocedasticidad
library(sandwich)

modelomincerwrob <- coeftest(modelomincerw,vcov. = vcovHC(modelomincerw,type="HC1"))
modelospencecivilrob <- coeftest(modelospencecivil,vcov. = vcovHC(modelospencecivil, type="HC1"))
modelospencemujerrob <- coeftest(modelospencemujer,vcov. = vcovHC(modelospencemujer,type="HC1"))
modelospencewrob <- coeftest(modelospencew,vcov. = vcovHC(modelospencew,type="HC1"))


## exportar regresiones
library(stargazer)

stargazer(modelomincerw,modelomincerwrob, modelospencew, modelospencewrob, type = "html", out= "Regresion_Sheepskin.html",
          dep.var.labels = "Ln(ingreso laboral)",digits = 8,
          covariate.labels = c("Años educativos","Experiencia laboral", 
                               "Experiencia laboral 2", "Diploma primaria", 
                               "Diploma secundaria", "Diploma técnico superior", 
                               "Diploma licenciatura","Constante"))


stargazer(modelospencecivil, modelospencecivilrob, type = "html", out= "Regresion2_Sheepskin.html",
          dep.var.labels = "Ln(ingreso laboral)",digits = 8,
          covariate.labels = c("Años educativos","Experiencia laboral", 
                               "Experiencia laboral 2", "Diploma primaria", 
                               "Diploma secundaria", "Diploma técnico superior", 
                               "Diploma licenciatura", "Union Libre", "Viudo", "Separado", "Casado", "Constante"))

stargazer(modelospencemujer,modelospencemujerrob, type = "html", out= "Regresion3_Sheepskin.html",
          dep.var.labels = "Ln(ingreso laboral)", digits = 8,
          covariate.labels = c("Años educativos","Experiencia laboral", 
                               "Experiencia laboral 2", "Diploma primaria", 
                               "Diploma secundaria", "Diploma técnico superior", 
                               "Diploma licenciatura", "Mujer", "Constante"))