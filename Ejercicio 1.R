#Instalar la libreria
#install.packages("faraway")

rm(list = ls(all.names = TRUE))
gc()

#Usar la paqueteria
library(faraway)
library(MASS)
data(fat)

datos <- subset(fat, select = -c(siri, density, free))

#Eliminar todos los datos que tengan valor cero en brozek
datos <- subset(datos, brozek != 0)

summary(datos["weight"])

summary(datos["height"])

#Creamos dos boxplot para identificar outliers

# Crear un boxplot de la columna 'weight' del data frame 'datos'
# Tenemos dos valores que estan significativamente alejados de los demas puntos
# Por lo que podemos conciderarlos outliers
boxplot(datos$weight, 
        main = "Boxplot de Weight",
        ylab = "Weight",
        col = "lightblue")


# Crear un boxplot de la columna 'height' del data frame 'datos'
# Tenemos un valor que esta significativiamente alejados de los demas puntos
boxplot(datos$height, 
        main = "Boxplot de height",
        ylab = "height",
        col = "lightblue")

#Filtramos los outliers
datos <- datos[!(datos$weight > 250 | datos$height < 60), ]


#Volvemos a hacer los boxplots para ver si siguen los valores atipicos

boxplot(datos$weight, 
        main = "Boxplot de Weight",
        ylab = "Weight",
        col = "lightblue")

boxplot(datos$height, 
        main = "Boxplot de height",
        ylab = "height",
        col = "lightblue")



################################################################################
############################### Ejercicio 1 ####################################

######### Solo efectos principales ############

#Modelo lineal generalizado con liga identidad y distribucion gaussiana
modglm <- glm(brozek ~ ., data = datos, family = gaussian(link = "identity"))
summary(modglm)

#Evaluacion del metodo usando K-Cross Validation
n=dim(datos)[1]
K=5
(labK=rep(1:K, length.out = n))
table(labK)

# realizamos una permutaci?n aleatoria de los pliegues
set.seed(1234)
(Pliegues <- sample(labK))#seleccionados n de n, pero orden aleatorio

###### Calculo del poder predictivo con MSE
mod1KCV=function(x, Plie, Dat){
  train <- which(Plie != x)
  test = (-train)
  mod1glm=glm(brozek ~ ., Dat[train,],family = gaussian(link = "identity"))
  predm1t=predict(mod1glm, Dat[test,])
  MSE=mean((Dat$brozek[test]-predm1t)^2)
  return(MSE)
}
MSE.K.mod1= sapply(1:K,mod1KCV, Plie=Pliegues, Dat=datos)
summary(MSE.K.mod1)
#MSE por grupo
MSE.K.mod1
# Estimacion del poder predictivo usando
# MSE y K-Cross Validation es
(MSE.KCV.mod1=mean(MSE.K.mod1))


###### Calculo del poder predictivo con MAE
mod1KCVMAE = function(x, Plie, Dat){
  train <- which(Plie != x)
  test = (-train)
  mod1glm=glm(brozek ~ ., Dat[train,],family = gaussian(link = "identity"))
  predm1t=predict(mod1glm, Dat[test,])
  MAE=mean(abs(Dat$brozek[test]-predm1t))
  return(MAE)
}

MAE.K.mod1= sapply(1:K,mod1KCVMAE, Plie=Pliegues, Dat=datos)
summary(MAE.K.mod1)
#MSE por grupo
MAE.K.mod1

# Estimacion del poder predictivo usando
# MSE y K-Cross Validation es
(MAE.KCV.mod1=mean(MAE.K.mod1))


###### Calculo del poder predictivo con Coeficiente de correlacion al cuadrado

mod1KCVCor = function(x, Plie, Dat){
  train <- which(Plie != x)
  test = (-train)
  mod1glm=glm(brozek ~ ., Dat[train,],family = gaussian(link = "identity"))
  predm1t=predict(mod1glm, Dat[test,])
  COR=cor(Dat$brozek[test],predm1t)^2
  return(COR)
}


COR.K.mod1= sapply(1:K,mod1KCVCor, Plie=Pliegues, Dat=datos)
summary(COR.K.mod1)
#MSE por grupo
COR.K.mod1

# Estimacion del poder predictivo usando
# el promedio del coeficiente de correlacion y K-Cross Validation es
(COR.KCV.mod1=mean(COR.K.mod1))



################# Con interacciones de segundo grado ####################

###Modelos con iteracciones de segundo orden
modglm_int <- glm(brozek ~ .^2, data = datos, family = gaussian(link = "identity"))
summary(modglm_int)

###### Calculo del poder predictivo con MSE
mod2KCV_int=function(x, Plie, Dat){
  train <- which(Plie != x)
  test = (-train)
  mod2glm_int=glm(brozek ~ .^2, Dat[train,],family = gaussian(link = "identity"))
  predm1t=predict(mod2glm_int, Dat[test,])
  MSE=mean((Dat$brozek[test]-predm1t)^2)
  return(MSE)
}

MSE.K.mod_int= sapply(1:K,mod2KCV_int, Plie=Pliegues, Dat=datos)
summary(MSE.K.mod_int)
MSE.K.mod_int

# Estimaci?n del poder predictivo usando
# MSE y K-Cross Validation es
(MSE.KCV.mod2=mean(MSE.K.mod_int))

###### Calculo del poder predictivo con MAE

mod2KCV_int_MAE=function(x, Plie, Dat){
  train <- which(Plie != x)
  test = (-train)
  mod2glm_int=glm(brozek ~ .^2, Dat[train,],family = gaussian(link = "identity"))
  predm1t=predict(mod2glm_int, Dat[test,])
  MAE=mean(abs(Dat$brozek[test]-predm1t))
  return(MAE)
}

MAE.K.mod_int= sapply(1:K,mod2KCV_int_MAE, Plie=Pliegues, Dat=datos)
summary(MAE.K.mod_int)
#MAE por cada grupo
MAE.K.mod_int

# Estimaci?n del poder predictivo usando
# El promedio del MAE y K-Cross Validation es
(MAE.KCV.mod2=mean(MAE.K.mod_int))


###### Calculo del poder predictivo con el coeficiente de correlacion al cuadrado
mod2KCV_int_COR=function(x, Plie, Dat){
  train <- which(Plie != x)
  test = (-train)
  mod2glm_int=glm(brozek ~ .^2, Dat[train,],family = gaussian(link = "identity"))
  predm1t=predict(mod2glm_int, Dat[test,])
  COR=cor(Dat$brozek[test],predm1t)^2
  return(COR)
}

COR.K.mod_int= sapply(1:K,mod2KCV_int_COR, Plie=Pliegues, Dat=datos)
summary(COR.K.mod_int)
#Coeficientes de correlacion por grupo
COR.K.mod_int

# Estimaci?n del poder predictivo usando
# El promedio de los coeficientes de correlacion al cuadrado y K-Cross Validation es
(COR.KCV.mod2=mean(COR.K.mod_int))

#Podemos ver que es menor que el promedio es menor que .5, por lo que es un modelo muy pobre


################ incluyendo las variables al cuadrado ####################

# Construir el modelo con términos cuadráticos
mod3glm_cuad <- glm(brozek ~ . + I(age^2) + I(weight^2) + I(height^2) 
                             + I(adipos^2) + I(neck^2) + I(chest^2) + I(abdom^2) 
                             + I(hip^2) + I(thigh^2) + I(knee^2) + I(ankle^2) + 
                               I(biceps^2) + I(forearm^2) + I(wrist^2), 
                             data = datos, family = gaussian(link = "identity"))

summary(mod3glm_cuad)

###### Calculo del poder predictivo con MSE
mod3KCV_cuad=function(x, Plie, Dat){
  train <- which(Plie != x)
  test = (-train)
  mod3glm_cuad= glm(brozek ~ . + I(age^2) + I(weight^2) + I(height^2) 
                   + I(adipos^2) + I(neck^2) + I(chest^2) + I(abdom^2) 
                   + I(hip^2) + I(thigh^2) + I(knee^2) + I(ankle^2) + 
                     I(biceps^2) + I(forearm^2) + I(wrist^2), 
                   data = Dat[train,], family = gaussian(link = "identity"))
  predm1t=predict(mod3glm_cuad, Dat[test,])
  MSE=mean((Dat$brozek[test]-predm1t)^2)
  return(MSE)
}

MSE.K.mod_cuad= sapply(1:K,mod3KCV_cuad, Plie=Pliegues, Dat=datos)
summary(MSE.K.mod_cuad)
MSE.K.mod_cuad

# Estimaci?n del poder predictivo usando
# el promedio del MSE por grupo y K-Cross Validation es
(MSE.KCV.mod3=mean(MSE.K.mod_cuad))



###### Calculo del poder predictivo con MAE
mod3KCV_cuad_MAE=function(x, Plie, Dat){
  train <- which(Plie != x)
  test = (-train)
  mod3glm_cuad= glm(brozek ~ . + I(age^2) + I(weight^2) + I(height^2) 
                    + I(adipos^2) + I(neck^2) + I(chest^2) + I(abdom^2) 
                    + I(hip^2) + I(thigh^2) + I(knee^2) + I(ankle^2) + 
                      I(biceps^2) + I(forearm^2) + I(wrist^2), 
                    data = Dat[train,], family = gaussian(link = "identity"))
  predm1t=predict(mod3glm_cuad, Dat[test,])
  MAE=mean(abs(Dat$brozek[test]-predm1t))
  return(MAE)
}

MAE.K.mod_cuad= sapply(1:K,mod3KCV_cuad_MAE, Plie=Pliegues, Dat=datos)
summary(MAE.K.mod_cuad)
#Calculo del MAE por grupo
MAE.K.mod_cuad

# Estimaci?n del poder predictivo usando
# el promedio del MEE por grupo y K-Cross Validation es
(MAE.KCV.mod3=mean(MAE.K.mod_cuad))


###### Calculo del poder predictivo con el coeficiente de correlacion al cuadrado
mod3KCV_cuad_Cor=function(x, Plie, Dat){
  train <- which(Plie != x)
  test = (-train)
  mod3glm_cuad= glm(brozek ~ . + I(age^2) + I(weight^2) + I(height^2) 
                    + I(adipos^2) + I(neck^2) + I(chest^2) + I(abdom^2) 
                    + I(hip^2) + I(thigh^2) + I(knee^2) + I(ankle^2) + 
                      I(biceps^2) + I(forearm^2) + I(wrist^2), 
                    data = Dat[train,], family = gaussian(link = "identity"))
  predm1t=predict(mod3glm_cuad, Dat[test,])
  COR=cor(Dat$brozek[test],predm1t)^2
  return(COR)
}

COR.K.mod_cuad= sapply(1:K,mod3KCV_cuad_Cor, Plie=Pliegues, Dat=datos)
summary(COR.K.mod_cuad)
#Correlacion por grupo
COR.K.mod_cuad

# Estimaci?n del poder predictivo usando
# el promedio del coeficiente de correlacion al cuadrado por grupo y K-Cross Validation es
(COR.KCV.mod3=mean(COR.K.mod_cuad))




#################################################################################
############################### Ejercicio 2 ####################################

############ Aplicaremos seleccin por pasos usando criterio BIC ################
#### Primero definimos la penalizacion que dependera del numero de datos

pen=log(dim(datos)[1])

######### Solo efectos principales ############

modglm_var <- stepAIC(modglm, trace = FALSE,direction ="both", k=pen)
summary(modglm_var)

mod1KCV_selecVariables=function(x, Plie, Dat){
  train <- which(Plie != x)
  test = (-train)
  modaux=glm(brozek ~ ., Dat[train,],family = gaussian(link = "identity"))
  penAux=log(dim(Dat[train,])[1])
  modtr = stepAIC(modaux, trace = FALSE,direction ="both", k=penAux)
  predm1t=predict(modtr, Dat[test,])
  MSE=mean((Dat$brozek[test]-predm1t)^2)
  return(MSE)
}
MSE.K.modselec_var= sapply(1:K,mod1KCV_selecVariables, Plie=Pliegues, Dat=datos)
summary(MSE.K.modselec_var)

# Estimaci?n del poder predictivo usando
# MSE y K-Cross Validation es
(MSE.KCV..modselec_var=mean(MSE.K.modselec_var))



######### Modelo con interacciones ############
# Define todas las interacciones de segundo orden manualmente
interacciones <- combn(names(datos)[-1], 2, FUN = function(x) paste(x, collapse = ":"))
interacciones_formula <- as.formula(paste("brozek ~ . + ", paste(interacciones, collapse = " + ")))
interacciones_formula

#Modelo con los datos completos
modglm_var_int <- stepAIC(modglm_int, scope = list(upper = interacciones_formula, lower = ~1), trace = FALSE, direction = "both", k = pen)

# Definir la función para K-fold cross-validation con selección de variables e interacciones
mod2KCV_selecVar_int <- function(x, Plie, Dat, interacciones) {
  train <- which(Plie != x)
  test <- (-train)
  mod2glm_int_aux <- glm(brozek ~ .^2, data = Dat[train,], family = gaussian(link = "identity"))
  penAux <- log(dim(Dat[train,])[1])
  modtr <- stepAIC(mod2glm_int_aux, scope = list(upper = interacciones, lower = ~1), trace = FALSE, direction = "both", k = penAux)
  predm1t <- predict(modtr, newdata = Dat[test,])
  MSE <- mean((Dat$brozek[test] - predm1t)^2)
  return(MSE)
}

# Aplicar la función de K-fold cross-validation
MSE.K.mod_int_var <- sapply(1:K, mod2KCV_selecVar_int, Plie = Pliegues, Dat = datos, interacciones = interacciones_formula)
summary(MSE.K.mod_int_var)

# Estimaci?n del poder predictivo usando
# MSE y K-Cross Validation es
(MSE.KCV.mod2_var=mean(MSE.K.mod_int_var))


########### Modelo con variables al cuadrado ##############
#Definimos la formula
# Definir las variables continuas excluyendo 'brozek'
continuas <- names(which(sapply(datos, is.numeric))) 
xnames <- continuas[!continuas %in% "brozek"]
# Crear la fórmula con términos cuadráticos
upperfor <- as.formula(paste('~ . +', paste(paste('I(', xnames, '^2)', collapse = ' + '), collapse = ' + ')))
# Modelo inicial con todos los datos y selección de variables
modglm_var_cuad <- stepAIC(glm(brozek ~ ., data = datos, family = gaussian(link = "identity")), 
                           scope = list(upper = upperfor, lower = ~1), 
                           trace = FALSE, direction = "both", k = pen)

# Definir la función para K-fold cross-validation con selección de variables y términos cuadráticos
mod3KCV_selecVar_cuad <- function(x, Plie, Dat, upform) {
  train <- which(Plie != x)
  test <- (-train)
  mod3glm_cuad_aux <- glm(brozek ~ . + I(age^2) + I(weight^2) + I(height^2) 
                          + I(adipos^2) + I(neck^2) + I(chest^2) + I(abdom^2) 
                          + I(hip^2) + I(thigh^2) + I(knee^2) + I(ankle^2) + 
                            I(biceps^2) + I(forearm^2) + I(wrist^2), 
                          data = Dat[train,], family = gaussian(link = "identity"))
  penAux <- log(dim(Dat[train,])[1])
  modtr <- stepAIC(mod3glm_cuad_aux, scope = list(upper = upform, lower = ~1), trace = FALSE, direction = "both", k = penAux)
  predm1t <- predict(modtr, newdata = Dat[test,])
  MSE <- mean((Dat$brozek[test] - predm1t)^2)
  return(MSE)
}

# Aplicar la función de K-fold cross-validation
MSE.K.mod_cuad_var <- sapply(1:K, mod3KCV_selecVar_cuad, Plie = Pliegues, Dat = datos, upform = upperfor)
summary(MSE.K.mod_cuad_var)

# Estimaci?n del poder predictivo usando
# MSE y K-Cross Validation es
(MSE.KCV.mod3_var=mean(MSE.K.mod_cuad_var))


