# cargamos las librerias.......
library(MASS); library(dplyr); library(caret)
library(mlbench); library(tidyverse)  
library(modelr); library(broom); library(pROC) 
library(ggplot2); library(mice); library("gridExtra")
library(PerformanceAnalytics)



############################## PARTE A ##########################################



#1
getwd()
setwd("C:/Users/Miguel Angel/Documents/materias/Ciencias de datos/DS - 2021/tareas en clase/Tarea 1 Corte 3") 
heart <- read_delim("heart_disease_class.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
heart <- data.frame(heart)




#2
dim(heart) # Dimensiones
head(heart, 10)
str(heart)
names(heart)
summary(heart)
glimpse(heart)



#3
sum(is.na(heart)) # Omitir datos faltantes
heart <- na.omit(heart)
dim(heart)
sum(is.na(heart))



#4
#ComprobaciÃ³n de Outliers (quitar)
boxplot(heart , horizontal = TRUE)
boxplot.stats(heart$age)
boxplot.stats(heart$cp)
boxplot.stats(heart$chol)
boxplot.stats(heart$fbs)
boxplot.stats(heart$thalach)
boxplot.stats(heart$oldpeak)
boxplot.stats(heart$ca)
boxplot.stats(heart$thal)



heart$age[heart$age < 3]<- round(mean(heart$age),2)
heart$cp[heart$cp == 1]<- round(mean(heart$cp),2)
heart$chol[heart$chol > 392]<- round(mean(heart$chol),2)
heart$fbs[heart$fbs == 1]<- round(mean(heart$fbs),2)
heart$thalach[heart$thalach > 21 ]<- round(mean(heart$thalach),2)
heart$oldpeak[heart$oldpeak > 4.1]<- round(mean(heart$oldpeak),2)
heart$ca[heart$ca == -1e+05 & heart$ca == 3e+00 ]<- round(mean(heart$ca),2)
heart$thal[heart$thal == -1e+05 ]<- round(mean(heart$thal),2)
boxplot(heart , horizontal = TRUE)



############################## PARTE B ##########################################



#
library(caret) # Usaremos funciones del paquete caret
set.seed(12345)

heart <- na.omit(heart)
dim(heart)


# Paso 3: Construir el Modelo -----
# Modelo de Regresion Logistica 
# Model1: Todas los predictores

model1 <- glm(heart ~.,family = binomial, data= heart) # si queremos utilizar todas las varaibles 
summary(model1)

# Formula <-  glm (y ~ X1 + x2 + xn ...)

# Model2: predictores significativo

model2 <- glm(heart ~ trestbps + thal + ca + thalach,
              family = binomial,data=heart)

summary(model2)

# Info. Modelo
model1$fitted.values

# Paso 4: Evaluación de los Modelos -----

library(dplyr)
# selecciona 20 observaciones del dataset original
prueba <- heart %>%
  sample_n(20, replace = FALSE)
dim(prueba)
head(prueba, 5)
str(prueba)
# quita el outcome(heart)

prueba1 <- prueba %>%
  select(-outcome)
dim(prueba1)
head(prueba1,5)

# Validacion con dataset de validación (20 Obs random) 
?predict
pred1 <- predict(model1, newdata = prueba1, type = "response")
pred2 <- ifelse(pred1 > 0.5, "pos", "neg")
pred2 <- factor(pred2)

typeof(pred2)
actual <- factor(prueba$diabetes)
actual

prediction <- data.frame(pred2,actual)
prediction

mean(pred2==actual)

# Paso 5: Evaluación (Métricas)  ---- 
library(caret)

confusionMatrix(pred2,actual, positive = "pos")
??confusionMatrix

# Paso 6: Curva Roc   -----
install.packages("pROC")
library(pROC)

# Curva Roc
library(pROC)
plot.roc(prueba$diabetes, pred1,
         main="Curva ROC del Model", percent=TRUE,
         ci=TRUE, of="thresholds", # compute  the threshold of prediction 
         thresholds="best", # select the (best) threshold
         print.thres="best", 
         print.auc=TRUE,ci.auc=TRUE) # a

