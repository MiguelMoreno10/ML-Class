# cargamos las librerias.......
library(MASS); library(dplyr); library(caret)
library(mlbench); library(tidyverse)  
library(modelr); library(broom); library(pROC) 
library(ggplot2); library(mice); library("gridExtra")
library(PerformanceAnalytics); library(mice);
library(corrplot); library(knitr); library(readr);
library(reshape2); library(tibble); library(skimr)  

## Cagar la base datos
getwd()
setwd("C:/Users/Miguel Angel/Documents/materias/Ciencias de datos/DS - 2021/tareas en clase/Parcial 3") 
Estudiantes <- read_csv("StudentsPerformance.csv", 
              col_types = cols(`math score` = col_number(), 
              `reading score` = col_number(), `writing score` = col_number()))
Estudiantes <- data.frame(Estudiantes)

## Examinar los datos 
glimpse(Estudiantes)
head(Estudiantes)
skim(Estudiantes) ## Importante para el analisis de datos 
summary(Estudiantes)

which(is.na(Estudiantes))  # odentificar NA en el dataframe
sum(is.na(Estudiantes))    # Contar NA en el dataframe


## La dimension del data set es de 8 variables y 1000 observaciones 
## 5 variables son categorica y 3 son numericas
## No hay datos NA y tampocos Ouliders en el data set 

## Se conversion de los datos a factor 
#Estudiantes$gender <- as.factor(Estudiantes$gender) 
#Estudiantes$race.ethnicity <- as.factor(Estudiantes$race.ethnicity) 
#Estudiantes$parental.level.of.education <- as.factor(Estudiantes$parental.level.of.education) 
#Estudiantes$lunch <- as.factor(Estudiantes$lunch ) 
Estudiantes$test.preparation.course <- as.factor(Estudiantes$test.preparation.course) 
glimpse(Estudiantes)
str(Estudiantes)

#Grafica de Completos Vs Icompletos
Estudiantes %>%
  ggplot() +
  geom_bar(aes(x= test.preparation.course), fill= c("brown","steelblue")) + 
  labs(x='Prepacion del curso', y="Numero de Personas", title='Graficas Completos vs Incompletos') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Porcentaje de personas con preparacion completa 
round(((sum(Estudiantes$test.preparation.course=="completed"))/(sum(Estudiantes$test.preparation.course=="completed")+
                                             sum(Estudiantes$test.preparation.course=="none")))*100 , 2) 
# Porcentaje de personas con preparacion incompleta
round(((sum(Estudiantes$test.preparation.course=="none"))/(sum(Estudiantes$test.preparation.course=="completed")+
                                             sum(Estudiantes$test.preparation.course=="none")))*100 , 2) 

# Por los graficos y el porcentaje podemos decir que hay mas personas que no completaron sus estudios de preparacion 

Estudiantes %>%
  ggplot() +
  geom_bar(aes(x= gender), fill= c("brown","steelblue")) + 
  labs(x='Genero', y="Numero de Personas", title='Graficas Hombre vs Mujeres') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Tambien podemos decir que hay mas mujeres que hombres

Estudiantes %>%
  ggplot() +
  geom_bar(aes(x= lunch), fill= c("brown","steelblue")) + 
  labs(x='Personas que almorzaron', y="Numero de Personas", title='Graficas Comida gratis vs estandar') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Podemos que la mayoria de los estudiantes prefieren la comida estandar

## Analisis de datos con graficas 

Estudiantes %>%
  ggplot( aes(x= test.preparation.course, y= math.score)) +
  geom_boxplot(fill="slateblue", alpha=0.2) +
  geom_jitter(color="black", size=0.8, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot Nota de Matematica") +
  xlab("")


Estudiantes %>%
  ggplot( aes(x= test.preparation.course, y= reading.score)) +
  geom_boxplot(fill="slateblue", alpha=0.2) +
  geom_jitter(color="black", size=0.8, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot Noda de lectura") +
  xlab("")

Estudiantes %>%
  ggplot( aes(x= test.preparation.course, y= writing.score)) +
  geom_boxplot(fill="slateblue", alpha=0.2) +
  geom_jitter(color="black", size=0.8, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot Nota de escritura") +
  xlab("")

## podemos ver que la distribucion de los datos es mejor para los que terminaron la preparacion para el curso, su nota es 
## mejor respetos a los otros que no lo terminaron en los tres graficos 



#### Modelos 


# Modelo de Regresion Logistica 
# Model1: Todas los predictores

model1 <- glm(Estudiantes ~.,family = binomial, data= Estudiantes ) # si queremos utilizar todas las varaibles 
summary(model1)

# Formula <-  glm (y ~ X1 + x2 + xn ...)

# Model2: predictores significativo

model2 <- glm(Estudiantes ~ math.score + reading.score + writing.score,
              family = binomial,data=Estudiantes)

summary(model2)

# Info. Modelo
model1$fitted.values

# Paso 4: Evaluación de los Modelos -----

library(dplyr)
# selecciona 20 observaciones del dataset original
prueba <- diabetes %>%
  sample_n(200, replace = FALSE)
dim(prueba)
head(prueba, 5)
str(prueba)
# quita el outcome(diabetes)

prueba1 <- prueba %>%
  select(-diabetes)
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
