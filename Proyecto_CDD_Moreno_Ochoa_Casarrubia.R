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
Estudiantes <- read_csv("StudentsPerformance.csv")
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

## Se covierte las variable preparation.course en factor ya que este sera nuestro outcome
Estudiantes$test.preparation.course <- as.factor(Estudiantes$test.preparation.course) 
glimpse(Estudiantes)

#Grafica de Completos Vs Icompletos
Estudiantes %>%
  ggplot() +
  geom_bar(aes(x= Estudiantes$test.preparation.course), fill= c("brown","steelblue")) + 
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
  geom_bar(aes(x= Estudiantes$gender), fill= c("brown","steelblue")) + 
  labs(x='Genero', y="Numero de Personas", title='Graficas Hombre vs Mujeres') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Tambien podemos decir que hay mas hombres que mujeres 