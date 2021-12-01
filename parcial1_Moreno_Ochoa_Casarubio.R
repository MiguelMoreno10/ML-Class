if(!require(devtools)) install.packages("corrplot") 
if(!require(ggpubr)) install.packages("ggplot2")
if(!require(ggplot2)) install.packages("tidyverse")
if(!require(hrbrthmes)) install.packages("tidyverse")

library(hrbrthmes)
install.packages("hrbrthmes")
library(readr)
library(corrplot)
library(tidyverse)
library(ggplot2)

heart <- read_delim("heart.csv", ";")
heart <-as.data.frame(heart)
head(heart,5)

         #####################Solucion###################################

        #######################Punto 1 ##################################

#punto A
str(heart)

#Punto B
typesvar <- c("categorica", "numerica" , "categorica" , "categorica" , "numerica" , 
              "numerica" , "categorica" , "categorica" , "numerica" , "categorica" , 
              "numerica" , "numerica" , "categorica" , "categorica" , "categorica")
typesvar

names(typesvar) <- c(X1 = 'id' , X2 = 'age' , X3 = 'sex' , X4 = 'cp' , 
                     X5 = 'trestbps' , X6 = 'chol' , X7 = 'fbs' , 
                     X8 = 'restecg' , X9 = 'thalach' , X10 = 'exang' , 
                     X11 = 'oldpeak' , X12 = 'slope' , X13 = 'ca' , 
                     X14 = 'thal' , X15 = 'diag')
typesvar

str(heart)

#Punto C
sum(is.na(heart)) 

colSums(is.na(heart))



#Punto D
heart$ca[heart$ca == 4] <- NA
heart
heart$ca[is.na(heart$ca)]<- round(median(na.omit(heart$ca) , na.rm = TRUE),2)
heart

#Punto E
heart$age[is.na(heart$age)]<- round(mean(na.omit(heart$age), na.rm = TRUE),2)
heart$trestbps[is.na(heart$trestbps)]<- round(mean(na.omit(heart$trestbps), na.rm = TRUE),2)
heart$chol[is.na(heart$chol)]<- round(mean(na.omit(heart$chol), na.rm = TRUE),2)
heart$thalach[is.na(heart$thalach)]<- round(mean(na.omit(heart$thalach), na.rm = TRUE),2)
heart

colSums(is.na(heart))

   ############################### Punto 2 ###############################

#Punto F
borrar <- c("id")
heart <- heart[ , !(names(heart) %in% borrar)]
heart

as.factor(heart$diag)
colnames(heart) <- c("age" , "sex" , "cp" , 
                     "trestbps" , "chol" , "fbs" , 
                     "restecg" , "thalach" , "exang" , 
                     "oldpeak" , "slope" , "ca" , 
                     "thal" , "outcome")
head(heart)

#punto G
sum(heart$outcome==1)
sum(heart$outcome==0)

round(((sum(heart$outcome==0))/(sum(heart$outcome==1)+sum(heart$outcome==0)))*100 , 2) #porcentaje de 0
round(((sum(heart$outcome==1))/(sum(heart$outcome==1)+sum(heart$outcome==0)))*100 , 2) #porcentaje de 1

Color = c("blue", "purple")

ggplot(heart, aes(x= outcome)) + geom_bar(fill=Color ) + 
  labs(x = "Personas con enfermedades cardiacas Si = 0 , No = 1", 
       y = "Numeros de personas") + theme_dark()

#Punto H
boxplot(heart , horizontal = TRUE)

boxplot.stats(heart$age)
boxplot.stats(heart$sex)
boxplot.stats(heart$cp)      ## NO categorico
boxplot.stats(heart$trestbps)
boxplot.stats(heart$chol)
boxplot.stats(heart$fbs)    ## NO categorico
boxplot.stats(heart$restecg) 
boxplot.stats(heart$thalach)
boxplot.stats(heart$exang) 
boxplot.stats(heart$oldpeak)
boxplot.stats(heart$slope) 
boxplot.stats(heart$ca)
boxplot.stats(heart$thal)


  #Punto I 

heart$age[heart$age < 3]<- round(mean(heart$age),2)
boxplot.stats(heart$age)
heart$trestbps[heart$trestbps > 171] <- round(mean(heart$trestbps),2)
boxplot.stats(heart$trestbps)
heart$chol[heart$chol > 393]<- round(mean(heart$chol),2)
boxplot.stats(heart$chol)
heart$chol[heart$chol < 127]<- round(mean(heart$chol),2)
boxplot.stats(heart$chol)
heart$chol[heart$chol > 359]<- round(mean(heart$chol),2)
boxplot.stats(heart$chol)
heart$chol[heart$chol < 141]<- round(mean(heart$chol),2)
boxplot.stats(heart$chol)
heart$chol[heart$chol > 343]<- round(mean(heart$chol),2)
boxplot.stats(heart$chol)

heart$thalach[heart$thalach < 89]<- round(mean(heart$thalach),2)
boxplot.stats(heart$thalach)
heart$thalach[heart$thalach > 203]<- round(mean(heart$thalach),2)
boxplot.stats(heart$thalach)
heart$thalach[heart$thalach < 96]<- round(mean(heart$thalach),2)
boxplot.stats(heart$thalach)
heart$thalach[heart$thalach < 99]<- round(mean(heart$thalach),2)
boxplot.stats(heart$thalach)
heart$thalach[heart$thalach < 103]<- round(mean(heart$thalach),2)
boxplot.stats(heart$thalach)

heart$oldpeak[heart$oldpeak > 4.1]<- round(mean(heart$oldpeak),2)
boxplot.stats(heart$oldpeak)
heart$ca[heart$ca < 0 ]<- round(mean(heart$ca),2)
boxplot.stats(heart$ca)
heart$ca[heart$ca < -1]<- round(mean(heart$ca),2)
boxplot.stats(heart$ca)
heart$ca[heart$ca < 0 ]<- round(mean(heart$ca),2)
boxplot.stats(heart$ca)
heart$thal[heart$thal < 0 ]<- round(mean(heart$thal),2)
boxplot.stats(heart$thal)
heart$thal[heart$thal < 3 ]<- round(mean(heart$thal),2)
boxplot.stats(heart$thal)

############################### Punto 3 ###############################
summary(heart)

#J
ggplot(heart, mapping =  aes(x = age)) + geom_histogram(bins = 10, fill="#69b3a2", color="#e9ecef", alpha=0.9) + 
  labs(x = "Edad", y = "Numeros de personas", title = "Edad") + xlim(20,80) + theme_bw()

hist(heart$age)

#K 
sum(heart$age>50)
sum(heart$age>50 & heart$outcome == 1) #Numero de personas mayores de 50 años que no tienen enfermdedad
sum(heart$age>50 & heart$outcome == 0) #Numero de personas mayores de 50 años que tienen enfermdedad
round((sum(heart$age>50))/sum(heart$age>0)*100, 2)  #Porcentaje

#L
sum(heart$age>0 & heart$outcome == 1)  # personas sin enfermedad
sum(heart$age>0 & heart$outcome == 0)  #Personas con efermedad
sum(heart$sex==1) #hombres
sum(heart$sex==0) #mujeres
sum(heart$sex==1 & heart$outcome == 1) #hombres sin enfermedad cardiaca
round((sum(heart$sex==1 & heart$outcome == 1))/sum(heart$age>0 & heart$outcome == 1)*100, 2)
sum(heart$sex==0 & heart$outcome == 1) #mujeres sin enfermedad cardiaca
round((sum(heart$sex==0 & heart$outcome == 1))/sum(heart$age>0 & heart$outcome == 1)*100, 2)
sum(heart$sex==1 & heart$outcome == 0) #hombres con enfermedad cardiaca 
round((sum(heart$sex==1 & heart$outcome == 0))/sum(heart$age>0 & heart$outcome == 0)*100, 2)
sum(heart$sex==0 & heart$outcome == 0) #mujeres con enfermedad cardiaca
round((sum(heart$sex==0 & heart$outcome == 0))/sum(heart$age>0 & heart$outcome == 0)*100, 2)

ggplot(heart, mapping =  aes(x = age, fill = factor(sex) )) + 
  geom_histogram(bins = 10, alpha=0.9) +
  labs(x = "Edad", y = "Numeros de personas", title = "Hombre vs Mujeres") + xlim(20,80) + theme_bw()

#M
tres1 <- heart[heart$outcome == 1 & heart$trestbps, c("trestbps","outcome")]
str(tres1)
tres1
tres2 <- heart[heart$outcome == 0 & heart$trestbps, c("trestbps","outcome")]
str(tres2)
tres2

#N
fbs1 <- heart[heart$fbs > 0 & heart$outcome == 1, c("fbs","outcome")]
str(fbs1)
fbs1
fbs2 <- heart[heart$fbs == 0 & heart$outcome == 1, c("fbs","outcome")]
str(fbs2)
fbs2
fbs3 <- heart[heart$fbs > 0 & heart$outcome == 0, c("fbs","outcome")]
str(fbs3)
fbs3
fbs4 <- heart[heart$fbs == 0 & heart$outcome == 0, c("fbs","outcome")]
str(fbs4)
fbs4
#O
library(corrplot)
corrplot(cor(heart), type="upper")

