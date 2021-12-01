
install.packages("datasets.load") 
library(datasets.load)
data("VADeaths")
muertes <- VADeaths
deaths <- (VADeaths) # matrix
muertes <- data.frame(muertes)

attributes(muertes) # Atributos 
dim(muertes) # Dimension del data set
str(muertes) #Determinamos la estructura del data set 
summary(muertes) # Analisi estadistico 
?VADeaths

tasa_mas <- muertes[, c("Rural.Male", "Urban.Male")]
dim(tasa_mas)
tmahom <- muertes[3:5, c(1,3)]
dim(tmahom)

Avg_by_local <- c(mean(muertes$Rural.Male),mean(muertes$Rural.Female),mean(muertes$Urban.Male),mean(muertes$Urban.Female))
Avg_by_local

muertes <- rbind(muertes, Avg_by_local)
rownames(muertes) <- c("50-54", "55-59", "60-64", "65-69", "70-74","Avg_by_local")
muertes
deaths <- rbind(deaths, Avg_by_local)

Avg_by_Age <- c(mean(deaths[1, ]),mean(deaths[2, ]),mean(deaths[3, ]),mean(deaths[4, ]),mean(deaths[5, ]),mean(deaths[6, ]))
muertes <- cbind(muertes, Avg_by_Age)
muertes

#Parte 2

library(datasets)
data("mtcars")
cars <- mtcars
cars <- data.frame(cars)

str(cars)

motor <- cars[cars$mpg>20,]
motor

motor2 <- motor[motor$cyl==6,]
motor2

cars2 <- cars[c("mpg", "cyl", "wt")]
cars2

summary(cars) #Promedio de todas la columnas mean

sd(cars$mpg)
sd(cars$cyl)
sd(cars$disp)
sd(cars$hp)
sd(cars$drat) #calcular desviacion
sd(cars$wt)
sd(cars$qsec)
sd(cars$vs)
sd(cars$am)
sd(cars$gear)
sd(cars$carb)

summary(cars) #Valor mas representativo de todas las columnas MAx
