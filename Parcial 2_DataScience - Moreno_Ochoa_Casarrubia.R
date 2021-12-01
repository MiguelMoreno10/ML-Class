# 2 examen parcial ciencias de datos
install.packages("mice")

# Librerias  ----

  library(caret); library(dplyr); library(ggplot2);
  library(tidyverse);  library(mice);  library(knitr);
  library(corrplot); library(knitr); library(readr); library(dplyr); 
  library(reshape2); library(tibble) 
  
  # Cargar datos ----
  
  cancer <- read_delim("breastCancer_parcial.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
  cancer <- data.frame(cancer)
  
  # Exploracion de datos ----
  
  dim(cancer)
  head(cancer)
  glimpse(cancer)
  
  #  Preguntas partes 1 ---
  
  ##  Pregunta 1
  cancer$diagnosis <- as.factor(cancer$diagnosis) # Se convierte en una variable factor a diagnosis
  glimpse(cancer)
  
  
  ## Pregunta 2 
  
  dim(cancer) # Numero de paciente 500 y predictores 30 sin contar id, diagnosis y outcome
  
  ## Pregunta 3
   
  Na <- sum(is.na(cancer)) # numero de datos NA en el data set
  Na
  # Crear una tabla con valores faltantes
  missing.values <- cancer %>%
    gather(key = "key", value = "val") %>%   
    mutate(is.missing = is.na(val)) %>%
    group_by(key, is.missing) %>%
    summarise(num.missing = n()) %>%
    filter(is.missing==T) %>%
    arrange(desc(num.missing)) 
  missing.values  %>% knitr::kable()
  
  Null <- sum(is.null(cancer)) # numeros de nulos en el data set
  Null
  
  ## Pregunta 4
  
  # graficar los NAs 
  missing.values %>%
    ggplot() +
    geom_bar(aes(x=key, y = num.missing), stat = 'identity', fill="steelblue") + 
    labs(x='variable', y="Conteo de NA's", title='Numero de Variables Faltantes') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  ## Pregunta 5
  
  cancer$diagnosis <- as.numeric(cancer$diagnosis) # Se convierte en variable numerica
  cancer$diagnosis[cancer$diagnosis == 2] <- "Maligno" # Se cambia el valor 
  cancer$diagnosis[cancer$diagnosis == 1] <- "Benigno" # Se cambia el valor
  cancer$diagnosis <- as.factor(cancer$diagnosis) # Se convierte nueva mente en factor
  str(cancer$diagnosis)
  
  ## Pregunta 6
  
  # Porcentaje de personas con diagnostico maligno
  round(((sum(cancer$diagnosis=="Maligno"))/(sum(cancer$diagnosis=="Maligno")+
                                               sum(cancer$diagnosis=="Benigno")))*100 , 2) 
  # Porcentaje de personas con diagnostico benigno
  round(((sum(cancer$diagnosis=="Benigno"))/(sum(cancer$diagnosis=="Maligno")+
                                               sum(cancer$diagnosis=="Benigno")))*100 , 2) 
  
  #Grafica de B vs M
  cancer %>%
    ggplot() +
    geom_bar(aes(x= cancer$diagnosis), fill= c("brown","steelblue")) + 
    labs(x='Diagnosis', y="Numero de Personas", title='Graficas B vs M') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  #Paso 3 
  
  ## Pregunta 7
  
  #Remplazo de los datos Na por la media 
  cancer$radius_mean[is.na(cancer$radius_mean)] <- mean(cancer$radius_mean, na.rm = TRUE)
  cancer$texture_mean[is.na(cancer$texture_mean)] <- mean(cancer$texture_mean, na.rm = TRUE)
  cancer$perimeter_mean[is.na(cancer$perimeter_mean)] <- mean(cancer$perimeter_mean, na.rm = TRUE)
  cancer$area_mean[is.na(cancer$area_mean)] <- mean(cancer$area_mean, na.rm = TRUE)
  cancer$smoothness_mean[is.na(cancer$smoothness_mean)] <- mean(cancer$smoothness_mean, na.rm = TRUE)
  cancer$compactness_mean[is.na(cancer$compactness_mean)] <- mean(cancer$compactness_mean, na.rm = TRUE)
  cancer$concavity_mean[is.na(cancer$concavity_mean)] <- mean(cancer$concavity_mean, na.rm = TRUE)
  cancer$concave.points_mean[is.na(cancer$concave.points_mean)] <- mean(cancer$concave.points_mean, na.rm = TRUE)
  cancer$symmetry_mean[is.na(cancer$symmetry_mean)] <- mean(cancer$symmetry_mean, na.rm = TRUE)
  cancer$fractal_dimension_mean[is.na(cancer$fractal_dimension_mean)] <- mean(cancer$fractal_dimension_mean, na.rm = TRUE)
  cancer$radius_se[is.na(cancer$radius_se)] <- mean(cancer$radius_se, na.rm = TRUE)
  cancer$texture_se[is.na(cancer$texture_se)] <- mean(cancer$texture_se, na.rm = TRUE)
  cancer$perimeter_se[is.na(cancer$perimeter_se)] <- mean(cancer$perimeter_se, na.rm = TRUE)
  cancer$area_se[is.na(cancer$area_se)] <- mean(cancer$area_se, na.rm = TRUE)
  cancer$smoothness_se[is.na(cancer$smoothness_se)] <- mean(cancer$smoothness_se, na.rm = TRUE)
  cancer$compactness_se[is.na(cancer$compactness_se)] <- mean(cancer$compactness_se, na.rm = TRUE)
  cancer$concavity_se[is.na(cancer$concavity_se)] <- mean(cancer$concavity_se, na.rm = TRUE)
  cancer$concave.points_se[is.na(cancer$concave.points_se)] <- mean(cancer$concave.points_se, na.rm = TRUE)
  cancer$symmetry_se[is.na(cancer$symmetry_se)] <- mean(cancer$symmetry_se, na.rm = TRUE)
  cancer$fractal_dimension_se[is.na(cancer$fractal_dimension_se)] <- mean(cancer$fractal_dimension_se, na.rm = TRUE)
  cancer$radius_worst[is.na(cancer$radius_worst)] <- mean(cancer$radius_worst, na.rm = TRUE)
  cancer$texture_worst[is.na(cancer$texture_worst)] <- mean(cancer$texture_worst, na.rm = TRUE)
  cancer$perimeter_worst[is.na(cancer$perimeter_worst)] <- mean(cancer$perimeter_worst, na.rm = TRUE)
  cancer$area_worst[is.na(cancer$area_worst)] <- mean(cancer$area_worst, na.rm = TRUE)
  cancer$smoothness_worst[is.na(cancer$smoothness_worst)] <- mean(cancer$smoothness_worst, na.rm = TRUE)
  cancer$compactness_worst[is.na(cancer$compactness_worst)] <- mean(cancer$compactness_worst, na.rm = TRUE)
  cancer$concave.points_worst[is.na(cancer$concave.points_worst)] <- mean(cancer$concave.points_worst, na.rm = TRUE)
  cancer$fractal_dimension_worst[is.na(cancer$fractal_dimension_worst)] <- mean(cancer$fractal_dimension_worst, na.rm = TRUE)
  summary(cancer)
  
  # Pregunta 8
  
  # Distribucion de los datos antes de eliminar los outlider 
  
  ## distribucion de los datos 
  cancer %>%
    ggplot( aes(x=cancer$diagnosis, y=cancer$radius_mean)) +
    geom_boxplot(fill="slateblue", alpha=0.2) +
    geom_jitter(color="black", size=0.8, alpha=0.9) +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle("Boxplot Radius Mean") +
    xlab("")
  
   ## eliminacion de out por variables 
  
  boxplot.stats(cancer$radius_mean) # out encima de 22.271
  cancer$radius_mean[cancer$radius_mean > 22.270]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  cancer$radius_mean[cancer$radius_mean > 21.370]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  cancer$radius_mean[cancer$radius_mean > 20.940]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  cancer$radius_mean[cancer$radius_mean > 20.730]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  cancer$radius_mean[cancer$radius_mean > 20.640]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  cancer$radius_mean[cancer$radius_mean > 20.590]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  cancer$radius_mean[cancer$radius_mean > 20.510]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  cancer$radius_mean[cancer$radius_mean > 20.340]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  cancer$radius_mean[cancer$radius_mean > 19.810]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  cancer$radius_mean[cancer$radius_mean > 19.690]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  cancer$radius_mean[cancer$radius_mean > 19.550]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  cancer$radius_mean[cancer$radius_mean > 19.440]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  cancer$radius_mean[cancer$radius_mean > 19.020]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  cancer$radius_mean[cancer$radius_mean > 18.490]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  cancer$radius_mean[cancer$radius_mean < 8.1960]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  cancer$radius_mean[cancer$radius_mean > 17.930]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  cancer$radius_mean[cancer$radius_mean > 17.750]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  cancer$radius_mean[cancer$radius_mean < 8.5970]<- mean(cancer$radius_mean)
  boxplot.stats(cancer$radius_mean)
  
  # Distribucion de los datos despues de eliminar los outlider 
  
  ## distribucion de los datos 
  cancer %>%
    ggplot( aes(x=cancer$diagnosis, y=cancer$radius_mean)) +
    geom_boxplot(fill="slateblue", alpha=0.2) +
    geom_jitter(color="black", size=0.8, alpha=0.9) +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle("Boxplot Radius Mean") +
    xlab("")
  
  
  boxplot.stats(cancer$texture_mean) # out encima de 29.290
  cancer$texture_mean[cancer$texture_mean > 29.290]<- mean(cancer$texture_mean)
  boxplot.stats(cancer$texture_mean) 
  
  
  boxplot.stats(cancer$perimeter_mean) # out encima de 147.300
  cancer$perimeter_mean[cancer$perimeter_mean > 147.300]<- mean(cancer$perimeter_mean)
  boxplot.stats(cancer$perimeter_mean)
  cancer$perimeter_mean[cancer$perimeter_mean > 142.700]<- mean(cancer$perimeter_mean)
  boxplot.stats(cancer$perimeter_mean)
  cancer$perimeter_mean[cancer$perimeter_mean > 141.300]<- mean(cancer$perimeter_mean)
  boxplot.stats(cancer$perimeter_mean)
  
  
  boxplot.stats(cancer$area_mean) # out encima de 1335
  cancer$area_mean[cancer$area_mean > 1335]<- mean(cancer$area_mean)
  boxplot.stats(cancer$area_mean)
  cancer$area_mean[cancer$area_mean > 1174]<- mean(cancer$area_mean)
  boxplot.stats(cancer$area_mean)
  cancer$area_mean[cancer$area_mean > 1007]<- mean(cancer$area_mean)
  boxplot.stats(cancer$area_mean)
  cancer$area_mean[cancer$area_mean > 994]<- mean(cancer$area_mean)
  boxplot.stats(cancer$area_mean)
  cancer$area_mean[cancer$area_mean > 963.700]<- mean(cancer$area_mean)
  boxplot.stats(cancer$area_mean)
  cancer$area_mean[cancer$area_mean > 951.6]<- mean(cancer$area_mean)
  boxplot.stats(cancer$area_mean)
  
  
  boxplot.stats(cancer$smoothness_mean) # out encima de 0.140
  cancer$smoothness_mean[cancer$smoothness_mean > 0.13600]<- mean(cancer$smoothness_mean)
  boxplot.stats(cancer$smoothness_mean)
  cancer$smoothness_mean[cancer$smoothness_mean > 0.129100]<- mean(cancer$smoothness_mean)
  boxplot.stats(cancer$smoothness_mean)
  
  
  boxplot.stats(cancer$compactness_mean) #out encima de 0.229301
  cancer$compactness_mean[cancer$compactness_mean > 0.229300]<- mean(cancer$compactness_mean)
  boxplot.stats(cancer$compactness_mean)
  cancer$compactness_mean[cancer$compactness_mean > 0.223300]<- mean(cancer$compactness_mean)
  boxplot.stats(cancer$compactness_mean)
  cancer$compactness_mean[cancer$compactness_mean > 0.219000]<- mean(cancer$compactness_mean)
  boxplot.stats(cancer$compactness_mean)
  
  
  boxplot.stats(cancer$concavity_mean) # out encima de 0.28100
  cancer$concavity_mean[cancer$concavity_mean > 0.28100]<- mean(cancer$concavity_mean)
  boxplot.stats(cancer$concavity_mean)
  cancer$concavity_mean[cancer$concavity_mean > 0.25080]<- mean(cancer$concavity_mean)
  boxplot.stats(cancer$concavity_mean)
  cancer$concavity_mean[cancer$concavity_mean > 0.24170]<- mean(cancer$concavity_mean)
  boxplot.stats(cancer$concavity_mean)
  cancer$concavity_mean[cancer$concavity_mean > 0.231900]<- mean(cancer$concavity_mean)
  boxplot.stats(cancer$concavity_mean)
  
  boxplot.stats(cancer$concave.points_mean) # out encima de 0.150400
  cancer$concave.points_mean[cancer$concave.points_mean > 0.150400]<- mean(cancer$concave.points_mean)
  boxplot.stats(cancer$concave.points_mean)
  cancer$concave.points_mean[cancer$concave.points_mean > 0.141000]<- mean(cancer$concave.points_mean)
  boxplot.stats(cancer$concave.points_mean)
  cancer$concave.points_mean[cancer$concave.points_mean > 0.131000]<- mean(cancer$concave.points_mean)
  boxplot.stats(cancer$concave.points_mean)
  
  
  boxplot.stats(cancer$symmetry_mean) # out encima 0.24190
  cancer$symmetry_mean[cancer$symmetry_mean > 0.24190]<- mean(cancer$symmetry_mean)
  boxplot.stats(cancer$symmetry_mean)
  cancer$symmetry_mean[cancer$symmetry_mean > 0.24030]<- mean(cancer$symmetry_mean)
  boxplot.stats(cancer$symmetry_mean)
  cancer$symmetry_mean[cancer$symmetry_mean > 0.23980]<- mean(cancer$symmetry_mean)
  boxplot.stats(cancer$symmetry_mean)
  cancer$symmetry_mean[cancer$symmetry_mean > 0.23950]<- mean(cancer$symmetry_mean)
  boxplot.stats(cancer$symmetry_mean)
  cancer$symmetry_mean[cancer$symmetry_mean > 0.23840]<- mean(cancer$symmetry_mean)
  boxplot.stats(cancer$symmetry_mean)
  
  
  boxplot.stats(cancer$fractal_dimension_mean) #out encima de 0.077690
  cancer$fractal_dimension_mean[cancer$fractal_dimension_mean > 0.077690]<- mean(cancer$fractal_dimension_mean)
  boxplot.stats(cancer$fractal_dimension_mean)
  cancer$fractal_dimension_mean[cancer$fractal_dimension_mean > 0.075420]<- mean(cancer$fractal_dimension_mean)
  boxplot.stats(cancer$fractal_dimension_mean)
  cancer$fractal_dimension_mean[cancer$fractal_dimension_mean > 0.074690]<- mean(cancer$fractal_dimension_mean)
  boxplot.stats(cancer$fractal_dimension_mean)
  cancer$fractal_dimension_mean[cancer$fractal_dimension_mean > 0.074510]<- mean(cancer$fractal_dimension_mean)
  boxplot.stats(cancer$fractal_dimension_mean)
  
  
  boxplot.stats(cancer$radius_se) # out encima de 0.88110
  cancer$radius_se[cancer$radius_se > 0.88110]<- mean(cancer$radius_se)
  boxplot.stats(cancer$radius_se)
  cancer$radius_se[cancer$radius_se > 0.71280]<- mean(cancer$radius_se)
  boxplot.stats(cancer$radius_se)
  cancer$radius_se[cancer$radius_se > 0.6643000]<- mean(cancer$radius_se)
  boxplot.stats(cancer$radius_se)
  
  boxplot.stats(cancer$texture_se) #out encima de 2.3420
  cancer$texture_se[cancer$texture_se > 2.3420]<- mean(cancer$texture_se)
  boxplot.stats(cancer$texture_se)
  cancer$texture_se[cancer$texture_se > 2.2200]<- mean(cancer$texture_se)
  boxplot.stats(cancer$texture_se)
  cancer$texture_se[cancer$texture_se > 2.2000]<- mean(cancer$texture_se)
  boxplot.stats(cancer$texture_se)
  cancer$texture_se[cancer$texture_se > 2.1880]<- mean(cancer$texture_se)
  boxplot.stats(cancer$texture_se)
  cancer$texture_se[cancer$texture_se > 2.1740]<- mean(cancer$texture_se)
  boxplot.stats(cancer$texture_se)
  cancer$texture_se[cancer$texture_se > 2.1290]<- mean(cancer$texture_se)
  boxplot.stats(cancer$texture_se)
  
  boxplot.stats(cancer$perimeter_se) #out encima de 6.0510
  cancer$perimeter_se[cancer$perimeter_se > 6.0510]<- mean(cancer$perimeter_se)
  boxplot.stats(cancer$perimeter_se)
  cancer$perimeter_se[cancer$perimeter_se > 5.0290]<- mean(cancer$perimeter_se)
  boxplot.stats(cancer$perimeter_se)
  cancer$perimeter_se[cancer$perimeter_se > 4.78200]<- mean(cancer$perimeter_se)
  boxplot.stats(cancer$perimeter_se)
  
  
  boxplot.stats(cancer$area_se) #out encima de 89.740
  cancer$area_se[cancer$area_se > 89.740]<- mean(cancer$area_se)
  boxplot.stats(cancer$area_se)
  cancer$area_se[cancer$area_se > 75.09000]<- mean(cancer$area_se)
  boxplot.stats(cancer$area_se)
  
  
  boxplot.stats(cancer$smoothness_se) #out encima de 0.0119300
  cancer$smoothness_se[cancer$smoothness_se > 0.0119300]<- mean(cancer$smoothness_se)
  boxplot.stats(cancer$smoothness_se)
  cancer$smoothness_se[cancer$smoothness_se > 0.0109800]<- mean(cancer$smoothness_se)
  boxplot.stats(cancer$smoothness_se)
  cancer$smoothness_se[cancer$smoothness_se > 0.0107500]<- mean(cancer$smoothness_se)
  boxplot.stats(cancer$smoothness_se)
  cancer$smoothness_se[cancer$smoothness_se > 0.0106100]<- mean(cancer$smoothness_se)
  boxplot.stats(cancer$smoothness_se)
  cancer$smoothness_se[cancer$smoothness_se < 0.001714]<- mean(cancer$smoothness_se)
  boxplot.stats(cancer$smoothness_se)
  cancer$smoothness_se[cancer$smoothness_se > 0.0105600]<- mean(cancer$smoothness_se)
  boxplot.stats(cancer$smoothness_se)
  
  
  boxplot.stats(cancer$compactness_se) #out encima de 0.06630
  cancer$compactness_se[cancer$compactness_se > 0.060630]<- mean(cancer$compactness_se)
  boxplot.stats(cancer$compactness_se)
  cancer$compactness_se[cancer$compactness_se > 0.054700]<- mean(cancer$compactness_se)
  boxplot.stats(cancer$compactness_se)
  cancer$compactness_se[cancer$compactness_se > 0.051560]<- mean(cancer$compactness_se)
  boxplot.stats(cancer$compactness_se)
  cancer$compactness_se[cancer$compactness_se > 0.050570]<- mean(cancer$compactness_se)
  boxplot.stats(cancer$compactness_se)
  cancer$compactness_se[cancer$compactness_se > 0.049600]<- mean(cancer$compactness_se)
  boxplot.stats(cancer$compactness_se)
  
  
  boxplot.stats(cancer$concavity_se) #out encima de 0.082320
  cancer$concavity_se[cancer$concavity_se > 0.08230]<- mean(cancer$concavity_se)
  boxplot.stats(cancer$concavity_se)
  cancer$concavity_se[cancer$concavity_se > 0.068990]<- mean(cancer$concavity_se)
  boxplot.stats(cancer$concavity_se)
  cancer$concavity_se[cancer$concavity_se > 0.065770]<- mean(cancer$concavity_se)
  boxplot.stats(cancer$concavity_se)
  cancer$concavity_se[cancer$concavity_se > 0.063890]<- mean(cancer$concavity_se)
  boxplot.stats(cancer$concavity_se)
  cancer$concavity_se[cancer$concavity_se > 0.063290]<- mean(cancer$concavity_se)
  boxplot.stats(cancer$concavity_se)
  cancer$concavity_se[cancer$concavity_se > 0.062710]<- mean(cancer$concavity_se)
  boxplot.stats(cancer$concavity_se)
  cancer$concavity_se[cancer$concavity_se > 0.061650]<- mean(cancer$concavity_se)
  boxplot.stats(cancer$concavity_se)
  
  
  boxplot.stats(cancer$concave.points_se) #out encima de 0.024800
  cancer$concave.points_se[cancer$concave.points_se > 0.024800]<- mean(cancer$concave.points_se)
  boxplot.stats(cancer$concave.points_se)
  cancer$concave.points_se[cancer$concave.points_se > 0.023110]<- mean(cancer$concave.points_se)
  boxplot.stats(cancer$concave.points_se)
  cancer$concave.points_se[cancer$concave.points_se > 0.022580]<- mean(cancer$concave.points_se)
  boxplot.stats(cancer$concave.points_se)
  cancer$concave.points_se[cancer$concave.points_se > 0.022340]<- mean(cancer$concave.points_se)
  boxplot.stats(cancer$concave.points_se)
  cancer$concave.points_se[cancer$concave.points_se > 0.02215000]<- mean(cancer$concave.points_se)
  boxplot.stats(cancer$concave.points_se)
  
  boxplot.stats(cancer$symmetry_se) #out encima de 0.035460
  cancer$symmetry_se[cancer$symmetry_se > 0.035460]<- mean(cancer$symmetry_se)
  boxplot.stats(cancer$symmetry_se)
  cancer$symmetry_se[cancer$symmetry_se > 0.032320]<- mean(cancer$symmetry_se)
  boxplot.stats(cancer$symmetry_se)
  cancer$symmetry_se[cancer$symmetry_se > 0.031270]<- mean(cancer$symmetry_se)
  boxplot.stats(cancer$symmetry_se)
  cancer$symmetry_se[cancer$symmetry_se > 0.030030]<- mean(cancer$symmetry_se)
  boxplot.stats(cancer$symmetry_se)
  cancer$symmetry_se[cancer$symmetry_se > 0.02951]<- mean(cancer$symmetry_se)
  boxplot.stats(cancer$symmetry_se)
  cancer$symmetry_se[cancer$symmetry_se > 0.02897]<- mean(cancer$symmetry_se)
  boxplot.stats(cancer$symmetry_se)
  
  boxplot.stats(cancer$fractal_dimension_se) #out encima de 0.0077310
  cancer$fractal_dimension_se[cancer$fractal_dimension_se > 0.0077310]<- mean(cancer$fractal_dimension_se)
  boxplot.stats(cancer$fractal_dimension_se)
  cancer$fractal_dimension_se[cancer$fractal_dimension_se > 0.0068840]<- mean(cancer$fractal_dimension_se)
  boxplot.stats(cancer$fractal_dimension_se)
  cancer$fractal_dimension_se[cancer$fractal_dimension_se > 0.0065170]<- mean(cancer$fractal_dimension_se)
  boxplot.stats(cancer$fractal_dimension_se)
  cancer$fractal_dimension_se[cancer$fractal_dimension_se > 0.0063550]<- mean(cancer$fractal_dimension_se)
  boxplot.stats(cancer$fractal_dimension_se)
  
  boxplot.stats(cancer$radius_worst) #out encima de 26.730 y por debajo de 7.930
  cancer$radius_worst[cancer$radius_worst > 26.730] <- mean(cancer$radius_worst)
  boxplot.stats(cancer$radius_worst)
  cancer$radius_worst[cancer$radius_worst < 7.930] <- mean(cancer$radius_worst)
  boxplot.stats(cancer$radius_worst)
  cancer$radius_worst[cancer$radius_worst > 24.560] <- mean(cancer$radius_worst)
  boxplot.stats(cancer$radius_worst)
  cancer$radius_worst[cancer$radius_worst > 22.750] <- mean(cancer$radius_worst)
  boxplot.stats(cancer$radius_worst)
  cancer$radius_worst[cancer$radius_worst > 20.880] <- mean(cancer$radius_worst)
  boxplot.stats(cancer$radius_worst)
  cancer$radius_worst[cancer$radius_worst > 19.96000] <- mean(cancer$radius_worst)
  boxplot.stats(cancer$radius_worst)
  cancer$radius_worst[cancer$radius_worst < 9.41400] <- mean(cancer$radius_worst)
  boxplot.stats(cancer$radius_worst)
  cancer$radius_worst[cancer$radius_worst > 19.2800] <- mean(cancer$radius_worst)
  boxplot.stats(cancer$radius_worst)
  cancer$radius_worst[cancer$radius_worst < 9.56500] <- mean(cancer$radius_worst)
  boxplot.stats(cancer$radius_worst)
  cancer$radius_worst[cancer$radius_worst < 9.62800] <- mean(cancer$radius_worst)
  boxplot.stats(cancer$radius_worst)
  cancer$radius_worst[cancer$radius_worst < 9.69900] <- mean(cancer$radius_worst)
  boxplot.stats(cancer$radius_worst)
  cancer$radius_worst[cancer$radius_worst > 19.200] <- mean(cancer$radius_worst)
  boxplot.stats(cancer$radius_worst)
  
  boxplot.stats(cancer$texture_worst) #out encima de 40.68 y de bajo de 12.0200
  cancer$texture_worst[cancer$texture_worst > 40.6800]<- mean(cancer$texture_worst)
  boxplot.stats(cancer$texture_worst)
  cancer$texture_worst[cancer$texture_worst < 12.0200]<- mean(cancer$texture_worst)
  boxplot.stats(cancer$texture_worst)
  
  boxplot.stats(cancer$perimeter_worst) #out encima de 186.80 y de bajo 50.41
  cancer$perimeter_worst[cancer$perimeter_worst > 186.80]<- mean(cancer$perimeter_worst)
  boxplot.stats(cancer$perimeter_worst)
  cancer$perimeter_worst[cancer$perimeter_worst < 50.41]<- mean(cancer$perimeter_worst)
  boxplot.stats(cancer$perimeter_worst)
  cancer$perimeter_worst[cancer$perimeter_worst > 174.90]<- mean(cancer$perimeter_worst)
  boxplot.stats(cancer$perimeter_worst)
  cancer$perimeter_worst[cancer$perimeter_worst > 166.80]<- mean(cancer$perimeter_worst)
  boxplot.stats(cancer$perimeter_worst)
  cancer$perimeter_worst[cancer$perimeter_worst > 160.50]<- mean(cancer$perimeter_worst)
  boxplot.stats(cancer$perimeter_worst)
  cancer$perimeter_worst[cancer$perimeter_worst > 153.90]<- mean(cancer$perimeter_worst)
  boxplot.stats(cancer$perimeter_worst)
  cancer$perimeter_worst[cancer$perimeter_worst > 146.60]<- mean(cancer$perimeter_worst)
  boxplot.stats(cancer$perimeter_worst)
  cancer$perimeter_worst[cancer$perimeter_worst > 136.10]<- mean(cancer$perimeter_worst)
  boxplot.stats(cancer$perimeter_worst)
  cancer$perimeter_worst[cancer$perimeter_worst > 132.900]<- mean(cancer$perimeter_worst)
  boxplot.stats(cancer$perimeter_worst)
  cancer$perimeter_worst[cancer$perimeter_worst < 56.65]<- mean(cancer$perimeter_worst)
  boxplot.stats(cancer$perimeter_worst)
  
  boxplot.stats(cancer$area_worst) #out encima de 2089.00
  cancer$area_worst[cancer$area_worst > 2089]<- mean(cancer$area_worst)
  boxplot.stats(cancer$area_worst)
  cancer$area_worst[cancer$area_worst > 1688]<- mean(cancer$area_worst)
  boxplot.stats(cancer$area_worst)
  cancer$area_worst[cancer$area_worst > 1437]<- mean(cancer$area_worst)
  boxplot.stats(cancer$area_worst)
  cancer$area_worst[cancer$area_worst > 1292]<- mean(cancer$area_worst)
  boxplot.stats(cancer$area_worst)
  cancer$area_worst[cancer$area_worst < 185.2]<- mean(cancer$area_worst)
  boxplot.stats(cancer$area_worst)
  cancer$area_worst[cancer$area_worst > 1272]<- mean(cancer$area_worst)
  boxplot.stats(cancer$area_worst)
  
  boxplot.stats(cancer$smoothness_worst) #out encima de 0.18830
  cancer$smoothness_worst[cancer$smoothness_worst > 0.18830]<- mean(cancer$smoothness_worst)
  boxplot.stats(cancer$smoothness_worst)
  cancer$smoothness_worst[cancer$smoothness_worst < 0.08125]<- mean(cancer$smoothness_worst)
  boxplot.stats(cancer$smoothness_worst)
  cancer$smoothness_worst[cancer$smoothness_worst > 0.18730]<- mean(cancer$smoothness_worst)
  boxplot.stats(cancer$smoothness_worst)
  cancer$smoothness_worst[cancer$smoothness_worst > 0.18620]<- mean(cancer$smoothness_worst)
  boxplot.stats(cancer$smoothness_worst)
  
  boxplot.stats(cancer$compactness_worst) #out encima de 0.62470
  cancer$compactness_worst[cancer$compactness_worst > 0.62470]<- mean(cancer$compactness_worst)
  boxplot.stats(cancer$compactness_worst)
  cancer$smoothness_worst[cancer$smoothness_worst > 0.57750]<- mean(cancer$smoothness_worst)
  boxplot.stats(cancer$smoothness_worst)
  
  boxplot.stats(cancer$concavity_worst) #out encima de 0.78920
  cancer$concavity_worst[cancer$concavity_worst > 0.78920]<- mean(cancer$concavity_worst)
  boxplot.stats(cancer$concavity_worst)
  cancer$concavity_worst[cancer$concavity_worst > 0.7727]<- mean(cancer$concavity_worst)
  boxplot.stats(cancer$concavity_worst)
  
  boxplot.stats(cancer$concave.points_worst) #no tiene out
  
  boxplot.stats(cancer$symmetry_worst) #out encima de 0.4228
  cancer$symmetry_worst[cancer$symmetry_worst > 0.4228]<- mean(cancer$symmetry_worst)
  boxplot.stats(cancer$symmetry_worst)
  cancer$symmetry_worst[cancer$symmetry_worst > 0.40450]<- mean(cancer$symmetry_worst)
  boxplot.stats(cancer$symmetry_worst)
  cancer$symmetry_worst[cancer$symmetry_worst > 0.39930]<- mean(cancer$symmetry_worst)
  boxplot.stats(cancer$symmetry_worst)
  cancer$symmetry_worst[cancer$symmetry_worst < 0.1648]<- mean(cancer$symmetry_worst)
  boxplot.stats(cancer$symmetry_worst)
  cancer$symmetry_worst[cancer$symmetry_worst > 0.39560]<- mean(cancer$symmetry_worst)
  boxplot.stats(cancer$symmetry_worst)
  cancer$symmetry_worst[cancer$symmetry_worst < 0.16520]<- mean(cancer$symmetry_worst)
  boxplot.stats(cancer$symmetry_worst)
  cancer$symmetry_worst[cancer$symmetry_worst < 0.17120]<- mean(cancer$symmetry_worst)
  boxplot.stats(cancer$symmetry_worst)
  
  boxplot.stats(cancer$fractal_dimension_worst) #out encima de 0.12240
  cancer$fractal_dimension_worst[cancer$fractal_dimension_worst > 0.12240]<- mean(cancer$fractal_dimension_worst)
  boxplot.stats(cancer$fractal_dimension_worst)
  cancer$fractal_dimension_worst[cancer$fractal_dimension_worst > 0.1155]<- mean(cancer$fractal_dimension_worst)
  boxplot.stats(cancer$fractal_dimension_worst)
  cancer$fractal_dimension_worst[cancer$fractal_dimension_worst > 0.1123]<- mean(cancer$fractal_dimension_worst)
  boxplot.stats(cancer$fractal_dimension_worst)
  cancer$fractal_dimension_worst[cancer$fractal_dimension_worst > 0.11080]<- mean(cancer$fractal_dimension_worst)
  boxplot.stats(cancer$fractal_dimension_worst)
  cancer$fractal_dimension_worst[cancer$fractal_dimension_worst < 0.05504]<- mean(cancer$fractal_dimension_worst)
  boxplot.stats(cancer$fractal_dimension_worst)
  cancer$fractal_dimension_worst[cancer$fractal_dimension_worst > 0.1094]<- mean(cancer$fractal_dimension_worst)
  boxplot.stats(cancer$fractal_dimension_worst)
  
  # Pregunta 9
  
  summary(cancer)
  
  ## Analisis de los datos filtrados 
  plot(cancer$radius_mean,cancer$texture_mean)
  plot(cancer$radius_mean,cancer$texture_mean, xlab='Radio', ylab='Textura',
       main='Analisis de datos', xlim=c(8,19), 
       ylim=c(9,31), pch=20, col='black')
  points(cancer$radius_mean[cancer$diagnosis=='Maligno'],
         cancer$texture_mean[cancer$diagnosis=='Maligno'], 
         pch=20, col='red')
  points(cancer$radius_mean[cancer$diagnosis=='Benigno'],
         cancer$texture_mean[cancer$diagnosis=='Benigno'], 
         pch=20, col='blue')
  
  # ya se han eliminados todos los outlider y no tiene datos NA en data set
  
  # Pregunta 10
  
  # a imputacion de los datos faltantes o nulo en el data set, si afecta la distribucion de los datos, ya que
  # van variando las medias de los valores en cada variable respecto al data set original 
  
  ## Paso 4
  
  #Pregunta 11
  
  # se elimina la variable id 
   cancer <- cancer %>%
    dplyr::select(-id)
   
   # Pregunta 12
   
   summary(cancer$area_mean)
   summary(cancer$symmetry_mean)
   
   # normalizacion de las variables con media de 0 y deviacion de 1
  cancer <- cancer %>% mutate_at (c ('radius_mean','texture_mean','perimeter_mean',
                                 'area_mean','smoothness_mean','compactness_mean', 
                                 'symmetry_mean','concavity_mean','concave.points_mean',
                                 'radius_se','texture_se','perimeter_se','area_se',
                                 'smoothness_se','compactness_se','concavity_se','concave.points_se',
                                 'symmetry_se','fractal_dimension_se','radius_worst','texture_worst',
                                 'perimeter_worst','area_worst','compactness_worst',
                                 'concave.points_worst','fractal_dimension_worst'), ~ ( scale (.)%>% as.vector ))
  summary(cancer) # analisis de los datos despues de la estandarizacion 
  sd(cancer$radius_mean) # observacion de la desviciancion 
  sd(cancer$texture_mean)
  
  #copia del data set estandarizado
  df <- cancer
  
  ## paso 5
  
  # correlacion de los datos 
  Correl <- cancer
  Correl <- Correl %>%
    dplyr::select(-diagnosis)

library(PerformanceAnalytics) # para usar chart.correlation, instalelo sino lo tiene.
chart.Correlation(cancer[,c(3:11)],histogram=TRUE, col="grey10", pch=1, main="Cancer Mean")

# Nos aseguramos que nuestro resultados sean repetibles
set.seed(5464)
names(cancer)
str(cancer)

# Medir la correlación entre dos variables 
cor(cancer$concavity_mean, cancer$concave.points_mean, use = 'complete.obs')

# Teniendo en cuenta la prueba de significancia y el valor de confianza 
cor.test(cancer$concavity_mean, cancer$concave.points_mean, use = 'complete.obs')

# Calculamos la matrix de correlación para todo el dataframe
correlationMatrix <- cor(Correl[,1:30])  # Remover el outcome

# Resumir la matrix de correlaciòn
print(correlationMatrix)

# encontrar variables que están altamente correlacionas (idealmente> 0,75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)

# Nombre de la variable(s) correlacionadas
cat("Variable(s) correlacionada(s):")
colnames(Correl[highlyCorrelated])

#variables correlacionada
#concave.points_worst 
#concavity_mean 
#concave.points_mean 
#compactness_mean 
#concavity_worst 
#concavity_se 
#area_se 
#perimeter_worst 
#area_worst 
#perimeter_se 
#area_mean 
#radius_worst 
#texture_mean

# Eliminar las variables que no tienen fuerte asociacion con el outcome ............
# Correlation Matrix     
correlationMatrix <- cor(cancer[,3:31])                              # Select only numerical Variables
hcorrelated       <- findCorrelation(correlationMatrix, cutoff=0.6)  # Threshold >0.6, Find Features that are highly corrected 
print(hcorrelated)                                                   # print indexes of highly correlated attributes
highly_cor_var    <- colnames(cancer[hcorrelated])                   # displaying highly correlated variables
data.frame(highly_cor_var)

#Variables correlacionada
#compactness_worst
# smoothness_mean
# compactness_mean
# area_mean
# smoothness_worst
# area_worst
# smoothness_se
# radius_mean
# compactness_se
# texture_se
# area_se
# adius_worst
# texture_worst
# radius_se
# symmetry_se
# perimeter_mean
# concavity_mean
# concave.points_worst
# fractal_dimension_se

#Pregunta 13

#Dividir data set

# Solo se selecciona las variables correlacionas para la prueba y evaluacion 
#cancer1 <- cancer %>%
 # dplyr::select(diagnosis, compactness_worst, smoothness_mean, compactness_mean,
  #              area_mean, smoothness_worst, area_worst, smoothness_se,
   #             radius_mean,compactness_se, texture_se,
    #            area_se, radius_worst, texture_worst, radius_se,
     #           symmetry_se, perimeter_mean, concavity_mean,
      #          concave.points_worst, fractal_dimension_se, texture_mean)

cancer1 <- cancer %>%
  dplyr::select(diagnosis, concave.points_worst, concavity_mean, concave.points_mean,
                compactness_mean, concavity_worst, concavity_se,
                area_se, perimeter_worst, area_worst, perimeter_se,
                area_mean, radius_worst, texture_mean)


set.seed(899)   # Valores aleatorios
inTrain <- createDataPartition(y=cancer$diagnosis, p = 0.80, list =FALSE)  # Split 80% y 20%
train   <- cancer[inTrain,]
test    <- cancer[-inTrain,]

set.seed(899)   # Valores aleatorios
inTrain2 <- createDataPartition(y=cancer1$diagnosis, p = 0.80, list =FALSE)  # Split 80% y 20%
train2   <- cancer1[inTrain2,]
test2    <- cancer1[-inTrain2,]

#Pregunta 14

#MODELOS LOGISTICA Y KNN 

# ProporciÃ³n de outcome en entrenamiento 
prop.table(table(train$diagnosis))
# ProporciÃ³n de Outcome en test
prop.table(table(test$diagnosis))

# Visualizar la distribucion de training y test
a1 <-ggplot(data=train,aes(x=diagnosis))+ 
  geom_bar() + 
  ggtitle('Distribucion de dataset de entrenamiento')+
  geom_text(stat='Count',aes(label=..count..),vjust=-0.4)
a1

a2 <- ggplot(data=test,aes(x=diagnosis))+
  geom_bar() + 
  ggtitle('Distribucion de dataset de validaciÃ³n')+
  geom_text(stat='Count',aes(label=..count..),vjust=-0.4)
a2

library("gridExtra")
grid.arrange(a1, a2, nrow=1)

#Para entrenar los modelos usamos el mÃ©todo Cross validation PUNTO15----
control <- trainControl(method = 'repeatedcv',number = 10,repeats = 3)

# Logistic Model -----------------------------------
set.seed(1156)
logFit <- train(diagnosis ~.,
                data = train,
                method = 'glm',
                preProc = c("center", "scale"),
                trControl = control)
logFit$results 

#MODELO KNN-----
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)

set.seed(445)
knnFit <- train(diagnosis ~.,
                data = train,
                method = "knn",
                preProc = c("center", "scale"),
                tuneGrid = data.frame(.k = 1:10),
                trControl = control1)
knnFit$results

##  Datos filtrado 
#Para entrenar los modelos usamos el mÃ©todo Cross validation PUNTO15----
control2 <- trainControl(method = 'repeatedcv',number = 10,repeats = 3)

# Logistic Model -----------------------------------
set.seed(1156)
logFit1 <- train(diagnosis ~.,
                 data = train2,
                 method = 'glm',
                 preProc = c("center", "scale"),
                 trControl = control2)
logFit1$results

#MODELO KNN-----
control3 <- trainControl(method="repeatedcv", number=10, repeats=3)

set.seed(445)
knnFit1 <- train(diagnosis ~.,
                 data = train2,
                 method = "knn",
                 preProc = c("center", "scale"),
                 tuneGrid = data.frame(.k = 1:10),
                 trControl = control3)
knnFit1$results

#################paso 6


## Validacion de los datos 

## validacion datos completos 

# Modelo logico

predictionglm <- predict(logFit, newdata = test)  # Probabilidad de que sea 1(no cumple)
predglm01     <- ifelse(predictionglm > 0.5,"Maligno" ,"Benigno")
predglm01     <- as.factor(predglm01)
length(predglm01)
confusionMatrix(predictionglm,test$diagnosis)

#Modelo KNN
predictionglm1 <- predict(knnFit, newdata = test)  # Probabilidad de que sea 1(no cumple)
predglm010     <- ifelse(predictionglm1 > 0.5,"Maligno" ,"Benigno")
predglm010     <- as.factor(predglm010)
length(predglm01)
confusionMatrix(predictionglm1,test$diagnosis)

## validacion datos completos  filtrado

# Modelo logico

predictionglm2 <- predict(logFit1, newdata = test2)  # Probabilidad de que sea 1(no cumple)
predglm012     <- ifelse(predictionglm2 > 0.5,"Maligno" ,"Benigno")
predglm012     <- as.factor(predglm02)
length(predglm02)
confusionMatrix(predictionglm2,test2$diagnosis)

#Modelo KNN
predictionglm3 <- predict(knnFit1, newdata = test2)  # Probabilidad de que sea 1(no cumple)
predglm013     <- ifelse(predictionglm3 > 0.5,"Maligno" ,"Benigno")
predglm013     <- as.factor(predglm013)
length(predglm03)
confusionMatrix(predictionglm3,test2$diagnosis)

# AUC and CI   .................................. 
library(pROC)

print(aucglm   <- auc(test$diagnosis,predictionglm))
print(ciaucglm <- ci.auc(test$diagnosis, predictionglm))
rocglm          <-roc(test$diagnosis,predictionglm)$auc

# Compute error  ................................
confusionmatrix  <- table(predglm01, 
                          test[,which(names(test) == "diagnosis")])
